module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { subreddit = "all", requestState = Loading }
    , fetchSubreddit "all" )


type alias Model =
    { subreddit : String
    , requestState : RequestState
    }


type RequestState
    = Loading
    | Failure Http.Error
    | Success Posts



-- Update


type Msg
    = Fetch
    | Receive (Result Http.Error Posts)
    | SetSubreddit String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( { model | requestState = Loading }
            , fetchSubreddit model.subreddit )

        Receive result ->
            case result of
                Ok posts ->
                    ( { model | requestState = Success posts }
                    , Cmd.none )

                Err error ->
                    ( { model | requestState = Failure error }
                    , Cmd.none )

        SetSubreddit subreddit ->
            ( { model | subreddit = subreddit }
            , Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Views


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput SetSubreddit ] []
        , button [ onClick Fetch ] [ text "Submit" ]
        , viewPosts model
        ]


viewPosts : Model -> Html Msg
viewPosts model =
    case model.requestState of
        Success posts ->
            div [] [ renderPosts posts ]

        Loading ->
            div [] [ text "Loading..." ]

        Failure _ ->
            div [] [ text "Invalid subreddit." ]


renderPosts : Posts -> Html Msg
renderPosts posts =
    posts.posts
        |> List.map (\post -> renderPost post)
        |> ol []


renderPost : Post -> Html Msg
renderPost post =
    li []
        [ a [ href post.source ] [ text post.title ]
        , br [] []
        , span []
            [ text (renderAuthorMeta post ++ " | ")
            , renderCommentMeta post
            ]
        ]


renderAuthorMeta : Post -> String
renderAuthorMeta post =
    "Author: " ++ post.author


renderCommentMeta : Post -> Html Msg
renderCommentMeta post =
    a [ href ("https://www.reddit.com" ++ post.permalink) ]
        [ text (String.fromInt post.comments ++ " comments")
        ]



-- Posts


type alias Posts =
    { posts : List Post }


postsDecoder : D.Decoder Posts
postsDecoder =
    D.map
        Posts
        (D.field "data" (D.field "children" (D.list (D.field "data" postDecoder))))


type alias Post =
    { title : String
    , author : String
    , source : String
    , permalink : String
    , ups : Int
    , downs : Int
    , comments : Int
    }


postDecoder : D.Decoder Post
postDecoder =
    D.map7
        Post
        (D.field "title" D.string)
        (D.field "author" D.string)
        (D.field "url" D.string)
        (D.field "permalink" D.string)
        (D.field "ups" D.int)
        (D.field "downs" D.int)
        (D.field "num_comments" D.int)


fetchSubreddit : String -> Cmd Msg
fetchSubreddit subreddit =
    Http.get
        { url = "https://www.reddit.com/r/" ++ subreddit ++ "/.json"
        , expect = Http.expectJson Receive postsDecoder
        }
