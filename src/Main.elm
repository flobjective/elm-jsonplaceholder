module Main exposing (Post)

import Browser
import Html exposing (Html, div, h1, h2, section, text)
import Http
import Json.Decode exposing (Decoder, field, int, map4, string)


type alias Post =
    { id : Int
    , userId : Int
    , title : String
    , body : String
    }



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Model
    = Failure
    | Loading
    | Success (List Post)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "https://jsonplaceholder.typicode.com/posts"
        , expect = Http.expectJson GotPosts postsDecoder
        }
    )


postsDecoder : Decoder (List Post)
postsDecoder =
    Json.Decode.list postDecoder


postDecoder : Decoder Post
postDecoder =
    map4 Post
        (field "id" int)
        (field "userId" int)
        (field "title" string)
        (field "body" string)



-- UPDATE


type Msg
    = GotPosts (Result Http.Error (List Post))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotPosts result ->
            case result of
                Err _ ->
                    ( Failure, Cmd.none )

                Ok posts ->
                    ( Success posts, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewLoading : Bool -> Html Msg
viewLoading loading =
    if loading then
        div [] [ text "Loading " ]

    else
        div [] []


viewPosts : List Post -> Html Msg
viewPosts posts =
    div [] (List.map (\post -> section [] (viewPost post)) posts)


viewPost : Post -> List (Html Msg)
viewPost post =
    [ h2 [] [ text post.title ], div [] [ text post.body ] ]


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            viewLoading True

        Success posts ->
            div [] [ h1 [] [ text "Posts from https://jsonplaceholder.typicode.com/" ], viewPosts posts ]

        Failure ->
            div [] [ text "Failure" ]
