module Main exposing (Msg(..), main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events


type alias Model =
    { count : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { count = 0 }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "flex flex-col items-center" ]
        [ Html.h1 [ Attributes.class "text-4xl font-bold text-blue-500" ] [ Html.text "Vite and Elm starter" ]
        , Html.div [ Attributes.class "flex flex-col items-center" ]
            [ Html.button
                [ Events.onClick Increment
                , Attributes.class "border px-8 hover:bg-slate-200"
                ]
                [ Html.text "+1" ]
            , Html.div [] [ Html.text (String.fromInt model.count) ]
            , Html.button
                [ Events.onClick Decrement
                , Attributes.class "border px-8 hover:bg-slate-200"
                ]
                [ Html.text "-1" ]
            ]
        ]


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , view =
            \model ->
                { title = "elm-template"
                , body = [ view model ]
                }
        , subscriptions = \_ -> Sub.none
        }
