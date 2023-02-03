module Main exposing (Msg(..), main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)


type alias Model =
    { width : Float
    , height : Float
    }


init : ( Float, Float ) -> ( Model, Cmd Msg )
init ( width, height ) =
    ( Model width height, Cmd.none )


view : Model -> Html msg
view { width, height } =
    Canvas.toHtml ( round width, round height )
        []
        [ shapes [ fill Color.black ] [ rect ( 0, 0 ) width height ] ]


type Msg
    = Frame Float
    | BrowserResized Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserResized w h ->
            let
                width =
                    toFloat w

                height =
                    toFloat h
            in
            ( { model
                | width = width
                , height = height
              }
            , Cmd.none
            )

        Frame deltaTime ->
            ( model, Cmd.none )


main : Program ( Float, Float ) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onAnimationFrameDelta Frame
                    , onResize BrowserResized
                    ]
        }
