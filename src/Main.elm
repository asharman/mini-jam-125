module Main exposing (Msg(..), main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyPress, onResize)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (scale)
import Color
import Html exposing (Html)
import Json.Decode as Decode
import Vector exposing (Vector)


type alias Model =
    { width : Float
    , height : Float
    , player : Player
    }


type alias Player =
    { height : Float
    , velocity : Float
    , state : PlayerState
    }


type PlayerState
    = Running
    | Jumping


gravity : number
gravity =
    2


updatePlayer : Player -> Float -> Player
updatePlayer player deltaTime =
    let
        newHeight =
            min 0 <| player.height + (player.velocity * deltaTime)
    in
    { height = newHeight
    , velocity = player.velocity + gravity
    , state =
        if newHeight == 0 then
            Running

        else
            Jumping
    }


initialPlayer : Player
initialPlayer =
    { height = 0
    , velocity = 0
    , state = Running
    }


init : ( Float, Float ) -> ( Model, Cmd Msg )
init ( width, height ) =
    ( Model width height initialPlayer, Cmd.none )


view : Model -> Html Msg
view model =
    Canvas.toHtml ( round model.width, round model.height )
        []
        [ Canvas.clear ( 0, 0 ) model.width model.height
        , shapes [ fill Color.black ] [ rect ( 0, 0 ) model.width model.height ]
        , shapes [ fill Color.white ] [ Canvas.circle ( 50, (model.height / 2) + model.player.height ) 10 ]
        ]


type Msg
    = Frame Float
    | BrowserResized Int Int
    | KeyPress String


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
            let
                scaledDeltaTime =
                    deltaTime * 0.1
            in
            ( { model | player = updatePlayer model.player scaledDeltaTime }, Cmd.none )

        KeyPress key ->
            let
                player =
                    model.player
            in
            if key == " " && player.state == Running then
                ( { model
                    | player = { player | velocity = -15, state = Jumping }
                  }
                , Cmd.none
                )

            else
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
                    , onKeyPress
                        (Decode.map KeyPress (Decode.field "key" Decode.string))
                    ]
        }
