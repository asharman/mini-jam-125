module Main exposing (Msg(..), main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyPress, onResize)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Json.Decode as Decode
import Obstacle exposing (Obstacle)
import Player exposing (Player)


type alias Model =
    { width : Float
    , height : Float
    , player : Player
    , obstacles : List Obstacle
    , obstacleCounter : Float
    }


init : ( Float, Float ) -> ( Model, Cmd Msg )
init ( width, height ) =
    ( Model width height Player.init [ Obstacle (width - 50) ] 0, Cmd.none )


view : Model -> Html Msg
view model =
    Canvas.toHtml ( round model.width, round model.height )
        []
        [ Canvas.clear ( 0, 0 ) model.width model.height
        , shapes [ fill Color.black ] [ rect ( 0, 0 ) model.width model.height ]
        , Player.view model.height model.player
        , Canvas.group [] <| List.map (Obstacle.view model.height) model.obstacles
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

                maybeNewObstacle =
                    newObstacle model deltaTime

                updatedCounter =
                    case maybeNewObstacle of
                        Just _ ->
                            toFloat (modBy 600 (round model.obstacleCounter) + round deltaTime)

                        Nothing ->
                            model.obstacleCounter + deltaTime

                obstacles =
                    maybeNewObstacle
                        |> Maybe.map (\o -> o :: model.obstacles)
                        |> Maybe.withDefault model.obstacles
                        |> List.filterMap (Obstacle.update scaledDeltaTime)
            in
            ( { model | player = Player.update scaledDeltaTime model.player, obstacles = obstacles, obstacleCounter = updatedCounter }, Cmd.none )

        KeyPress key ->
            let
                player =
                    model.player
            in
            if key == " " && player.state == Player.Running then
                ( { model
                    | player = { player | velocity = -15, state = Player.Jumping }
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


newObstacle : Model -> Float -> Maybe Obstacle
newObstacle model deltaTime =
    if (model.obstacleCounter + deltaTime) > 600 then
        Just <| Obstacle.init model.width

    else
        Nothing


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
