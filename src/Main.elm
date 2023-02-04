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
    , obstacles : List Obstacle
    , obstacleCounter : Float
    }


type alias Player =
    { height : Float
    , velocity : Float
    , state : PlayerState
    }


type alias Obstacle =
    { position : Float
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
    ( Model width height initialPlayer [ Obstacle (width - 50) ] 0, Cmd.none )


view : Model -> Html Msg
view model =
    Canvas.toHtml ( round model.width, round model.height )
        []
        [ Canvas.clear ( 0, 0 ) model.width model.height
        , shapes [ fill Color.black ] [ rect ( 0, 0 ) model.width model.height ]
        , shapes [ fill Color.white ]
            [ Canvas.circle
                ( 50
                , (model.height / 2) + model.player.height
                )
                10
            ]
        , shapes [ fill Color.white ] <| List.map (\obstacle -> rect ( obstacle.position, (model.height / 2) - 12.5 ) 25 25) model.obstacles
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

                spawnNewObstacle =
                    model.obstacleCounter > 600

                movedObstacles =
                    model.obstacles
                        |> List.map (\obstacle -> { obstacle | position = obstacle.position - scaledDeltaTime * 5 })
                        |> List.filter isObstacleOnscreen

                obstacles =
                    if spawnNewObstacle then
                        Obstacle model.width :: movedObstacles

                    else
                        movedObstacles

                newObstacleCounter =
                    if spawnNewObstacle then
                        toFloat (modBy 600 (round model.obstacleCounter) + round deltaTime)

                    else
                        model.obstacleCounter + deltaTime
            in
            ( { model | player = updatePlayer model.player scaledDeltaTime, obstacles = obstacles, obstacleCounter = newObstacleCounter }, Cmd.none )

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


isObstacleOnscreen : Obstacle -> Bool
isObstacleOnscreen obstacle =
    obstacle.position + 25 >= 0


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
