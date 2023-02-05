port module Main exposing (Msg(..), main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyPress, onResize)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Collision
import Color
import Config exposing (Config)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Obstacle exposing (Obstacle)
import Player exposing (Player)
import Random
import Types.Canvas exposing (Canvas)



-- GameMode & Score


type alias TimeElapsed =
    Float


type GameMode
    = Menu
    | Playing TimeElapsed
    | GameOver TimeElapsed


timeElapsed : GameMode -> TimeElapsed
timeElapsed gameMode =
    case gameMode of
        Playing n ->
            n

        GameOver n ->
            n

        Menu ->
            0



-- Key


type Key
    = Space
    | Other


fromString : String -> Key
fromString string =
    case String.toLower string of
        " " ->
            Space

        _ ->
            Other



-- Model


type alias Model =
    { player : Player
    , obstacles : List Obstacle
    , state : GameMode
    , config : Config
    , canvas : Canvas
    , tempo : Float
    }


initialModel : Canvas -> Model
initialModel canvas =
    { player = Player.init ( 50, canvas.height / 2 )
    , obstacles = []
    , state = Menu
    , config = Config.default
    , canvas = canvas
    , tempo = 1.0
    }


increaseTempo : Model -> Model
increaseTempo model =
    { model | tempo = model.tempo * 1.05 }


decreaseTempo : Model -> Model
decreaseTempo model =
    { model | tempo = model.tempo * 0.95 }


updatePlayer : Float -> Model -> Model
updatePlayer deltaTime model =
    { model | player = Player.update model.canvas model.config deltaTime model.player }


updateObstacles : Float -> Model -> Model
updateObstacles deltaTime model =
    { model | obstacles = List.filterMap (Obstacle.update deltaTime) model.obstacles }


incrementTimeElapsed : Float -> Model -> Model
incrementTimeElapsed deltaTime model =
    case model.state of
        Playing t ->
            { model | state = Playing (t + deltaTime) }

        _ ->
            model


tick : Float -> Model -> Model
tick deltaTime =
    updatePlayer deltaTime >> updateObstacles deltaTime



-- Msg, Init, Update


type Msg
    = Frame Float
    | BrowserResized Int Int
    | KeyPress Key
    | GeneratedObstacle Obstacle


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        defaultCanvas : Canvas
        defaultCanvas =
            { width = 1000, height = 1000 }

        canvasDecoder : Decoder Canvas
        canvasDecoder =
            Decode.list Decode.float
                |> Decode.andThen
                    (\twoElementList ->
                        case twoElementList of
                            w :: h :: _ ->
                                Decode.succeed { width = w, height = h }

                            _ ->
                                Decode.fail ""
                    )

        initialCanvas =
            Decode.decodeValue canvasDecoder flags
                |> Result.withDefault defaultCanvas
    in
    ( initialModel initialCanvas, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserResized w h ->
            ( { model | canvas = { width = toFloat w, height = toFloat h } }
            , Cmd.none
            )

        Frame deltaTime ->
            case model.state of
                Playing _ ->
                    let
                        scaledWithTempo =
                            deltaTime * model.tempo
                    in
                    processFrame model scaledWithTempo

                _ ->
                    ( model, Cmd.none )

        KeyPress key ->
            case model.state of
                Menu ->
                    ( { model | state = Playing 0 }
                    , audioEvent "gameStarted" model
                    )

                Playing _ ->
                    case key of
                        Space ->
                            if Player.canJump model.player then
                                ( { model | player = Player.jump model.player }
                                , audioEvent "playerJumped" model
                                )

                            else
                                ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                GameOver _ ->
                    if key /= Space then
                        ( initialModel model.canvas, Cmd.none )

                    else
                        ( model, Cmd.none )

        GeneratedObstacle obstacle ->
            ( { model | obstacles = obstacle :: model.obstacles }
            , audioEvent "obstacleSpawned" model
            )



-- Helper Functions


spawnCount : Float -> Config -> Int
spawnCount time { obstacleSpawnFrequency } =
    truncate <| time / obstacleSpawnFrequency


audioEvent : String -> Model -> Cmd msg
audioEvent message model =
    audioMsg
        { message = message
        , tempo = model.tempo
        , spawns =
            spawnCount (timeElapsed model.state) model.config
        }


newObstacle : Canvas -> Config -> TimeElapsed -> Float -> Cmd Msg
newObstacle canvas config t dt =
    let
        timeForNewObstacle =
            spawnCount (t + dt) config
                - spawnCount t config
    in
    if timeForNewObstacle > 0 then
        Random.generate GeneratedObstacle <|
            Obstacle.randomObstacle ( canvas.width, canvas.height / 2 )

    else
        Cmd.none


handleCollision : (Model -> Model) -> Model -> Obstacle -> ( Model, Cmd Msg )
handleCollision tickFn model obstacle =
    case obstacle.variant of
        Obstacle.Wall ->
            ( { model | state = GameOver (timeElapsed model.state) }
            , audioEvent "gameOver" model
            )

        Obstacle.TempoIncrease ->
            ( model
                |> (increaseTempo >> tickFn)
            , audioEvent "tempoIncrease" model
            )

        Obstacle.TempoDecrease ->
            ( model
                |> (decreaseTempo >> tickFn)
            , audioEvent "tempoDecrease" model
            )


processFrame : Model -> Float -> ( Model, Cmd Msg )
processFrame model deltaTime =
    let
        scaledDeltaTime =
            deltaTime * 0.1

        updatedModel =
            incrementTimeElapsed deltaTime model
    in
    List.filter (Collision.intersects model.player) model.obstacles
        |> List.head
        |> Maybe.map (handleCollision (tick scaledDeltaTime) updatedModel)
        |> Maybe.withDefault
            ( tick scaledDeltaTime updatedModel
            , newObstacle model.canvas model.config (timeElapsed model.state) deltaTime
            )



-- View Code


view : Model -> Html Msg
view model =
    Canvas.toHtml ( round model.canvas.width, round model.canvas.height ) [] <|
        case model.state of
            Menu ->
                viewMenu model

            Playing _ ->
                viewPlaying model

            GameOver _ ->
                viewGameOver model


viewMenu : Model -> List Renderable
viewMenu model =
    let
        canvas =
            model.canvas
    in
    [ Canvas.clear ( 0, 0 ) canvas.width canvas.height
    , shapes [ fill Color.black ]
        [ rect ( 0, 0 ) canvas.width canvas.height ]
    , text [ stroke Color.white ]
        ( canvas.width / 2, canvas.height / 2 - 50 )
        "Dodge the Blocks!"
    , text [ stroke Color.white ]
        ( canvas.width / 2, canvas.height / 2 - 30 )
        "Press any key to play."
    , Player.view model.player
    , Canvas.group [] <|
        List.map Obstacle.view model.obstacles
    ]


viewPlaying : Model -> List Renderable
viewPlaying model =
    let
        canvas =
            model.canvas
    in
    [ Canvas.clear ( 0, 0 ) canvas.width canvas.height
    , shapes [ fill Color.black ]
        [ rect ( 0, 0 ) canvas.width canvas.height ]
    , Player.view model.player
    , viewScore ( canvas.width / 2, canvas.height / 2 - 40 ) (timeElapsed model.state)
    , Canvas.group [] <|
        List.map Obstacle.view model.obstacles
    ]


viewGameOver : Model -> List Renderable
viewGameOver model =
    let
        canvas =
            model.canvas
    in
    [ Canvas.clear ( 0, 0 ) canvas.width canvas.height
    , shapes [ fill Color.black ]
        [ rect ( 0, 0 ) canvas.width canvas.height ]
    , text [ stroke Color.white ]
        ( canvas.width / 2, canvas.height / 2 - 50 )
        "You died!"
    , viewScore ( canvas.width / 2, canvas.height / 2 - 40 ) (timeElapsed model.state)
    , text [ stroke Color.white ]
        ( canvas.width / 2, canvas.height / 2 - 30 )
        "Press any non-space key to continue."
    , Player.view model.player
    , Canvas.group [] <|
        List.map Obstacle.view model.obstacles
    ]


viewScore : Point -> TimeElapsed -> Renderable
viewScore point score =
    text [ stroke Color.white ] point <|
        ("Score: " ++ String.fromInt (truncate ((score * score) * 0.5)))



-- Program


port audioMsg : { message : String, tempo : Float, spawns : Int } -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onResize BrowserResized
        , onKeyPress
            (Decode.map (KeyPress << fromString) (Decode.field "key" Decode.string))
        ]


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            subscriptions
        }
