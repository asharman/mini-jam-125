port module Main exposing (Msg(..), main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyPress, onResize)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Collision
import Color
import Config exposing (Config)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Obstacle exposing (Obstacle)
import Player exposing (Player)
import Random
import Types.Buffer as Buffer exposing (Buffer)
import Types.Canvas exposing (Canvas)
import Types.Timer as Timer exposing (Timer)



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
    | Enter
    | Other


fromString : String -> Key
fromString string =
    case String.toLower string of
        " " ->
            Space

        "enter" ->
            Enter

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
    , obstacleBuffer : Buffer Obstacle
    , obstacleSpawnTimer : Timer
    , spawnCount : Int
    , audioEnabled : Bool
    }


initialModel : Canvas -> Model
initialModel canvas =
    let
        config =
            Config.default
    in
    { player = Player.init ( 50, canvas.height / 2 )
    , obstacles = []
    , state = Menu
    , config = config
    , canvas = canvas
    , tempo = 1.0
    , obstacleBuffer = Buffer.empty .id
    , obstacleSpawnTimer = Timer.init config.obstacleSpawnFrequency
    , spawnCount = 0
    , audioEnabled = False
    }


increaseTempo : Model -> Model
increaseTempo model =
    { model | tempo = model.tempo * 1.02 }


decreaseTempo : Model -> Model
decreaseTempo model =
    { model | tempo = model.tempo * 0.98 }


incrementTimeElapsed : Float -> Model -> Model
incrementTimeElapsed deltaTime model =
    case model.state of
        Playing t ->
            { model | state = Playing (t + deltaTime) }

        _ ->
            model


collidedWithObstacle : Obstacle -> Model -> Model
collidedWithObstacle obstacle model =
    { model | obstacleBuffer = Buffer.insert obstacle model.obstacleBuffer }


tick : Float -> Model -> ( Model, Cmd Msg )
tick deltaTime model =
    let
        ( obstacleTimer, cmd ) =
            Timer.update deltaTime model.obstacleSpawnTimer
                |> Maybe.map (\t -> ( t, Cmd.none ))
                |> Maybe.withDefault
                    ( Timer.init model.config.obstacleSpawnFrequency
                    , newObstacle model.canvas model.spawnCount
                    )
    in
    ( { model
        | player = Player.update model.canvas model.config deltaTime model.player
        , obstacles = List.filterMap (Obstacle.update deltaTime) model.obstacles
        , obstacleBuffer = Buffer.update deltaTime model.obstacleBuffer
        , obstacleSpawnTimer = obstacleTimer
      }
        |> incrementTimeElapsed deltaTime
    , cmd
    )



-- Msg, Init, Update


type Msg
    = Frame Float
    | BrowserResized Int Int
    | KeyPress Key
    | GeneratedObstacle Obstacle
    | EnableAudioClicked
    | AudioToggled Bool


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
                    let
                        newModel =
                            initialModel model.canvas
                    in
                    if key == Enter then
                        ( { newModel | audioEnabled = model.audioEnabled }, Cmd.none )

                    else
                        ( model, Cmd.none )

        GeneratedObstacle obstacle ->
            case obstacle.variant of
                Obstacle.Rest ->
                    ( { model | spawnCount = model.spawnCount + 1 }
                    , audioEvent "obstacleSpawned" model
                    )

                _ ->
                    ( { model | obstacles = obstacle :: model.obstacles, spawnCount = model.spawnCount + 1 }
                    , audioEvent "obstacleSpawned" model
                    )

        EnableAudioClicked ->
            ( model, enableAudio () )

        AudioToggled bool ->
            ( { model | audioEnabled = bool }, Cmd.none )



-- Helper Functions


audioEvent : String -> Model -> Cmd msg
audioEvent message model =
    audioMsg
        { message = message
        , tempo = model.tempo
        , spawns = model.spawnCount
        }


newObstacle : Canvas -> Int -> Cmd Msg
newObstacle canvas id =
    case modBy 2 id of
        0 ->
            Random.generate GeneratedObstacle <| Obstacle.new id ( canvas.width, canvas.height / 2 ) Obstacle.Wall

        1 ->
            Random.generate GeneratedObstacle <|
                Obstacle.randomObstacle id ( canvas.width, canvas.height / 2 )

        _ ->
            Cmd.none


handleCollision : (Model -> ( Model, Cmd Msg )) -> Model -> Obstacle -> ( Model, Cmd Msg )
handleCollision tickFn model obstacle =
    case obstacle.variant of
        Obstacle.Wall ->
            ( { model | state = GameOver (timeElapsed model.state) }
            , audioEvent "gameOver" model
            )

        Obstacle.TempoIncrease ->
            let
                ( tickedModel, cmds ) =
                    tickFn model
            in
            ( tickedModel
                |> (collidedWithObstacle obstacle >> increaseTempo)
            , Cmd.batch [ audioEvent "tempoIncrease" model, cmds ]
            )

        Obstacle.TempoDecrease ->
            let
                ( tickedModel, cmds ) =
                    tickFn model
            in
            ( tickedModel
                |> (collidedWithObstacle obstacle >> decreaseTempo)
            , Cmd.batch [ audioEvent "tempoDecrease" model, cmds ]
            )

        Obstacle.Rest ->
            let
                ( tickedModel, _ ) =
                    tickFn model
            in
            ( tickedModel
                |> collidedWithObstacle obstacle
            , Cmd.none
            )


processFrame : Model -> Float -> ( Model, Cmd Msg )
processFrame model deltaTime =
    List.filter (Collision.intersects model.player) model.obstacles
        |> List.head
        |> Maybe.map (handleCollision (tick deltaTime) model)
        |> Maybe.withDefault
            (tick deltaTime model)



-- View Code


view : Model -> Html Msg
view model =
    Html.div [ class "fixed inset-0 w-screen h-screen font-sans" ]
        [ Canvas.toHtml ( round model.canvas.width, round model.canvas.height ) [] <|
            case model.state of
                Menu ->
                    viewMenu model

                Playing _ ->
                    viewPlaying model

                GameOver _ ->
                    []
        , Html.div [ class "absolute inset-0" ]
            [ case model.state of
                Menu ->
                    viewMenuUi model

                Playing t ->
                    Html.div [ class "absolute top-1/2 left-0 text-xl py-10 px-3" ]
                        [ Html.p []
                            [ Html.text <| "Score: " ++ (t / 1000 |> round |> String.fromInt) ]
                        ]

                GameOver t ->
                    Html.div [ class "w-full h-full bg-blue-700 flex flex-col text-white justify-center items-center" ]
                        [ Html.div [ class "space-y-8" ]
                            [ Html.p [ class "text-9xl" ] [ Html.text ":(" ]
                            , Html.div [ class "space-y-4 text-xl" ]
                                [ Html.p [ class "text-2xl" ] [ Html.text <| "Score: " ++ (t / 1000 |> round |> String.fromInt) ]
                                , Html.p [] [ Html.text "Oh dear, it looks like you've lost everything" ]
                                , Html.p [] [ Html.text "Press [Enter] to start again" ]
                                ]
                            ]
                        ]
            ]
        ]


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


viewMenuUi : Model -> Html Msg
viewMenuUi model =
    Html.div [ class "text-black  flex flex-col justify-center items-center w-full h-full" ]
        [ Html.div [ class "bg-white p-4 space-y-6" ]
            [ Html.h1 [ class "text-3xl" ] [ Html.text "JUMPR" ]
            , Html.div [ class "space-y-3" ]
                [ Html.p [] [ Html.text "Jump over the White blocks to stay alive!" ]
                , Html.p [] [ Html.text "Press [SPACE] to jump" ]
                , Html.p [] [ Html.text "Touching Green blocks will increase the game's tempo" ]
                , Html.p [] [ Html.text "Touching Yellow blocks will decrease the game's tempo" ]
                ]
            , Html.div [ class "flex justify-center" ]
                [ Html.button
                    [ Html.Events.onClick EnableAudioClicked
                    , class "bg-blue-500 text-white px-5 py-3 disabled:opacity-60"
                    , Html.Attributes.disabled model.audioEnabled
                    ]
                    [ if model.audioEnabled then
                        Html.text "Audio Enabled"

                      else
                        Html.text "Enable Audio"
                    ]
                ]
            , Html.p [ class "text-center" ] [ Html.text "Press Any Key to Start!" ]
            ]
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
    , Canvas.group [] <|
        List.map Obstacle.view model.obstacles
    ]



-- Program


port audioMsg : { message : String, tempo : Float, spawns : Int } -> Cmd msg


port enableAudio : () -> Cmd msg


port audioEnabled : (Bool -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onResize BrowserResized
        , onKeyPress
            (Decode.map (KeyPress << fromString) (Decode.field "key" Decode.string))
        , audioEnabled AudioToggled
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
