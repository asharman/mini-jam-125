module Main exposing (Msg(..), main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyPress, onResize)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Config exposing (Config)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Obstacle exposing (Obstacle)
import Player exposing (Player)



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



-- Canvas


type alias Canvas =
    { width : Float, height : Float }



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
    { player = Player.init
    , obstacles = []
    , state = Menu
    , config = Config.default
    , canvas = canvas
    , tempo = 1.0
    }



-- Msg, Init, Update


type Msg
    = Frame Float
    | BrowserResized Int Int
    | KeyPress Key


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
            let
                newModel =
                    case model.state of
                        Playing _ ->
                            processFrame model (deltaTime * model.tempo)

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        KeyPress key ->
            let
                newModel =
                    case model.state of
                        Menu ->
                            { model | state = Playing 0 }

                        Playing _ ->
                            case key of
                                Space ->
                                    { model
                                        | player =
                                            Player.tryJump model.player
                                    }

                                _ ->
                                    model

                        GameOver _ ->
                            if key /= Space then
                                initialModel model.canvas

                            else
                                model
            in
            ( newModel, Cmd.none )



-- Helper Functions


newObstacle : Canvas -> Config -> TimeElapsed -> Float -> Maybe Obstacle
newObstacle canvas config t dt =
    let
        timeForNewObstacle =
            (truncate <| (t + dt) / config.obstacleSpawnFrequency)
                - (truncate <| t / config.obstacleSpawnFrequency)
    in
    if timeForNewObstacle > 0 then
        Just <| Obstacle.init canvas.width

    else
        Nothing


checkCollision : Player -> List Obstacle -> Bool
checkCollision player obstacles =
    List.any (Obstacle.intersectsPlayer player.height) obstacles


processFrame : Model -> Float -> Model
processFrame model deltaTime =
    let
        scaledDeltaTime =
            deltaTime * 0.1

        maybeNewObstacle =
            newObstacle model.canvas
                model.config
                (timeElapsed model.state)
                deltaTime

        newObstacles =
            maybeNewObstacle
                |> Maybe.map (\o -> o :: model.obstacles)
                |> Maybe.withDefault model.obstacles
                |> List.filterMap (Obstacle.update scaledDeltaTime)

        newState =
            case model.state of
                Playing n ->
                    Playing (n + deltaTime)

                _ ->
                    model.state
    in
    if checkCollision model.player model.obstacles then
        { model | state = GameOver (timeElapsed model.state) }

    else
        { model
            | player = Player.update model.config scaledDeltaTime model.player
            , obstacles = newObstacles
            , state = newState
            , tempo = model.tempo + model.config.tempoIncrement
        }



-- View Code


view : Model -> Html Msg
view model =
    Canvas.toHtml ( round model.canvas.width, round model.canvas.height )
        []
    <|
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
    , Player.view canvas.height model.player
    , Canvas.group [] <|
        List.map (Obstacle.view canvas.height) model.obstacles
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
    , Player.view
        canvas.height
        model.player
    , viewScore ( canvas.width / 2, canvas.height / 2 - 40 ) (timeElapsed model.state)
    , Canvas.group [] <|
        List.map
            (Obstacle.view canvas.height)
            model.obstacles
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
    , Player.view canvas.height model.player
    , Canvas.group [] <|
        List.map
            (Obstacle.view canvas.height)
            model.obstacles
    ]


viewScore : Point -> TimeElapsed -> Renderable
viewScore point score =
    text [ stroke Color.white ] point <|
        ("Score: " ++ String.fromInt (truncate ((score * score) * 0.5)))



-- Program


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
