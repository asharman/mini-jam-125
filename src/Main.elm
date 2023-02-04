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


type GameMode
    = Menu
    | Playing Int
    | GameOver Int


type alias Model =
    { width : Float
    , height : Float
    , player : Player
    , obstacles : List Obstacle
    , obstacleCounter : Float
    , state : GameMode
    }


initialModel : ( Float, Float ) -> Model
initialModel ( width, height ) =
    Model width
        height
        Player.init
        [ Obstacle (width - 50) ]
        0
        Menu


getScore : Model -> Int
getScore model =
    case model.state of
        Playing n ->
            n

        GameOver n ->
            n

        Menu ->
            0


init : ( Float, Float ) -> ( Model, Cmd Msg )
init screenSize =
    ( initialModel screenSize, Cmd.none )


view : Model -> Html Msg
view model =
    Canvas.toHtml ( round model.width, round model.height )
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
    [ Canvas.clear ( 0, 0 ) model.width model.height
    , shapes [ fill Color.black ]
        [ rect ( 0, 0 ) model.width model.height ]
    , text [ stroke Color.white ]
        ( model.width / 2, model.height / 2 - 50 )
        "Dodge the Blocks!"
    , text [ stroke Color.white ]
        ( model.width / 2, model.height / 2 - 30 )
        "Press any key to play."
    , Player.view model.height model.player
    , Canvas.group [] <|
        List.map (Obstacle.view model.height) model.obstacles
    ]


viewPlaying : Model -> List Renderable
viewPlaying model =
    [ Canvas.clear ( 0, 0 ) model.width model.height
    , shapes [ fill Color.black ]
        [ rect ( 0, 0 ) model.width model.height ]
    , Player.view
        model.height
        model.player
    , viewScore model
    , Canvas.group [] <|
        List.map
            (Obstacle.view model.height)
            model.obstacles
    ]


viewGameOver : Model -> List Renderable
viewGameOver model =
    [ Canvas.clear ( 0, 0 ) model.width model.height
    , shapes [ fill Color.black ]
        [ rect ( 0, 0 ) model.width model.height ]
    , text [ stroke Color.white ]
        ( model.width / 2, model.height / 2 - 50 )
        "You died!"
    , viewScore model
    , text [ stroke Color.white ]
        ( model.width / 2, model.height / 2 - 30 )
        "Press any non-space key to continue."
    , Player.view model.height model.player
    , Canvas.group [] <|
        List.map
            (Obstacle.view model.height)
            model.obstacles
    ]


viewScore : Model -> Renderable
viewScore model =
    text [ stroke Color.white ]
        ( model.width / 2, model.height / 2 - 40 )
        ("Score: "
            ++ String.fromInt (getScore model)
        )


type Msg
    = Frame Float
    | BrowserResized Int Int
    | KeyPress Key


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
                newModel =
                    case model.state of
                        Playing _ ->
                            processFrame model deltaTime

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
                                initialModel
                                    ( model.width
                                    , model.height
                                    )

                            else
                                model
            in
            ( newModel, Cmd.none )


processFrame : Model -> Float -> Model
processFrame model deltaTime =
    let
        scaledDeltaTime =
            deltaTime * 0.1

        maybeNewObstacle =
            newObstacle model deltaTime

        updatedCounter =
            toFloat
                (modBy 1200
                    (round model.obstacleCounter)
                    + round deltaTime
                )

        newObstacles =
            maybeNewObstacle
                |> Maybe.map (\o -> o :: model.obstacles)
                |> Maybe.withDefault model.obstacles
                |> List.filterMap (Obstacle.update scaledDeltaTime)

        newState =
            case model.state of
                Playing n ->
                    Playing (n + round scaledDeltaTime)

                _ ->
                    model.state
    in
    if checkCollision model then
        { model | state = GameOver (getScore model) }

    else
        { model
            | player = Player.update scaledDeltaTime model.player
            , obstacles = newObstacles
            , state = newState
            , obstacleCounter = updatedCounter
        }


newObstacle : Model -> Float -> Maybe Obstacle
newObstacle model deltaTime =
    if (model.obstacleCounter + deltaTime) > 1200 then
        Just <| Obstacle.init model.width

    else
        Nothing


checkCollision : Model -> Bool
checkCollision model =
    List.any (Obstacle.intersectsPlayer model.player.height) model.obstacles


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onResize BrowserResized
        , onKeyPress
            (Decode.map toKeyPress (Decode.field "key" Decode.string))
        ]


type Key
    = Space
    | Other


toKeyPress : String -> Msg
toKeyPress keyEvent =
    case String.toLower keyEvent of
        " " ->
            KeyPress Space

        _ ->
            KeyPress Other


main : Program ( Float, Float ) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            subscriptions
        }
