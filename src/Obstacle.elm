module Obstacle exposing (Obstacle, Variant(..), new, randomObstacle, update, view)

import Canvas exposing (Point, Renderable)
import Canvas.Settings as Settings
import Collision exposing (Hitbox(..))
import Color exposing (Color)
import Random exposing (Generator)


obstacleSpeed : number
obstacleSpeed =
    5


type Variant
    = Wall
    | TempoIncrease
    | TempoDecrease
    | Rest


type alias Obstacle =
    { position : Point
    , width : Float
    , height : Float
    , hitbox : Hitbox
    , variant : Variant
    , id : String
    }


init : String -> Point -> Variant -> Obstacle
init id point variant =
    { position = point
    , width = 25
    , height = 25
    , hitbox = hitbox point 25 25
    , variant = variant
    , id = id
    }


new : Int -> Point -> Variant -> Generator Obstacle
new id point variant =
    Random.map3 init
        (Random.constant <| String.fromInt id)
        (Random.constant point)
        (Random.constant variant)


randomObstacle : Int -> Point -> Generator Obstacle
randomObstacle spawnCount point =
    let
        restRate =
            toFloat (max (6 - (spawnCount // 12)) 1)
    in
    Random.map3 init
        (Random.constant <| String.fromInt spawnCount)
        (Random.constant point)
        (Random.weighted
            ( restRate, Rest )
            [ ( 3, TempoIncrease )
            , ( 1, TempoDecrease )
            ]
        )


hitbox : Point -> Float -> Float -> Hitbox
hitbox position width height =
    Rect (Tuple.mapBoth (\x -> x - (width / 2)) (\y -> y - (height / 2)) position) width height


update : Float -> Obstacle -> Maybe Obstacle
update deltaTime obstacle =
    let
        newPosition =
            Tuple.mapFirst (\x -> x - deltaTime * obstacleSpeed * 0.1) obstacle.position

        updatedObstacle =
            { obstacle
                | position = newPosition
                , hitbox = hitbox newPosition obstacle.width obstacle.height
            }
    in
    if isObstacleOnscreen updatedObstacle then
        Just updatedObstacle

    else
        Nothing


isObstacleOnscreen : Obstacle -> Bool
isObstacleOnscreen { position, width } =
    Tuple.first position |> (\x -> x + width >= 0)


view : Obstacle -> Renderable
view obstacle =
    case obstacle.variant of
        Wall ->
            viewWall obstacle

        TempoIncrease ->
            viewRect Color.green obstacle

        TempoDecrease ->
            viewRect Color.yellow obstacle

        Rest ->
            viewRect Color.black obstacle


viewRect : Color -> Obstacle -> Renderable
viewRect color obstacle =
    Canvas.shapes [ Settings.fill color ]
        [ Canvas.rect
            (Tuple.mapBoth
                (\x -> x - (obstacle.width / 2))
                (\y -> y - (obstacle.height / 2))
                obstacle.position
            )
            obstacle.width
            obstacle.height
        ]


viewWall : Obstacle -> Renderable
viewWall obstacle =
    Canvas.group []
        [ viewRect Color.white obstacle
        , Canvas.shapes
            [ Settings.fill Color.red ]
            [ Canvas.circle
                (Tuple.mapBoth
                    (\x -> x - (obstacle.width / 2) + 7)
                    (\y -> y - 3)
                    obstacle.position
                )
                4
            ]
        ]
