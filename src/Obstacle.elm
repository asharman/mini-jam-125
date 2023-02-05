module Obstacle exposing (Obstacle, init, update, view)

import Canvas exposing (Point, Renderable)
import Canvas.Settings as Settings
import Collision exposing (Hitbox(..))
import Color


obstacleSpeed : number
obstacleSpeed =
    5


type alias Obstacle =
    { position : Point
    , width : Float
    , height : Float
    , hitbox : Hitbox
    }


init : Point -> Obstacle
init point =
    { position = point
    , width = 25
    , height = 25
    , hitbox = hitbox point
    }


hitbox : Point -> Hitbox
hitbox position =
    Rect (Tuple.mapBoth (\x -> x - 12.5) (\y -> y - 12.5) position) 25 25


update : Float -> Obstacle -> Maybe Obstacle
update deltaTime obstacle =
    let
        newPosition =
            Tuple.mapFirst (\x -> x - deltaTime * obstacleSpeed) obstacle.position

        updatedObstacle =
            { obstacle
                | position = newPosition
                , hitbox = hitbox newPosition
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
    Canvas.group []
        [ Canvas.shapes [ Settings.fill Color.white ]
            [ Canvas.rect
                (Tuple.mapBoth
                    (\x -> x - (obstacle.width / 2))
                    (\y -> y - (obstacle.height / 2))
                    obstacle.position
                )
                25
                25
            ]
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
