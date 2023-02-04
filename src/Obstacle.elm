module Obstacle exposing (Obstacle, atPlayer, init, intersectsPlayer, update, view)

import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Color


type alias Obstacle =
    { position : Float
    }


init : Float -> Obstacle
init =
    Obstacle


update : Float -> Obstacle -> Maybe Obstacle
update deltaTime obstacle =
    let
        updatedObstacle =
            { obstacle | position = obstacle.position - deltaTime * 5 }
    in
    if isObstacleOnscreen updatedObstacle then
        Just updatedObstacle

    else
        Nothing


isObstacleOnscreen : Obstacle -> Bool
isObstacleOnscreen obstacle =
    obstacle.position + 25 >= 0


atPlayer : Obstacle -> Bool
atPlayer obstacle =
    obstacle.position <= 75 && obstacle.position >= 25


intersectsPlayer : Float -> Obstacle -> Bool
intersectsPlayer playerPos obstacle =
    atPlayer obstacle
        && playerPos
        >= -25


view : Float -> Obstacle -> Renderable
view canvasHeight obstacle =
    Canvas.group []
        [ Canvas.shapes [ Settings.fill Color.white ] [ Canvas.rect ( obstacle.position, (canvasHeight / 2) - 12.5 ) 25 25 ]
        , Canvas.shapes
            [ Settings.fill Color.red ]
            [ Canvas.circle ( obstacle.position + 7, canvasHeight / 2 - 3 ) 4
            ]
        ]
