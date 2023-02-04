module Obstacle exposing (Obstacle, init, update, view)

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


view : Float -> Obstacle -> Renderable
view canvasHeight obstacle =
    Canvas.shapes [ Settings.fill Color.white ] [ Canvas.rect ( obstacle.position, (canvasHeight / 2) - 12.5 ) 25 25 ]
