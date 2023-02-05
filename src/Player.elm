module Player exposing (..)

import Canvas exposing (Point, Renderable)
import Canvas.Settings as Settings
import Collision exposing (Hitbox(..))
import Color
import Config exposing (Config)
import Types.Canvas exposing (Canvas)


type alias Player =
    { position : Point
    , size : Float
    , velocity : Float
    , state : PlayerState
    , hitbox : Hitbox
    }


type PlayerState
    = Running
    | Jumping


init : Point -> Player
init point =
    { position = point
    , size = 10
    , velocity = 0
    , state = Running
    , hitbox = hitbox point
    }


hitbox : Point -> Hitbox
hitbox position =
    Rect (Tuple.mapBoth (\x -> x - 10) (\y -> y - 10) position) 20 20


update : Canvas -> Config -> Float -> Player -> Player
update canvas config deltaTime player =
    let
        ( xPos, yPos ) =
            player.position

        newPosition =
            ( xPos
            , max ((canvas.height / 2) - 150) <|
                (min (canvas.height / 2) <| yPos + (player.velocity * deltaTime))
            )
    in
    { player
        | position = newPosition
        , hitbox = hitbox newPosition
        , velocity = player.velocity + config.gravity
        , state =
            if Tuple.second newPosition == (canvas.height / 2) then
                Running

            else
                Jumping
    }


tryJump : Player -> Player
tryJump player =
    if player.state == Running then
        { player | velocity = -25, state = Jumping }

    else
        player


view : Player -> Renderable
view player =
    Canvas.group []
        [ Canvas.shapes [ Settings.fill Color.white ]
            [ Canvas.circle player.position 10 ]
        , Canvas.shapes
            [ Settings.fill Color.black ]
            [ Canvas.circle (Tuple.mapBoth (\x -> x + 4) (\y -> y - 2) player.position) 3
            ]
        ]
