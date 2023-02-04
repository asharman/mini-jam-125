module Player exposing (..)

import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Color
import Config exposing (Config)


type alias Player =
    { height : Float
    , velocity : Float
    , state : PlayerState
    }


type PlayerState
    = Running
    | Jumping


init : Player
init =
    { height = 0
    , velocity = 0
    , state = Running
    }


update : Config -> Float -> Player -> Player
update config deltaTime player =
    let
        newHeight =
            max -150 (min 0 <| player.height + (player.velocity * deltaTime))
    in
    { height = newHeight
    , velocity = player.velocity + config.gravity
    , state =
        if newHeight == 0 then
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


view : Float -> Player -> Renderable
view canvasHeight player =
    Canvas.group []
        [ Canvas.shapes [ Settings.fill Color.white ]
            [ Canvas.circle
                ( 50
                , (canvasHeight / 2) + player.height
                )
                10
            ]
        , Canvas.shapes
            [ Settings.fill Color.black ]
            [ Canvas.circle ( 54, (canvasHeight / 2) - 2 + player.height ) 3
            ]
        ]
