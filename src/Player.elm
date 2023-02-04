module Player exposing (..)

import Canvas exposing (Renderable)
import Canvas.Settings as Settings
import Color


type alias Player =
    { height : Float
    , velocity : Float
    , state : PlayerState
    }


type PlayerState
    = Running
    | Jumping


gravity : number
gravity =
    2


init : Player
init =
    { height = 0
    , velocity = 0
    , state = Running
    }


update : Float -> Player -> Player
update deltaTime player =
    let
        newHeight =
            min 0 <| player.height + (player.velocity * deltaTime)
    in
    { height = newHeight
    , velocity = player.velocity + gravity
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
