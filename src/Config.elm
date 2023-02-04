module Config exposing (Config, default)


type alias Config =
    { gravity : Float
    , obstacleCountThreshold : Int
    }


default : Config
default =
    { gravity = 2
    , obstacleCountThreshold = 1200
    }
