module Config exposing (Config, default)


type alias Config =
    { gravity : Float
    , obstacleCountThreshold : Float
    , tempoIncrement : Float
    }


default : Config
default =
    { gravity = 2
    , obstacleCountThreshold = 1000
    , tempoIncrement = 0.0001
    }
