module Config exposing (Config, default)


type alias Config =
    { gravity : Float
    , obstacleSpawnFrequency : Float
    , tempoIncrement : Float
    }


default : Config
default =
    { gravity = 2
    , obstacleSpawnFrequency = 1000
    , tempoIncrement = 0.0005
    }
