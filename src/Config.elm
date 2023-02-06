module Config exposing (Config, default)


type alias Config =
    { gravity : Float
    , obstacleSpawnFrequency : Float
    }


default : Config
default =
    { gravity = 1.5
    , obstacleSpawnFrequency = 900
    }
