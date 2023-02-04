module Config exposing (Config, default)


type alias Config =
    { gravity : Float
    , obstacleCountThreshold : Int
    }


default : Config
default =
    Config 2 1200
