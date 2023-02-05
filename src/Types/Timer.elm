module Types.Timer exposing (..)


type Timer
    = Timer Float


init : Float -> Timer
init timeOut =
    Timer timeOut


update : Float -> Timer -> Maybe Timer
update deltaTime (Timer t) =
    if (t - deltaTime) > 0 then
        Just <| Timer (t - deltaTime)

    else
        Nothing
