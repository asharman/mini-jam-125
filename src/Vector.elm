module Vector exposing
    ( Vector
    , add
    , limitMagnitude
    , magnitude
    , normalize
    , scale
    )


type alias Vector =
    ( Float, Float )


add : Vector -> Vector -> Vector
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


scale : Float -> Vector -> Vector
scale factor ( x, y ) =
    ( x * factor, y * factor )


magnitude : Vector -> Float
magnitude ( x, y ) =
    sqrt (x * x + y * y)


normalize : Vector -> Vector
normalize ( x, y ) =
    let
        mag =
            magnitude ( x, y )
    in
    ( x / mag, y / mag )


limitMagnitude : Float -> Vector -> Vector
limitMagnitude mag vec =
    if abs (magnitude vec) > mag then
        vec |> normalize |> scale mag

    else
        vec
