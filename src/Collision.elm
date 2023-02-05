module Collision exposing (..)

import Canvas exposing (Point)


type Hitbox
    = Rect Point Float Float


type alias Collides a =
    { a | hitbox : Hitbox }


intersects : Collides a -> Collides b -> Bool
intersects a b =
    hitboxIntersection a.hitbox b.hitbox


hitboxIntersection : Hitbox -> Hitbox -> Bool
hitboxIntersection (Rect ( x1, y1 ) w1 h1) (Rect ( x2, y2 ) w2 h2) =
    (x1 < x2 + w2)
        && (x1 + w1 > x2)
        && (y1 < y2 + h2)
        && (h1 + y1 > y2)
