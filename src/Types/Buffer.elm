module Types.Buffer exposing (Buffer, empty, insert, update)

import Dict.Any as AnyDict exposing (AnyDict)
import Types.Timer as Timer exposing (Timer)


type Buffer a
    = Buffer (AnyDict String a Timer)


update : Float -> Buffer a -> Buffer a
update deltaTime (Buffer dict) =
    let
        updateTimer _ t =
            Timer.update deltaTime t
    in
    Buffer <| AnyDict.filterMap updateTimer dict


empty : (a -> String) -> Buffer a
empty toString =
    Buffer (AnyDict.empty toString)


insert : a -> Buffer a -> Buffer a
insert a (Buffer dict) =
    Buffer <| AnyDict.insert a (Timer.init 500) dict
