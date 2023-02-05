module Types.Buffer exposing (Buffer, empty, insert, update)

import Dict.Any as AnyDict exposing (AnyDict)


type alias Ref =
    Float


type Buffer a
    = Buffer (AnyDict String a Ref)


update : Float -> Buffer a -> Buffer a
update deltaTime (Buffer dict) =
    let
        updateTimer : a -> Ref -> Maybe Ref
        updateTimer _ r =
            if (r - deltaTime) > 0 then
                Just (r - deltaTime)

            else
                Nothing
    in
    Buffer <| AnyDict.filterMap updateTimer dict


empty : (a -> String) -> Buffer a
empty toString =
    Buffer (AnyDict.empty toString)


insert : a -> Buffer a -> Buffer a
insert a (Buffer dict) =
    Buffer <| AnyDict.insert a 500 dict
