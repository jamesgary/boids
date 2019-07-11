module Utils exposing (fromTuple, toTuple)

import Math.Vector2 as Vec2 exposing (Vec2)


toTuple : Vec2 -> ( Float, Float )
toTuple vec2 =
    vec2
        |> Vec2.toRecord
        |> (\{ x, y } -> ( x, y ))


fromTuple : ( Float, Float ) -> Vec2
fromTuple ( x, y ) =
    Vec2.vec2 x y
