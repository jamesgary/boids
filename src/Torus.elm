module Torus exposing (..)

import Math.Vector2 as V2 exposing (Vec2, vec2)


type alias Torus =
    { width : Float
    , height : Float
    }



-- returns distance between two vectors (direction and magnitude)


dist : Torus -> Vec2 -> Vec2 -> Vec2
dist ({ width, height } as t) v1 v2 =
    let
        ( x1, y1 ) =
            v1 |> clamp t |> V2.toTuple

        ( x2, y2 ) =
            v2 |> clamp t |> V2.toTuple

        dx =
            (x2 - x1)
                |> (\dx ->
                        if dx > width / 2 then
                            width - dx
                        else if dx < -width / 2 then
                            dx - width
                        else
                            dx
                   )

        dy =
            (y2 - y1)
                |> (\dy ->
                        if dy > height / 2 then
                            height - dy
                        else if dy < -height / 2 then
                            dy - height
                        else
                            dy
                   )
    in
    V2.fromTuple ( dx, dy )



-- returns list of vec2s that are visible if vec were rendered on a torus


wrappees : Torus -> Vec2 -> Float -> List Vec2
wrappees torus vec radius =
    []



-- returns vec clamped within the torus


clamp : Torus -> Vec2 -> Vec2
clamp { width, height } vec =
    let
        ( x, y ) =
            V2.toTuple vec
    in
    V2.fromTuple ( fmod x width, fmod y height )


fmod : Float -> Float -> Float
fmod numer denom =
    numer - toFloat (floor (numer / denom)) * denom
