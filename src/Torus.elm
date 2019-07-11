module Torus exposing (Torus, clamp, dist, fmod, wrappees)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Utils exposing (fromTuple, toTuple)


type alias Torus =
    { width : Float
    , height : Float
    }



-- returns distance between two vectors (direction and magnitude)


dist : Torus -> Vec2 -> Vec2 -> Vec2
dist ({ width, height } as t) v1 v2 =
    let
        ( x1, y1 ) =
            v1 |> clamp t |> toTuple

        ( x2, y2 ) =
            v2 |> clamp t |> toTuple

        dx =
            (x2 - x1)
                |> (\dx_ ->
                        if dx_ > width / 2 then
                            width - dx_

                        else if dx_ < -width / 2 then
                            dx_ + width

                        else
                            dx_
                   )

        dy =
            (y2 - y1)
                |> (\dy_ ->
                        if dy_ > height / 2 then
                            height - dy_

                        else if dy_ < -height / 2 then
                            dy_ + height

                        else
                            dy_
                   )
    in
    fromTuple ( dx, dy )



-- returns list of vec2s that are visible if vec were rendered on a torus


wrappees : Torus -> Vec2 -> Float -> List Vec2
wrappees torus vec radius =
    []



-- returns vec clamped within the torus


clamp : Torus -> Vec2 -> Vec2
clamp { width, height } vec =
    let
        ( x, y ) =
            toTuple vec
    in
    fromTuple ( fmod x width, fmod y height )


fmod : Float -> Float -> Float
fmod numer denom =
    numer - toFloat (floor (numer / denom)) * denom
