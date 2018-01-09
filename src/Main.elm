module Main exposing (main)

import AnimationFrame
import Html exposing (Html)
import Math.Vector2 as V2 exposing (Vec2, getX, getY, vec2)
import Random
import Time exposing (Time)
import Types exposing (..)
import View exposing (view)


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { timestamp, width, height } =
    let
        ( boids, seed ) =
            boidGenerator width height
                |> Random.list defaultNumBoids
                |> (\gen -> Random.step gen (Random.initialSeed timestamp))
    in
    ( { boids = boids
      , width = width
      , height = height
      , seed = seed
      }
    , Cmd.none
    )


boidGenerator : Float -> Float -> Random.Generator Boid
boidGenerator width height =
    Random.map3
        (\x y a ->
            { pos = vec2 x y
            , vel =
                ( defaultSpeed, turns a )
                    |> fromPolar
                    |> V2.fromTuple
            }
        )
        (Random.float 0 width)
        (Random.float 0 height)
        (Random.float 0 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boids, width, height } as model) =
    case msg of
        Tick time ->
            ( { model
                | boids =
                    List.map (applyVel time >> wrapBoid width height)
                        boids
              }
            , Cmd.none
            )


applyVel : Time -> Boid -> Boid
applyVel time ({ pos, vel } as boid) =
    { boid | pos = V2.add pos (V2.scale time vel) }


wrapBoid : Float -> Float -> Boid -> Boid
wrapBoid width height ({ pos } as boid) =
    { boid
        | pos =
            pos
                |> V2.toTuple
                |> (\( x, y ) ->
                        ( wrap width x
                        , wrap height y
                        )
                   )
                |> V2.fromTuple
    }


wrap : Float -> Float -> Float
wrap max val =
    if val < 0 then
        wrap max (val + max)
    else if val > max then
        wrap max (val - max)
    else
        val


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick
