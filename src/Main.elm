module Main exposing (main)

import AnimationFrame
import Color
import Html exposing (Html)
import Math.Vector2 as V2 exposing (Vec2, getX, getY, vec2)
import Ports exposing (saveConfig)
import Random
import Random.Extra
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
init { config, timestamp, width, height } =
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
      , config =
            case config of
                Just c ->
                    c

                Nothing ->
                    defaultConfig
      }
    , Cmd.none
    )


boidGenerator : Float -> Float -> Random.Generator Boid
boidGenerator width height =
    Random.map4
        (\x y a color ->
            { pos = vec2 x y
            , vel =
                ( defaultSpeed, turns a )
                    |> fromPolar
                    |> V2.fromTuple
            , color = color |> Maybe.withDefault Color.white
            }
        )
        (Random.float 0 width)
        (Random.float 0 height)
        (Random.float 0 1)
        (Random.Extra.sample niceColors)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boids, width, height, config } as model) =
    case msg of
        Tick time ->
            ( { model
                | boids =
                    List.map
                        (\boid ->
                            let
                                newVel =
                                    [ boid.vel
                                    , centerOfMassVec config width height boids boid
                                    , avoidOtherBoids width height boids boid
                                    , vecAvg
                                        (List.map .vel
                                            (getNearbyBoids defaultSightDistance width height boids boid)
                                        )
                                        |> V2.scale config.alignment
                                    ]
                                        |> vecSum
                                        |> (\v ->
                                                if V2.length v <= config.maxSpeed then
                                                    v
                                                else
                                                    V2.scale config.maxSpeed (V2.normalize v)
                                           )

                                newPos =
                                    V2.add boid.pos (V2.scale time newVel)
                            in
                            { boid
                                | vel = newVel
                                , pos = newPos
                            }
                                |> wrapBoid width height
                        )
                        boids
              }
            , Cmd.none
            )

        ChangeMaxSpeed inputStr ->
            let
                newConfig =
                    { config | maxSpeed = String.toFloat inputStr |> Result.withDefault config.maxSpeed }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]

        ChangeCohesion inputStr ->
            let
                newConfig =
                    { config | cohesion = String.toFloat inputStr |> Result.withDefault config.maxSpeed }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]

        ChangeAlignment inputStr ->
            let
                newConfig =
                    { config | alignment = String.toFloat inputStr |> Result.withDefault config.alignment }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]


avoidOtherBoids : Float -> Float -> List Boid -> Boid -> Vec2
avoidOtherBoids width height boids boid =
    boid
        |> getNearbyBoids defaultBoidRad width height boids
        |> List.map (\b -> V2.sub boid.pos b.pos)
        |> vecSum



--|> V2.scale 0.1


centerOfMassVec : Config -> Float -> Float -> List Boid -> Boid -> Vec2
centerOfMassVec config width height boids boid =
    boid
        |> getNearbyBoids defaultSightDistance width height boids
        |> List.map .pos
        |> vecSum
        |> (\p -> V2.sub p boid.pos)
        |> V2.scale config.cohesion


getNearbyBoids : Float -> Float -> Float -> List Boid -> Boid -> List Boid
getNearbyBoids maxDist width height boids boid =
    let
        ( x, y ) =
            boid.pos
                |> V2.toTuple
    in
    boids
        |> List.filter
            (\b ->
                if b == boid then
                    False
                else
                    let
                        ( x_, y_ ) =
                            b.pos
                                |> V2.toTuple

                        dx =
                            abs (x - x_)
                                |> (\dx ->
                                        if dx > width / 2 then
                                            width - dx
                                        else
                                            dx
                                   )

                        dy =
                            abs (y - y_)
                                |> (\dy ->
                                        if dy > height / 2 then
                                            width - dy
                                        else
                                            dy
                                   )

                        dist =
                            sqrt (dx ^ 2 + dy ^ 2)
                    in
                    dist <= maxDist
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



--|> always Sub.none
