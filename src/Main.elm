module Main exposing (main)

--import Torus exposing (..)

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
init { maybeConfig, timestamp, width, height } =
    let
        config =
            case maybeConfig of
                Just c ->
                    c

                Nothing ->
                    defaultConfig

        ( boids, seed ) =
            boidGenerator width height
                |> Random.list config.numBoids
                |> (\gen -> Random.step gen (Random.initialSeed timestamp))
    in
    ( { boids = boids
      , width = width
      , height = height
      , seed = seed
      , config = config
      }
    , Cmd.none
    )


boidGenerator : Float -> Float -> Random.Generator Boid
boidGenerator width height =
    Random.map5
        (\x y a ta color ->
            { pos = vec2 x y
            , angle = a
            , turningAcc = ta
            , color = color |> Maybe.withDefault Color.white
            }
        )
        (Random.float 0 width)
        (Random.float 0 height)
        (Random.float 0 (turns 1))
        (Random.float 0 1)
        (Random.Extra.sample niceColors)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boids, width, height, seed, config } as model) =
    case msg of
        Tick time ->
            ( tick (max (1000 / 60) time) model
            , Cmd.none
            )

        -- TODO some way to DRY these up?
        ChangeVel inputStr ->
            let
                newConfig =
                    { config
                        | vel =
                            String.toFloat inputStr
                                |> Result.withDefault config.vel
                    }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]

        ChangeCohesion inputStr ->
            let
                newConfig =
                    { config
                        | cohesion =
                            String.toFloat inputStr
                                |> Result.withDefault config.cohesion
                    }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]

        ChangeAlignment inputStr ->
            let
                newConfig =
                    { config
                        | alignment =
                            String.toFloat inputStr
                                |> Result.withDefault config.alignment
                    }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]

        ChangeBoidDiameter inputStr ->
            let
                newConfig =
                    { config
                        | boidDiameter =
                            String.toFloat inputStr
                                |> Result.withDefault config.boidDiameter
                    }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]

        ChangePersonalSpace inputStr ->
            let
                newConfig =
                    { config
                        | personalSpace =
                            String.toFloat inputStr
                                |> Result.withDefault config.personalSpace
                    }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]

        ChangeSightDist inputStr ->
            let
                newConfig =
                    { config
                        | sightDist =
                            String.toFloat inputStr
                                |> Result.withDefault config.sightDist
                    }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]

        ChangeJerkiness inputStr ->
            let
                newConfig =
                    { config
                        | jerkiness =
                            String.toFloat inputStr
                                |> Result.withDefault config.jerkiness
                    }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]

        ChangeMaxTurnRate inputStr ->
            let
                newConfig =
                    { config
                        | maxTurnRate =
                            String.toFloat inputStr
                                |> Result.withDefault config.maxTurnRate
                    }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]

        ChangeNumBoids inputStr ->
            let
                numBoids =
                    String.toInt inputStr |> Result.withDefault config.numBoids

                ( newBoids, newSeed ) =
                    if numBoids > List.length boids then
                        boidGenerator width height
                            |> Random.list (numBoids - List.length boids)
                            |> (\gen -> Random.step gen seed)
                    else
                        ( List.drop (List.length boids - numBoids) boids, seed )

                newConfig =
                    { config | numBoids = String.toInt inputStr |> Result.withDefault config.numBoids }
            in
            { model
                | config = newConfig
                , boids = newBoids
                , seed = newSeed
            }
                ! [ saveConfig newConfig ]

        ToggleSightDist ->
            let
                newConfig =
                    { config | showSightDist = not config.showSightDist }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]


tick : Time.Time -> Model -> Model
tick time ({ width, height, config } as model) =
    let
        ( newBoids, newSeed ) =
            List.foldr
                (\boid ( boids, seed ) ->
                    let
                        ( newTurningAcc, newSeed ) =
                            --( boid.turningAcc, seed )
                            Random.step
                                (Random.float (-1 * config.jerkiness) config.jerkiness
                                    |> Random.map
                                        (\j ->
                                            boid.turningAcc + j
                                        )
                                )
                                seed

                        newAngle =
                            boid.angle + (config.maxTurnRate * cos (newTurningAcc * time))

                        newPos =
                            V2.add boid.pos
                                (fromPolar ( config.vel * time, newAngle ) |> V2.fromTuple)
                    in
                    ( ({ boid
                        | turningAcc = newTurningAcc
                        , angle = newAngle
                        , pos = newPos
                       }
                        |> wrapBoid width height
                      )
                        :: boids
                    , newSeed
                    )
                )
                ( [], model.seed )
                model.boids
    in
    { model
        | boids =
            newBoids
        , seed = newSeed
    }


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
                        dist =
                            getDistBetweenBoids width height boid b
                                |> V2.length
                    in
                    dist <= maxDist
            )


getDistBetweenBoids : Float -> Float -> Boid -> Boid -> Vec2
getDistBetweenBoids width height b1 b2 =
    let
        ( x1, y1 ) =
            b1.pos
                |> V2.toTuple

        ( x2, y2 ) =
            b2.pos
                |> V2.toTuple

        dx =
            abs (x1 - x2)
                |> (\dx ->
                        if dx > width / 2 then
                            width - dx
                        else
                            dx
                   )

        dy =
            abs (y1 - y2)
                |> (\dy ->
                        if dy > height / 2 then
                            width - dy
                        else
                            dy
                   )
    in
    V2.fromTuple ( dx, dy )


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



--Time.every (Time.second * 5) Tick
--|> always Sub.none
