module Main exposing (main)

import AnimationFrame
import Color
import Html exposing (Html)
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Ports exposing (saveConfig)
import Random
import Random.Extra
import Time exposing (Time)
import Torus exposing (Torus)
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
            boidGenerator (Torus width height)
                |> Random.list config.numBoids
                |> (\gen -> Random.step gen (Random.initialSeed timestamp))
    in
    ( { boids = boids
      , torus = Torus width height
      , seed = seed
      , config = config
      }
    , Cmd.none
    )


boidGenerator : Torus -> Random.Generator Boid
boidGenerator { width, height } =
    Random.map4
        (\x y a color ->
            { pos = vec2 x y
            , angle = a
            , targetAngle = a
            , color = color |> Maybe.withDefault Color.white
            }
        )
        (Random.float 0 width)
        (Random.float 0 height)
        (Random.float 0 (turns 1))
        (Random.Extra.sample niceColors)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boids, torus, seed, config } as model) =
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
                        | cohesionWeight =
                            String.toFloat inputStr
                                |> Result.withDefault config.cohesionWeight
                    }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]

        ChangeMomentumWeight inputStr ->
            let
                newConfig =
                    { config
                        | momentumWeight =
                            String.toFloat inputStr
                                |> Result.withDefault config.momentumWeight
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

        ChangeSeparationWeight inputStr ->
            let
                newConfig =
                    { config
                        | separationWeight =
                            String.toFloat inputStr
                                |> Result.withDefault config.separationWeight
                    }
            in
            { model | config = newConfig } ! [ saveConfig newConfig ]

        ChangeNumBoids inputStr ->
            let
                numBoids =
                    String.toInt inputStr |> Result.withDefault config.numBoids

                ( newBoids, newSeed ) =
                    if numBoids > List.length boids then
                        boidGenerator torus
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

        ResetDefaults ->
            let
                newConfig =
                    defaultConfig
            in
            init
                { timestamp = Random.step (Random.int 0 Random.maxInt) model.seed |> Tuple.first
                , width = torus.width
                , height = torus.height
                , maybeConfig = Nothing
                }
                |> Tuple.first
                |> (\model -> model ! [ saveConfig model.config ])


tick : Time.Time -> Model -> Model
tick time ({ torus, config } as model) =
    let
        ( newBoids, newSeed ) =
            List.foldr
                (\boid ( boids, seed ) ->
                    let
                        newSeed =
                            seed

                        targetAngleForCohesion =
                            getNearbyBoids config.sightDist torus model.boids boid
                                |> List.map Tuple.second
                                |> (\list ->
                                        if List.isEmpty list then
                                            boid.angle
                                        else
                                            list
                                                |> vecSum
                                                |> V2.toTuple
                                                |> (\( x, y ) -> atan2 y x)
                                   )

                        ( targetAngleForSeparation, separationWeight ) =
                            getNearbyBoids config.personalSpace torus model.boids boid
                                |> List.map Tuple.second
                                |> (\list ->
                                        if List.isEmpty list then
                                            ( boid.angle, 0 )
                                        else
                                            ( list
                                                |> vecSum
                                                |> V2.toTuple
                                                |> (\( x, y ) -> atan2 -y -x)
                                            , config.separationWeight
                                            )
                                   )

                        newAngle =
                            vecAvg
                                [ V2.fromTuple
                                    (fromPolar
                                        ( config.momentumWeight
                                        , boid.angle
                                        )
                                    )
                                , V2.fromTuple
                                    (fromPolar
                                        ( config.cohesionWeight
                                        , targetAngleForCohesion
                                        )
                                    )
                                , V2.fromTuple
                                    (fromPolar
                                        ( separationWeight
                                        , targetAngleForSeparation
                                        )
                                    )
                                ]
                                |> V2.toTuple
                                |> toPolar
                                |> Tuple.second

                        newPos =
                            V2.add boid.pos
                                (fromPolar ( config.vel * time, -newAngle )
                                    |> (\( x, y ) ->
                                            ( x, -y )
                                                |> V2.fromTuple
                                       )
                                )
                                |> Torus.clamp torus
                    in
                    ( { boid
                        | pos = newPos
                        , angle = newAngle
                      }
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


getNearbyBoids : Float -> Torus -> List Boid -> Boid -> List ( Boid, Vec2 )
getNearbyBoids maxDist torus boids boid =
    boids
        |> List.filterMap
            (\b ->
                if b == boid then
                    Nothing
                else
                    let
                        dist =
                            Torus.dist torus boid.pos b.pos
                    in
                    if V2.length dist <= maxDist then
                        Just ( boid, dist )
                    else
                        Nothing
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



--Time.every (Time.second * 5) Tick
--|> always Sub.none
