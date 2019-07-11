module Main exposing (main)

import Browser
import Browser.Events
import Color
import Html exposing (Html)
import Json.Decode as JD
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Ports exposing (saveConfig)
import Random
import Random.Extra
import Torus exposing (Torus)
import Types exposing (..)
import Utils exposing (fromTuple, toTuple)
import View exposing (view)


main =
    Browser.element
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
      , mousePos = Nothing
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
        (Random.float (turns 0.5) (turns -0.5))
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
                                |> Maybe.withDefault config.vel
                    }
            in
            ( { model | config = newConfig }
            , saveConfig newConfig
            )

        ChangeCohesion inputStr ->
            let
                newConfig =
                    { config
                        | cohesionWeight =
                            String.toFloat inputStr
                                |> Maybe.withDefault config.cohesionWeight
                    }
            in
            ( { model | config = newConfig }
            , saveConfig newConfig
            )

        ChangeMomentumWeight inputStr ->
            let
                newConfig =
                    { config
                        | momentumWeight =
                            String.toFloat inputStr
                                |> Maybe.withDefault config.momentumWeight
                    }
            in
            ( { model | config = newConfig }
            , saveConfig newConfig
            )

        ChangeAlignment inputStr ->
            let
                newConfig =
                    { config
                        | alignment =
                            String.toFloat inputStr
                                |> Maybe.withDefault config.alignment
                    }
            in
            ( { model | config = newConfig }
            , saveConfig newConfig
            )

        ChangeBoidDiameter inputStr ->
            let
                newConfig =
                    { config
                        | boidDiameter =
                            String.toFloat inputStr
                                |> Maybe.withDefault config.boidDiameter
                    }
            in
            ( { model | config = newConfig }
            , saveConfig newConfig
            )

        ChangePersonalSpace inputStr ->
            let
                newConfig =
                    { config
                        | personalSpace =
                            String.toFloat inputStr
                                |> Maybe.withDefault config.personalSpace
                    }
            in
            ( { model | config = newConfig }
            , saveConfig newConfig
            )

        ChangeSightDist inputStr ->
            let
                newConfig =
                    { config
                        | sightDist =
                            String.toFloat inputStr
                                |> Maybe.withDefault config.sightDist
                    }
            in
            ( { model | config = newConfig }
            , saveConfig newConfig
            )

        ChangeJerkiness inputStr ->
            let
                newConfig =
                    { config
                        | jerkiness =
                            String.toFloat inputStr
                                |> Maybe.withDefault config.jerkiness
                    }
            in
            ( { model | config = newConfig }
            , saveConfig newConfig
            )

        ChangeMaxTurnRate inputStr ->
            let
                newConfig =
                    { config
                        | maxTurnRate =
                            String.toFloat inputStr
                                |> Maybe.withDefault config.maxTurnRate
                    }
            in
            ( { model | config = newConfig }
            , saveConfig newConfig
            )

        ChangeSeparationWeight inputStr ->
            let
                newConfig =
                    { config
                        | separationWeight =
                            String.toFloat inputStr
                                |> Maybe.withDefault config.separationWeight
                    }
            in
            ( { model | config = newConfig }
            , saveConfig newConfig
            )

        ChangeFollowWeight inputStr ->
            let
                newConfig =
                    { config
                        | followWeight =
                            String.toFloat inputStr
                                |> Maybe.withDefault config.followWeight
                    }
            in
            ( { model | config = newConfig }
            , saveConfig newConfig
            )

        TogglePause ->
            let
                newConfig =
                    { config | paused = not config.paused }
            in
            ( { model | config = newConfig }
            , saveConfig newConfig
            )

        ChangeNumBoids inputStr ->
            let
                numBoids =
                    String.toInt inputStr |> Maybe.withDefault config.numBoids

                ( newBoids, newSeed ) =
                    if numBoids > List.length boids then
                        boidGenerator torus
                            |> Random.list (numBoids - List.length boids)
                            |> (\gen -> Random.step gen seed)

                    else
                        ( List.drop (List.length boids - numBoids) boids, seed )

                newConfig =
                    { config | numBoids = String.toInt inputStr |> Maybe.withDefault config.numBoids }
            in
            ( { model
                | config = newConfig
                , boids = newBoids
                , seed = newSeed
              }
            , saveConfig newConfig
            )

        ToggleSightDist ->
            let
                newConfig =
                    { config | showSightDist = not config.showSightDist }
            in
            ( { model | config = newConfig }
            , saveConfig newConfig
            )

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
                |> (\newModel ->
                        ( newModel
                        , saveConfig newModel.config
                        )
                   )

        MouseMoves mousePos ->
            ( { model
                | mousePos =
                    mousePos
                        |> (\( x, y ) ->
                                ( x, model.torus.height - y )
                                    |> fromTuple
                                    |> Just
                           )
              }
            , Cmd.none
            )


tick : Float -> Model -> Model
tick delta ({ torus, config, mousePos } as model) =
    let
        ( newBoids, newSeed ) =
            List.foldr
                (\boid ( appendingBoidList, seed ) ->
                    let
                        newSeed_ =
                            seed

                        ( targetAngleForCohesion, cohesionWeight ) =
                            getNearbyBoids config.sightDist torus model.boids boid
                                |> List.map Tuple.second
                                |> (\dists ->
                                        if List.isEmpty dists then
                                            ( boid.angle, 0 )

                                        else
                                            ( dists
                                                |> vecSum
                                                |> toTuple
                                                |> (\( x, y ) -> atan2 y x)
                                            , config.cohesionWeight
                                            )
                                   )

                        ( targetAngleForAlignment, alignmentWeight ) =
                            getNearbyBoids config.sightDist torus model.boids boid
                                |> List.map Tuple.first
                                |> (\boids ->
                                        if List.isEmpty boids then
                                            ( boid.angle, 0 )

                                        else
                                            ( boids
                                                |> List.map
                                                    (\b ->
                                                        fromPolar ( 1, b.angle )
                                                            |> fromTuple
                                                     --|> Debug.log ("to " ++ toString b.color)
                                                    )
                                                |> vecAvg
                                                |> toTuple
                                                |> toPolar
                                                |> Tuple.second
                                            , config.alignment
                                            )
                                   )

                        ( targetAngleForSeparation, separationWeight ) =
                            getNearbyBoids config.personalSpace torus model.boids boid
                                |> List.map Tuple.second
                                |> (\dists ->
                                        if List.isEmpty dists then
                                            ( boid.angle, 0 )

                                        else
                                            ( dists
                                                |> vecSum
                                                |> toTuple
                                                |> (\( x, y ) -> atan2 -y -x)
                                            , config.separationWeight
                                            )
                                   )

                        ( targetAngleForFollow, followWeight ) =
                            case mousePos of
                                Nothing ->
                                    ( boid.angle, 0 )

                                Just mp ->
                                    ( Vec2.sub mp boid.pos |> vecAngle, config.followWeight )

                        newAngle =
                            vecAvg
                                [ fromTuple
                                    (fromPolar
                                        ( config.momentumWeight
                                        , boid.angle
                                        )
                                    )
                                , fromTuple
                                    (fromPolar
                                        ( cohesionWeight
                                        , targetAngleForCohesion
                                        )
                                    )
                                , fromTuple
                                    (fromPolar
                                        ( alignmentWeight
                                        , targetAngleForAlignment
                                        )
                                    )
                                , fromTuple
                                    (fromPolar
                                        ( separationWeight
                                        , targetAngleForSeparation
                                        )
                                    )
                                , fromTuple
                                    (fromPolar
                                        ( followWeight
                                        , targetAngleForFollow
                                        )
                                    )
                                ]
                                |> toTuple
                                |> toPolar
                                |> Tuple.second

                        newPos =
                            Vec2.add boid.pos
                                (fromPolar ( config.vel * delta, -newAngle )
                                    |> (\( x, y ) ->
                                            ( x, -y )
                                                |> fromTuple
                                       )
                                )
                                |> Torus.clamp torus
                    in
                    ( { boid
                        | pos = newPos
                        , angle = newAngle
                      }
                        :: appendingBoidList
                    , newSeed_
                    )
                )
                ( [], model.seed )
                model.boids
    in
    { model
        | boids = newBoids
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

                        --|> Debug.log "dist"
                    in
                    if Vec2.length dist <= maxDist then
                        Just ( b, dist )

                    else
                        Nothing
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.config.paused then
        Sub.none

    else
        Sub.batch
            [ Browser.Events.onAnimationFrameDelta Tick
            , Browser.Events.onMouseMove
                (JD.map2 (\a b -> ( a, b ))
                    (JD.field "movementX" JD.float)
                    (JD.field "movementY" JD.float)
                    |> JD.map MouseMoves
                )
            ]



--Time.every (Time.second * 5) Tick
--|> always Sub.none
