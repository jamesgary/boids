module Types exposing (..)

import Color exposing (Color)
import Math.Vector2 as V2 exposing (Vec2)
import Random
import Time exposing (Time)


type alias Boid =
    { pos : Vec2
    , vel : Vec2
    , color : Color
    }


type alias Model =
    { boids : List Boid
    , seed : Random.Seed
    , width : Float
    , height : Float
    }


type Msg
    = Tick Time


type alias Flags =
    { timestamp : Int
    , width : Float
    , height : Float
    }


vecSum : List Vec2 -> Vec2
vecSum vList =
    List.foldl (\v vSum -> V2.add v vSum) (V2.fromTuple ( 0, 0 )) vList


vecAvg : List Vec2 -> Vec2
vecAvg vList =
    case List.length vList of
        0 ->
            V2.fromTuple ( 0, 0 )

        length ->
            vecSum vList
                |> V2.scale (1 / toFloat length)


defaultBoidRad =
    25


defaultSpeed =
    0.00005


defaultMaxSpeed =
    defaultSpeed


defaultNumBoids =
    10


boidViewRad =
    -- body and beak
    defaultBoidRad * 2


defaultSightDistance =
    200


niceColors =
    [ Color.red
    , Color.orange
    , Color.yellow
    , Color.green
    , Color.blue
    , Color.purple
    , Color.brown
    , Color.darkRed
    , Color.darkOrange
    , Color.darkYellow
    , Color.darkGreen
    , Color.darkBlue
    , Color.darkPurple
    , Color.darkBrown
    , Color.lightRed
    , Color.lightOrange
    , Color.lightYellow
    , Color.lightGreen
    , Color.lightBlue
    , Color.lightPurple
    , Color.lightBrown
    ]
