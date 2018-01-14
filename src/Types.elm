module Types exposing (..)

import Color exposing (Color)
import Math.Vector2 as V2 exposing (Vec2)
import Random
import Time exposing (Time)


type alias Model =
    { boids : List Boid
    , seed : Random.Seed
    , width : Float
    , height : Float
    , config : Config
    }


type alias Boid =
    { pos : Vec2
    , vel : Vec2
    , color : Color
    }


type alias Config =
    { maxSpeed : Float
    , numBoids : Int
    , cohesion : Float
    , alignment : Float
    , boidRadius : Float
    }


defaultConfig =
    { maxSpeed = 0.3
    , numBoids = 10
    , cohesion = 0.0001
    , alignment = 0.1
    , boidRadius = 20
    }


type Msg
    = Tick Time
    | ChangeMaxSpeed String
    | ChangeCohesion String
    | ChangeAlignment String
    | ChangeNumBoids String


type alias Flags =
    { timestamp : Int
    , width : Float
    , height : Float
    , maybeConfig : Maybe Config
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
    50


defaultSpeed =
    0.3


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
