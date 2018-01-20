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


type alias Angle =
    Float


type alias Boid =
    { pos : Vec2
    , angle : Angle
    , turningAcc : Angle
    , color : Color
    }


type alias Rule =
    { name : String
    , weight : Float
    , sightRange : Float
    , behavior : List Boid -> Boid -> Boid
    }


type alias Config =
    { numBoids : Int
    , boidDiameter : Float
    , vel : Float
    , jerkiness : Float
    , maxTurnRate : Float

    --
    , cohesion : Float
    , alignment : Float
    , personalSpace : Float
    , sightDist : Float
    , showSightDist : Bool
    }


defaultConfig =
    { numBoids = 30
    , vel = 0.25
    , boidDiameter = 40
    , jerkiness = 0.5
    , maxTurnRate = 0.05
    , cohesion = 0.0001
    , alignment = 0.1
    , personalSpace = 10
    , sightDist = 70
    , showSightDist = False
    }


type Msg
    = Tick Time
    | ChangeVel String
    | ChangeCohesion String
    | ChangeAlignment String
    | ChangeNumBoids String
    | ChangeBoidDiameter String
    | ChangeJerkiness String
    | ChangeMaxTurnRate String
    | ChangePersonalSpace String
    | ChangeSightDist String
    | ToggleSightDist


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


defaultSpeed =
    0.3


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
