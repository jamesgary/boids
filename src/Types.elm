module Types exposing (..)

import Color exposing (Color)
import Math.Vector2 as V2 exposing (Vec2)
import Random
import Time exposing (Time)
import Torus exposing (Torus)


type alias Model =
    { boids : List Boid
    , seed : Random.Seed
    , torus : Torus
    , config : Config
    }


type alias Angle =
    Float


type alias Boid =
    { pos : Vec2
    , angle : Angle
    , targetAngle : Angle
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
    , vel : Float
    , boidDiameter : Float

    -- rule 0: momentum
    , momentumWeight : Float

    -- rule 1: cohesion
    , cohesionWeight : Float

    -- rule 3: separation
    , separationWeight : Float

    -- etc
    , jerkiness : Float
    , maxTurnRate : Float
    , alignment : Float
    , personalSpace : Float
    , sightDist : Float
    , showSightDist : Bool
    }


defaultConfig =
    { numBoids = 30
    , vel = 0.25
    , boidDiameter = 40

    -- rule 0 : momenetum
    , momentumWeight = 1

    -- rule 1: cohesion
    , cohesionWeight = 0.01
    , sightDist = 250
    , showSightDist = False

    -- rule 3 separation
    , separationWeight = 0.1

    -- etc
    , maxTurnRate = 0.05
    , jerkiness = 0.5
    , alignment = 0.1
    , personalSpace = 10
    }


type Msg
    = Tick Time
      -- config
    | ChangeNumBoids String
    | ChangeVel String
    | ChangeBoidDiameter String
      -- rule 0 : momentum
    | ChangeMomentumWeight String
      -- rule 1 : momentum
    | ChangeCohesion String
    | ChangeSightDist String
    | ToggleSightDist
      -- rule 3 : separation
    | ChangeSeparationWeight String
      -- etc
    | ChangeAlignment String
    | ChangeJerkiness String
    | ChangeMaxTurnRate String
    | ChangePersonalSpace String
    | ResetDefaults


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
