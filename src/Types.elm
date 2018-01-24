module Types exposing (..)

import Color exposing (Color)
import Math.Vector2 as V2 exposing (Vec2)
import Mouse
import Random
import Time exposing (Time)
import Torus exposing (Torus)


type alias Model =
    { boids : List Boid
    , seed : Random.Seed
    , torus : Torus
    , config : Config
    , mousePos : Maybe Vec2
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
    , sightDist : Float
    , showSightDist : Bool

    -- rule 2: alignment
    , alignment : Float

    -- rule 3: separation
    , separationWeight : Float
    , personalSpace : Float

    -- rule 4: follow
    , followWeight : Float

    -- etc
    , paused : Bool

    -- unused
    , jerkiness : Float
    , maxTurnRate : Float
    }


defaultConfig =
    { numBoids = 30
    , vel = 0.3
    , boidDiameter = 40

    -- rule 0 : momenetum
    , momentumWeight = 500

    -- rule 1: cohesion
    , cohesionWeight = 2
    , sightDist = 250
    , showSightDist = False

    -- rule 2: alignment
    , alignment = 30

    -- rule 3: separation
    , separationWeight = 10
    , personalSpace = 60

    -- rule 4: follow
    , followWeight = 50

    -- etc
    , paused = False

    -- unused
    , maxTurnRate = 0.05
    , jerkiness = 0.5
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
      -- rule 2 : alignment
    | ChangeAlignment String
      -- rule 3 : separation
    | ChangeSeparationWeight String
    | ChangePersonalSpace String
      -- rule 3 : separation
    | ChangeFollowWeight String
      -- etc
    | ResetDefaults
    | TogglePause
    | MouseMoves Mouse.Position
      -- unused
    | ChangeJerkiness String
    | ChangeMaxTurnRate String


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


vecAngle : Vec2 -> Angle
vecAngle v =
    v
        |> V2.toTuple
        |> (\( x, y ) -> atan2 y x)


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
