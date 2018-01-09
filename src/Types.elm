module Types exposing (..)

import Math.Vector2 as V2 exposing (Vec2)
import Random
import Time exposing (Time)


type alias Boid =
    { pos : Vec2
    , vel : Vec2
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


defaultSpeed =
    0.4


defaultNumBoids =
    10


boidViewRad =
    50
