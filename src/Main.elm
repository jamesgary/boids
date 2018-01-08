module Main exposing (..)

import AnimationFrame
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Math.Vector2 as V2 exposing (Vec2, getX, getY, vec2)
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


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { timestamp, width, height } =
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
      }
    , Cmd.none
    )


boidGenerator : Float -> Float -> Random.Generator Boid
boidGenerator width height =
    Random.map3
        (\x y a ->
            { pos = vec2 x y
            , vel =
                ( defaultSpeed, turns a )
                    |> fromPolar
                    |> V2.fromTuple
            }
        )
        (Random.float 0 width)
        (Random.float 0 height)
        (Random.float 0 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boids, width, height } as model) =
    case msg of
        Tick time ->
            ( { model
                | boids =
                    List.map (applyVel time >> wrapBoid width height)
                        boids
              }
            , Cmd.none
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


view : Model -> Html Msg
view { boids } =
    div [ class "boids" ]
        (boids
            |> List.map drawBoid
            |> List.concat
        )


drawBoid : Boid -> List (Html Msg)
drawBoid ({ pos } as boid) =
    -- TODO: wrap
    [ drawBoidHelper boid ]


drawBoidHelper : Boid -> Html Msg
drawBoidHelper { pos, vel } =
    let
        translateVal =
            px (getX pos) ++ "," ++ px (getY pos)

        rotateVal =
            vel
                |> V2.toTuple
                |> (\( x, y ) -> atan2 y x)
                |> toString
    in
    div
        [ class "boid"
        , style
            [ ( "transform"
              , "translate(" ++ translateVal ++ ") rotate(" ++ rotateVal ++ "rad)"
              )
            ]
        ]
        []


px : number -> String
px num =
    toString num ++ "px"


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick
