module Main exposing (..)

import AnimationFrame
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Math.Vector2 as V2 exposing (Vec2, getX, getY, vec2)
import Time exposing (Time)


type alias Boid =
    { id : Int
    , pos : Vec2
    , vel : Vec2
    }


type alias Model =
    { boids : List Boid
    }


type Msg
    = Tick Time


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { boids =
            [ { id = 0
              , pos = vec2 50 50
              , vel = vec2 0.1 0
              }
            ]
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boids } as model) =
    case msg of
        Tick time ->
            ( { model | boids = List.map (applyVel time) boids }, Cmd.none )


applyVel : Time -> Boid -> Boid
applyVel time ({ pos, vel } as boid) =
    { boid | pos = V2.add pos (V2.scale time vel) }


view : Model -> Html Msg
view { boids } =
    div [ class "boids" ]
        (boids
            |> List.map drawBoid
        )


drawBoid : Boid -> Html Msg
drawBoid { pos } =
    div
        [ class "boid"
        , style [ ( "transform", "translate(" ++ px (getX pos) ++ "," ++ px (getY pos) ++ ")" ) ]
        ]
        []


px : number -> String
px num =
    toString num ++ "px"


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick
