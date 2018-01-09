module View exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Math.Vector2 as V2 exposing (Vec2, getX, getY, vec2)
import Types exposing (..)


view : Model -> Html Msg
view { boids, width, height } =
    div [ class "boids" ]
        (boids
            |> List.map (drawBoid width height)
            |> List.concat
        )


drawBoid : Float -> Float -> Boid -> List (Html Msg)
drawBoid width height ({ pos } as boid) =
    let
        ( x, y ) =
            V2.toTuple pos
    in
    [ drawBoidHelper boid ]
        |> (\boids ->
                if x < boidViewRad then
                    drawBoidHelper { boid | pos = V2.fromTuple ( x + width, y ) } :: boids
                else
                    boids
           )
        |> (\boids ->
                if x > width - boidViewRad then
                    drawBoidHelper { boid | pos = V2.fromTuple ( x - width, y ) } :: boids
                else
                    boids
           )
        |> (\boids ->
                if y < boidViewRad then
                    drawBoidHelper { boid | pos = V2.fromTuple ( x, y + height ) } :: boids
                else
                    boids
           )
        |> (\boids ->
                if y > height - boidViewRad then
                    drawBoidHelper { boid | pos = V2.fromTuple ( x, y - height ) } :: boids
                else
                    boids
           )


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
