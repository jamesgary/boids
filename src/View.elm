module View exposing (view)

import Color
import Html exposing (Html, div, h1, h2, input, label, text)
import Html.Attributes exposing (class, defaultValue, style)
import Html.Events exposing (onInput)
import Math.Vector2 as V2 exposing (Vec2, getX, getY, vec2)
import Types exposing (..)


view : Model -> Html Msg
view { boids, width, height, config } =
    div [ class "container" ]
        [ div [ class "boids" ]
            (boids
                |> List.map (drawBoid width height)
                |> List.concat
            )
        , div [ class "config" ]
            [ h1 [] [ text "Boids" ]
            , div [ class "config-item" ]
                [ label []
                    [ text "Max Speed"
                    , input
                        [ defaultValue (toString config.maxSpeed)
                        , onInput ChangeMaxSpeed
                        ]
                        []
                    ]
                ]
            , h2 [] [ text "Rule 1: Fly towards center of mass" ]
            , div [ class "config-item" ]
                [ label []
                    [ text "Cohesion"
                    , input
                        [ defaultValue (toString config.cohesion)
                        , onInput ChangeCohesion
                        ]
                        []
                    ]
                ]
            , h2 [] [ text "Rule 2: Avoid collisions" ]
            , h2 [] [ text "Rule 3: Match velocity" ]
            , div [ class "config-item" ]
                [ label []
                    [ text "Alignment"
                    , input
                        [ defaultValue (toString config.alignment)
                        , onInput ChangeAlignment
                        ]
                        []
                    ]
                ]
            ]
        ]


drawBoid : Float -> Float -> Boid -> List (Html Msg)
drawBoid width height ({ pos } as boid) =
    let
        ( x, y ) =
            V2.toTuple pos

        wrappedXList =
            if x < boidViewRad then
                [ x, x + width ]
            else if x > width - boidViewRad then
                [ x, x - width ]
            else
                [ x ]

        wrappedYList =
            if y < boidViewRad then
                [ y, y + height ]
            else if y > height - boidViewRad then
                [ y, y - height ]
            else
                [ y ]

        wrappedPosList =
            wrappedXList
                |> List.map
                    (\x ->
                        List.map (\y -> V2.fromTuple ( x, y )) wrappedYList
                    )
                |> List.concat
    in
    wrappedPosList
        |> List.map
            (\pos ->
                drawBoidHelper { boid | pos = pos }
            )


drawBoidHelper : Boid -> Html Msg
drawBoidHelper { pos, vel, color } =
    let
        translateVal =
            px (getX pos) ++ "," ++ px (getY pos)

        rotateVal =
            vel
                |> V2.toTuple
                |> (\( x, y ) -> atan2 y x)
                |> toString

        colorStr =
            color
                |> Color.toRgb
                |> (\{ red, green, blue } -> "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")")
    in
    div
        [ class "boid"
        , style
            [ ( "transform"
              , "translate(" ++ translateVal ++ ") rotate(" ++ rotateVal ++ "rad)"
              )
            , ( "background", colorStr )
            ]
        ]
        []


px : number -> String
px num =
    toString num ++ "px"
