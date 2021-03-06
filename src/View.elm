module View exposing (view)

import Color exposing (Color)
import Html exposing (Html, button, div, h1, h2, hr, input, label, node, text)
import Html.Attributes exposing (checked, class, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Math.Vector2 as V2 exposing (Vec2, getX, getY, vec2)
import Torus exposing (Torus)
import Types exposing (..)
import Utils exposing (fromTuple, toTuple)


view : Model -> Html Msg
view { boids, torus, config } =
    div [ class "container" ]
        [ boidCss config
        , div [ class "boids" ]
            (boids
                |> List.map (drawBoid config torus)
                |> List.concat
            )
        , div [ class "config" ]
            [ h1 [] [ text "Boids" ]
            , configInputInt "# of Boids" config.numBoids ChangeNumBoids
            , configInput "Speed" config.vel ChangeVel
            , configInput "Boid Size" config.boidDiameter ChangeBoidDiameter
            , h2 [] [ text "Rule 0: Momentum" ]
            , configInput "Momentum Weight" config.momentumWeight ChangeMomentumWeight
            , h2 [] [ text "Rule 1: Cohesion" ]
            , configInput "Cohesion Weight" config.cohesionWeight ChangeCohesion
            , configInput "Sight Range" config.sightDist ChangeSightDist
            , configCheckbox "Show Sight Range" config.showSightDist ToggleSightDist
            , h2 [] [ text "Rule 2: Alignment" ]
            , configInput "Alignment Weight" config.alignment ChangeAlignment
            , h2 [] [ text "Rule 3: Separation" ]
            , configInput "Separation Weight" config.separationWeight ChangeSeparationWeight
            , configInput "Personal Space" config.personalSpace ChangePersonalSpace
            , h2 [] [ text "Rule 4: Follow Mouse" ]
            , configInput "Follow Weight" config.followWeight ChangeFollowWeight

            -- gotta refresh to see new form values (shrug)
            , button [ onClick ResetDefaults ] [ text "Reset Defaults" ]
            , button [ onClick TogglePause ]
                [ text
                    (if config.paused then
                        "Unpause"

                     else
                        "Pause"
                    )
                ]
            , case boids of
                boid :: [] ->
                    div
                        [ style "font-family" "monospace"
                        , style "font-size" "11px"
                        ]
                        [ hr [] []
                        , h1 [] [ text "Debug" ]

                        --, debug "Pos" boid.pos
                        --, debug "Angle" boid.angle
                        --, debug "Target Angle" boid.targetAngle
                        ]

                _ ->
                    text ""
            ]
        ]


debug : String -> a -> (a -> String) -> Html Msg
debug title val toString =
    div
        [ style "max-height" "70px"
        , style "margin" "5px 0"
        ]
        [ title
            ++ ": "
            ++ toString val
            |> text
        ]


boidCss : Config -> Html Msg
boidCss { boidDiameter, sightDist, showSightDist } =
    let
        css =
            """
.boid {
  width: DIAMETER;
  height: DIAMETER;
  top: NEG_RAD;
  left: NEG_RAD;
}

.boid:after {
  width: DIAMETER;
  transform: translate(RAD_MINUS_ONE, RAD_MINUS_ONE);
}

.boid:before {
  width: SIGHT_DIST;
  height: SIGHT_DIST;
  transform: translate(SIGHT_OFFSET, SIGHT_OFFSET);
  display: SIGHT_DISPLAY;
}
"""
                |> String.replace "DIAMETER" (px boidDiameter)
                |> String.replace "NEG_RAD" (px (boidDiameter * -0.5))
                |> String.replace "RAD_MINUS_ONE" (px ((boidDiameter * 0.5) - 1))
                |> String.replace "SIGHT_OFFSET" (px ((boidDiameter * 0.5) + (sightDist * -1) - 1))
                |> String.replace "SIGHT_DIST" (px (sightDist * 2))
                |> String.replace "SIGHT_DISPLAY"
                    (if showSightDist then
                        "block"

                     else
                        "none"
                    )
    in
    node "style"
        [ type_ "text/css" ]
        [ text css ]


configInput : String -> Float -> (String -> Msg) -> Html Msg
configInput title val msg =
    div [ class "config-item" ]
        [ label []
            [ text title
            , input
                [ Html.Attributes.value (String.fromFloat val)
                , onInput msg
                ]
                []
            ]
        ]


configInputInt : String -> Int -> (String -> Msg) -> Html Msg
configInputInt title val msg =
    div [ class "config-item" ]
        [ label []
            [ text title
            , input
                [ Html.Attributes.value (String.fromInt val)
                , onInput msg
                ]
                []
            ]
        ]


configCheckbox : String -> Bool -> Msg -> Html Msg
configCheckbox title isChecked msg =
    div [ class "config-item" ]
        [ label []
            [ text title
            , input
                [ type_ "checkbox"
                , checked isChecked
                , onClick msg
                ]
                []
            ]
        ]


drawBoid : Config -> Torus -> Boid -> List (Html Msg)
drawBoid { boidDiameter, showSightDist, sightDist } { width, height } ({ pos } as boid) =
    let
        ( x, y ) =
            toTuple pos

        drawRad =
            if showSightDist then
                sightDist

            else
                boidDiameter

        wrappedXList =
            if x < drawRad then
                [ x, x + width ]

            else if x > width - drawRad then
                [ x, x - width ]

            else
                [ x ]

        wrappedYList =
            if y < drawRad then
                [ y, y + height ]

            else if y > height - drawRad then
                [ y, y - height ]

            else
                [ y ]

        wrappedPosList =
            wrappedXList
                |> List.map
                    (\x_ ->
                        List.map (\y_ -> fromTuple ( x_, y_ )) wrappedYList
                    )
                |> List.concat
    in
    wrappedPosList
        |> List.map
            (\pos_ ->
                drawBoidHelper height { boid | pos = pos_ }
            )


drawBoidHelper : Float -> Boid -> Html Msg
drawBoidHelper height { pos, angle, color } =
    let
        translateVal =
            -- canvas coordinates are not euclidean coordinates!
            px (getX pos) ++ "," ++ px (height - getY pos)

        rotateVal =
            -angle |> String.fromFloat

        colorStr =
            color |> Color.toCssString
    in
    div
        [ class "boid"
        , style "transform" ("translate(" ++ translateVal ++ ") rotate(" ++ rotateVal ++ "rad) scale(1,-1)")
        , style "background" colorStr
        ]
        []


px : Float -> String
px num =
    String.fromFloat num ++ "px"
