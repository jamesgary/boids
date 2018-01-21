module View exposing (view)

import Color exposing (Color)
import Html exposing (Html, button, div, h1, h2, hr, input, label, node, text)
import Html.Attributes exposing (checked, class, defaultValue, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Math.Vector2 as V2 exposing (Vec2, getX, getY, vec2)
import String.Extra
import Torus exposing (Torus)
import Types exposing (..)


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
            , configInput "# of Boids" config.numBoids ChangeNumBoids
            , configInput "Speed" config.vel ChangeVel
            , configInput "Boid Size" config.boidDiameter ChangeBoidDiameter
            , h2 [] [ text "Rule 1: Wiggle!" ]
            , configInput "Jerkiness" config.jerkiness ChangeJerkiness
            , configInput "Max Turn Rate" config.maxTurnRate ChangeMaxTurnRate
            , configInput "Sight Range" config.sightDist ChangeSightDist
            , configCheckbox "Show Sight Range" config.showSightDist ToggleSightDist

            -- gotta refresh to see new form values (shrug)
            , button [ onClick ResetDefaults ] [ text "Reset Defaults" ]
            , case boids of
                boid :: [] ->
                    div
                        [ style
                            [ ( "font-family", "monospace" )
                            , ( "font-size", "11px" )
                            ]
                        ]
                        [ hr [] []
                        , h1 [] [ text "Debug" ]
                        , debug "Pos" boid.pos
                        , debug "Angle" boid.angle
                        , debug "Target Angle" boid.targetAngle
                        ]

                _ ->
                    text ""
            ]
        ]


debug : String -> a -> Html Msg
debug title val =
    div
        [ style
            [ ( "max-height", "70px" )
            , ( "margin", "5px 0" )
            ]
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
                |> String.Extra.replace "DIAMETER" (px boidDiameter)
                |> String.Extra.replace "NEG_RAD" (px (boidDiameter * -0.5))
                |> String.Extra.replace "RAD_MINUS_ONE" (px ((boidDiameter * 0.5) - 1))
                |> String.Extra.replace "SIGHT_OFFSET" (px ((boidDiameter * 0.5) + (sightDist * -1) - 1))
                |> String.Extra.replace "SIGHT_DIST" (px (sightDist * 2))
                |> String.Extra.replace "SIGHT_DISPLAY"
                    (if showSightDist then
                        "block"
                     else
                        "none"
                    )
    in
    node "style"
        [ type_ "text/css" ]
        [ text css ]


configInput : String -> number -> (String -> Msg) -> Html Msg
configInput title val msg =
    div [ class "config-item" ]
        [ label []
            [ text title
            , input
                [ defaultValue (toString val)
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
            V2.toTuple pos

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
                    (\x ->
                        List.map (\y -> V2.fromTuple ( x, y )) wrappedYList
                    )
                |> List.concat
    in
    wrappedPosList
        |> List.map
            (\pos ->
                drawBoidHelper height { boid | pos = pos }
            )


drawBoidHelper : Float -> Boid -> Html Msg
drawBoidHelper height { pos, angle, color } =
    let
        translateVal =
            -- canvas coordinates are not euclidean coordinates!
            px (getX pos) ++ "," ++ px (height - getY pos)

        rotateVal =
            -angle |> toString

        colorStr =
            color |> toRgb
    in
    div
        [ class "boid"
        , style
            [ ( "transform"
              , "translate(" ++ translateVal ++ ") rotate(" ++ rotateVal ++ "rad) scale(1,-1)"
              )
            , ( "background", colorStr )
            ]
        ]
        []


toRgb : Color -> String
toRgb color =
    color
        |> Color.toRgb
        |> (\{ red, green, blue } -> "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")")


px : number -> String
px num =
    toString num ++ "px"
