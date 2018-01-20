module View exposing (view)

import Color
import Html exposing (Html, div, h1, h2, input, label, node, text)
import Html.Attributes exposing (checked, class, defaultValue, style, type_)
import Html.Events exposing (onClick, onInput)
import Math.Vector2 as V2 exposing (Vec2, getX, getY, vec2)
import String.Extra
import Types exposing (..)


view : Model -> Html Msg
view { boids, width, height, config } =
    div [ class "container" ]
        [ boidCss config
        , div [ class "boids" ]
            (boids
                |> List.map (drawBoid config width height)
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

            --, configInput "Sight Range" config.sightDist ChangeSightDist
            --, configCheckbox "Show Sight Range" config.showSightDist ToggleSightDist
            --, h2 [] [ text "Rule 1: Fly towards center of mass" ]
            --, configInput "Cohesion" config.cohesion ChangeCohesion
            --, h2 [] [ text "Rule 2: Match velocity" ]
            --, configInput "Alignment" config.alignment ChangeAlignment
            --, h2 [] [ text "Rule 3: Avoid collisions" ]
            --, configInput "Personal Space" config.personalSpace ChangePersonalSpace
            ]
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


drawBoid : Config -> Float -> Float -> Boid -> List (Html Msg)
drawBoid { boidDiameter } width height ({ pos } as boid) =
    let
        ( x, y ) =
            V2.toTuple pos

        wrappedXList =
            if x < boidDiameter then
                [ x, x + width ]
            else if x > width - boidDiameter then
                [ x, x - width ]
            else
                [ x ]

        wrappedYList =
            if y < boidDiameter then
                [ y, y + height ]
            else if y > height - boidDiameter then
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
drawBoidHelper { pos, angle, color } =
    let
        translateVal =
            px (getX pos) ++ "," ++ px (getY pos)

        rotateVal =
            angle |> toString

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
