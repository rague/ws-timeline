module Timeline.Axis exposing (axis, getGrid, hview, vview)

import Array exposing (Array)
import Color
import Html exposing (Html)
import List.Extra as Extra
import Moment exposing (Moment(..))
import Svg exposing (g, line, text, text_)
import Svg.Attributes as SA
    exposing
        ( height
        , stroke
        , strokeWidth
        , width
        , x
        , x1
        , x2
        , y
        , y1
        , y2
        )
import Time
import Timeline.Models exposing (Direction(..))


axisUnit : Float
axisUnit =
    10


lineColor : String
lineColor =
    "#aaa"


type alias AxisDef =
    { unit : Float
    , snap : Float
    , divs :
        List
            { delta : Int
            , unit : Moment
            , hformat : Maybe String
            , vformat : Maybe String
            }
    }


lastDef : AxisDef
lastDef =
    { unit = 200
    , snap = 168
    , divs =
        [ { delta = 1, unit = Week, hformat = Just "dd", vformat = Nothing }
        , { delta = 1, unit = Month, hformat = Nothing, vformat = Nothing }
        , { delta = 1, unit = Year, hformat = Nothing, vformat = Nothing }
        ]
    }


axisDefs : List AxisDef
axisDefs =
    [ -- unit  = fraction of 1 hour
      { unit = 1 / 12
      , snap = 1 / 12
      , divs =
            [ { delta = 5, unit = Minute, hformat = Just "", vformat = Just "" }
            , { delta = 15, unit = Minute, hformat = Nothing, vformat = Nothing }
            , { delta = 1, unit = Hour, hformat = Just "EEE dd/MM", vformat = Just "EEE dd/MM" }
            ]
      }
    , { unit = 1 / 6
      , snap = 1 / 12
      , divs =
            [ { delta = 15, unit = Minute, hformat = Nothing, vformat = Nothing }
            , { delta = 1, unit = Hour, hformat = Nothing, vformat = Nothing }
            , { delta = 1, unit = Day, hformat = Just "EEE dd/MM", vformat = Just "EEE dd/MM" }
            ]
      }
    , { unit = 1 / 3
      , snap = 1 / 4
      , divs =
            [ { delta = 30, unit = Minute, hformat = Nothing, vformat = Nothing }
            , { delta = 1, unit = Hour, hformat = Nothing, vformat = Nothing }
            , { delta = 1, unit = Day, hformat = Just "EEE dd/MM", vformat = Just "EEE dd/MM" }
            ]
      }
    , { unit = 1 / 2
      , snap = 1 / 2
      , divs =
            [ { delta = 1, unit = Hour, hformat = Nothing, vformat = Nothing }
            , { delta = 6, unit = Hour, hformat = Just "", vformat = Nothing }
            , { delta = 1, unit = Day, hformat = Just "EEE dd/MM", vformat = Just "EEE dd/MM" }
            ]
      }
    , { unit = 1
      , snap = 1
      , divs =
            [ { delta = 2, unit = Hour, hformat = Nothing, vformat = Nothing }
            , { delta = 12, unit = Hour, hformat = Just "", vformat = Nothing }
            , { delta = 1, unit = Day, hformat = Just "EEE dd/MM", vformat = Just "EEE dd/MM" }
            ]
      }
    , { unit = 2
      , snap = 2
      , divs =
            [ { delta = 4, unit = Hour, hformat = Nothing, vformat = Nothing }
            , { delta = 12, unit = Hour, hformat = Just "", vformat = Nothing }
            , { delta = 1, unit = Day, hformat = Just "EEE dd/MM", vformat = Just "EEE dd/MM" }
            ]
      }
    , { unit = 4
      , snap = 6
      , divs =
            [ { delta = 12, unit = Hour, hformat = Just "", vformat = Nothing }
            , { delta = 1, unit = Day, hformat = Nothing, vformat = Nothing }
            , { delta = 1, unit = Month, hformat = Just "MMM y", vformat = Just "MMM y" }
            ]
      }
    , { unit = 6
      , snap = 12
      , divs =
            [ { delta = 1, unit = Day, hformat = Nothing, vformat = Nothing }
            , { delta = 1, unit = Week, hformat = Nothing, vformat = Nothing }
            , { delta = 1, unit = Month, hformat = Just "MMM y", vformat = Just "MMM y" }
            ]
      }
    , { unit = 12
      , snap = 12
      , divs =
            [ { delta = 1, unit = Day, hformat = Just "dd", vformat = Nothing }
            , { delta = 1, unit = Week, hformat = Nothing, vformat = Nothing }
            , { delta = 1, unit = Month, hformat = Just "MMM y", vformat = Just "MMM y" }
            ]
      }
    , { unit = 40
      , snap = 24
      , divs =
            [ { delta = 1, unit = Day, hformat = Just "", vformat = Nothing }
            , { delta = 1, unit = Week, hformat = Just "EEE dd", vformat = Nothing }
            , { delta = 1, unit = Month, hformat = Just "MMM y", vformat = Just "MMM y" }
            ]
      }
    -- ,{ unit = 80
    -- , snap = 168
    -- , divs =
    --     [ { delta = 1, unit = Week, hformat = Just "EEE dd", vformat = Nothing }
    --     , { delta = 1, unit = Month, hformat = Nothing, vformat = Nothing }
    --     , { delta = 1, unit = Year, hformat = Nothing, vformat = Nothing }
    --     ]
    -- }
    , lastDef
    ]


axisWeight : Array Float
axisWeight =
    Array.fromList [ 0.5, 1, 2 ]


fontSize : Array Float
fontSize =
    Array.fromList [ 9, 10, 11 ]


type Instruction
    = DrawLine Float Float Float
    | DrawText Float Float Float String


getGrid : Float -> AxisDef
getGrid val =
    let
        unit =
            val / 3600000 * axisUnit
    in
    Extra.find (\g -> unit < g.unit) axisDefs
        |> Maybe.withDefault lastDef


axis : Direction -> Time.Zone -> Float -> Float -> Float -> List Instruction
axis dir zone from to size =
    let
        duration =
            to - from

        fac =
            size / duration

        grid =
            getGrid (duration / size)

        safe =
            if dir == Horizontal then
                70

            else
                20
    in
    List.concat <|
        List.indexedMap
            (\wi div ->
                let
                    dt =
                        Time.millisToPosix (round from)

                    start =
                        Moment.startOf div.unit zone div.delta dt

                    makei list last =
                        let
                            date =
                                Moment.add div.unit div.delta zone last

                            pos =
                                ((Time.posixToMillis last |> toFloat) - from) * fac

                            new =
                                List.append
                                    list
                                    [ DrawLine pos
                                        (toFloat wi)
                                        (Array.get wi axisWeight
                                            |> Maybe.withDefault 0.05
                                        )
                                    , DrawText pos
                                        (toFloat wi)
                                        (Array.get wi fontSize
                                            |> Maybe.withDefault 10
                                        )
                                        (Moment.format zone
                                            div.unit
                                            (if dir == Horizontal then
                                                div.hformat

                                             else
                                                div.vformat
                                            )
                                            last
                                        )
                                    ]
                        in
                        if Time.posixToMillis date < round to then
                            makei new date

                        else
                            new

                    inslist =
                        makei [] start
                in
                case inslist of
                    l1 :: (DrawText timepos1 indexpos1 size1 label1) :: l2 :: ((DrawText timepos2 _ _ _) as t2) :: xs ->
                        l1 :: DrawText (max timepos1 (min (timepos2 - safe) (max 0 timepos1))) indexpos1 size1 label1 :: l2 :: t2 :: xs

                    l1 :: (DrawText timepos1 indexpos1 size1 label1) :: xs ->
                        l1 :: DrawText (max 0 timepos1) indexpos1 size1 label1 :: xs

                    _ ->
                        inslist
            )
            grid.divs


hview : List (Html.Attribute msg) -> Time.Zone -> Int -> Int -> Float -> Float -> Html msg
hview attrs zone width height from to =
    let
        instructions =
            axis Horizontal zone from to (toFloat width)
    in
    Svg.svg
        (attrs
            ++ [ SA.width (String.fromInt width)
               , SA.height (String.fromInt height)
               , SA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
               , SA.style <|
                    "position:absolute;top:0px;left:0px;width:"
                        ++ String.fromInt width
                        ++ "px;height:"
                        ++ String.fromInt height
                        ++ "px;user-select: none;"

               -- ++"background-color: red;"
               ]
        )
        [ Svg.g [] <|
            List.map
                (\ins ->
                    case ins of
                        DrawLine left top weight ->
                            line
                                [ x1 <| String.fromFloat left
                                , x2 <| String.fromFloat left
                                , y1 <| String.fromFloat (-15 * top + 35)
                                , y2 <| String.fromInt height

                                -- , stroke lineColor
                                , stroke (Color.hsl 1 0 (1 - (weight / 5)) |> Color.toCssString)
                                , strokeWidth "1" --<| String.fromFloat weight
                                ]
                                []

                        DrawText left top size label ->
                            text_
                                [ x <| String.fromFloat (2 + left)
                                , y <| String.fromFloat (-15 * top + 45)
                                , SA.style ("font-size:" ++ String.fromFloat size ++ "px")
                                ]
                                [ text label ]
                )
                instructions
        ]


vview : List (Html.Attribute msg) -> Time.Zone -> Int -> Int -> Float -> Float -> Html msg
vview attrs zone width height from to =
    let
        instructions =
            axis Vertical zone from to (toFloat height)
    in
    Svg.svg
        (attrs
            ++ [ SA.width (String.fromInt width)
               , SA.height (String.fromInt height)
               , SA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
               , SA.style <|
                    "position:absolute;top:0px;left:0px;width:"
                        ++ String.fromInt width
                        ++ "px;height:"
                        ++ String.fromInt height
                        ++ "px;user-select: none;"
               ]
        )
        [ Svg.g [] <|
            List.map
                (\ins ->
                    case ins of
                        DrawLine top left weight ->
                            line
                                [ y1 <| String.fromFloat top
                                , y2 <| String.fromFloat top
                                , x1 <| String.fromFloat (-60 * left + 120)
                                , x2 <| String.fromInt width
                                , stroke lineColor
                                , strokeWidth <| String.fromFloat weight
                                ]
                                []

                        DrawText top left size label ->
                            text_
                                [ y <| String.fromFloat (12 + top)
                                , x <| String.fromFloat (-60 * left + 175)
                                , SA.textAnchor "end"
                                , SA.style ("font-size:" ++ String.fromFloat size ++ "px")
                                ]
                                [ text label ]
                )
                instructions
        ]
