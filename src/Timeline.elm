module Timeline exposing (Msg(..), applyAction, calcLayersSize, canEditGroups, canEditSections, canSortGroups, changeDirection, changeGroupsSize, changeLineSize, changeStartAndZoom, changeYOffset, displayAxis, init, reinit, setLanguage, setWrapText, styles, subscriptions, update, view, zoomAllTime)

import Browser.Dom
import Browser.Events
import Cldr.Format.DateTime as FDateTime
import Cldr.Format.Length as FLength
import Cldr.Locale exposing (Locale, th)
import Color
import Dict exposing (Dict)
import DnDList
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Html.Keyed as HKeyed
import Html.Lazy
import Json.Decode as Decode exposing (int)
import List.Extra as Extra
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4)
import Moment
import Set
import Svg.Attributes exposing (height, width, x, y)
import Svg.Events exposing (..)
import Task
import Time exposing (Posix)
import TimeZone
import Timeline.Action exposing (..)
import Timeline.Axis as Axis
import Timeline.Event exposing (..)
import Timeline.Models exposing (..)
import Timeline.Update exposing (..)
import Timeline.Utils exposing (findSection)
import Tuple exposing (first, second)
import WebGL exposing (Mesh, Shader)


subscriptions : TimelineBox -> Sub Msg
subscriptions box =
    Sub.batch
        [ if box.canSortGroups then
            (system box.direction).subscriptions box.dnd

          else
            Sub.none
        , case box.interaction of
            ResizeGroups _ ->
                let
                    field =
                        if box.direction == Horizontal then
                            "pageX"

                        else
                            "pageY"
                in
                Sub.batch
                    [ Browser.Events.onMouseMove (Decode.field field Decode.int |> Decode.map GroupsBarResizing)
                    , Browser.Events.onMouseUp (Decode.succeed EndGroupsBarResize)
                    ]

            _ ->
                Sub.none
        , if box.standby == False then
            Browser.Events.onMouseUp (Decode.succeed StopInteraction)

          else
            Sub.none
        , if box.zoomChange > 0 then
            Browser.Events.onAnimationFrame (\_ -> NoOp)

          else
            Sub.none
        , if Time.posixToMillis box.currentPosix == 0 then
            Time.every 200 UpdateTime

          else
            Time.every 20000 UpdateTime
        ]



------------
-- layering
------------


calcLayer : List ( Period a, Int ) -> Period a -> Int
calcLayer prevlist duration =
    let
        max =
            Extra.maximumBy Tuple.second prevlist
                |> Maybe.map Tuple.second
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault 0

        rev =
            List.reverse prevlist
    in
    Maybe.withDefault 0 <|
        Extra.find
            (\i ->
                Extra.find (\( _, lay ) -> lay == i) rev
                    |> Maybe.map (\( dur, _ ) -> Moment.lessOrEqualThan dur.end duration.start)
                    |> Maybe.withDefault True
            )
            (List.range 0 max)


toLayers : List (Period a) -> List ( Period a, Int )
toLayers =
    List.foldl
        (\dur layered ->
            List.append layered [ ( dur, calcLayer layered dur ) ]
        )
        []



-----------
-- calcul displayable
-----------


calcLayersSize : List ( Period a, Int ) -> Int
calcLayersSize group =
    Extra.maximumBy second group
        |> Maybe.map (second >> (+) 1)
        |> Maybe.withDefault 1


generateSectionBoxes :
    ( GroupBox, List ( Period Sectionic, Int ) )
    -> List SectionBox
    -> List SectionBox
generateSectionBoxes ( gbox, layers ) boxes =
    List.append boxes <|
        List.map
            (\( s, level ) ->
                { groupId = gbox.id
                , section = s
                , line = level + gbox.position
                }
            )
            layers


default : Time.Zone -> Posix -> TimelineBox
default zone posix =
    let
        dir =
            Horizontal
    in
    { groups = Dict.empty
    , srcgroups = []
    , groupsLen = 0
    , lines = 0
    , sections = []
    , meshesSelection = emptySelection
    , selectedMeshes = Dict.empty
    , meshes = Dict.empty
    , sectionOffsetY = 0
    , start = 0
    , zoom = 50
    , zoomChange = 0
    , lineSize = 38
    , first = posix
    , direction = dir
    , selection = emptySelection
    , interaction = MouseOver ( Time.millisToPosix -1, -1 )
    , standby = False
    , dnd = (system dir).model
    , locale = Cldr.Locale.en
    , zone = zone
    , canSortGroups = True
    , canEditGroups = True
    , canEditSections = True
    , displayAxis = True
    , wrapText = False
    , currentPosix = Time.millisToPosix 0
    , groupsSize = 250
    }



-- dnd


config : DnDList.Config GroupBox
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Vertical
    , listen = DnDList.OnDrop
    , operation = DnDList.InsertBefore
    }


system : Direction -> DnDList.System GroupBox Msg
system dir =
    DnDList.create
        { config
            | movement =
                if dir == Vertical then
                    DnDList.Horizontal

                else
                    DnDList.Vertical
        }
        DndMsg



-- fin dnd


init : List Group -> Time.Zone -> Posix -> TimelineBox
init groups zone posix =
    toTimelineBox groups (default zone posix)


reinit : List Group -> TimelineBox -> TimelineBox
reinit groups old =
    toTimelineBox groups old


applyAction : Action -> TimelineBox -> TimelineBox
applyAction action tl =
    case action of
        Timeline.Action.ModifySections ids ( addstart, addend ) ->
            let
                groups =
                    tl.srcgroups
                        |> List.map
                            (\g ->
                                { g
                                    | sections =
                                        List.map
                                            (\s ->
                                                if isSelected g.id s.id ids then
                                                    let
                                                        start =
                                                            Moment.addDurationToPosix s.start addstart
                                                    in
                                                    { s
                                                        | start = start
                                                        , end =
                                                            Moment.addDurationToPosix start <|
                                                                (max (Moment.fromDuration addend + Time.posixToMillis s.end - Time.posixToMillis s.start) 0 |> Moment.toDuration)
                                                    }

                                                else
                                                    s
                                            )
                                            g.sections
                                }
                            )
            in
            reinit groups tl

        Timeline.Action.MoveSections ids gid ->
            let
                sections =
                    selectedSections tl

                groups =
                    tl.srcgroups
                        |> List.map
                            (\g ->
                                let
                                    filtered =
                                        List.filter
                                            (\s ->
                                                isSelected g.id s.id ids
                                                    |> not
                                            )
                                            g.sections
                                in
                                { g
                                    | sections =
                                        if gid == g.id then
                                            filtered
                                                ++ sections
                                                |> List.sortBy (.start >> Time.posixToMillis)

                                        else
                                            filtered
                                }
                            )
            in
            reinit groups tl

        _ ->
            tl


zoomAllTime : Int -> TimelineBox -> TimelineBox
zoomAllTime width tl =
    let
        mbfirst =
            List.head tl.sections
                |> Maybe.map (.section >> .start >> Time.posixToMillis)

        mblast =
            Extra.last tl.sections
                |> Maybe.map (.section >> .end >> Time.posixToMillis)
    in
    case ( mbfirst, mblast ) of
        ( Just first, Just last ) ->
            let
                dur =
                    (last - first |> Basics.toFloat) / duration.day
            in
            { tl
                | start = toFloat (width - tl.groupsSize) * 0.02
                , zoom = toFloat (width - tl.groupsSize) / dur * 0.96
            }

        _ ->
            tl


zoomOver : Posix -> Posix -> Int -> TimelineBox -> TimelineBox
zoomOver first last size tl =
    let
        firstms =
            Time.posixToMillis first

        lastms =
            Time.posixToMillis last

        dur =
            (lastms - firstms |> Basics.toFloat) / duration.day

        zoom =
            toFloat (size - tl.groupsSize) / dur * 0.96
    in
    { tl
        | start = ((Time.posixToMillis tl.first - firstms) |> toFloat) * (zoom / duration.day) + (toFloat (size - tl.groupsSize) * 0.02)
        , zoom = zoom
        , interaction = MouseOver ( Time.millisToPosix -1, -1 )
    }


showDate : Posix -> Int -> TimelineBox -> TimelineBox
showDate first size tl =
    let
        firstms =
            Time.posixToMillis first
    in
    { tl
        | start = ((Time.posixToMillis tl.first - firstms) |> toFloat) * (tl.zoom / duration.day) + (toFloat (size - tl.groupsSize) * 0.02)
        , interaction = MouseOver ( Time.millisToPosix -1, -1 )
    }


unscale zoom delta =
    (delta * duration.day) / zoom


changeStartAndZoom : Posix -> Float -> TimelineBox -> TimelineBox
changeStartAndZoom posix zoom tl =
    let
        start =
            -((Time.posixToMillis posix - Time.posixToMillis tl.first |> toFloat) * zoom / duration.day)
    in
    { tl | start = start, zoom = zoom }


changeYOffset : Float -> TimelineBox -> TimelineBox
changeYOffset y tl =
    { tl | sectionOffsetY = y }


changeLineSize : Float -> TimelineBox -> TimelineBox
changeLineSize s tl =
    { tl | lineSize = s }


changeGroupsSize : Int -> TimelineBox -> TimelineBox
changeGroupsSize s tl =
    { tl | groupsSize = max 40 s }


toTimelineBox : List Group -> TimelineBox -> TimelineBox
toTimelineBox groups base =
    let
        foldfunc group res =
            let
                ( globalSections, normalSections ) =
                    List.partition .isGlobal group.sections

                -- ( [], group.sections )
                layers =
                    toLayers normalSections

                size =
                    calcLayersSize layers

                position =
                    Extra.last res
                        |> Maybe.map (first >> (\g -> g.size + g.position))
                        |> Maybe.withDefault 0
            in
            res
                ++ [ ( { id = group.id
                       , position = position
                       , size = size
                       , label = group.label
                       , isSubtotal = group.isSubtotal
                       , sections =
                            List.map
                                (\s ->
                                    { groupId = group.id
                                    , section = s
                                    , line = 1
                                    }
                                )
                                globalSections
                                ++ List.map
                                    (\( s, level ) ->
                                        { groupId = group.id
                                        , section = s
                                        , line = level
                                        }
                                    )
                                    layers
                       }
                     , List.map (\s -> ( s, 0 )) globalSections ++ layers
                     )
                   ]

        grouped =
            List.foldl foldfunc [] groups

        groupboxes =
            List.unzip grouped |> first

        groupsDict =
            groupboxes |> List.map (\g -> ( g.id, g )) |> Dict.fromList

        sections =
            List.foldl generateSectionBoxes [] grouped |> List.sortBy (.section >> .start >> Time.posixToMillis)

        --start =
        --    List.head sections
        --        |> Maybe.map (\s -> -s.start / duration.day * base.zoom)
        --        |> Maybe.withDefault base.start
        firstDate =
            List.head sections
                |> Maybe.map (\s -> s.section.start)
                |> Maybe.withDefault base.first

        groupsDiff =
            Dict.merge (\gid _ res -> gid :: res)
                (\gid a b res ->
                    if a == b && firstDate == base.first then
                        res

                    else
                        gid :: res
                )
                (\gid _ res -> gid :: res)
                base.groups
                groupsDict
                []

        ( meshes, selected ) =
            meshesForGroups firstDate groupsDict groupsDiff base.meshesSelection ( base.meshes, base.selectedMeshes )

        deltaStart =
            if Time.posixToMillis base.first == 0 then
                0

            else
                (Time.posixToMillis firstDate - Time.posixToMillis base.first) |> toFloat |> (*) (base.zoom / duration.day)
    in
    { base
        | groups = groupsDict
        , srcgroups = groups
        , groupsLen = List.length groups
        , lines = List.map .size groupboxes |> List.sum
        , sections = sections
        , meshesSelection = base.meshesSelection
        , selectedMeshes = selected
        , meshes = meshes
        , start = base.start + deltaStart
        , zoom = base.zoom
        , zoomChange = base.zoomChange
        , lineSize = base.lineSize
        , first = firstDate
        , standby = base.standby
        , dnd = base.dnd
        , zone = base.zone
    }


canSortGroups : Bool -> TimelineBox -> TimelineBox
canSortGroups b tl =
    { tl | canSortGroups = b }


canEditGroups : Bool -> TimelineBox -> TimelineBox
canEditGroups b tl =
    { tl | canEditGroups = b }


canEditSections : Bool -> TimelineBox -> TimelineBox
canEditSections b tl =
    { tl | canEditSections = b }


changeDirection : Direction -> TimelineBox -> TimelineBox
changeDirection dir tl =
    { tl
        | direction = dir
        , dnd = (system dir).model
    }


displayAxis : Bool -> TimelineBox -> TimelineBox
displayAxis b tl =
    { tl | displayAxis = b }


setWrapText : Bool -> TimelineBox -> TimelineBox
setWrapText bool tl =
    { tl
        | wrapText = bool
    }


setLanguage : String -> TimelineBox -> TimelineBox
setLanguage str tl =
    { tl | locale = Cldr.Locale.fromString Cldr.Locale.basicLocales str |> Maybe.withDefault Cldr.Locale.en }


axisHeight : number
axisHeight =
    50


axisWidth : number
axisWidth =
    180


axisAttrs =
    [ wheelEvent SectionsWheel, moveY0Event SectionsMove ]


view : List (Html.Attribute Msg) -> TimelineBox -> { width : Int, height : Int } -> Html Msg
view attrs box rect =
    let
        lateral =
            if box.direction == Horizontal then
                box.groupsSize

            else
                0

        top =
            if box.direction == Horizontal then
                0

            else
                box.groupsSize

        axisSize =
            if box.displayAxis then
                if box.direction == Horizontal then
                    axisHeight

                else
                    axisWidth

            else
                0

        width =
            rect.width - lateral

        height =
            rect.height - top

        from =
            -- box.first + unscale box.zoom -box.start
            unscale box.zoom -box.start + (Time.posixToMillis box.first |> toFloat)

        end =
            from
                + (unscale box.zoom <|
                    toFloat
                        (if box.direction == Horizontal then
                            width

                         else
                            height
                        )
                  )

        grid =
            Axis.getGrid (duration.day / box.zoom)

        unit =
            grid.snap * 3600000 |> round

        mbcursor =
            (case box.interaction of
                MouseOver ( time, _ ) ->
                    time |> snapToGridForZoom box.zoom box.zone |> Just

                Draw _ time _ ->
                    Just time

                Move _ sbox _ ( dur, _ ) ->
                    Moment.addDurationToPosix sbox.section.start dur
                        |> snapToGridForZoom box.zoom box.zone
                        |> Just

                ResizeLeft ( time, _ ) dur ->
                    Moment.addDurationToPosix time (Moment.mapDuration (\d -> (toFloat d / toFloat unit |> round) * unit) dur)
                        |> snapToGridForZoom box.zoom box.zone
                        |> Just

                ResizeRight ( time, _ ) dur ->
                    Moment.addDurationToPosix time (Moment.mapDuration (\d -> (toFloat d / toFloat unit |> round) * unit) dur)
                        |> snapToGridForZoom box.zoom box.zone
                        |> Just

                _ ->
                    Nothing
            )
                |> Maybe.map
                    (\snapped ->
                        case box.direction of
                            Horizontal ->
                                ((snapped |> Time.posixToMillis |> toFloat) - from)
                                    * toFloat width
                                    / (end - from)
                                    |> round
                                    |> Tuple.pair snapped

                            Vertical ->
                                ((snapped |> Time.posixToMillis |> toFloat) - from)
                                    * toFloat height
                                    / (end - from)
                                    |> round
                                    |> Tuple.pair snapped
                    )

        currentTime =
            case box.direction of
                Horizontal ->
                    ((box.currentPosix |> Time.posixToMillis |> toFloat) - from)
                        * toFloat width
                        / (end - from)
                        |> round

                Vertical ->
                    ((box.currentPosix |> Time.posixToMillis |> toFloat) - from)
                        * toFloat height
                        / (end - from)
                        |> round
    in
    Html.div
        ([ HA.style "width" (String.fromInt rect.width ++ "px")
         , HA.style "height" (String.fromInt rect.height ++ "px")
         , HA.style "position" "relative"
         , HA.class "timeline"
         , HA.tabindex 0
         , Html.Events.on "keyup" (Decode.map Keypress Html.Events.keyCode)
         , case box.interaction of
            ResizeGroups _ ->
                HA.class ""

            _ ->
                mouseUpEvent SectionsUp
         , HA.style "cursor" (mouseCursor box)
         ]
            ++ attrs
        )
        [ Html.div
            (if box.direction == Horizontal then
                [ HA.style "position" "relative"
                , HA.style "width" (String.fromInt lateral ++ "px")
                , HA.style "height" (String.fromInt (rect.height - axisSize) ++ "px")
                , HA.style "top" (String.fromInt axisSize ++ "px")
                , HA.style "overflow" "hidden"
                , wheelEvent GroupsWheel
                , moveX0Event SectionsMove
                ]

             else
                [ HA.style "position" "relative"
                , HA.style "width" (String.fromInt (rect.width - axisSize) ++ "px")
                , HA.style "height" (String.fromInt top ++ "px")
                , HA.style "left" (String.fromInt axisSize ++ "px")
                , HA.style "overflow" "hidden"
                , wheelEvent GroupsWheel
                , moveY0Event SectionsMove
                ]
            )
            [ Html.Lazy.lazy2 drawGroups
                box
                box.groupsSize
            ]
        , Html.div
            (if box.direction == Horizontal then
                [ HA.class "groups-separator"
                , HA.style "position" "absolute"
                , HA.style "left" (String.fromInt (lateral - 1) ++ "px")
                , HA.style "height" "100%"
                , HA.style "top" "0"
                , HA.style "width" "1px"
                , HA.style "box-sizing" "border-box"
                , HA.style "border-right" "1px solid #ddd"
                ]

             else
                [ HA.class "groups-separator"
                , HA.style "position" "absolute"
                , HA.style "width" "100%"
                , HA.style "top" (String.fromInt (top - 1) ++ "px")
                , HA.style "left" "0"
                , HA.style "height" "1px"
                , HA.style "box-sizing" "border-box"
                , HA.style "border-bottom" "1px solid #ddd"
                ]
            )
            []
        , Html.div
            (if box.direction == Horizontal then
                [ HA.class "resize-handle-horiz"
                , HA.style "left" (String.fromInt (lateral - 2) ++ "px")
                , Html.Events.on "mousedown"
                    (Decode.field "pageX" Decode.int
                        |> Decode.map StartGroupsBarResize
                    )
                ]

             else
                [ HA.class "resize-handle-vert"
                , Html.Events.on "mousedown"
                    (Decode.field "pageY" Decode.int
                        |> Decode.map StartGroupsBarResize
                    )
                ]
            )
            []
        , if box.direction == Horizontal then
            Html.div
                [ HA.style "position" "absolute"
                , HA.style "overflow" "hidden"
                , HA.style "left" (String.fromInt lateral ++ "px")
                , HA.style "top" (String.fromInt (top + axisSize) ++ "px")
                , HA.style "width" <| String.fromInt width ++ "px"
                , HA.style "height" <| String.fromInt (height - axisSize) ++ "px"
                ]
                [ Html.div
                    [ HA.style "position" "absolute"
                    , HA.style "overflow" "hidden"
                    , HA.style "left" "0px"
                    , HA.style "width" <|
                        String.fromInt width
                            ++ "px"
                    , HA.style "top" <|
                        (String.fromFloat
                            box.sectionOffsetY
                            ++ "px"
                        )
                    ]
                    [ Html.Lazy.lazy3 drawRowsBackground box.groups box.lineSize box.selection ]
                ]

          else
            Html.div
                [ HA.style "position" "absolute"
                , HA.style "overflow" "hidden"
                , HA.style "left" (String.fromInt (lateral + axisSize) ++ "px")
                , HA.style "top" (String.fromInt top ++ "px")
                , HA.style "width" <| String.fromInt (width - axisSize) ++ "px"
                , HA.style "height" <| String.fromInt height ++ "px"
                ]
                [ Html.div
                    [ HA.style "position" "absolute"
                    , HA.style "overflow" "hidden"
                    , HA.style "top" "0px"
                    , HA.style "width" <| (List.map .size (Dict.values box.groups) |> List.sum |> toFloat |> (*) box.lineSize |> ceiling |> String.fromInt) ++ "px"
                    , HA.style "height" <|
                        String.fromInt height
                            ++ "px"
                    , HA.style "left" <|
                        (String.fromFloat
                            box.sectionOffsetY
                            ++ "px"
                        )
                    ]
                    [ Html.Lazy.lazy3 drawColsBackground box.groups box.lineSize box.selection ]
                ]
        , case ( mbcursor, box.standby ) of
            ( Just ( posix, pos ), False ) ->
                let
                    date =
                        Moment.formatI18n box.locale box.zone "yyyy-MM-dd " posix

                    time =
                        FDateTime.format
                            (FDateTime.TimeOnly FLength.Short)
                            box.locale
                            box.zone
                            posix
                in
                case box.direction of
                    Horizontal ->
                        Html.div
                            [ HA.style "position" "absolute"
                            , HA.style "left" (String.fromInt (pos + lateral) ++ "px")
                            , HA.style "top" (String.fromInt top ++ "px")

                            -- , HA.style "width" "0"
                            , HA.style "height" (String.fromInt height ++ "px")
                            , HA.style "border-left" "1px solid steelblue"
                            , HA.style "z-index" "1"
                            , HA.style "pointer-events" "none"
                            ]
                            [ Html.div
                                [ HA.style "font-size" "12px"
                                , HA.style "text-wrap" "nowrap"
                                , HA.style "line-height" "1.2"
                                , HA.style "background-color" "steelblue"
                                , HA.style "color" "white"
                                , HA.style "padding" "3px"
                                , HA.style "position" "relative"
                                ]
                                [ Html.div [] [ Html.text date ]
                                , Html.div [] [ Html.text time ]
                                ]
                            ]

                    Vertical ->
                        Html.div
                            [ HA.style "position" "absolute"
                            , HA.style "left" (String.fromInt (lateral + 5) ++ "px")
                            , HA.style "top" (String.fromInt (top + pos) ++ "px")
                            , HA.style "width" (String.fromInt width ++ "px")
                            , HA.style "height" "0"
                            , HA.style "border-top" "1px solid steelblue"
                            , HA.style "z-index" "1"
                            , HA.style "pointer-events" "none"
                            ]
                            [ Html.div
                                [ HA.style "font-size" "12px"
                                , HA.style "text-wrap" "nowrap"
                                , HA.style "line-height" "1.5"
                                , HA.style "background-color" "steelblue"
                                , HA.style "box-sizing" "border-box"
                                , HA.style "color" "white"
                                , HA.style "padding" "3px"
                                , HA.style "width" "75px"
                                ]
                                [ Html.div [] [ Html.text date ]
                                , Html.div [] [ Html.text time ]
                                ]
                            ]

            _ ->
                Html.text ""
        , Html.div
            [ HA.style "position" "absolute"
            , HA.style "left" (String.fromInt lateral ++ "px")
            , HA.style "top" (String.fromInt top ++ "px")
            , HA.style "width" <| String.fromInt width ++ "px"
            , HA.style "height" <| String.fromInt height ++ "px"

            -- , HA.style "z-index" "1"
            ]
            [ if box.direction == Horizontal then
                Html.Lazy.lazy8 Axis.hview axisAttrs box.locale box.zone box.displayAxis width height from end

              else
                Html.Lazy.lazy8 Axis.vview axisAttrs box.locale box.zone box.displayAxis width height from end
            ]
        , if box.direction == Horizontal then
            Html.div
                [ HA.style "position" "absolute"
                , HA.style "overflow" "hidden"
                , HA.style "left" (String.fromInt lateral ++ "px")
                , HA.style "top" (String.fromInt (top + axisSize) ++ "px")
                , HA.style "width" <| String.fromInt width ++ "px"
                , HA.style "height" <| String.fromInt (height - axisSize) ++ "px"
                ]
                [ sectionsView box box.sections width (height - axisSize) from end ]

          else
            Html.div
                [ HA.style "position" "absolute"
                , HA.style "overflow" "hidden"
                , HA.style "left" (String.fromInt (lateral + axisSize) ++ "px")
                , HA.style "top" (String.fromInt top ++ "px")
                , HA.style "width" <| String.fromInt width ++ "px"
                , HA.style "height" <| String.fromInt height ++ "px"
                ]
                [ sectionsView box box.sections (width - axisSize) height from end ]
        , if Moment.between box.currentPosix (Time.millisToPosix (round from)) (Time.millisToPosix (round end)) then
            if box.direction == Horizontal then
                Html.div
                    [ HA.style "position" "absolute"
                    , HA.style "left" (String.fromInt (currentTime + lateral) ++ "px")
                    , HA.style "top" (String.fromInt (top + 5) ++ "px")
                    , HA.style "width" "0"
                    , HA.style "height" (String.fromInt height ++ "px")
                    , HA.style "border-left" "2px solid rgba(255,100,0,0.7)"
                    , HA.style "pointer-events" "none"
                    ]
                    []

            else
                Html.div
                    [ HA.style "position" "absolute"
                    , HA.style "left" (String.fromInt (lateral + 5) ++ "px")
                    , HA.style "top" (String.fromInt (currentTime + top) ++ "px")
                    , HA.style "height" "0"
                    , HA.style "width" (String.fromInt width ++ "px")
                    , HA.style "border-top" "2px solid rgba(255,100,0,0.7)"
                    , HA.style "pointer-events" "none"
                    ]
                    []

          else
            Html.text ""
        ]


drawRowsBackground : Dict GroupId GroupBox -> Float -> Selection -> Html Msg
drawRowsBackground groups lineSize sel =
    Html.div [] <|
        List.indexedMap
            (\j ( id, g ) ->
                Html.div
                    [ if modBy 2 j == 0 then
                        HA.class "group even"

                      else
                        HA.class "group odd"
                    , if isGroupSelected id sel then
                        HA.class "selected"

                      else
                        HA.class ""
                    , HA.style "width" "100%"
                    , HA.style "height" <| String.fromFloat (lineSize * toFloat g.size) ++ "px"
                    ]
                    []
            )
            (groups |> Dict.toList |> List.sortBy (Tuple.second >> .position))


drawColsBackground : Dict GroupId GroupBox -> Float -> Selection -> Html Msg
drawColsBackground groups lineSize sel =
    Html.div
        [ HA.style "height" "100%" ]
    <|
        List.indexedMap
            (\j ( id, g ) ->
                Html.div
                    [ if modBy 2 j == 0 then
                        HA.class "group veven"

                      else
                        HA.class "group odd"
                    , if isGroupSelected id sel then
                        HA.class "selected"

                      else
                        HA.class ""
                    , HA.style "width" <| String.fromFloat (lineSize * toFloat g.size) ++ "px"
                    , HA.style "height" "100%"
                    , HA.style "display" "inline-block"
                    ]
                    []
            )
            (groups |> Dict.toList |> List.sortBy (Tuple.second >> .position))


type Msg
    = NoOp
    | SectionsWheel Event
    | SectionsMove Event
    | SectionsDown Event
    | SectionsUp Event
    | SectionsOver Event
    | SectionsOut
    | SectionsDoubleClick
    | StopInteraction
    | DndMsg DnDList.Msg
    | GroupsWheel Event
    | SelectGroup GroupId Bool
    | DoubleClickGroup GroupId
    | UpdateGroupLabel GroupId String
    | ValidateGroupLabel GroupId String
    | CancelGroupLabelEdit
    | Keypress Int
    | UpdateTime Time.Posix
    | StartGroupsBarResize Int
    | GroupsBarResizing Int
    | EndGroupsBarResize


drawGroups : TimelineBox -> Int -> Html Msg
drawGroups box size =
    let
        groups =
            box.groups
    in
    Html.div
        (if box.direction == Horizontal then
            [ HA.style "top" (String.fromInt (round box.sectionOffsetY) ++ "px")
            , HA.style "position" "relative"
            ]

         else
            [ HA.style "left" (String.fromInt (round box.sectionOffsetY) ++ "px")
            , HA.style "position" "relative"
            , HA.style "width" <| (List.map .size (Dict.values groups) |> List.sum |> toFloat |> (*) box.lineSize |> ceiling |> String.fromInt) ++ "px"
            ]
        )
        [ Html.node "style" [] [ Html.text ".handle:hover {background-color:rgba(0,0,250,0.05);}" ]
        , groups
            |> Dict.toList
            |> List.sortBy (Tuple.second >> .position)
            |> List.indexedMap
                (\idx ( id, grp ) ->
                    groupView box.dnd
                        box.direction
                        box.lineSize
                        size
                        size
                        (case box.interaction of
                            EditGroupLabel gid str ->
                                if grp.id == gid then
                                    Just str

                                else
                                    Nothing

                            _ ->
                                Nothing
                        )
                        idx
                        grp
                        box.canEditGroups
                        box.canSortGroups
                        (isGroupSelected id box.selection)
                )
            |> HKeyed.node "div"
                (if box.direction == Horizontal then
                    case box.interaction of
                        MouseOver _ ->
                            []

                        _ ->
                            [ HA.style "pointer-events" "none" ]

                 else
                    (case box.interaction of
                        MouseOver _ ->
                            []

                        _ ->
                            [ HA.style "pointer-events" "none" ]
                    )
                        ++ [ HA.style "display" "flex"
                           , HA.style "flex-wrap" "wrap"
                           , HA.style "pointer-events" "none"
                           ]
                )
        , ghostView box.dnd box.direction size (round box.lineSize) (Dict.values groups |> List.sortBy .position)
        ]


handleStyle : List (Html.Attribute msg)
handleStyle =
    [ HA.style "width" "15px"
    , HA.style "height" "80%"
    , HA.style "border-radius" "20px"
    , HA.style "color" "grey"

    -- , HA.style "background-color" "rgba(0,0,0,0.1)"
    -- , HA.style "border-right" "dotted 1px lightgrey"
    , HA.style "margin-right" "5px"
    , HA.style "margin-left" "5px"
    , HA.style "font-size" "1.5em"
    , HA.style "justify-content" "center"
    , HA.style "display" "flex"
    , HA.style "align-items" "center"
    , HA.style "color" "#AAA"
    , HA.class "handle"
    ]


groupView : DnDList.Model -> Direction -> Float -> Int -> Int -> Maybe String -> Int -> GroupBox -> Bool -> Bool -> Bool -> ( String, Html.Html Msg )
groupView dnd direction lineSize size fullSize mbedit index group canEditG canSortG selected =
    let
        itemId =
            "id-" ++ group.id

        fontSize =
            case direction of
                Horizontal ->
                    HA.style "font-size" ((String.fromFloat <| min 15 <| ((lineSize |> logBase 5) * 5)) ++ "px")

                Vertical ->
                    HA.style "font-size" ((String.fromFloat <| min 15 <| ((lineSize |> logBase 6) * 4)) ++ "px")

        ( ( w, h, disp ), ( nw, nh ) ) =
            if direction == Horizontal then
                ( ( toFloat fullSize, lineSize * toFloat group.size, "block" )
                , ( String.fromInt size ++ "px", "100%" )
                )

            else
                ( ( lineSize * toFloat group.size, toFloat fullSize, "inline-block" )
                , ( "100%", String.fromInt size ++ "px" )
                )
    in
    Tuple.pair itemId <|
        case (system direction).info dnd of
            Just { dragIndex, dropIndex } ->
                if dragIndex /= index then
                    Html.div
                        [ HA.style "height" (String.fromFloat h ++ "px")
                        , HA.style "width" (String.fromFloat w ++ "px")
                        , HA.class <|
                            if index == dropIndex then
                                "group move"

                            else if modBy 2 index == 0 then
                                if direction == Horizontal then
                                    "group even"

                                else
                                    "group veven"

                            else
                                "group odd"
                        , if selected then
                            HA.class "selected"

                          else
                            HA.class ""
                        , HA.id itemId
                        ]
                        [ Html.div
                            ([ HA.style "height" nh
                             , HA.style "width" nw
                             , HA.style "background-color" "rgba(100,100,255,0.1)"
                             , HA.style "display" "flex"
                             , HA.style "align-items" "center"
                             , fontSize
                             ]
                                ++ (system direction).dropEvents index itemId
                            )
                            [ Html.div handleStyle [ Html.text "⋮" ], Html.div [] (groupLabelsToHtml group.label) ]
                        ]

                else
                    Html.div
                        ([ HA.style "height" (String.fromFloat h ++ "px")
                         , HA.style "width" (String.fromFloat w ++ "px")
                         , HA.style "background-color" "#aaa"
                         ]
                            ++ (system direction).dropEvents index itemId
                        )
                        []

            Nothing ->
                Html.div
                    [ HA.style "height" (String.fromFloat h ++ "px")
                    , HA.style "width" (String.fromFloat w ++ "px")
                    , HA.class <|
                        if modBy 2 index == 0 then
                            if direction == Horizontal then
                                "group even"

                            else
                                "group veven"

                        else
                            "group odd"
                    , if selected then
                        HA.class "selected"

                      else
                        HA.class ""
                    , HA.id itemId
                    , HA.style "display" disp
                    ]
                    [ Html.div
                        (if canEditG then
                            [ Html.Events.onDoubleClick (DoubleClickGroup group.id) ]

                         else
                            [ HA.style "width" nw
                            , HA.style "height" nh
                            , HA.style "display" "flex"
                            , HA.style "align-items" "center"
                            , HA.style "white-space" "nowrap"
                            , HA.style "overflow" "hidden"
                            , fontSize
                            , clickEvent <| \{ shiftKey } -> SelectGroup group.id shiftKey
                            ]
                        )
                        [ if canSortG then
                            Html.div
                                (handleStyle
                                    ++ (system direction).dragEvents index itemId
                                )
                                [ Html.text "⋮" ]

                          else
                            Html.div [ HA.style "width" "10px" ] [ Html.text "" ]
                        , case mbedit of
                            Just str ->
                                Html.input
                                    [ HA.value str
                                    , HA.id <| group.id ++ "label"
                                    , Html.Events.onBlur CancelGroupLabelEdit
                                    , Html.Events.onInput <| UpdateGroupLabel group.id
                                    , keyEvent <|
                                        \code ->
                                            case code of
                                                13 ->
                                                    Decode.succeed <| ValidateGroupLabel group.id str

                                                27 ->
                                                    Decode.succeed <| CancelGroupLabelEdit

                                                _ ->
                                                    Decode.fail ""
                                    ]
                                    []

                            Nothing ->
                                Html.div [] (groupLabelsToHtml group.label)
                        ]
                    ]


ghostView : DnDList.Model -> Direction -> Int -> Int -> List GroupBox -> Html.Html Msg
ghostView dnd direction width lineSize items =
    let
        maybeDragItem : Maybe GroupBox
        maybeDragItem =
            (system direction).info dnd
                |> Maybe.andThen (\{ dragIndex } -> items |> List.drop dragIndex |> List.head)
    in
    case maybeDragItem of
        Just item ->
            Html.div
                ([ HA.style "height" (String.fromInt (lineSize * item.size) ++ "px")
                 , HA.style "max-width" (String.fromInt width ++ "px")
                 , HA.style "background-color" "rgba(100,100,200,.6)"
                 , HA.style "z-index" "100"
                 , HA.style "display" "flex"
                 , HA.style "align-items" "center"
                 ]
                    ++ (system direction).ghostStyles dnd
                )
                [ Html.div handleStyle [ Html.text "⋮" ], Html.div [] (groupLabelsToHtml item.label) ]

        Nothing ->
            Html.text ""


groupLabelsToHtml : List String -> List (Html.Html Msg)
groupLabelsToHtml labels =
    List.map (\label -> Html.div [] [ Html.text label ]) labels


type alias SectionView a =
    { section :
        { a
            | start : Posix
            , end : Posix
            , color : String
            , isLocked : Bool
            , labels : List String
            , hasComment : Bool
            , isGlobal : Bool
            , id : String
        }
    , line : Int
    , selected : Bool
    , left : Float
    , top : Float
    , width : Float
    , height : Float
    , isSubtotal : Bool
    }


sectionsView : TimelineBox -> List SectionBox -> Int -> Int -> Float -> Float -> Html Msg
sectionsView ({ direction } as box) sections width height fromTime endTime =
    let
        getter =
            directionGetter direction

        margin =
            6
                * duration.day
                / box.zoom
                |> round
                |> Moment.toDuration

        -- pixel =
        --     1 * duration.day / box.zoom
        grid =
            Axis.getGrid (duration.day / box.zoom)

        unit =
            grid.snap * 3600000

        snapToGrid =
            Moment.mapDuration <| \x -> toFloat (round (toFloat x / unit)) * unit |> round

        firstLine =
            floor (-box.sectionOffsetY / box.lineSize)

        lastLine =
            ceiling <|
                if box.direction == Horizontal then
                    (-box.sectionOffsetY + toFloat height) / box.lineSize

                else
                    (-box.sectionOffsetY + toFloat width) / box.lineSize

        selection =
            box.meshesSelection

        ( moveTime, moveGroup, resize ) =
            case box.interaction of
                Move SimpleMove _ _ ( deltaMoveT, deltaMoveG ) ->
                    ( snapToGrid deltaMoveT, deltaMoveG, Moment.toDuration 0 )

                ResizeLeft _ x ->
                    let
                        minDuration =
                            minDurationOf box.groups selection

                        minSnap =
                            min (round unit) minDuration
                    in
                    ( snapToGrid <| Moment.mapDuration (\d -> min d (minDuration - minSnap)) x
                    , 0
                    , Moment.mapDuration negate (snapToGrid <| Moment.mapDuration (\d -> min d (minDuration - minSnap)) x)
                    )

                ResizeRight _ x ->
                    let
                        minDuration =
                            minDurationOf box.groups selection

                        minSnap =
                            min (round unit) minDuration
                    in
                    ( Moment.toDuration 0
                    , 0
                    , snapToGrid <| Moment.mapDuration (\d -> max d (minSnap - minDuration)) x
                    )

                _ ->
                    ( Moment.toDuration 0, 0, Moment.toDuration 0 )

        ( globalSections, ( selectSections, unselectSections ) ) =
            sections
                |> List.map
                    (\{ section, groupId, line } ->
                        let
                            isSubtotal =
                                Dict.get groupId box.groups
                                    |> Maybe.map (\gbox -> gbox.isSubtotal)
                                    |> Maybe.withDefault False
                        in
                        if isSelected groupId section.id selection then
                            let
                                start =
                                    if Moment.durationNotZero resize then
                                        Moment.minPosix (Moment.addDurationToPosix section.start moveTime) (Moment.subtractDuration section.end margin)

                                    else
                                        Moment.addDurationToPosix section.start moveTime
                            in
                            { section =
                                { section
                                    | start = start
                                    , end =
                                        Moment.addDurationToPosix start <|
                                            (max (Moment.fromDuration resize + Time.posixToMillis section.end - Time.posixToMillis section.start) 0 |> Moment.toDuration)
                                }
                            , line = line + moveGroup
                            , selected = True
                            , isSubtotal = isSubtotal
                            }

                        else
                            { section = section
                            , line = line
                            , selected = False
                            , isSubtotal = isSubtotal
                            }
                    )
                |> List.foldr
                    (\({ section } as sbox) ( ( ns, line ), list ) ->
                        if sbox.selected then
                            let
                                sbox2 =
                                    { sbox
                                        | section =
                                            { section
                                                | end =
                                                    if Time.posixToMillis ns == 0 || line /= sbox.line then
                                                        section.end

                                                    else
                                                        Moment.minPosix ns section.end
                                            }
                                    }
                            in
                            ( ( sbox2.section.start, sbox2.line ), sbox2 :: list )

                        else
                            ( ( ns, line ), sbox :: list )
                    )
                    ( ( Time.millisToPosix 0, -1 ), [] )
                |> Tuple.second
                |> List.filterMap
                    (\({ section } as sbox) ->
                        if
                            Moment.intersect section.start section.end (Time.millisToPosix <| round fromTime) (Time.millisToPosix <| round endTime)
                                && ((sbox.line >= firstLine)
                                        && (sbox.line <= lastLine)
                                        || sbox.section.isGlobal
                                   )
                        then
                            let
                                pos =
                                    ( (Moment.durationBetween box.first section.start |> Moment.fromDuration |> toFloat) * box.zoom / duration.day
                                    , if sbox.section.isGlobal then
                                        0

                                      else
                                        toFloat sbox.line * box.lineSize + 2
                                    )

                                size =
                                    ( (Moment.durationBetween section.start section.end |> Moment.fromDuration |> toFloat) * box.zoom / duration.day
                                    , if sbox.section.isGlobal then
                                        toFloat box.lines * box.lineSize

                                      else
                                        box.lineSize - 4
                                    )
                            in
                            if getter.h size < 12 || getter.v size < 14 then
                                Nothing

                            else
                                Just
                                    { section = sbox.section
                                    , line = sbox.line
                                    , selected = sbox.selected
                                    , left = getter.h pos
                                    , top = getter.v pos
                                    , width = getter.h size
                                    , height = getter.v size
                                    , isSubtotal = sbox.isSubtotal
                                    }

                        else
                            Nothing
                    )
                -- |> List.partition (always False)
                |> List.partition (.section >> .isGlobal)
                |> Tuple.mapSecond (List.partition .selected)

        ( mbselection, mbdraw ) =
            case box.interaction of
                Select _ _ _ ( ( posTime, posLine ), ( sizeTime, sizeLine ) ) ->
                    ( Just <|
                        ( getter.xy
                            ( (Moment.durationBetween box.first posTime |> Moment.fromDuration |> toFloat) * box.zoom / duration.day
                            , posLine * box.lineSize + 2
                            )
                        , getter.xy
                            ( (Moment.fromDuration sizeTime |> toFloat) * box.zoom / duration.day
                            , sizeLine * box.lineSize - 4
                            )
                        )
                    , Nothing
                    )

                Draw dstart dend line ->
                    ( Nothing
                    , let
                        ( start, end ) =
                            if Moment.greaterThan dstart dend then
                                ( dend, dstart )

                            else
                                ( dstart, dend )

                        ( left, top ) =
                            getter.xy
                                ( (Moment.durationBetween box.first start |> Moment.fromDuration |> toFloat)
                                    * box.zoom
                                    / duration.day
                                , toFloat line * box.lineSize + 2
                                )

                        ( w, h ) =
                            getter.xy
                                ( (Moment.durationBetween start end |> Moment.fromDuration |> toFloat)
                                    * box.zoom
                                    / duration.day
                                , box.lineSize - 4
                                )
                      in
                      Just [ { start = start, end = end, left = left, top = top, w = w, h = h, section = Nothing } ]
                    )

                Move Clone _ _ ( deltaMoveT, deltaMoveG ) ->
                    let
                        ( moveCloneTime, moveCloneGroup ) =
                            ( snapToGrid deltaMoveT, deltaMoveG )

                        clones =
                            sections
                                |> List.filterMap
                                    (\{ section, groupId, line } ->
                                        let
                                            isSubtotal =
                                                Dict.get groupId box.groups
                                                    |> Maybe.map .isSubtotal
                                                    |> Maybe.withDefault False
                                        in
                                        if isSelected groupId section.id selection && not isSubtotal then
                                            Just
                                                { section =
                                                    { section
                                                        | start = Moment.addDurationToPosix section.start moveCloneTime
                                                        , end = Moment.addDurationToPosix section.end moveCloneTime
                                                    }
                                                , line = line + moveCloneGroup
                                                , selected = True
                                                }

                                        else
                                            Nothing
                                    )
                                |> List.filterMap
                                    (\({ section } as sbox) ->
                                        if
                                            Moment.intersect section.start section.end (Time.millisToPosix <| round fromTime) (Time.millisToPosix <| round endTime)
                                                && (sbox.line >= firstLine)
                                                && (sbox.line <= lastLine)
                                        then
                                            let
                                                pos =
                                                    ( (Moment.durationBetween box.first section.start |> Moment.fromDuration |> toFloat) * box.zoom / duration.day
                                                    , toFloat sbox.line * box.lineSize + 2
                                                    )

                                                size =
                                                    ( (Moment.durationBetween section.start section.end |> Moment.fromDuration |> toFloat) * box.zoom / duration.day
                                                    , box.lineSize - 4
                                                    )
                                            in
                                            -- if getter.h size < 30 || getter.v size < 14 then
                                            --     Nothing
                                            -- else
                                            Just
                                                { start = section.start
                                                , end = section.end
                                                , section = Just section

                                                -- , line = sbox.line
                                                , left = getter.h pos
                                                , top = getter.v pos
                                                , w = getter.h size
                                                , h = getter.v size
                                                }

                                        else
                                            Nothing
                                    )
                    in
                    ( Nothing, Just clones )

                _ ->
                    ( Nothing, Nothing )

        scrollX =
            getter.h ( box.start, box.sectionOffsetY )

        scrollY =
            getter.v ( box.start, box.sectionOffsetY )
    in
    Html.div []
        [ Html.div
            [ HA.style "width" (String.fromInt width ++ "px")
            , HA.style "height" (String.fromInt height ++ "px")
            , HA.style "left" "0"
            , HA.style "top" "0"
            , HA.style "position" "absolute"
            ]
            [ drawHtmlSections
                direction
                box.locale
                box.zone
                width
                height
                scrollX
                scrollY
                globalSections
                Nothing
                Nothing
                box.wrapText
            ]
        , Html.div
            [ HA.style "width" (String.fromInt width ++ "px")
            , HA.style "height" (String.fromInt height ++ "px")
            , HA.style "left" "0"
            , HA.style "top" "0"
            , HA.style "position" "absolute"
            ]
            [ drawAllGlSections direction
                width
                height
                box.first
                (vec2 (Moment.fromDuration moveTime |> toFloat) (toFloat moveGroup))
                (Moment.fromDuration resize |> toFloat)
                { left = fromTime
                , right = endTime
                , bottom = -box.sectionOffsetY / box.lineSize
                , top =
                    if box.direction == Horizontal then
                        (-box.sectionOffsetY + toFloat height) / box.lineSize

                    else
                        (-box.sectionOffsetY + toFloat width) / box.lineSize
                }
                box.meshes
            ]
        , Html.div
            [ HA.style "width" (String.fromInt width ++ "px")
            , HA.style "height" (String.fromInt height ++ "px")
            , HA.style "left" "0"
            , HA.style "top" "0"
            , HA.style "position" "absolute"
            ]
            [ drawHtmlSections
                direction
                box.locale
                box.zone
                width
                height
                scrollX
                scrollY
                unselectSections
                Nothing
                Nothing
                box.wrapText
            ]
        , Html.div
            [ HA.style "width" (String.fromInt width ++ "px")
            , HA.style "height" (String.fromInt height ++ "px")
            , HA.style "left" "0"
            , HA.style "top" "0"
            , HA.style "position" "absolute"
            ]
            [ drawAllGlSections direction
                width
                height
                box.first
                (vec2 (Moment.fromDuration moveTime |> toFloat) (toFloat moveGroup))
                (Moment.fromDuration resize |> toFloat)
                { left = fromTime
                , right = endTime
                , bottom = -box.sectionOffsetY / box.lineSize
                , top =
                    if box.direction == Horizontal then
                        (-box.sectionOffsetY + toFloat height) / box.lineSize

                    else
                        (-box.sectionOffsetY + toFloat width) / box.lineSize
                }
                box.selectedMeshes
            ]
        , Html.div
            [ HA.style "width" (String.fromInt width ++ "px")
            , HA.style "height" (String.fromInt height ++ "px")
            , HA.style "left" "0"
            , HA.style "top" "0"
            , HA.style "position" "absolute"
            ]
            [ drawHtmlSections
                direction
                box.locale
                box.zone
                width
                height
                scrollX
                scrollY
                selectSections
                mbselection
                mbdraw
                box.wrapText
            ]
        , Html.div
            [ HA.style "position" "absolute"
            , HA.style "left" "0"
            , HA.style "top" "0"
            , HA.style "width" (String.fromInt width ++ "px")
            , HA.style "height" (String.fromInt height ++ "px")
            , wheelEvent SectionsWheel
            , moveEvent SectionsMove
            , mouseDownEvent SectionsDown

            -- , Html.Events.onDoubleClick SectionsDoubleClick
            , overEvent SectionsOver
            , Html.Events.onMouseOut SectionsOut
            ]
            []
        ]


mouseCursor : TimelineBox -> String
mouseCursor box =
    let
        margin =
            6
                * duration.day
                / box.zoom
                |> round
                |> Moment.toDuration
    in
    case box.interaction of
        ResizeLeft _ _ ->
            if box.direction == Horizontal then
                "ew-resize"

            else
                "ns-resize"

        ResizeRight _ _ ->
            if box.direction == Horizontal then
                "ew-resize"

            else
                "ns-resize"

        Select _ _ _ ( _, ( sizeDuration, sizeLine ) ) ->
            if ( Moment.fromDuration sizeDuration, sizeLine ) > ( 0, 0 ) then
                "crosshair"

            else
                "default"

        MouseOver ( posix, y ) ->
            let
                mbsec =
                    findSection posix ( y, 1 - (4 / box.lineSize) ) box.sections
            in
            case mbsec of
                Just sec ->
                    -- if h > (sec.section.end - pixel) || h < (sec.section.start + pixel) then
                    --     getter.h ( "col-resize", "row-resize" )
                    -- else
                    if Moment.greaterThan posix (Moment.subtractDuration sec.section.end margin) then
                        (directionGetter box.direction).h ( "ew-resize", "ns-resize" )

                    else if Moment.lessThan posix (Moment.addDurationToPosix sec.section.start margin) then
                        (directionGetter box.direction).h ( "ew-resize", "ns-resize" )

                    else
                        "default"

                Nothing ->
                    "default"

        ResizeGroups _ ->
            if box.direction == Horizontal then
                "col-resize"

            else
                "row-resize"

        _ ->
            "default"


drawHtmlSections :
    Direction
    -> Locale
    -> Time.Zone
    -> Int
    -> Int
    -> Float
    -> Float
    -> List (SectionView a)
    -> Maybe ( ( Float, Float ), ( Float, Float ) )
    -> Maybe (List { b | section : Maybe Section, left : Float, top : Float, w : Float, h : Float, start : Posix, end : Posix })
    -> Bool
    -> Html Msg
drawHtmlSections direction locale zone _ _ scrollX scrollY allSections mbselection mbdraw wrapText =
    Html.div
        [ HA.style "position" "absolute"
        , HA.style "left" ((String.fromFloat <| scrollX) ++ "px")
        , HA.style "top" ((String.fromFloat <| scrollY) ++ "px")
        ]
        [ HKeyed.node "div"
            []
            (List.map
                (\({ section } as sbox) ->
                    ( sbox.section.id
                    , Html.Lazy.lazy8 sectionBox2html
                        ( locale, zone )
                        direction
                        ( sbox.left, sbox.top )
                        ( sbox.width, sbox.height )
                        sbox.selected
                        wrapText
                        section
                        (not sbox.isSubtotal)
                    )
                )
                allSections
            )
        , case ( mbselection, mbdraw ) of
            ( Just ( ( left, top ), ( w, h ) ), Nothing ) ->
                Html.div
                    [ HA.style "left" ((String.fromFloat <| left) ++ "px")
                    , HA.style "top" ((String.fromFloat <| top) ++ "px")
                    , HA.style "width" ((String.fromFloat <| max 0 w) ++ "px")
                    , HA.style "height" ((String.fromFloat <| max 0 h) ++ "px")
                    , HA.style "position" "absolute"
                    , HA.style "background-color" "rgba(70, 130, 180, 0.1)"
                    , HA.style "border" "1px solid steelblue"
                    ]
                    []

            ( Nothing, Just drawList ) ->
                drawList
                    |> List.map
                        (\draw ->
                            sectionBox2html ( locale, zone )
                                direction
                                ( draw.left, draw.top )
                                ( draw.w, draw.h )
                                False
                                wrapText
                                { start = draw.start
                                , end = draw.end
                                , color = "new"
                                , labels =
                                    case draw.section of
                                        Just s ->
                                            s.labels

                                        Nothing ->
                                            []
                                , isLocked = False
                                , comment = Nothing
                                , isGlobal = False
                                }
                                True
                        )
                    |> Html.div []

            _ ->
                Html.text ""
        ]


sectionBox2html :
    ( Locale, Time.Zone )
    -> Direction
    -> ( Float, Float )
    -> ( Float, Float )
    -> Bool
    -> Bool
    ->
        { a
            | start : Posix
            , end : Posix
            , color : String
            , labels : List String
            , isGlobal : Bool
        }
    -> Bool
    -> Html msg
sectionBox2html ( locale, zone ) direction ( positionh, positionv ) ( sizeh, sizev ) isSelected wrapText { color, labels, start, end, isGlobal } drawTime =
    let
        posx =
            positionh

        posy =
            positionv

        dx =
            0

        dy =
            0

        hideTime =
            (direction == Horizontal && (sizeh < 55))
                || (direction == Vertical && (sizev < 35))

        hideTimeBox =
            if drawTime then
                (direction == Horizontal && (sizev < 25))
                    || (direction == Vertical && (sizeh < 25))

            else
                True

        labelsSel =
            labels
                |> (if wrapText then
                        List.concatMap (String.split "\n")

                    else
                        identity
                   )
                |> List.take
                    ((if sizev < 25 then
                        sizev / 13

                      else
                        (sizev - 13) / 13
                     )
                        |> ceiling
                    )

        maxLabelsSel =
            List.length labels - 1

        color_ =
            findColor color

        cssColor =
            if isSelected then
                Color.mapLightness (always 0.8) color_ |> Color.mapAlpha (always 1.0) |> Color.toCssString

            else if Color.lightness color_ > 0.5 then
                "black"

            else
                "#FFF"
    in
    Html.div
        [ HA.class "section"
        , HA.class <|
            if isSelected then
                "selected"

            else
                ""
        , HA.style "color" cssColor
        , if color == "new" then
            HA.class color

          else
            HA.class ""
        , if isGlobal then
            (if isSelected then
                Color.toRgba color_
                    |> (\{ red, green, blue, alpha } -> { red = red * 0.4 * alpha, green = green * 0.4 * alpha, blue = blue * 0.4 * alpha, alpha = 1 })
                    |> Color.fromRgba
                    |> Color.toCssString

             else
                Color.toCssString color_
            )
                |> HA.style "background-color"

          else
            HA.class ""
        , HA.style "left" ((String.fromFloat <| posx) ++ "px")
        , HA.style "top" ((String.fromFloat <| posy) ++ "px")
        , HA.style "width" ((String.fromFloat <| max 5 (sizeh + dx)) ++ "px")
        , HA.style "height" ((String.fromFloat <| max 5 (sizev + dy)) ++ "px")
        ]
        ([]
            ++ List.filterMap identity
                [ if hideTimeBox then
                    Nothing

                  else
                    Just <|
                        Html.div
                            [ HA.style "font-size" "10px"
                            , HA.class "dates"
                            , HA.style "padding-top" "2px"
                            ]
                            [ Html.div
                                [ HA.style "padding-left" "2px"
                                ]
                                [ Html.text (Moment.format locale zone Moment.Hour Nothing start) ]
                            , if hideTime then
                                Html.text ""

                              else
                                Html.div
                                    (if direction == Horizontal then
                                        [ HA.class "h-end-date"
                                        ]

                                     else
                                        [ HA.class "v-end-date"
                                        , HA.style "top" ((String.fromInt <| round <| (sizev - 12)) ++ "px")
                                        , HA.style "right" "2px"
                                        ]
                                    )
                                    [ Html.text <| Moment.format locale zone Moment.Hour Nothing end ]
                            ]
                , Just <|
                    Html.div
                        [ HA.style "font-size"
                            (if sizev < 15 then
                                "10px"

                             else
                                "12px"
                            )
                        , HA.style "padding-left" "2px"
                        , HA.style "padding-top"
                            (if hideTimeBox then
                                "2px"

                             else
                                "0"
                            )
                        , HA.style "height"
                            ((String.fromFloat <|
                                max 5
                                    (sizev
                                        + dy
                                        - (if hideTimeBox then
                                            0

                                           else
                                            11
                                          )
                                    )
                             )
                                ++ "px"
                            )
                        , HA.style "overflow" "hidden"
                        ]
                        (List.indexedMap
                            (\i label ->
                                Html.text label
                                    |> List.singleton
                                    |> Html.div
                                        [ HA.style "overflow" "hidden"
                                        , HA.style "white-space"
                                            (if i >= maxLabelsSel && wrapText then
                                                "wrap"

                                             else
                                                "nowrap"
                                            )

                                        {- , HA.style "text-overflow" "ellipsis" -}
                                        ]
                            )
                            labelsSel
                        )
                ]
        )


drawAllGlSections :
    Direction
    -> Int
    -> Int
    -> Posix
    -> Vec2
    -> Float
    -> { left : Float, right : Float, top : Float, bottom : Float }
    -> Dict GroupId { position : Float, meshes : Mesh Vertex }
    -> Html Msg
drawAllGlSections dir width height firstDate move resize visible meshesDict =
    let
        firstms =
            Time.posixToMillis firstDate |> toFloat
    in
    WebGL.toHtmlWith
        [ WebGL.alpha True, WebGL.antialias ]
        [ HA.width width
        , HA.height height
        , HA.style "display" "block"
        ]
    <|
        List.map
            (\{ meshes, position } ->
                WebGL.entity
                    (if dir == Horizontal then
                        vertexShaderHoriz

                     else
                        vertexShaderVert
                    )
                    (if dir == Horizontal then
                        fragmentShaderHoriz

                     else
                        fragmentShaderVert
                    )
                    meshes
                    { perspective = Mat4.makeOrtho2D ((visible.left - firstms) / 300000) ((visible.right - firstms) / 300000) visible.top visible.bottom |> Mat4.translate3 0 position 0
                    , iResolution = vec3 (toFloat width) (toFloat height) 0
                    , move = Math.Vector2.setX (Math.Vector2.getX move / 300000) move
                    , resize = resize / 300000
                    }
            )
            (Dict.values meshesDict)


type alias Uniforms =
    { perspective : Mat4, iResolution : Vec3, move : Vec2, resize : Float }


vertexShaderHoriz : Shader Vertex Uniforms { vcolor : Vec4, bcolor : Vec4, borderSize : Float, location : Vec2, size : Vec2, hasPoint : Float }
vertexShaderHoriz =
    [glsl|
        
        attribute vec2 position;
        attribute vec4 color;
        attribute float selected;
        attribute float border;
        attribute vec2 middle;
        attribute float comment;


        uniform mat4 perspective;
        uniform vec2 move;
        uniform float resize;
        varying vec4 vcolor;
        varying vec4 bcolor;
        varying float borderSize;
        varying vec2 location;
        varying vec2 size;
        varying float hasPoint;



        void main () {

            vec4 coord;
            vec2 moved = move * selected;
            float resized = resize * selected;
            if(position.x < middle.x) {
                coord = perspective * vec4(position.xy+moved.xy, 0.0, 1.0);
            } else {
                coord = perspective * vec4(position.x+moved.x+resized, position.y+moved.y, 0.0, 1.0);
            }
            
            gl_Position = coord;

            location = (perspective * vec4(middle.x + moved.x + (resized * 0.5), middle.y+moved.y, 0.0, 1.0)).xy;
            size = abs(location.xy-coord.xy);

       
            vcolor = selected > 0.0 ? vec4(color.rgb * color.a * 0.4, 1)  : color;
            bcolor = vec4(color.rgb*0.8, 1);
            borderSize = border;
            hasPoint = comment;

           
        }

    |]


vertexShaderVert : Shader Vertex Uniforms { vcolor : Vec4, bcolor : Vec4, borderSize : Float, location : Vec2, size : Vec2, hasPoint : Float }
vertexShaderVert =
    [glsl|
        
        attribute vec2 position;
        attribute vec4 color;
        attribute float selected;
        attribute float border;
        attribute vec2 middle;
        attribute float comment;


        uniform mat4 perspective;
        uniform vec2 move;
        uniform float resize;
        varying vec4 vcolor;
        varying vec4 bcolor;
        varying float borderSize;
        varying vec2 location;
        varying vec2 size;
        varying float hasPoint;



        void main () {

            vec4 coord;
            vec2 moved = move * selected;
            float resized = resize * selected;
            if(position.x < middle.x) {
                coord = perspective * vec4(position.xy+moved.xy, 0.0, 1.0);
            } else {
                coord = perspective * vec4(position.x+moved.x+resized, position.y+moved.y, 0.0, 1.0);
            }
            
            gl_Position = coord;

            location = (perspective * vec4(middle.x + moved.x + (resized * 0.5), middle.y+moved.y, 0.0, 1.0)).xy;
            size = abs(location.xy-coord.xy);

       
            vcolor = selected > 0.0 ? vec4(color.rgb * color.a * 0.4, 1)  : color;
            bcolor = vec4(color.rgb*0.8, 1);
            borderSize = border;
            hasPoint = comment;

            // inversion de position
            location = vec2(-location.y, -location.x);
            size = vec2(size.y, size.x);
            gl_Position = vec4(-coord.y, -coord.x, coord.z, coord.a);
        }

    |]


fragmentShaderHoriz : Shader {} Uniforms { vcolor : Vec4, bcolor : Vec4, borderSize : Float, location : Vec2, size : Vec2, hasPoint : Float }
fragmentShaderHoriz =
    [glsl|
        precision lowp float;
        uniform vec3 iResolution;
        varying vec4 vcolor;
        varying vec4 bcolor;
        varying float borderSize;
        varying vec2 location;
        varying vec2 size;
        varying float hasPoint;
        



        // from https://iquilezles.org/articles/distfunctions
        // additional thanks to iq for optimizing my conditional block for individual corner radii!
        float roundedBoxSDF(vec2 CenterPosition, vec2 Size, vec4 Radius) {
            Radius.xy = (CenterPosition.x>0.0)?Radius.xy : Radius.zw;
            Radius.x  = (CenterPosition.y>0.0)?Radius.x  : Radius.y;
            
            vec2 q = abs(CenterPosition)-Size+Radius.x;
            return min(max(q.x,q.y),0.0) + length(max(q,0.0)) - Radius.x;
        }
        void main( ) {

            // How soft the edges should be (in pixels). Higher values could be used to simulate a drop shadow.
            float edgeSoftness  = 1.0;
            
            // The radius of the corners (in pixels) clockwise starting in the top left.
            vec4 radius =  (borderSize == 128.0) ? vec4(0.0)
                    : (hasPoint > 0.0 ? vec4(5.0, 4.0, 5.0, 5.0) : vec4(5.0));
            
            // Calculate distance to edge.   
            vec2 center = gl_FragCoord.xy - ((location+vec2(1.0)) * iResolution.xy/2.0);
            vec2 sizepix = (size * iResolution.xy / 2.0 - vec2(0.0, 3.0));
            float distance = roundedBoxSDF( center, sizepix, radius);
            
            // Smooth the result (free antialiasing).
            float smoothedAlpha =  1.0-smoothstep(0.0, edgeSoftness,distance);
            
            // Border.  
            float borderThickness = (borderSize == 128.0) ? 1.0 : borderSize;
            float borderSoftness  = 1.0;
            float borderAlpha     = 1.0-smoothstep(borderThickness - borderSoftness, borderThickness, abs(distance));

            // has point
            float pointSize = 2.5;
            float pointSoftness = 1.0;
            float pointDistance = (hasPoint > 0.0)  ?  length(sizepix+vec2(-center.x, center.y)-2.5) : pointSize;
            float pointAlpha = 1.0 - smoothstep(pointSize - pointSoftness, pointSize, abs(pointDistance));
            vec4 pointColor = (vcolor.a == 1.0) ? bcolor : vec4(0.0, 0.0, 0.0, 1.0);

            // Colors
            vec4 rectColor =  vcolor;
            vec4 borderColor = (borderSize == 128.0) ? vec4(0.4,0.4,0.4,1.0): bcolor;
            vec4 bgColor = vec4(1.0,1.0,1.0,0.0);
            
            vec4 rgba = mix(bgColor, mix(mix(rectColor, pointColor, pointAlpha), borderColor, borderAlpha), smoothedAlpha);
            float a = rgba.a;
            gl_FragColor = vec4(rgba.r * a, rgba.g * a, rgba.b * a, a);
        }
   


    |]


fragmentShaderVert : Shader {} Uniforms { vcolor : Vec4, bcolor : Vec4, borderSize : Float, location : Vec2, size : Vec2, hasPoint : Float }
fragmentShaderVert =
    [glsl|
        precision lowp float;
        uniform vec3 iResolution;
        varying vec4 vcolor;
        varying vec4 bcolor;
        varying float borderSize;
        varying vec2 location;
        varying vec2 size;
        varying float hasPoint;
        



        // from https://iquilezles.org/articles/distfunctions
        // additional thanks to iq for optimizing my conditional block for individual corner radii!
        float roundedBoxSDF(vec2 CenterPosition, vec2 Size, vec4 Radius) {
            Radius.xy = (CenterPosition.x>0.0)?Radius.xy : Radius.zw;
            Radius.x  = (CenterPosition.y>0.0)?Radius.x  : Radius.y;
            
            vec2 q = abs(CenterPosition)-Size+Radius.x;
            return min(max(q.x,q.y),0.0) + length(max(q,0.0)) - Radius.x;
        }
        void main( ) {

            // How soft the edges should be (in pixels). Higher values could be used to simulate a drop shadow.
            float edgeSoftness  = 1.0;
            
            // The radius of the corners (in pixels) clockwise starting in the top left.
            vec4 radius =  (borderSize == 128.0) ? vec4(0.0)
                    : (hasPoint > 0.0 ? vec4(5.0, 4.0, 5.0, 5.0) : vec4(5.0));
            
            // Calculate distance to edge.   
            vec2 center = gl_FragCoord.xy - ((location+vec2(1.0)) * iResolution.xy/2.0);
            vec2 sizepix = size * iResolution.xy / 2.0 - vec2(3.0, 0.0);
            float distance = roundedBoxSDF( center, sizepix, radius);
            
            // Smooth the result (free antialiasing).
            float smoothedAlpha =  1.0-smoothstep(0.0, edgeSoftness,distance);
            
            // Border.  
            float borderThickness = (borderSize == 128.0) ? 1.0 : borderSize;
            float borderSoftness  = 1.0;
            float borderAlpha     = 1.0-smoothstep(borderThickness - borderSoftness, borderThickness, abs(distance));

            // has point
            float pointSize = 2.5;
            float pointSoftness = 1.0;
            float pointDistance = (hasPoint > 0.0) ?  length(sizepix+vec2(-center.x, center.y)-2.5) : pointSize;
            float pointAlpha = 1.0 - smoothstep(pointSize - pointSoftness, pointSize, abs(pointDistance));
            vec4 pointColor = (vcolor.a == 1.0) ? bcolor : vec4(0.0, 0.0, 0.0, 1.0);

            // Colors
            vec4 rectColor =  vcolor;
            vec4 borderColor = (borderSize == 128.0) ? vec4(0.4,0.4,0.4,1.4): bcolor;
            vec4 bgColor = vec4(1.0,1.0,1.0,0.0);
            
            vec4 rgba = mix(bgColor, mix(mix(rectColor, pointColor, pointAlpha), borderColor, borderAlpha), smoothedAlpha);
            float a = rgba.a;
            gl_FragColor = vec4(rgba.r * a, rgba.g * a, rgba.b * a, a);
        }
   


    |]



----------
-- update
----------


update : Msg -> TimelineBox -> { width : Int, height : Int } -> ( TimelineBox, Action, Cmd Msg )
update msg bb rect =
    let
        box =
            { bb | zoomChange = max 0 (bb.zoomChange - 1) }

        axisSize =
            if box.displayAxis then
                if box.direction == Horizontal then
                    axisHeight

                else
                    axisWidth

            else
                0
    in
    case msg of
        NoOp ->
            noAction box

        SectionsWheel event ->
            sectionsWheel box
                (if box.direction == Horizontal then
                    { rect | height = rect.height - axisSize }

                 else
                    { rect | width = rect.width - axisSize }
                )
                event

        SectionsMove event ->
            sectionsMove box event

        SectionsDown event ->
            sectionsDown box event

        SectionsDoubleClick ->
            noAction (zoomAllTime rect.width box)

        SectionsUp event ->
            sectionsUp box event

        SectionsOver _ ->
            noAction { box | standby = False }

        SectionsOut ->
            noAction { box | standby = box.interaction /= mouseOverInteraction box -1 -1 }

        StopInteraction ->
            noAction
                { box
                    | standby = True
                    , interaction = mouseOverInteraction box -1 -1
                }

        GroupsWheel event ->
            groupsWheel box
                (if box.direction == Horizontal then
                    { top = axisSize
                    , width = rect.width
                    , height = rect.height - axisSize
                    }

                 else
                    { top = axisSize
                    , width = rect.height
                    , height = rect.width - axisSize
                    }
                )
                event

        DndMsg dndmsg ->
            let
                orig =
                    Dict.values box.groups |> List.sortBy .position

                ( dnd, groups ) =
                    (system box.direction).update dndmsg box.dnd orig
            in
            ( { box | dnd = dnd }
            , if groups == orig then
                NoAction

              else
                ReorderGroups <| List.map .id groups
            , (system box.direction).commands dnd
            )

        SelectGroup gid shiftKey ->
            let
                selection =
                    addToSelection gid
                        (Dict.get gid box.groups
                            |> Maybe.map .sections
                            |> Maybe.withDefault []
                            |> List.map (.section >> .id)
                        )
                        (if shiftKey then
                            box.selection

                         else
                            emptySelection
                        )
            in
            ( updateSelection selection box, SelectSections selection, Cmd.none )

        DoubleClickGroup gid ->
            case Dict.get gid box.groups of
                Just group ->
                    ( { box | interaction = EditGroupLabel gid (List.head group.label |> Maybe.withDefault "") }
                    , NoAction
                    , Task.attempt (\_ -> NoOp) (Browser.Dom.focus <| gid ++ "label")
                    )

                Nothing ->
                    noAction box

        UpdateGroupLabel gid string ->
            noAction { box | interaction = EditGroupLabel gid string }

        ValidateGroupLabel gid string ->
            ( { box | interaction = MouseOver ( Time.millisToPosix -1, -1 ) }, ModifyGroupLabel gid string, Cmd.none )

        CancelGroupLabelEdit ->
            noAction { box | interaction = MouseOver ( Time.millisToPosix -1, -1 ) }

        Keypress int ->
            case ( int, box.canEditSections ) of
                ( 8, True ) ->
                    -- backspace
                    let
                        sections =
                            box.selection |> selectionToSet |> Set.toList
                    in
                    ( updateSelection emptySelection { box | interaction = MouseOver ( Time.millisToPosix -1, -1 ) }, DeleteSections sections, Cmd.none )

                ( 68, True ) ->
                    -- "d"
                    ( box, DuplicateSections box.selection, Cmd.none )

                ( 78, _ ) ->
                    -- "n"
                    showDate box.currentPosix
                        (if box.direction == Horizontal then
                            rect.width

                         else
                            rect.height
                        )
                        box
                        |> updateSelection emptySelection
                        |> selectAction

                ( 83, True ) ->
                    -- "s"
                    case box.interaction of
                        MouseOver ( time, _ ) ->
                            ( box, Split box.selection (snapToGridForZoom box.zoom box.zone time), Cmd.none )

                        _ ->
                            noAction box

                ( 90, _ ) ->
                    -- "z"
                    let
                        selection =
                            if selectionIsEmpty box.selection then
                                box.sections |> List.map .section

                            else
                                selectedSections box
                    in
                    case ( List.head selection, Extra.last selection ) of
                        ( Just first, Just last ) ->
                            zoomOver first.start
                                last.end
                                (if box.direction == Horizontal then
                                    rect.width

                                 else
                                    rect.height
                                )
                                box
                                |> updateSelection emptySelection
                                |> selectAction

                        _ ->
                            noAction box

                _ ->
                    noAction box

        UpdateTime posix ->
            noAction { box | currentPosix = posix }

        StartGroupsBarResize int ->
            noAction { box | interaction = ResizeGroups int }

        GroupsBarResizing int ->
            case box.interaction of
                ResizeGroups start ->
                    noAction
                        { box
                            | groupsSize = max (box.groupsSize + int - start) 40
                            , interaction = ResizeGroups int
                        }

                _ ->
                    noAction box

        EndGroupsBarResize ->
            ( { box | interaction = MouseOver ( Time.millisToPosix -1, -1 ) }
            , ChangeGroupsSize box.groupsSize
            , Cmd.none
            )



-- noAction { box | groupsResizing = Nothing }
{-
   CSS functions
-}


sister : String -> List (String -> ( String, String )) -> String -> ( String, String )
sister selector list parent =
    let
        fullsel =
            parent ++ selector

        ( rules, rule ) =
            List.foldl
                (\func ( resRules, resRule ) ->
                    let
                        ( fc, fr ) =
                            func fullsel
                    in
                    ( resRules ++ fc, resRule ++ fr )
                )
                ( "", fullsel ++ "{" )
                list
    in
    ( rule ++ "}" ++ rules, "" )


root : String -> List (String -> ( String, String )) -> String
root selector list =
    sister selector list "" |> Tuple.first


child : String -> List (String -> ( String, String )) -> String -> ( String, String )
child selector list parent =
    sister selector list (parent ++ " ")


prop : String -> String -> String -> ( String, String )
prop property value _ =
    ( "", property ++ ":" ++ value ++ ";" )


rgba : Int -> Int -> Int -> Float -> String
rgba r g b a =
    "rgba(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ "," ++ String.fromFloat a ++ ")"


darken : Int -> Int -> Int -> Float -> String
darken r g b d =
    "rgb(" ++ String.fromInt (toFloat r * d |> round) ++ "," ++ String.fromInt (toFloat g * d |> round) ++ "," ++ String.fromInt (toFloat b * d |> round) ++ ")"


sisterColor : String -> String -> ( String, String )
sisterColor colname =
    let
        color =
            findColor colname
    in
    sister ("." ++ colname)
        [ prop "border-color" <| Color.toCssString color
        , sister ".selected"
            [ prop "color" (Color.mapLightness ((*) 1.1) color |> Color.mapAlpha (always 1.0) |> Color.toCssString)
            ]
        ]


styles : String
styles =
    root ".timeline"
        [ prop "user-select" "none"
        , prop "-webkit-user-select" "none"
        , prop "position" "relative"
        , prop "font-family" "sans-serif"
        , prop "overflow" "hidden"
        , child ".axis-text"
            [ prop "fill" "black"
            ]
        , child ".axis-line-5"
            [ prop "stroke" "#DDD"
            ]
        , child ".axis-line-10"
            [ prop "stroke" "#AAA"
            ]
        , child ".axis-line-20"
            [ prop "stroke" "#666"
            ]
        , child ".group div" [ prop "white-space" "wrap" ]
        , child ".group.move" [ prop "background-color" "#77f" ]
        , child ".group.even"
            [ prop "background-color" "#f7f7f7"
            , prop "box-sizing" "border-box"
            , prop "border-top" "solid 1px"
            , prop "border-bottom" "solid 1px"
            , prop "border-color" "#eaeaea"
            ]

        -- , child ".group.even.selected"
        --     [ prop "background-color" "#efeff7"
        --     ]
        , child ".group.veven"
            [ prop "background-color" "#f7f7f7"
            , prop "box-sizing" "border-box"
            , prop "border-left" "solid 1px"
            , prop "border-right" "solid 1px"
            , prop "border-color" "#eaeaea"
            ]
        , child ".group.odd" [ prop "background-color" "white" ]

        -- , child ".group.odd.selected"
        --     [ prop "background-color" "#f5f5ff"
        --     ]
        , child ".group.selected" [ prop "font-weight" "800" ]
        , sister ":focus-visible" [ prop "outline" "none" ]
        , sister ":focus" [ child ".group >div" [ prop "background-color" "rgba(70, 130, 180, 0.2)" ] ]
        , child "g.section"
            [ child "text"
                [ prop "fill" "black"
                , prop "stroke-width" "0"
                ]
            , child "text.dates"
                [ prop "fill" "rgba(0,0,0,0.5)"
                , prop "stroke-width" "0"
                ]
            , child "rect"
                [ prop "fill" <| rgba 150 150 150 alpha
                , prop "stroke" <| rgba 150 150 150 1
                ]
            , sister ".selected"
                [ child "rect" [ prop "fill" <| darken 150 150 150 0.3 ]
                , child "text" [ prop "fill" <| rgba 150 150 150 1 ]
                ]
            ]
        , child "div.section"
            [ prop "color" "black"
            , prop "position" "absolute"
            , prop "overflow" "hidden"
            , prop "font-family" "Arial, Helvetica, sans-serif"
            , prop "box-sizing" "border-box"
            , child ".dates"
                [ prop "color" "rgba(0,0,0,0.5)"
                , child "div.h-end-date"
                    [ prop "position" "absolute"
                    , prop "width" "100%"
                    , prop "padding-right" "3px"
                    , prop "box-sizing" "border-box"
                    , prop "top" "2px"
                    , prop "text-align" "end"
                    ]
                , child "div.v-end-date"
                    [ prop "position" "absolute"
                    , prop "left" "2px"
                    , prop "text-align" "end"
                    ]
                ]
            , sister ".selected"
                [ prop "color" <| rgba 150 150 150 1
                , child ".dates" [ prop "color" "rgba(255,255,255,0.4)" ]
                ]
            , sister ".new"
                [ prop "background-color" <| rgba 200 200 255 0.9
                , prop "border-color" "grey"
                , prop "box-shadow" "0px 0px 5px grey"
                , prop "border-radius" "5px"
                , child ".dates" [ prop "color" "black" ]
                ]
            , sisterColor "rose"
            , sisterColor "rose-pale"
            , sisterColor "magenta"
            , sisterColor "magenta-pale"
            , sisterColor "violet"
            , sisterColor "violet-pale"
            , sisterColor "bleu"
            , sisterColor "bleu-pale"
            , sisterColor "azur"
            , sisterColor "azur-pale"
            , sisterColor "cyan"
            , sisterColor "cyan-pale"
            , sisterColor "turquoise"
            , sisterColor "turquoise-pale"
            , sisterColor "vert"
            , sisterColor "vert-pale"
            , sisterColor "anis"
            , sisterColor "anis-pale"
            , sisterColor "jaune"
            , sisterColor "jaune-pale"
            , sisterColor "orange"
            , sisterColor "orange-pale"
            , sisterColor "rouge"
            , sisterColor "rouge-pale"
            , sisterColor "error"
            , sisterColor "problem"
            , sisterColor "warning"
            , sisterColor "ok"
            ]
        , child ".resize-handle-horiz"
            [ prop "position" "absolute"
            , prop "top" "0"
            , prop "width" "3px"
            , prop "height" "100%"
            , prop "cursor" "col-resize"
            , prop "background" "transparent"
            , prop "z-index" "100"
            ]
        , child ".resize-handle-vert"
            [ prop "position" "relative"
            , prop "top" "-1px"
            , prop "height" "3px"
            , prop "width" "100%"
            , prop "cursor" "row-resize"
            , prop "background" "transparent"
            , prop "z-index" "100"
            ]
        ]
