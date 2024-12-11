module Timeline exposing (Msg(..), applyAction, calcLayersSize, canEditGroups, canSortGroups, changeDirection, changeLineSize, changeStartAndZoom, changeYOffset, default, init, periodIsEqual, reinit, sectionsView, setLanguage, styles, subscriptions, update, vertical, view, zoomAllTime)

import Browser.Dom
import Browser.Events
import Cldr.Locale exposing (Locale)
import Color
import Dict exposing (Dict)
import DnDList
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Html.Keyed as HKeyed
import Html.Lazy
import Json.Decode as Decode
import List.Extra as Extra
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4)
import Moment
import Set
import Svg
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
--layout : List (Duration a) -> List (List (Duration a))
--layout group =
--    toLayer group
--        |> List.sortBy second
--        |> Extra.groupWhile (\( _, lay1 ) ( _, lay2 ) -> lay1 == lay2)
--        |> List.map (List.map first)


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


default : TimelineBox
default =
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
    , first = Time.millisToPosix 0
    , direction = dir
    , selection = emptySelection
    , interaction = MouseOver ( Time.millisToPosix -1, -1 )
    , standby = False
    , dnd = (system dir).model
    , locale = Cldr.Locale.en
    , zone = TimeZone.europe__paris ()
    , canSortGroups = True
    , canEditGroups = True
    , currentPosix = Time.millisToPosix 0
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


init : List Group -> TimelineBox
init groups =
    toTimelineBox groups default


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


periodIsEqual : TimelineBox -> TimelineBox -> Bool
periodIsEqual a b =
    let
        mbafirst =
            List.head a.sections
                |> Maybe.map (.section >> .start >> Time.posixToMillis)

        mbalast =
            Extra.last a.sections
                |> Maybe.map (.section >> .end >> Time.posixToMillis)

        mbbfirst =
            List.head b.sections
                |> Maybe.map (.section >> .start >> Time.posixToMillis)

        mbblast =
            Extra.last b.sections
                |> Maybe.map (.section >> .end >> Time.posixToMillis)
    in
    case ( ( mbafirst, mbalast ), ( mbbfirst, mbblast ) ) of
        ( ( Just afirst, Just alast ), ( Just bfirst, Just blast ) ) ->
            afirst == bfirst && alast == blast

        _ ->
            False


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
                | start = toFloat (width - groupsWidth) * 0.02
                , zoom = toFloat (width - groupsWidth) / dur * 0.96
            }

        _ ->
            tl


zoomOver : Posix -> Posix -> Int -> TimelineBox -> TimelineBox
zoomOver first last width tl =
    let
        firstms =
            Time.posixToMillis first

        lastms =
            Time.posixToMillis last

        dur =
            (lastms - firstms |> Basics.toFloat) / duration.day

        zoom =
            toFloat (width - groupsWidth) / dur * 0.96
    in
    { tl
        | start = ((Time.posixToMillis tl.first - firstms) |> toFloat) * (zoom / duration.day) + (toFloat (width - groupsWidth) * 0.02)
        , zoom = zoom
        , interaction = MouseOver ( Time.millisToPosix -1, -1 )
    }


showDate : Posix -> Int -> TimelineBox -> TimelineBox
showDate first width tl =
    let
        firstms =
            Time.posixToMillis first
    in
    { tl
        | start = ((Time.posixToMillis tl.first - firstms) |> toFloat) * (tl.zoom / duration.day) + (toFloat (width - groupsWidth) * 0.02)
        , interaction = MouseOver ( Time.millisToPosix -1, -1 )
    }


changeStartAndZoom : Float -> Float -> TimelineBox -> TimelineBox
changeStartAndZoom start zoom tl =
    { tl | start = start, zoom = zoom }


changeYOffset : Float -> TimelineBox -> TimelineBox
changeYOffset y tl =
    { tl | sectionOffsetY = y }


changeLineSize : Float -> TimelineBox -> TimelineBox
changeLineSize s tl =
    { tl | lineSize = s }


toTimelineBox : List Group -> TimelineBox -> TimelineBox
toTimelineBox groups base =
    let
        foldfunc group res =
            let
                layers =
                    toLayers group.sections

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
                       , sections =
                            List.map
                                (\( s, level ) ->
                                    { groupId = group.id
                                    , section = s
                                    , line = level
                                    }
                                )
                                layers
                       }
                     , layers
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


changeDirection : Direction -> TimelineBox -> TimelineBox
changeDirection dir tl =
    { tl
        | direction = dir
        , dnd = (system dir).model
    }


vertical : Bool -> TimelineBox -> TimelineBox
vertical bool tl =
    let
        dir =
            if bool then
                Vertical

            else
                Horizontal
    in
    changeDirection dir tl


setLanguage : String -> TimelineBox -> TimelineBox
setLanguage str tl =
    { tl | locale = Cldr.Locale.fromString Cldr.Locale.basicLocales str |> Maybe.withDefault Cldr.Locale.en }


axisHeight : number
axisHeight =
    50


axisWidth : number
axisWidth =
    180


groupsWidth : number
groupsWidth =
    250


view : TimelineBox -> { width : Int, height : Int } -> Html Msg
view box rect =
    let
        lateral =
            if box.direction == Horizontal then
                groupsWidth

            else
                0

        top =
            if box.direction == Horizontal then
                0

            else
                50

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

        -- mbcursor = (Maybe.map (\time -> (Moment.durationBetween box.first time |> Moment.fromDuration |> toFloat) * box.zoom / duration.day)
        --             (box.cursorTime))
        mbcursor =
            case box.direction of
                Horizontal ->
                    case box.interaction of
                        MouseOver ( time, _ ) ->
                            ((time |> snapToGridForZoom box.zoom box.zone |> Time.posixToMillis |> toFloat) - from)
                                * toFloat width
                                / (end - from)
                                |> round
                                |> Just

                        _ ->
                            Nothing

                Vertical ->
                    case box.interaction of
                        MouseOver ( time, _ ) ->
                            ((time |> snapToGridForZoom box.zoom box.zone |> Time.posixToMillis |> toFloat) - from)
                                * toFloat height
                                / (end - from)
                                |> round
                                |> Just

                        _ ->
                            Nothing

        currentTime =
            ((box.currentPosix |> Time.posixToMillis |> toFloat) - from)
                * toFloat width
                / (end - from)
                |> round
    in
    Html.div
        [ HA.style "width" (String.fromInt rect.width ++ "px")
        , HA.style "height" (String.fromInt rect.height ++ "px")
        , HA.style "font-family" "sans-serif"
        , HA.class "timeline"
        , HA.tabindex 0
        , Html.Events.on "keyup" (Decode.map Keypress Html.Events.keyCode)
        , mouseUpEvent SectionsUp
        ]
        [ Html.div
            (if box.direction == Horizontal then
                [ HA.style "position" "absolute"
                , HA.style "height" (String.fromInt (rect.height - axisHeight) ++ "px")
                , HA.style "width" (String.fromInt lateral {- rect.width -} ++ "px")
                , HA.style "top" (String.fromInt axisHeight ++ "px")
                , HA.style "overflow" "hidden"
                ]

             else
                [ HA.style "position" "absolute"
                , HA.style "height" (String.fromInt {- rect.height -} top ++ "px")
                , HA.style "width" (String.fromInt (rect.width - axisWidth) ++ "px")
                , HA.style "left" (String.fromInt axisWidth ++ "px")
                , HA.style "overflow" "hidden"
                ]
            )
            [ if box.direction == Horizontal then
                Html.Lazy.lazy4 drawGroups
                    box
                    lateral
                    lateral
                    rect.height

              else
                Html.Lazy.lazy4 drawGroups
                    box
                    top
                    rect.width
                    top
            ]
        , if box.direction == Horizontal then
            Html.div
                [ HA.style "position" "absolute"
                , HA.style "overflow" "hidden"
                , HA.style "left" (String.fromInt lateral ++ "px")
                , HA.style "top" (String.fromInt (top + axisHeight) ++ "px")
                , HA.style "width" <| String.fromInt width ++ "px"
                , HA.style "height" <| String.fromInt (height - axisHeight) ++ "px"
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
                    [ Html.Lazy.lazy2 drawRowsBackground box.groups box.lineSize ]
                ]

          else
            Html.div
                [ HA.style "position" "absolute"
                , HA.style "overflow" "hidden"
                , HA.style "left" (String.fromInt (lateral + axisWidth) ++ "px")
                , HA.style "top" (String.fromInt top ++ "px")
                , HA.style "width" <| String.fromInt (width - axisWidth) ++ "px"
                , HA.style "height" <| String.fromInt height ++ "px"
                ]
                [ Html.div
                    [ HA.style "position" "absolute"
                    , HA.style "overflow" "hidden"
                    , HA.style "top" "0px"
                    , HA.style "height" <|
                        String.fromInt height
                            ++ "px"
                    , HA.style "left" <|
                        (String.fromFloat
                            box.sectionOffsetY
                            ++ "px"
                        )
                    ]
                    [ Html.Lazy.lazy2 drawColsBackground box.groups box.lineSize ]
                ]
        , case ( box.interaction, mbcursor, box.standby ) of
            ( MouseOver _, Just pos, False ) ->
                case box.direction of
                    Horizontal ->
                        Html.div
                            [ HA.style "position" "absolute"
                            , HA.style "left" (String.fromInt (pos + lateral) ++ "px")
                            , HA.style "top" (String.fromInt (top + 5) ++ "px")
                            , HA.style "width" "0"
                            , HA.style "height" (String.fromInt height ++ "px")
                            , HA.style "border-left" "1px solid rgba(100,0,255,0.5)"
                            , HA.style "z-index" "1"
                            , HA.style "pointer-events" "none"
                            ]
                            []

                    Vertical ->
                        Html.div
                            [ HA.style "position" "absolute"
                            , HA.style "left" (String.fromInt (lateral + 5) ++ "px")
                            , HA.style "top" (String.fromInt (top + pos) ++ "px")
                            , HA.style "width" (String.fromInt width ++ "px")
                            , HA.style "height" "0"
                            , HA.style "border-top" "1px solid rgba(100,0,255,0.5)"
                            , HA.style "z-index" "1"
                            , HA.style "pointer-events" "none"
                            ]
                            []

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
                Html.Lazy.lazy7 Axis.hview [ wheelEvent SectionsWheel, moveY0Event SectionsMove ] box.locale box.zone width height from end

              else
                Html.Lazy.lazy7 Axis.vview [ wheelEvent SectionsWheel, moveY0Event SectionsMove ] box.locale box.zone width height from end
            ]
        , if box.direction == Horizontal then
            Html.div
                [ HA.style "position" "absolute"
                , HA.style "overflow" "hidden"
                , HA.style "left" (String.fromInt lateral ++ "px")
                , HA.style "top" (String.fromInt (top + axisHeight) ++ "px")
                , HA.style "width" <| String.fromInt width ++ "px"
                , HA.style "height" <| String.fromInt (height - axisHeight) ++ "px"
                ]
                [ sectionsView box box.sections width (height - axisHeight) from end ]

          else
            Html.div
                [ HA.style "position" "absolute"
                , HA.style "overflow" "hidden"
                , HA.style "left" (String.fromInt (lateral + axisWidth) ++ "px")
                , HA.style "top" (String.fromInt top ++ "px")
                , HA.style "width" <| String.fromInt width ++ "px"
                , HA.style "height" <| String.fromInt height ++ "px"
                ]
                [ sectionsView box box.sections (width - axisWidth) height from end ]
        , if Moment.between box.currentPosix (Time.millisToPosix (round from)) (Time.millisToPosix (round end)) then
            Html.div
                [ HA.style "position" "absolute"
                , HA.style "left" (String.fromInt (currentTime + lateral) ++ "px")
                , HA.style "top" (String.fromInt (top + 5) ++ "px")
                , HA.style "width" "0"
                , HA.style "height" (String.fromInt height ++ "px")
                , HA.style "border-left" "2px solid rgba(255,100,0,0.7)"

                -- , HA.style "z-index" "1"
                , HA.style "pointer-events" "none"
                ]
                []

          else
            Html.text ""
        ]


drawRowsBackground : Dict GroupId GroupBox -> Float -> Html Msg
drawRowsBackground groups lineSize =
    Html.div [] <|
        List.indexedMap
            (\j g ->
                Html.div
                    [ if modBy 2 j == 0 then
                        HA.class "group even"

                      else
                        HA.class "group odd"
                    , HA.style "width" "100%"
                    , HA.style "height" <| String.fromFloat (lineSize * toFloat g.size) ++ "px"
                    ]
                    []
            )
            (groups |> Dict.values |> List.sortBy .position)


drawColsBackground : Dict GroupId GroupBox -> Float -> Html Msg
drawColsBackground groups lineSize =
    Html.div [ HA.style "height" "100%" ] <|
        List.indexedMap
            (\j g ->
                Html.div
                    [ if modBy 2 j == 0 then
                        HA.class "group even"

                      else
                        HA.class "group odd"
                    , HA.style "width" <| String.fromFloat (lineSize * toFloat g.size) ++ "px"
                    , HA.style "height" "100%"
                    , HA.style "display" "inline-block"
                    ]
                    []
            )
            (groups |> Dict.values |> List.sortBy .position)


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


drawGroups : TimelineBox -> Int -> Int -> Int -> Html Msg
drawGroups box size fullWidth height =
    let
        groups =
            box.groups
    in
    Html.div
        (if box.direction == Horizontal then
            [ HA.style "top" (String.fromInt (round box.sectionOffsetY) ++ "px")
            , HA.style "position" "relative"
            , HA.style "height" (String.fromInt height ++ "px")
            , wheelEvent GroupsWheel
            , moveX0Event SectionsMove
            ]

         else
            [ HA.style "left" (String.fromInt (round box.sectionOffsetY) ++ "px")
            , HA.style "position" "relative"
            , HA.style "width" (String.fromInt fullWidth ++ "px")

            -- , HA.style "height" (String.fromInt height ++ "px")
            , wheelEvent GroupsWheel
            , moveY0Event SectionsMove
            ]
        )
        [ Html.node "style" [] [ Html.text ".handle:hover {background-color:rgba(0,0,250,0.05);}" ]
        , groups
            |> Dict.values
            |> List.sortBy .position
            |> List.indexedMap
                (\idx grp ->
                    groupView box.dnd
                        box.direction
                        box.lineSize
                        size
                        -- (if box.direction == Horizontal then
                        --     fullWidth
                        --  else
                        --     height
                        -- )
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
                           , HA.style "width" <| (List.map .size (Dict.values groups) |> List.sum |> (*) (round box.lineSize) |> String.fromInt) ++ "px"
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


groupView : DnDList.Model -> Direction -> Float -> Int -> Int -> Maybe String -> Int -> GroupBox -> Bool -> Bool -> ( String, Html.Html Msg )
groupView dnd direction lineSize size fullSize mbedit index group canEditG canSortG =
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
                ( ( toFloat fullSize, lineSize * toFloat group.size, "block" ), ( String.fromInt size ++ "px", "100%" ) )

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
                                "group even"

                            else
                                "group odd"

                        -- , HA.style "background-color"
                        --     (if index == dropIndex then
                        --         "#77f"
                        --      else if modBy 2 index == 0 then
                        --         "#f7f7f7"
                        --      else
                        --         "white"
                        --     )
                        , HA.id itemId

                        -- , HA.style "display" disp
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

                         --  , HA.id itemId
                         --  , HA.style "display" disp
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
                            "group even"

                        else
                            "group odd"

                    -- , HA.style "background-color"
                    --     (if modBy 2 index == 0 then
                    --         "#f7f7f7"
                    --      else
                    --         "white"
                    --     )
                    , HA.id itemId
                    , HA.style "display" disp
                    ]
                    [ Html.div
                        (if canEditG then
                            [ Html.Events.onDoubleClick (DoubleClickGroup group.id) ]

                         else
                            [ HA.style "width" nw
                            , HA.style "height" nh

                            -- , HA.style "background-color" "rgba(100,100,255,0.1)"
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
            , isFrozen : Bool
            , labels : List String
            , hasComment : Bool
            , id : String
        }
    , line : Int
    , selected : Bool
    , left : Float
    , top : Float
    , width : Float
    , height : Float
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

        ( selectSections, unselectSections ) =
            sections
                |> List.map
                    (\{ section, groupId, line } ->
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
                            }

                        else
                            { section = section
                            , line = line
                            , selected = False
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
                                    }

                        else
                            Nothing
                    )
                |> (\filtered ->
                        List.partition .selected filtered
                   )

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
                                        if isSelected groupId section.id selection then
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

        cursor =
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
                                getter.h ( "ew-resize", "ns-resize" )

                            else if Moment.lessThan posix (Moment.addDurationToPosix sec.section.start margin) then
                                getter.h ( "ew-resize", "ns-resize" )

                            else
                                "default"

                        Nothing ->
                            "default"

                _ ->
                    "default"

        scrollX =
            getter.h ( box.start, box.sectionOffsetY )

        scrollY =
            getter.v ( box.start, box.sectionOffsetY )
    in
    Html.div []
        [ Html.div
            [ HA.style "width" (String.fromInt width ++ "px")
            , HA.style "height" (String.fromInt height ++ "px")
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
            ]
        , Html.div
            [ HA.style "position" "absolute"
            , HA.style "left" "0"
            , HA.style "top" "0"
            , HA.style "width" (String.fromInt width ++ "px")
            , HA.style "height" (String.fromInt height ++ "px")
            , HA.style "cursor" cursor
            , wheelEvent SectionsWheel
            , moveEvent SectionsMove
            , mouseDownEvent SectionsDown

            -- , Html.Events.onDoubleClick SectionsDoubleClick
            , overEvent SectionsOver
            , Html.Events.onMouseOut SectionsOut
            ]
            []
        ]


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
    -> Html Msg
drawHtmlSections direction locale zone _ _ scrollX scrollY allSections mbselection mbdraw =
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
                        sbox.left
                        sbox.top
                        sbox.width
                        sbox.height
                        sbox.selected
                        section
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
                    , HA.style "background-color" "rgba(0,0,255,0.4)"
                    , HA.style "border" "1px solid blue"
                    ]
                    []

            ( Nothing, Just drawList ) ->
                drawList
                    |> List.map
                        (\draw ->
                            sectionBox2html ( locale, zone )
                                direction
                                draw.left
                                draw.top
                                draw.w
                                draw.h
                                False
                                { start = draw.start
                                , end = draw.end
                                , color = "new"
                                , labels =
                                    case draw.section of
                                        Just s ->
                                            s.labels

                                        Nothing ->
                                            []
                                , isFrozen = False
                                , comment = Nothing
                                }
                        )
                    |> Html.div []

            _ ->
                Html.text ""
        ]


sectionBox2html :
    ( Locale, Time.Zone )
    -> Direction
    -> Float
    -> Float
    -> Float
    -> Float
    -> Bool
    ->
        { a
            | start : Posix
            , end : Posix
            , color : String
            , labels : List String
        }
    -> Html msg
sectionBox2html ( locale, zone ) direction positionh positionv sizeh sizev isSelected { color, labels, start, end } =
    let
        posx =
            positionh

        dx =
            0

        -- posx - (round posx |> toFloat)
        dy =
            0

        -- posy - (round posy |> toFloat)
        posy =
            positionv

        hideTime =
            (direction == Horizontal && (sizeh < 55))
                || (direction == Vertical && (sizev < 35))

        hideTimeBox =
            (direction == Horizontal && (sizev < 25))
                || (direction == Vertical && (sizeh < 25))

        labelsSel =
            (List.take
                ((if sizev < 25 then
                    sizev / 13

                  else
                    (sizev - 13) / 13
                 )
                    |> ceiling
                )
                labels
             -- [ String.fromFloat posx ]
            )
    in
    Html.div
        [ HA.class "section"
        , HA.class <|
            if isSelected then
                "selected"

            else
                ""
        , HA.class <| Timeline.Models.findColorName color
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
                            [ HA.style "fontSize" "10px"
                            , HA.class "dates"
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
                                        ]
                                    )
                                    [ Html.text <| Moment.format locale zone Moment.Hour Nothing end ]
                            ]
                , Just <|
                    Html.div
                        [ HA.style "fontSize"
                            (if sizev < 15 then
                                "11px"

                             else
                                "13px"
                            )
                        , HA.style "padding-left" "2px"
                        , HA.style "height"
                            ((String.fromFloat <|
                                max 5
                                    (sizev
                                        + dy
                                        - (if hideTimeBox then
                                            0

                                           else
                                            13
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
                                            -- (if i == maxLabelsSel then
                                            -- "wrap"
                                            --  else
                                            "nowrap"

                                        -- )
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
            vec4 radius =  hasPoint > 0.0 ? vec4(5.0, 4.0, 5.0, 5.0) : vec4(5.0);
            
            // Calculate distance to edge.   
            vec2 center = gl_FragCoord.xy - ((location+vec2(1.0)) * iResolution.xy/2.0);
            vec2 sizepix = size * iResolution.xy / 2.0 - vec2(0.0, 3.0);
            float distance = roundedBoxSDF( center, sizepix, radius);
            
            // Smooth the result (free antialiasing).
            float smoothedAlpha =  1.0-smoothstep(0.0, edgeSoftness,distance);
            
            // Border.  
            float borderThickness = borderSize;
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
            vec4 borderColor = bcolor;
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
            vec4 radius =  hasPoint > 0.0 ? vec4(5.0, 4.0, 5.0, 5.0) : vec4(5.0);
            
            // Calculate distance to edge.   
            vec2 center = gl_FragCoord.xy - ((location+vec2(1.0)) * iResolution.xy/2.0);
            vec2 sizepix = size * iResolution.xy / 2.0 - vec2(3.0, 0.0);
            float distance = roundedBoxSDF( center, sizepix, radius);
            
            // Smooth the result (free antialiasing).
            float smoothedAlpha =  1.0-smoothstep(0.0, edgeSoftness,distance);
            
            // Border.  
            float borderThickness = borderSize;
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
            vec4 borderColor = bcolor;
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
    in
    case msg of
        NoOp ->
            noAction box

        SectionsWheel event ->
            sectionsWheel box
                (if box.direction == Horizontal then
                    { rect | height = rect.height - axisHeight }

                 else
                    { rect | width = rect.width - axisWidth }
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
                { top = axisHeight
                , width = rect.width
                , height = rect.height - axisHeight
                }
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
            case int of
                8 ->
                    -- backspace
                    let
                        sections =
                            box.selection |> selectionToSet |> Set.toList
                    in
                    ( updateSelection emptySelection { box | interaction = MouseOver ( Time.millisToPosix -1, -1 ) }, DeleteSections sections, Cmd.none )

                68 ->
                    -- "d"
                    ( box, DuplicateSections box.selection, Cmd.none )

                78 ->
                    -- "n"
                    showDate box.currentPosix rect.width box
                        |> updateSelection emptySelection
                        |> selectAction

                83 ->
                    -- "s"
                    case box.interaction of
                        MouseOver ( time, _ ) ->
                            ( box, Split box.selection (snapToGridForZoom box.zoom box.zone time), Cmd.none )

                        _ ->
                            noAction box

                90 ->
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
                            zoomOver first.start last.end rect.width box
                                |> updateSelection emptySelection
                                |> selectAction

                        _ ->
                            noAction box

                _ ->
                    noAction box

        UpdateTime posix ->
            noAction { box | currentPosix = posix }



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
        , child ".group.move" [ prop "background-color" "#77f" ]
        , child ".group.even"
            [ prop "background-color" "#f7f7f7"
            , prop "box-sizing" "border-box"
            , prop "border-top" "solid 1px #eeeeee"
            , prop "border-bottom" "solid 1px #eeeeee"
            ]
        , child ".group.odd" [ prop "background-color" "white" ]
        , sister ":focus-visible" [ prop "outline" "none" ]
        , sister ":focus" [ child ".group >div" [ prop "background-color" "rgba(100,100,255,0.1)" ] ]
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
            , prop "fontFamily" "Arial, Helvetica, sans-serif"
            , prop "box-sizing" "border-box"
            , child ".dates"
                [ prop "color" "rgba(0,0,0,0.5)"
                , child "div.h-end-date"
                    [ prop "position" "absolute"
                    , prop "width" "100%"
                    , prop "padding-right" "3px"
                    , prop "box-sizing" "border-box"
                    , prop "top" "0px"
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
        ]
