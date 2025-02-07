module Timeline.Update exposing (groupsWheel, sectionsDown, sectionsMove, sectionsUp, sectionsWheel, snapToGridForZoom, updateSelection)

import Dict
import List.Extra as Extra
import Moment
import Set
import Time
import Time.Extra as TimeX
import Timeline.Action exposing (..)
import Timeline.Axis as Axis
import Timeline.Event exposing (..)
import Timeline.Models exposing (..)
import Timeline.Utils exposing (..)


getUnit : Float -> Moment.Duration
getUnit zoom =
    (.snap <| Axis.getGrid (duration.day / zoom)) * 3600000 |> round |> Moment.toDuration


zoneOffset : Time.Zone -> Time.Posix -> Int
zoneOffset zone val =
    let
        delta =
            Time.toHour zone val - Time.toHour Time.utc val
    in
    if delta > 12 then
        delta - 24

    else if delta < -12 then
        delta + 24

    else
        delta


snapToGridForZoom : Float -> Time.Zone -> Time.Posix -> Time.Posix
snapToGridForZoom zoom zone time =
    snapToGrid zone (getUnit zoom) time


snapToGrid_ : (Float -> Int) -> Time.Zone -> Moment.Duration -> Time.Posix -> Time.Posix
snapToGrid_ func zone unit date =
    let
        gridUnit =
            Moment.fromDuration unit
    in
    if gridUnit > 86400000 then
        -- week snap
        let
            startWeek =
                TimeX.floor TimeX.Week zone date

            add =
                Moment.durationBetween startWeek date |> Moment.mapDuration (\dur -> (toFloat dur / 604800000 |> func) * 604800000)
        in
        Moment.addDurationToPosix startWeek add
            |> TimeX.floor TimeX.Week zone

    else
        let
            offset =
                if gridUnit > 3600000 then
                    zoneOffset zone date * 3600000

                else
                    0
        in
        func
            (((Time.posixToMillis date + offset)
                |> toFloat
             )
                / toFloat gridUnit
            )
            * gridUnit
            - offset
            |> Time.millisToPosix


snapToGrid : Time.Zone -> Moment.Duration -> Time.Posix -> Time.Posix
snapToGrid zone unit val =
    snapToGrid_ round zone unit val


snapToGridFloor : Time.Zone -> Moment.Duration -> Time.Posix -> Time.Posix
snapToGridFloor zone unit val =
    snapToGrid_ floor zone unit val


snapToGridInt grid =
    let
        unit =
            grid.snap * 3600000
    in
    Moment.mapDuration <| \xx -> toFloat (round (toFloat xx / unit)) * unit |> round



-- differentGroups : TimelineBox -> Bool
-- differentGroups box =
-- Dict.filter (\_ set -> Set.isEmpty set |> not) box.selection
-- |> Dict.size
-- |> (<) 1


sectionsWheel : TimelineBox -> { width : Int, height : Int } -> Event -> ( TimelineBox, Action, Cmd msg )
sectionsWheel box rect { x, y, dx, dy, altKey, shiftKey } =
    let
        getter =
            directionGetter box.direction

        startPixel =
            getter.h ( x, y )

        ( movex, movey ) =
            if shiftKey then
                ( dy, 0 )

            else
                ( dx, dy )

        offset =
            Basics.max
                (Basics.min 0 (toFloat -(box.lines + 0) * box.lineSize + toFloat rect.height))
                (Basics.min 0 (box.sectionOffsetY - getter.v ( movex, movey )))
    in
    if altKey then
        let
            zoom =
                Basics.min 6000
                    (Basics.max
                        (if box.direction == Horizontal then
                            2

                         else
                            5
                        )
                        (box.zoom - (dy / 200 * box.zoom))
                    )

            newstart =
                ((box.start - startPixel) / box.zoom * zoom) + startPixel

            newbox =
                -- rescale
                { box
                    | zoom = zoom
                    , zoomChange = 5
                    , start = newstart
                }
        in
        changeWheelAction newbox

    else
        let
            movetime =
                getter.h ( movex, movey ) |> unscale box.zoom

            newbox =
                { box
                    | start = box.start - getter.h ( movex, movey )
                    , sectionOffsetY = offset
                }

            unit =
                getUnit box.zoom
        in
        case box.interaction of
            Select selaction pos _ _ ->
                setLasso selaction newbox pos (normalize newbox x y)
                    |> updateMeshes
                    |> changeWheelAction

            Move mtype msec (( start, fromLine ) as pos) _ ->
                changeWheelAction
                    { newbox
                        | interaction =
                            normalize box x y
                                |> Tuple.mapBoth
                                    (\end -> Moment.durationBetween start end)
                                    (\toLine -> toLine - fromLine)
                                |> (\( deltax, deltay ) ->
                                        let
                                            ( pix, piy ) =
                                                Tuple.mapBoth (\d -> (Moment.fromDuration d |> toFloat) * box.zoom / duration.day)
                                                    (\v -> v * box.lineSize)
                                                    ( deltax, deltay )
                                        in
                                        if abs pix > abs piy || selectionOverManyGroups box.selection then
                                            ( deltax, 0 )

                                        else
                                            ( Moment.toDuration 0, round deltay )
                                   )
                                |> Move mtype msec pos
                    }

            ResizeLeft (( start, _ ) as pos) _ ->
                changeWheelAction { newbox | interaction = ResizeLeft pos <| Moment.durationBetween start (Tuple.first (normalize box x y)) }

            ResizeRight (( start, _ ) as pos) _ ->
                changeWheelAction { newbox | interaction = ResizeRight pos <| Moment.durationBetween start (Tuple.first (normalize box x y)) }

            Draw start _ line ->
                let
                    ax =
                        Tuple.first (normalize newbox x y)

                    snapx =
                        snapToGrid newbox.zone
                            unit
                            (if Moment.lessThan ax start then
                                Moment.minPosix (Moment.subtractDuration start unit) ax

                             else
                                Moment.maxPosix (Moment.addDurationToPosix start unit) ax
                            )
                in
                changeWheelAction <| { newbox | interaction = Draw start snapx line }

            MouseOver ( time, float ) ->
                changeWheelAction <|
                    { newbox
                        | interaction =
                            MouseOver
                                ( Moment.addDurationToPosix time (movetime |> round |> Moment.toDuration)
                                , float
                                )
                    }

            _ ->
                changeWheelAction newbox


sectionsMove : TimelineBox -> Event -> ( TimelineBox, Action, Cmd msg )
sectionsMove box { x, y } =
    let
        ( posix, toLine ) =
            normalize box x y

        unit =
            getUnit box.zoom
    in
    case box.interaction of
        MouseOver _ ->
            noAction { box | interaction = mouseOverInteraction box x y }

        Select selAction pos _ _ ->
            setLasso selAction box pos ( posix, toLine )
                |> updateMeshes
                |> noAction

        Move mtype msec (( posDate, posLine ) as pos) _ ->
            let
                deltaTime =
                    Moment.durationBetween posDate posix

                deltaLine =
                    toLine - posLine

                ( pix, piy ) =
                    Tuple.mapBoth (\d -> (Moment.fromDuration d |> toFloat) * box.zoom / duration.day)
                        (\v -> v * box.lineSize)
                        ( deltaTime, deltaLine )
            in
            noAction
                { box
                    | interaction =
                        Move mtype
                            msec
                            pos
                            (if abs pix > abs piy || selectionOverManyGroups box.selection then
                                ( deltaTime, 0 )

                             else
                                ( Moment.toDuration 0, round deltaLine )
                            )
                }

        ResizeLeft (( posDate, _ ) as pos) _ ->
            noAction { box | interaction = ResizeLeft pos (Moment.durationBetween posDate posix) }

        ResizeRight (( posDate, _ ) as pos) _ ->
            noAction { box | interaction = ResizeRight pos (Moment.durationBetween posDate posix) }

        Draw start _ line ->
            let
                snapx =
                    snapToGrid box.zone
                        unit
                        (if Moment.lessThan posix start then
                            Moment.minPosix (Moment.subtractDuration start unit) posix

                         else
                            Moment.maxPosix (Moment.addDurationToPosix start unit) posix
                        )
            in
            noAction <| { box | interaction = Draw start snapx line }

        EditGroupLabel _ _ ->
            noAction box


sectionsDown : TimelineBox -> Event -> ( TimelineBox, Action, Cmd msg )
sectionsDown box { x, y, altKey, shiftKey, button } =
    let
        ( posix, line ) =
            normalize box x y

        mbsec =
            findSection posix ( line, 1 - (4 / box.lineSize) ) box.sections
                |> Maybe.andThen
                    (\sbox ->
                        Dict.get sbox.groupId box.groups
                            |> Maybe.andThen
                                (\gbox ->
                                    if gbox.isSubtotal then
                                        Nothing

                                    else
                                        Just sbox
                                )
                    )

        margin =
            6 * duration.day / box.zoom |> round |> Moment.toDuration
    in
    case box.interaction of
        MouseOver _ ->
            if button == leftButton then
                if altKey && (mbsec == Nothing) then
                    let
                        unit =
                            getUnit box.zoom

                        snapx =
                            snapToGridFloor box.zone unit posix
                    in
                    { box
                        | interaction =
                            if box.canEditSections then
                                Draw snapx (Moment.addDurationToPosix snapx unit) (floor line)

                            else
                                box.interaction
                    }
                        |> updateSelection emptySelection
                        |> selectAction

                else
                    case mbsec of
                        Just ({ section } as sbox) ->
                            { box
                                | interaction =
                                    if shiftKey then
                                        Select
                                            (if isSelected sbox.groupId section.id box.selection then
                                                SubstractSelection

                                             else
                                                AddSelection
                                            )
                                            ( posix, line )
                                            (selectionWith sbox.groupId section.id)
                                            ( ( posix, line ), ( Moment.toDuration 0, 0 ) )

                                    else if (section :: selectedSections box) |> List.foldl (\s res -> res || s.isLocked) False then
                                        box.interaction

                                    else if box.canEditSections && Moment.greaterThan posix (Moment.subtractDuration section.end margin) then
                                        ResizeRight ( posix, line ) <| Moment.toDuration 0

                                    else if box.canEditSections && Moment.lessThan posix (Moment.addDurationToPosix section.start margin) then
                                        ResizeLeft ( posix, line ) <| Moment.toDuration 0

                                    else if box.canEditSections then
                                        Move
                                            (if altKey then
                                                Clone

                                             else
                                                SimpleMove
                                            )
                                            sbox
                                            ( posix, line )
                                            ( Moment.toDuration 0, 0 )

                                    else
                                        box.interaction
                            }
                                |> updateSelection
                                    (if shiftKey then
                                        box.selection

                                     else if isSelected sbox.groupId section.id box.selection then
                                        box.selection

                                     else
                                        selectionWith sbox.groupId section.id
                                    )
                                |> selectAction

                        Nothing ->
                            { box
                                | interaction =
                                    Select AddSelection ( posix, line ) emptySelection ( ( posix, line ), ( Moment.toDuration 0, 0 ) )
                            }
                                |> updateSelection
                                    (if shiftKey then
                                        box.selection

                                     else
                                        emptySelection
                                    )
                                |> selectAction

            else
                noAction box

        _ ->
            noAction box


sectionsUp : TimelineBox -> Event -> ( TimelineBox, Action, Cmd msg )
sectionsUp box { x, y } =
    let
        selection =
            mapSelection
                (\gid set ->
                    case Dict.get gid box.groups of
                        Just group ->
                            if group.isSubtotal then
                                Set.empty

                            else
                                set

                        _ ->
                            Set.empty
                )
                box.selection
    in
    case box.interaction of
        Select selAction _ ids _ ->
            let
                newsel =
                    case selAction of
                        AddSelection ->
                            addSelection ids box.selection

                        SubstractSelection ->
                            substractSelection ids box.selection
            in
            ( { box
                | interaction = mouseOverInteraction box x y
              }
                |> updateSelection newsel
            , SelectSections newsel
            , Cmd.none
            )

        Move mtype msec _ ( deltaTime, deltaLine ) ->
            ( { box
                | interaction = mouseOverInteraction box x y
              }
            , if Moment.durationNotZero deltaTime then
                let
                    grid =
                        Axis.getGrid (duration.day / box.zoom)
                in
                if mtype == SimpleMove then
                    ModifySections selection ( snapToGridInt grid deltaTime, Moment.toDuration 0 )

                else
                    CloneSections selection (snapToGridInt grid deltaTime) Nothing

              else if deltaLine /= 0 then
                let
                    destGI =
                        msec.line + deltaLine

                    destGroup =
                        findGroupAtPosition destGI box
                in
                case destGroup of
                    Maybe.Just g ->
                        if g.isSubtotal then
                            NoAction

                        else if mtype == SimpleMove then
                            MoveSections selection g.id

                        else
                            CloneSections selection (Moment.toDuration 0) (Just g.id)

                    Maybe.Nothing ->
                        NoAction

              else
                NoAction
            , Cmd.none
            )

        ResizeLeft _ delta ->
            ( { box
                | interaction = mouseOverInteraction box x y
              }
            , if Moment.durationNotZero delta then
                let
                    grid =
                        Axis.getGrid (duration.day / box.zoom)

                    minDuration =
                        minDurationOf box.groups selection

                    unit =
                        grid.snap * 3600000

                    minSnap =
                        min (round unit) minDuration

                    deltaSnap =
                        snapToGridInt grid <| Moment.mapDuration (\d -> min d (minDuration - minSnap)) delta
                in
                ModifySections selection ( deltaSnap, Moment.mapDuration negate deltaSnap )

              else
                NoAction
            , Cmd.none
            )

        ResizeRight _ delta ->
            ( { box
                | interaction = mouseOverInteraction box x y
              }
            , if Moment.durationNotZero delta then
                let
                    grid =
                        Axis.getGrid (duration.day / box.zoom)

                    minDuration =
                        minDurationOf box.groups selection

                    unit =
                        grid.snap * 3600000

                    minSnap =
                        min (round unit) minDuration

                    deltaSnap =
                        snapToGridInt grid <| Moment.mapDuration (\d -> max d (minSnap - minDuration)) delta
                in
                ModifySections selection ( Moment.toDuration 0, deltaSnap )

              else
                NoAction
            , Cmd.none
            )

        Draw start end lineInt ->
            let
                maybe =
                    findGroupAtPosition lineInt box

                maybeGId =
                    Maybe.map .id maybe

                create =
                    case maybe of
                        Just g ->
                            not g.isSubtotal

                        _ ->
                            False
            in
            ( { box | interaction = mouseOverInteraction box x y }
            , if create then
                if Moment.lessThan start end then
                    CreateSection maybeGId start end

                else
                    CreateSection maybeGId end start

              else
                NoAction
            , Cmd.none
            )

        _ ->
            noAction { box | interaction = mouseOverInteraction box x y }


findGroupAtPosition : Int -> TimelineBox -> Maybe GroupBox
findGroupAtPosition line box =
    Extra.find (\g -> line >= g.position && line < (g.position + g.size)) (Dict.values box.groups)


groupsWheel : TimelineBox -> { top : Int, width : Int, height : Int } -> Event -> ( TimelineBox, Action, Cmd msg )
groupsWheel box rect { clientY, dx, dy, altKey, shiftKey } =
    if altKey then
        let
            lineSize =
                min 300 <|
                    max
                        (if box.direction == Horizontal then
                            18

                         else
                            50
                        )
                    <|
                        box.lineSize
                            + (dy / 10)
        in
        changeWheelAction
            { box
                | lineSize = lineSize
                , zoomChange = 10
                , sectionOffsetY =
                    Basics.max
                        (Basics.min 0 (toFloat -(box.lines + 0) * lineSize + toFloat rect.height))
                        (Basics.min 0 <| box.sectionOffsetY - (((clientY - toFloat rect.top) - box.sectionOffsetY) / box.lineSize * (lineSize - box.lineSize)))
            }

    else
        let
            getter =
                directionGetter box.direction

            ( movex, movey ) =
                if shiftKey then
                    ( dy, 0 )

                else
                    ( dx, dy )

            offset =
                Basics.max
                    (Basics.min 0 (toFloat -(box.lines + 0) * box.lineSize + toFloat rect.height))
                    (Basics.min 0 (box.sectionOffsetY - getter.v ( movex, movey )))
        in
        changeWheelAction
            { box
                | start = box.start - getter.h ( movex, movey )
                , sectionOffsetY = offset
            }


updateSelection : Selection -> TimelineBox -> TimelineBox
updateSelection selection box =
    updateMeshes
        { box
            | selection = selection
        }


updateMeshes : TimelineBox -> TimelineBox
updateMeshes box =
    let
        selection =
            case box.interaction of
                Select selAction _ sel _ ->
                    case selAction of
                        AddSelection ->
                            addSelection sel box.selection

                        SubstractSelection ->
                            substractSelection sel box.selection

                _ ->
                    box.selection

        groups =
            groupDiffSelection selection box.meshesSelection

        ( meshes, selected ) =
            meshesForGroups box.first box.groups (Set.toList groups) selection ( box.meshes, box.selectedMeshes )

        -- Dict.map
        --         (\gid meshes ->
        --             if Set.member gid groups then
        --                 case Dict.get gid box.groups of
        --                     Nothing ->
        --                         meshes
        --                     Just group ->
        --                         List.map
        --                             (\sbox ->
        --                                 { section = sbox.section
        --                                 , line = sbox.line
        --                                 , selected = isSelected gid sbox.section.id selection
        --                                 }
        --                             )
        --                             group.sections
        --                             |> List.partition (.selected >> not)
        --                             |> (\( a, b ) -> a ++ b)
        --                             |> toMeshes
        --                             |> (\m -> { position = toFloat group.position, meshes = m })
        --             else
        --                 meshes
        --         )
        --         box.meshes
    in
    { box
        | meshesSelection = selection
        , meshes = meshes
        , selectedMeshes = selected
    }
