module Timeline.Action exposing (Action(..), changeWheelAction, noAction, selectAction)

import Html exposing (i)
import Moment
import Time exposing (Posix)
import Timeline.Models exposing (..)


type Action
    = NoAction
    | SelectSections Selection
    | CreateSection (Maybe GroupId) Posix Posix
    | MoveSections Selection GroupId
    | ModifySections Selection ( Moment.Duration, Moment.Duration )
    | ReorderGroups (List GroupId)
    | DeleteGroups (List GroupId)
    | DeleteSections (List SectionId)
    | ModifyGroupLabel GroupId String
    | CloneSections Selection Moment.Duration (Maybe GroupId)
    | DuplicateSections Selection
    | ChangeZoom { start : Posix, zoom : Float, sectionOffsetY : Float, lineSize : Float }
    | Split Selection Posix


noAction : TimelineBox -> ( TimelineBox, Action, Cmd msg )
noAction b =
    ( b, NoAction, Cmd.none )


selectAction : TimelineBox -> ( TimelineBox, Action, Cmd msg )
selectAction b =
    ( b, SelectSections b.selection, Cmd.none )


changeWheelAction : TimelineBox -> ( TimelineBox, Action, Cmd msg )
changeWheelAction tl =
    let
        start =
            unscale tl.zoom -tl.start
                + (Time.posixToMillis tl.first |> toFloat)
                |> round
                |> Time.millisToPosix
    in
    ( tl, ChangeZoom { start = start, zoom = tl.zoom, sectionOffsetY = tl.sectionOffsetY, lineSize = tl.lineSize }, Cmd.none )
