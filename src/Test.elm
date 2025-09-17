module Test exposing (main)

import Array
import Browser
import Browser.Dom
import Browser.Events
import Html
import Json.Decode exposing (Value)
import Phosphor exposing (IconWeight(..))
import Platform.Cmd as Cmd
import Task
import Time
import Timeline
import Timeline.Models exposing (Group, Interaction(..), Section)


type Msg
    = TimelineMsg Timeline.Msg
    | WindowResize
        { width : Int
        , height : Int
        }


type alias Model =
    { timelineState : Timeline.Models.TimelineBox
    , box :
        { width : Int
        , height : Int
        }
    }


main : Program Value Model Msg
main =
    Browser.document
        { init =
            \_ ->
                ( { timelineState =
                        Timeline.init
                            -- []
                            groupsData
                            Time.utc
                            (Time.millisToPosix 0)
                            |> Timeline.canEditGroups False
                            |> Timeline.canSortGroups False
                            |> Timeline.changeDirection Timeline.Models.Vertical
                  , box =
                        { width = 1000
                        , height = 500
                        }
                  }
                , Task.perform (\size -> sizeToMsg (round size.viewport.width) (round size.viewport.height)) Browser.Dom.getViewport
                )
        , update = update
        , view = view
        , subscriptions =
            \model ->
                Sub.batch
                    [ Browser.Events.onResize sizeToMsg
                    , Sub.map TimelineMsg (Timeline.subscriptions model.timelineState)
                    ]
        }


view : Model -> { title : String, body : List (Html.Html Msg) }
view model =
    { title = "Timeline"
    , body =
        [ Html.node "style" [] [ Html.text "body {margin:0}" ]
        , Html.node "style" [] [ Html.text Timeline.styles ]
        , Timeline.view [] model.timelineState model.box
            |> Html.map TimelineMsg
        ]
    }


sizeToMsg : Int -> Int -> Msg
sizeToMsg w h =
    WindowResize { width = w, height = h }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimelineMsg tmsg ->
            timelineUpdate tmsg model

        WindowResize b ->
            ( { model | box = b }, Cmd.none )


timelineUpdate : Timeline.Msg -> Model -> ( Model, Cmd Msg )
timelineUpdate tmsg model =
    let
        ( state, _, tcmd ) =
            Timeline.update tmsg model.timelineState model.box

        cmd =
            -- case action of
            --     Timeline.Action.ModifySections ids ( addstart, addend ) ->
            --     Timeline.Action.CloneSections ids addstart mbgroup ->
            --     Timeline.Action.DuplicateSections ids ->
            --     Timeline.Action.DeleteSections ids ->
            --     Timeline.Action.MoveSections ids gid ->
            --     Timeline.Action.CreateSection gid from to ->
            --     Timeline.Action.ChangeZoom _ ->
            --     Timeline.Action.SelectSections sel ->
            --     Timeline.Action.Split sel date ->
            --     _ ->
            Cmd.none
    in
    ( { model
        | timelineState =
            state
      }
    , Cmd.batch [ cmd, Cmd.map TimelineMsg tcmd ]
    )


duration : { day : Float }
duration =
    { day = 24 * 60 * 60 * 1000 }


makeDefSec : { id : String, start : Float, end : Float, color : String, isGlobal : Bool } -> Section
makeDefSec { id, start, end, color, isGlobal } =
    { start = start * duration.day + 1633478400000 |> round |> Time.millisToPosix
    , end = end * duration.day + 1633478400000 |> round |> Time.millisToPosix
    , id = id
    , color = color
    , isLocked = False
    , labels = [ id, "Hello world !" ]
    , hasComment =
        if modBy 3 (round start) == 1 then
            True

        else
            False
    , isGlobal = isGlobal
    }


cols : Int
cols =
    40


rows : Int
rows =
    20


groupsData : List Group
groupsData =
    List.map
        (\j ->
            { id = "g" ++ String.fromInt j
            , label = [ "group " ++ String.fromInt j ]
            , isSubtotal = False
            , sections =
                if j /= 2 then
                    List.map
                        (\i ->
                            makeDefSec
                                { id = String.fromInt (j * cols + i)
                                , start = toFloat i
                                , end =
                                    if i == 2 && j == 3 then
                                        toFloat (i + 2)

                                    else
                                        toFloat (i + 1)
                                , color =
                                    -- "rose"
                                    -- Array.get (modBy 17 i)
                                    --     (if modBy 2 j == 0 then
                                    --         colorsSat
                                    --      else
                                    --         colorsPale
                                    --     )
                                    --
                                    Array.get
                                        (modBy 29 i)
                                        colors
                                        |> Maybe.withDefault ""
                                , isGlobal = i == 2 && j == 3
                                }
                        )
                        (List.range 0 (cols - 1))

                else
                    []
            }
        )
        (List.range 0 rows)


colors =
    Array.fromList
        [ "rose"
        , "rose-pale"
        , "magenta"
        , "magenta-pale"
        , "violet"
        , "violet-pale"
        , "bleu"
        , "bleu-pale"
        , "azur"
        , "azur-pale"
        , "cyan"
        , "cyan-pale"
        , "turquoise"
        , "turquoise-pale"
        , "vert"
        , "vert-pale"
        , "anis"
        , "anis-pale"
        , "jaune"
        , "jaune-pale"
        , "orange"
        , "orange-pale"
        , "rouge"
        , "rouge-pale"
        , "error"
        , "problem"
        , "warning"
        , "ok"
        ]
