port module Widget exposing (main)

import Bounce exposing (Bounce)
import Browser
import Browser.Dom
import Browser.Events
import Css
import Dict exposing (Dict)
import Field exposing (Field)
import Html
import Html.Attributes as HA
import Html.Events
import Html.Styled as Styled
import Html.Styled.Attributes as SA
import Iso8601
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as DecodeX
import Json.Decode.Pipeline as Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import List.Extra as ListX
import Moment
import Platform.Cmd as Cmd
import Select
import Select.Styles as Styles
import Set
import Svg.Attributes exposing (amplitude)
import Task
import Time
import Time.Extra as TimeX
import Timeline
import Timeline.Action
import Timeline.Models exposing (Group, Interaction(..), selectionIsEmpty)
import Timeline.Update


type Msg
    = TimelineMsg Timeline.Msg
    | Receive Value
    | ChangeSelection Value
    | ChangeOptions Value
    | WindowResize
        { width : Int
        , height : Int
        }
    | OptionsBounceMsg
    | ChangeText String String
    | ValidateText String String
    | CancelChange String
    | FocusField String
    | NoOp
    | SelectMsg String (Select.Msg Field.ChoiceId)


type alias Model =
    { timelineState : Timeline.Models.TimelineBox
    , error : Maybe String
    , box :
        { width : Int
        , height : Int
        }
    , bounce : Bounce
    , fields : Dict String ( Field, FieldState )
    , focus : String
    , options : Options
    , records : Dict String Record
    , selectStates : Dict String Select.State
    , showInspector : Bool
    }


main : Program () Model Msg
main =
    Browser.document
        { init =
            \_ ->
                ( { timelineState =
                        Timeline.init []
                            -- groupsData
                            |> Timeline.canEditGroups False
                            |> Timeline.canSortGroups False
                  , error = Nothing
                  , box =
                        { width = 1000
                        , height = 500
                        }
                  , bounce = Bounce.init
                  , fields = Dict.empty
                  , options = Options 0 0 0 38
                  , records = Dict.empty
                  , selectStates = Dict.empty
                  , focus = ""
                  , showInspector = False
                  }
                  -- , initialSizeCmd
                , Task.perform (\size -> sizeToMsg (round size.viewport.width) (round size.viewport.height)) Browser.Dom.getViewport
                )
        , update = update
        , view =
            \model ->
                { title = "WeSchedule"
                , body =
                    case model.error of
                        Just err ->
                            [ Html.text err ]

                        Nothing ->
                            [ Html.node "style" [] [ Html.text Timeline.styles ]
                            , Html.node "style" [] [ Html.text styles ]
                            , Timeline.view model.timelineState model.box
                                |> Html.map TimelineMsg
                            , if model.showInspector == False || Timeline.Models.selectionIsEmpty model.timelineState.selection then
                                Html.text ""

                              else
                                inspectorView model
                            ]
                }
        , subscriptions =
            \model ->
                Sub.batch
                    [ setRecords Receive
                    , setSelection ChangeSelection
                    , setOptions ChangeOptions
                    , Browser.Events.onResize sizeToMsg
                    , Sub.map TimelineMsg (Timeline.subscriptions model.timelineState)
                    ]
        }


inspectorView : Model -> Html.Html Msg
inspectorView model =
    let
        selSize =
            Timeline.Models.selectionSize model.timelineState.selection

        records =
            Timeline.Models.selectionToSet model.timelineState.selection
                |> Set.toList
                |> List.filterMap (\id -> Dict.get id model.records)

        cumul =
            (List.map .amplitude records |> List.sum) / 3600

        maybeStart =
            List.map .date records |> List.minimum

        maybeEnd =
            List.map (\r -> r.date + (r.amplitude * 1000 |> round)) records |> List.maximum

        amplitude =
            Maybe.map2 (\start end -> toFloat (end - start) / 3600000) maybeStart maybeEnd |> Maybe.withDefault 0

        drawChoices =
            \field choices ->
                case Dict.get field.name model.selectStates of
                    Just selectState ->
                        let
                            selectedItem =
                                -- Maybe.map (\str -> Select.basicMenuItem { item = str, label = str }) field.str
                                Maybe.andThen (\str -> ListX.find (\c -> c.label == str) choices) field.str
                                    |> Maybe.map (\c -> Select.basicMenuItem { item = c.id, label = c.label })
                        in
                        Select.view
                            (Select.single selectedItem
                                |> Select.state selectState
                                |> Select.menuItems
                                    (List.map
                                        (\c ->
                                            Select.customMenuItem
                                                { item = c.id
                                                , label = c.label
                                                , view =
                                                    Styled.span
                                                        [ SA.css
                                                            [ Css.backgroundColor <| Css.hex c.backgroundColor
                                                            , Css.color <| Css.hex c.textColor
                                                            , Css.padding (Css.px 3)
                                                            , Css.borderRadius (Css.px 3)
                                                            ]
                                                        ]
                                                        [ Styled.text c.label ]
                                                }
                                        )
                                        choices
                                    )
                                |> Select.placeholder
                                    (if field.multi then
                                        "<multiple>"

                                     else
                                        "<vide>"
                                    )
                                |> Select.setStyles selectStyles
                                |> Select.clearable True
                            )
                            |> Styled.toUnstyled
                            |> Html.map (SelectMsg field.name)

                    Nothing ->
                        Html.text ""
    in
    [ (if selSize > 1 then
        String.fromInt selSize ++ " tâches"

       else
        String.fromInt selSize ++ " tâche"
      )
        |> Html.text
        |> List.singleton
        |> Html.div []
    , "Cumul Durées : "
        ++ (cumul |> String.fromFloat)
        ++ (if cumul > 1 then
                " heures"

            else
                "heure"
           )
        |> Html.text
        |> List.singleton
        |> Html.div []
    , ("Amplitude : "
        ++ String.fromFloat amplitude
        ++ (if amplitude > 1 then
                " heures"

            else
                "heure"
           )
      )
        |> Html.text
        |> List.singleton
        |> Html.div []
    , model.fields
        |> Dict.toList
        |> List.sortBy (\( _, ( f, _ ) ) -> f.position)
        |> List.map
            (\( key, ( field, value ) ) ->
                case value of
                    Val str ->
                        { field = field, name = key, label = field.label, str = Just str, multi = False, error = Nothing }

                    Multi ->
                        { field = field, name = key, label = field.label, str = Nothing, multi = True, error = Nothing }

                    _ ->
                        { field = field, name = key, label = field.label, str = Nothing, multi = False, error = Just "Erreur" }
            )
        |> List.map
            (\field ->
                Html.div [ HA.class "field" ]
                    [ Html.label [ HA.for field.name ] [ Html.text field.label ]
                    , case field.field.ofType of
                        Field.Choice choices ->
                            drawChoices field choices

                        Field.Ref choices ->
                            drawChoices field choices

                        Field.Text True ->
                            Html.textarea
                                [ HA.name field.name
                                , HA.id field.name
                                , HA.rows 4
                                , HA.cols 18
                                , HA.placeholder
                                    (if field.multi then
                                        "<multiple>"

                                     else
                                        ""
                                    )
                                , Html.Events.onInput (ChangeText field.name)
                                , Html.Events.on "keyup" <|
                                    Decode.andThen
                                        (\key ->
                                            case ( key, field.str ) of
                                                ( 27, _ ) ->
                                                    Decode.succeed (CancelChange field.name)

                                                _ ->
                                                    Decode.fail ""
                                        )
                                        Html.Events.keyCode
                                , Html.Events.onFocus (FocusField field.name)
                                , if field.name == model.focus then
                                    Html.Events.onBlur
                                        (case field.str of
                                            Just str ->
                                                ValidateText field.name str

                                            Nothing ->
                                                NoOp
                                        )

                                  else
                                    HA.class ""
                                ]
                                [ Html.text <| Maybe.withDefault "" field.str ]

                        _ ->
                            Html.input
                                [ HA.name field.name
                                , HA.id field.name
                                , HA.placeholder
                                    (if field.multi then
                                        "<multiple>"

                                     else
                                        ""
                                    )
                                , HA.property "indeterminate" (Encode.bool field.multi)
                                , HA.type_
                                    (case field.field.ofType of
                                        Field.DateTime ->
                                            "datetime-local"

                                        Field.Bool ->
                                            "checkbox"

                                        _ ->
                                            "text"
                                    )
                                , if field.field.ofType == Field.Bool then
                                    HA.checked (Maybe.map ((==) "true") field.str |> Maybe.withDefault False)

                                  else
                                    HA.value <| Maybe.withDefault "" field.str
                                , if field.field.ofType == Field.Bool then
                                    Html.Events.onClick
                                        (ChangeText field.name
                                            (Maybe.map
                                                (\b ->
                                                    if b == "true" then
                                                        "false"

                                                    else
                                                        "true"
                                                )
                                                field.str
                                                |> Maybe.withDefault "true"
                                            )
                                        )

                                  else
                                    Html.Events.onInput (ChangeText field.name)
                                , Html.Events.on "keyup" <|
                                    Decode.andThen
                                        (\key ->
                                            case ( key, field.str ) of
                                                ( 27, _ ) ->
                                                    Decode.succeed (CancelChange field.name)

                                                ( 13, Just str ) ->
                                                    Decode.succeed (ValidateText field.name str)

                                                _ ->
                                                    Decode.fail ""
                                        )
                                        Html.Events.keyCode
                                , Html.Events.onFocus (FocusField field.name)
                                , if field.name == model.focus then
                                    Html.Events.onBlur
                                        (case field.str of
                                            Just str ->
                                                ValidateText field.name str

                                            Nothing ->
                                                NoOp
                                        )

                                  else
                                    HA.class ""
                                ]
                                []
                    ]
            )
        |> Html.div []
    ]
        |> Html.div
            [ HA.style "position" "absolute"
            , HA.style "right" "0px"
            , HA.style "top" "0px"
            , HA.style "width" "170px"
            , HA.style "height" ((model.box.height |> String.fromInt) ++ "px")
            , HA.style "background-color" "#f7f7f7"
            , HA.style "font-family" "Arial, Helvetica, sans-serif"
            , HA.style "font-size" "12px"
            , HA.style "overflow-y" "scroll"

            -- , HA.style "box-shadow" "grey 1px 0px 14px"
            , HA.style "border-left" "1px solid #DDD"
            , HA.style "padding" "10px"
            ]


sizeToMsg : Int -> Int -> Msg
sizeToMsg w h =
    WindowResize { width = w, height = h }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimelineMsg tmsg ->
            timelineUpdate tmsg model

        Receive data ->
            receiveData data model

        ChangeSelection data ->
            case Decode.decodeValue (Decode.list (Decode.field "id" Decode.int)) data of
                Err err ->
                    ( { model
                        | error =
                            Just (Decode.errorToString err)
                      }
                    , Cmd.none
                    )

                Ok ids ->
                    let
                        groups =
                            model.timelineState.srcgroups

                        selection =
                            sectionIdsToSelection groups ids
                    in
                    ( { model | timelineState = Timeline.Update.updateSelection selection model.timelineState }, Cmd.none )

        ChangeOptions data ->
            case Decode.decodeValue optionsDecoder data of
                Err err ->
                    ( { model
                        | error =
                            Just (Decode.errorToString err)
                      }
                    , Cmd.none
                    )

                Ok options ->
                    ( { model
                        | timelineState =
                            Timeline.changeStartAndZoom options.start options.zoom model.timelineState
                                |> Timeline.changeYOffset options.sectionOffsetY
                                |> Timeline.changeLineSize options.lineSize
                        , options = options
                      }
                    , Cmd.none
                    )

        WindowResize b ->
            ( { model | box = b }, Cmd.none )

        OptionsBounceMsg ->
            ( model, encodeOptions model.options |> updateOptions )

        ChangeText field str ->
            ( { model | fields = Dict.update field (Maybe.map (\( f, _ ) -> ( f, Val str ))) model.fields }
            , Cmd.none
            )

        ValidateText field str ->
            ( { model
                | focus =
                    if model.focus == field then
                        ""

                    else
                        model.focus
              }
            , makeFieldUpdate model field str
            )

        CancelChange field ->
            let
                upd =
                    Dict.update field
                        (Maybe.andThen
                            (\v ->
                                Dict.singleton field v
                                    |> fieldsFromSelection model.timelineState.zone model.timelineState.selection model.records
                                    |> Dict.get field
                            )
                        )
                        model.fields
            in
            ( { model | fields = upd, focus = "" }, Browser.Dom.blur field |> Task.attempt (always NoOp) )

        NoOp ->
            ( model, Cmd.none )

        SelectMsg field selectMsg ->
            case Dict.get field model.selectStates of
                Just selectState ->
                    let
                        ( maybeAction, updatedSelectState, selectCmds ) =
                            Select.update selectMsg selectState

                        ( newDict, updMsg ) =
                            case maybeAction of
                                Just (Select.Select item) ->
                                    ( Dict.get field model.fields
                                        |> Maybe.andThen
                                            (\( f, _ ) ->
                                                case f.ofType of
                                                    Field.Choice choices ->
                                                        ListX.find (\c -> c.id == item) choices

                                                    Field.Ref choices ->
                                                        ListX.find (\c -> c.id == item) choices

                                                    _ ->
                                                        Nothing
                                            )
                                        |> Maybe.map
                                            (\choice ->
                                                Dict.update field (Maybe.map <| \( f, _ ) -> ( f, Val choice.label )) model.fields
                                            )
                                        |> Maybe.withDefault model.fields
                                    , makeFieldUpdate_ model.timelineState.selection field (Field.encodeChoiceId item)
                                    )

                                Just Select.Clear ->
                                    ( Dict.empty
                                    , makeFieldUpdate_ model.timelineState.selection field (Encode.string "")
                                    )

                                _ ->
                                    ( model.fields, Cmd.none )
                    in
                    ( { model
                        | selectStates = Dict.insert field updatedSelectState model.selectStates
                        , fields = newDict
                      }
                    , Cmd.batch [ updMsg, Cmd.map (SelectMsg field) selectCmds ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        FocusField field ->
            ( { model | focus = field }, Cmd.none )


timelineUpdate : Timeline.Msg -> Model -> ( Model, Cmd Msg )
timelineUpdate tmsg model =
    let
        ( state, action, tcmd ) =
            Timeline.update tmsg model.timelineState model.box

        cmd =
            case action of
                Timeline.Action.ModifySections ids ( addstart, addend ) ->
                    modifyRecordsDelta
                        { ids = Timeline.Models.selectionToSet ids |> Set.toList |> List.filterMap String.toInt
                        , changeDebut = Moment.fromDuration addstart // 1000
                        , changeAmplitude = Moment.fromDuration addend // 1000
                        }

                Timeline.Action.CloneSections ids addstart mbgroup ->
                    let
                        mbg =
                            Maybe.map unwrapGroupeId mbgroup
                    in
                    cloneRecords
                        { ids = Timeline.Models.selectionToSet ids |> Set.toList |> List.filterMap String.toInt
                        , changeDebut = Moment.fromDuration addstart // 1000
                        , groupeId = Maybe.map .groupeId mbg |> Maybe.withDefault ""
                        , sousGroupeId = Maybe.map (.sousGroupeId >> Maybe.withDefault "") mbg |> Maybe.withDefault ""
                        }

                Timeline.Action.DuplicateSections ids ->
                    cloneRecords
                        { ids = Timeline.Models.selectionToSet ids |> Set.toList |> List.filterMap String.toInt
                        , changeDebut = 0
                        , groupeId = ""
                        , sousGroupeId = ""
                        }

                Timeline.Action.DeleteSections ids ->
                    deleteRecords { ids = List.filterMap String.toInt ids }

                Timeline.Action.MoveSections ids gid ->
                    let
                        g =
                            unwrapGroupeId gid
                    in
                    modifyRecordsGroup
                        { ids = Timeline.Models.selectionToSet ids |> Set.toList |> List.filterMap String.toInt
                        , groupeId = g.groupeId
                        , sousGroupeId = g.sousGroupeId |> Maybe.withDefault ""
                        }

                Timeline.Action.CreateSection gid from to ->
                    let
                        g =
                            unwrapGroupeId gid
                    in
                    createRecord <|
                        { groupeId = g.groupeId
                        , sousGroupeId = g.sousGroupeId |> Maybe.withDefault ""
                        , date = Iso8601.toDateTimeString model.timelineState.zone from
                        , duree = (Moment.durationBetween from to |> Moment.fromDuration) // 1000
                        }

                Timeline.Action.ChangeZoom _ ->
                    Bounce.delay 500 OptionsBounceMsg

                Timeline.Action.SelectSections sel ->
                    Timeline.Models.selectionToSet sel
                        |> Set.toList
                        |> List.filterMap String.toInt
                        |> selectRecords

                Timeline.Action.Split sel date ->
                    if selectionIsEmpty sel then
                        Cmd.none

                    else
                        splitRecords
                            { ids = Timeline.Models.selectionToSet sel |> Set.toList |> List.filterMap String.toInt
                            , date = Iso8601.toDateTimeString model.timelineState.zone date
                            }

                _ ->
                    Cmd.none

        updateFieldCmd =
            if String.isEmpty model.focus then
                Cmd.none

            else
                case ( action, Dict.get model.focus model.fields ) of
                    ( Timeline.Action.SelectSections _, Just ( _, Val str ) ) ->
                        makeFieldUpdate model model.focus str

                    _ ->
                        Cmd.none
    in
    ( { model
        | timelineState =
            state
                |> Timeline.applyAction action
        , options =
            case action of
                Timeline.Action.ChangeZoom { start, zoom, sectionOffsetY, lineSize } ->
                    Options start zoom sectionOffsetY lineSize

                _ ->
                    model.options
        , fields =
            case action of
                Timeline.Action.SelectSections sel ->
                    if sel == model.timelineState.selection then
                        model.fields

                    else
                        fieldsFromSelection model.timelineState.zone sel model.records model.fields

                _ ->
                    model.fields
        , showInspector =
            if model.showInspector then
                not <| Timeline.Models.selectionIsEmpty state.selection

            else if
                case state.interaction of
                    Timeline.Models.MouseOver _ ->
                        True

                    _ ->
                        False
            then
                not <| Timeline.Models.selectionIsEmpty state.selection

            else
                False
      }
    , Cmd.batch [ cmd, updateFieldCmd, Cmd.map TimelineMsg tcmd ]
    )


receiveData : Value -> Model -> ( Model, Cmd Msg )
receiveData data model =
    case Decode.decodeValue receiveDecoder data of
        Err err ->
            ( { model | error = Just (Decode.errorToString err) }, Cmd.none )

        Ok { records, maybeSelection, editable } ->
            let
                groups =
                    records
                        |> ListX.gatherEqualsBy wrapGroupeId
                        |> List.map
                            (\( head, tail ) ->
                                { id = wrapGroupeId head
                                , label = wrapGroupe head
                                , sections =
                                    head
                                        :: tail
                                        |> List.map
                                            (\rec ->
                                                { start = rec.date |> Time.millisToPosix
                                                , end = rec.date + (rec.amplitude * 1000 |> round) |> Time.millisToPosix
                                                , id = rec.id |> String.fromInt
                                                , color = rec.couleur -- = Timeline.Models.findColorName rec.couleur
                                                , isFrozen = False
                                                , labels = rec.contenu
                                                , hasComment = rec.comment /= Nothing
                                                }
                                            )
                                        |> List.sortBy (.start >> Time.posixToMillis)
                                }
                            )

                newtl =
                    case maybeSelection of
                        Nothing ->
                            Timeline.reinit groups model.timelineState

                        Just sel ->
                            Timeline.reinit groups model.timelineState
                                |> Timeline.Update.updateSelection (sectionIdsToSelection groups sel)

                recs =
                    List.map (\r -> ( String.fromInt r.id, r )) records |> Dict.fromList
            in
            ( { model
                | timelineState =
                    -- if Timeline.periodIsEqual model.timelineState newtl then
                    if model.options.zoom == 0 then
                        newtl |> Timeline.zoomAllTime model.box.width

                    else
                        newtl
                , records = recs
                , fields =
                    List.indexedMap (\pos field -> ( field.id, ( { field | position = pos }, Error NoSelection "" ) )) editable
                        |> Dict.fromList
                        |> fieldsFromSelection model.timelineState.zone newtl.selection recs
                , selectStates =
                    List.filterMap
                        (\field ->
                            case field.ofType of
                                Field.Choice _ ->
                                    Just ( field.id, Select.initState (Select.selectIdentifier field.id) )

                                Field.Ref _ ->
                                    Just ( field.id, Select.initState (Select.selectIdentifier field.id) )

                                _ ->
                                    Nothing
                        )
                        editable
                        |> Dict.fromList
                , error = Nothing
              }
            , Cmd.none
            )


makeFieldUpdate : Model -> String -> String -> Cmd Msg
makeFieldUpdate model field str =
    if field == debutFieldId then
        case Decode.decodeString DecodeX.datetime ("\"" ++ str ++ "\"") of
            Ok date ->
                let
                    selSet =
                        Timeline.Models.selectionToSet model.timelineState.selection

                    records =
                        Dict.filter (\k _ -> Set.member k selSet) model.records
                            |> Dict.values

                    base =
                        List.map .date records
                            |> List.minimum
                            |> Maybe.map Time.millisToPosix
                            |> Maybe.withDefault date

                    offset =
                        TimeX.toOffset model.timelineState.zone date
                            * 60000

                    change =
                        (Moment.durationBetween base date
                            |> Moment.fromDuration
                        )
                            - offset
                in
                modifyRecordsDelta
                    { ids = selSet |> Set.toList |> List.filterMap String.toInt
                    , changeDebut = change // 1000
                    , changeAmplitude = 0
                    }

            Err err ->
                Cmd.none

    else if field == finFieldId then
        case Decode.decodeString DecodeX.datetime ("\"" ++ str ++ "\"") of
            Ok date ->
                let
                    selSet =
                        Timeline.Models.selectionToSet model.timelineState.selection
                in
                modifyRecordsFin
                    { ids = selSet |> Set.toList |> List.filterMap String.toInt
                    , setFin = Iso8601.toUtcDateTimeString date
                    }

            Err err ->
                Cmd.none

    else if field == dureeFieldId then
        case String.toFloat str of
            Just float ->
                let
                    selSet =
                        Timeline.Models.selectionToSet model.timelineState.selection
                in
                modifyRecordsDuree
                    { ids = selSet |> Set.toList |> List.filterMap String.toInt
                    , setDuree = float * 3600
                    }

            _ ->
                Cmd.none

    else
        makeFieldUpdate_ model.timelineState.selection field (Encode.string str)


makeFieldUpdate_ : Timeline.Models.Selection -> String -> Value -> Cmd Msg
makeFieldUpdate_ sel field value =
    Encode.object
        [ ( "ids"
          , Timeline.Models.selectionToSet sel
                |> Set.toList
                |> List.filterMap String.toInt
                |> Encode.list Encode.int
          )
        , ( "field", Encode.string field )
        , ( "value", value )
        ]
        |> updateField


fieldsFromSelection : Time.Zone -> Timeline.Models.Selection -> Dict String Record -> Dict String ( Field, FieldState ) -> Dict String ( Field, FieldState )
fieldsFromSelection zone selids allRecords fields =
    let
        selSet =
            Timeline.Models.selectionToSet selids

        records =
            Dict.filter (\k _ -> Set.member k selSet) allRecords
                |> Dict.values

        sel =
            List.map .fields records

        debuts =
            List.map .date records |> ListX.unique

        durees =
            List.map .amplitude records |> ListX.unique

        fins =
            List.map (\rec -> round (rec.amplitude * 1000) + rec.date) records |> ListX.unique

        debutVal =
            case debuts of
                [] ->
                    Error NoValue ""

                [ debut ] ->
                    Time.millisToPosix debut |> Iso8601.toDateTimeString zone |> Val

                _ :: _ ->
                    List.minimum debuts
                        |> Maybe.map (Time.millisToPosix >> Iso8601.toDateTimeString zone >> Val)
                        |> Maybe.withDefault (Error NoValue "")

        mbFinVal =
            case fins of
                [] ->
                    Error NoValue "" |> Just

                [ fin ] ->
                    Time.millisToPosix fin |> Iso8601.toDateTimeString zone |> Val |> Just

                _ ->
                    Nothing

        dureeVal =
            case durees of
                [] ->
                    Error NoValue ""

                [ duree ] ->
                    Val (String.fromFloat (duree / 3600))

                _ ->
                    Multi
    in
    Dict.map
        (\key ( field, value ) ->
            let
                values =
                    List.map (Dict.get key) sel
                        |> ListX.unique
            in
            case values of
                [] ->
                    ( field, Error NoValue "" )

                [ Nothing ] ->
                    ( field, Error NoValue "" )

                [ Just one ] ->
                    ( field, Val one )

                _ ->
                    ( field, Multi )
        )
        fields
        |> Dict.insert debutFieldId ( debutField, debutVal )
        |> (case mbFinVal of
                Just finVal ->
                    Dict.insert finFieldId ( finField, finVal )

                Nothing ->
                    Dict.remove finFieldId
           )
        |> Dict.insert dureeFieldId ( dureeField, dureeVal )


wrapGroupeId : { g | groupeId : String, sousGroupeId : Maybe String } -> String
wrapGroupeId g =
    case g.sousGroupeId of
        Just sId ->
            g.groupeId ++ ":" ++ sId

        Nothing ->
            g.groupeId


unwrapGroupeId : String -> { groupeId : String, sousGroupeId : Maybe String }
unwrapGroupeId id =
    case String.split ":" id of
        [ gid, sgid ] ->
            { groupeId = gid, sousGroupeId = Just sgid }

        [ gid ] ->
            { groupeId = gid, sousGroupeId = Nothing }

        _ ->
            { groupeId = "", sousGroupeId = Nothing }


wrapGroupe : { g | groupe : String, sousGroupe : Maybe String } -> List String
wrapGroupe g =
    case g.sousGroupe of
        Just sId ->
            [ g.groupe, sId ]

        Nothing ->
            [ g.groupe ]


sectionIdsToSelection : List Group -> List Int -> Timeline.Models.Selection
sectionIdsToSelection groups ids =
    List.foldl
        (\id sel ->
            ListX.findMap
                (\group ->
                    ListX.findMap
                        (\sec ->
                            if sec.id == String.fromInt id then
                                Just sec.id

                            else
                                Nothing
                        )
                        group.sections
                        |> Maybe.map (\sid -> Timeline.Models.addToSelection group.id [ sid ] sel)
                )
                groups
                |> Maybe.withDefault sel
        )
        Timeline.Models.emptySelection
        ids


port setRecords : (Value -> msg) -> Sub msg


port setSelection : (Value -> msg) -> Sub msg


port setOptions : (Value -> msg) -> Sub msg


modifyRecordsDelta : { ids : List Int, changeDebut : Int, changeAmplitude : Int } -> Cmd msg
modifyRecordsDelta args =
    Encode.object
        [ ( "ids", Encode.list Encode.int args.ids )
        , ( "changeDebut", Encode.int args.changeDebut )
        , ( "changeAmplitude", Encode.int args.changeAmplitude )
        ]
        |> modifyRecords


modifyRecordsGroup : { ids : List Int, groupeId : String, sousGroupeId : String } -> Cmd msg
modifyRecordsGroup args =
    Encode.object
        [ ( "ids", Encode.list Encode.int args.ids )
        , ( "groupeId", Encode.string args.groupeId )
        , ( "sousGroupeId", Encode.string args.sousGroupeId )
        ]
        |> modifyRecords


modifyRecordsDuree : { ids : List Int, setDuree : Float } -> Cmd msg
modifyRecordsDuree args =
    Encode.object
        [ ( "ids", Encode.list Encode.int args.ids )
        , ( "setDuree", Encode.float args.setDuree )
        ]
        |> modifyRecords


modifyRecordsFin : { ids : List Int, setFin : String } -> Cmd msg
modifyRecordsFin args =
    Encode.object
        [ ( "ids", Encode.list Encode.int args.ids )
        , ( "setFin", Encode.string args.setFin )
        ]
        |> modifyRecords


port modifyRecords : Value -> Cmd msg


port updateField : Value -> Cmd msg


port cloneRecords : { ids : List Int, changeDebut : Int, groupeId : String, sousGroupeId : String } -> Cmd msg


port splitRecords : { ids : List Int, date : String } -> Cmd msg


port deleteRecords : { ids : List Int } -> Cmd msg


port createRecord : { groupeId : String, sousGroupeId : String, date : String, duree : Int } -> Cmd msg


port updateOptions : Value -> Cmd msg


port selectRecords : List Int -> Cmd msg


type alias Record =
    { id : Int
    , date : Int
    , amplitude : Float
    , groupe : String
    , groupeId : String
    , sousGroupe : Maybe String
    , sousGroupeId : Maybe String
    , contenu : List String
    , fields : Dict String String
    , couleur : String
    , comment : Maybe String
    }


debutFieldId =
    "_timeline_Debut"


debutField =
    { id = debutFieldId
    , label = "Début"
    , position = -10
    , ofType = Field.DateTime
    , values = Field.ListInt []
    }


finFieldId =
    "_timeline_Fin"


finField =
    { id = finFieldId
    , label = "Fin"
    , position = -10
    , ofType = Field.DateTime
    , values = Field.ListInt []
    }


dureeFieldId =
    "_timeline_Duree"


dureeField =
    { id = dureeFieldId
    , label = "Durée"
    , position = -5
    , ofType = Field.Float Field.Standard False 0 2
    , values = Field.ListFloat []
    }


defaultChoice =
    { id = Field.ChoiceString "def"
    , label = "def"
    , textColor = "#000"
    , backgroundColor = "#EEE"
    , bold = False
    , italic = False
    , underline = False
    , crossedOut = False
    }


type FieldState
    = Val String
    | Multi
    | Error Error String


type Error
    = NoSelection
    | NoValue


type alias ReceiveData =
    { records : List Record, maybeSelection : Maybe (List Int), editable : List Field }


receiveDecoder : Decoder ReceiveData
receiveDecoder =
    Decode.map3 ReceiveData
        (Decode.field "rows" <| Decode.list recordDecoder)
        (Decode.maybe <| Decode.field "selection" (Decode.list (Decode.field "id" Decode.int)))
        (Decode.field "editable"
            (Decode.list (Field.decoder defaultChoice))
        )


recordDecoder : Decoder Record
recordDecoder =
    Decode.succeed Record
        |> required "id" Decode.int
        |> required "date" (DecodeX.datetime |> Decode.map Time.posixToMillis)
        |> (required "duree" <| Decode.oneOf [ Decode.float |> Decode.map ((*) 3600), Decode.null 25200 ])
        |> required "groupe" anyDecoder
        |> required "groupeId" anyDecoder
        |> optional "sousGroupe" (Decode.string |> Decode.map Just) Nothing
        |> optional "sousGroupeId" (Decode.string |> Decode.map Just) Nothing
        |> optional "contenu" (Decode.list anyDecoder) []
        |> optional "fields" (Decode.dict anyDecoder) Dict.empty
        |> required "couleur" (Decode.oneOf [ Decode.maybe Decode.string, Decode.nullable Decode.string ] |> Decode.map (Maybe.withDefault ""))
        |> optional "commentaire"
            (Decode.string
                |> Decode.map
                    (\s ->
                        if String.isEmpty s then
                            Nothing

                        else
                            Just s
                    )
            )
            Nothing


type alias Options =
    { start : Float
    , zoom : Float
    , sectionOffsetY : Float
    , lineSize : Float
    }


optionsDecoder : Decoder Options
optionsDecoder =
    Decode.succeed Options
        |> required "start" Decode.float
        |> required "zoom" Decode.float
        |> optional "sectionOffsetY" Decode.float 0
        |> optional "lineSize" Decode.float 38


encodeOptions : Options -> Value
encodeOptions options =
    Encode.object
        [ ( "start", Encode.float options.start )
        , ( "zoom", Encode.float options.zoom )
        , ( "sectionOffsetY", Encode.float options.sectionOffsetY )
        , ( "lineSize", Encode.float options.lineSize )
        ]



--


anyDecoder : Decoder String
anyDecoder =
    Decode.oneOf
        [ Decode.string
        , Decode.int |> Decode.map String.fromInt
        , Decode.float |> Decode.map String.fromFloat
        , Decode.bool
            |> Decode.map
                (\b ->
                    if b then
                        "true"

                    else
                        "false"
                )
        , Decode.null ""
        ]


styles =
    """
.field {
    padding-top: 10px;
}

.field label {
    text-transform: uppercase;
    font-size: 10px;
    color: #262626;
}

.field > input {
    display: block;
    margin-top: 5px;
    color: var(--grist-theme-input-fg, black);
    outline: none;
    height: 28px;
    font-size: 13px;
    border: 1px solid var(--grist-theme-input-border, var(--grist-color-dark-grey));
    border-radius: 3px;
    padding: 0 6px;

}

"""


selectStyles : Styles.Config
selectStyles =
    Styles.default
        |> Styles.setControlStyles
            (Styles.getControlConfig Styles.default
                |> Styles.setControlMinHeight 10
                |> Styles.setControlBorderRadius 3
                |> Styles.setControlBorderColor (Css.hex "#DDD")
                |> Styles.setControlBorderColorFocus (Css.rgba 0 0 0 0)
            )



-- makeDefSec : { id : String, start : Float, end : Float, color : String } -> Section
-- makeDefSec { id, start, end, color } =
--     { start = start * duration.day + 1633478400000 |> round |> Time.millisToPosix
--     , end = end * duration.day + 1633478400000 |> round |> Time.millisToPosix
--     , id = id
--     , color = color
--     , isFrozen = False
--     , labels = [ id, "top is the best" ]
--     , comment =
--         if modBy 3 (round start) == 1 then
--             Just "hello"
--         else
--             Nothing
--     }
-- cols : Int
-- cols =
--     35
-- rows : Int
-- rows =
--     10
-- groupsData : List Group
-- groupsData =
--     List.map
--         (\j ->
--             { id = "g" ++ String.fromInt j
--             , label = "group " ++ String.fromInt j
--             , sections =
--                 if j /= 2 then
--                     List.map
--                         (\i ->
--                             makeDefSec
--                                 { id = String.fromInt (j * cols + i)
--                                 , start = toFloat i
--                                 , end =
--                                     if i == 2 && j == 3 then
--                                         toFloat (i + 2)
--                                     else
--                                         toFloat (i + 1)
--                                 , color =
--                                     "rose"
--                                 -- Array.get (modBy 17 i)
--                                 --     (if modBy 2 j == 0 then
--                                 --         colorsSat
--                                 --      else
--                                 --         colorsPale
--                                 --     )
--                                 --
--                                 -- Array.get
--                                 -- (modBy 29 i)
--                                 -- colors
--                                 -- |> Maybe.withDefault ""
--                                 }
--                         )
--                         (List.range 0 (cols - 1))
--                 else
--                     []
--             }
--         )
--         (List.range 0 rows)
