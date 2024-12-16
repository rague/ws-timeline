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
import Http
import I18Next
import Iso8601
import Json.Decode as Decode exposing (Decoder, Value, maybe)
import Json.Decode.Extra as DecodeX
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import List.Extra as ListX
import Markdown
import Moment
import Phosphor exposing (IconWeight(..))
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
import Timeline.Models exposing (Direction(..), Group, Interaction(..), selectionIsEmpty)
import Timeline.Update
import View.Segment as Segment
import Widget.Language exposing (defaultLanguage)
import Widget.Translations as T


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
    | ChangeText View String String
    | ValidateText View String String
    | CancelChange View String
    | SelectMsg View String (Select.Msg Field.ChoiceId)
    | FocusField String
    | CreateNew
    | NoOp
    | CloseError Int
    | AddError String
    | GotHelp (Result Http.Error String)
    | GotTranslations (Result Http.Error I18Next.Translations)
    | ShowModal Modal
    | UpdateDirection Direction
    | UpdateWrap Bool
    | UpdateDurationUnit DurationUnit


type View
    = Inspector
    | NewMoment


type alias Model =
    { timelineState : Timeline.Models.TimelineBox
    , error : List ( Int, String )
    , errorId : Int
    , box :
        { width : Int
        , height : Int
        }
    , bounce : Bounce
    , fields : Dict String ( Field, FieldState )
    , groupsField : Dict String ( Field, FieldState )
    , focus : String
    , options : Options
    , records : Dict String Record
    , selectStates : Dict String Select.State
    , showInspector : Bool
    , language : String
    , translations : List I18Next.Translations
    , help : String
    , showModal : Modal
    , durationUnit : DurationUnit
    }


type Modal
    = None
    | Help
    | Settings
    | New


type DurationUnit
    = Hours
    | Minutes


durationUnitDecoder : Decoder DurationUnit
durationUnitDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Hours" ->
                        Decode.succeed Hours

                    "Minutes" ->
                        Decode.succeed Minutes

                    _ ->
                        Decode.fail "bad duration"
            )


encodeDurationUnit : DurationUnit -> Value
encodeDurationUnit du =
    case du of
        Hours ->
            Encode.string "Hours"

        Minutes ->
            Encode.string "Minutes"


main : Program Value Model Msg
main =
    Browser.document
        { init =
            init
        , update = update
        , view = view
        , subscriptions =
            \model ->
                Sub.batch
                    [ setRecords Receive
                    , setSelection ChangeSelection
                    , setOptions ChangeOptions
                    , setError AddError
                    , Browser.Events.onResize sizeToMsg
                    , Sub.map TimelineMsg (Timeline.subscriptions model.timelineState)
                    , case model.showModal of
                        None ->
                            Sub.none

                        _ ->
                            Browser.Events.onKeyDown
                                (Decode.field "key" Decode.string
                                    |> Decode.andThen
                                        (\key ->
                                            if key == "Escape" then
                                                Decode.succeed (ShowModal None)

                                            else
                                                Decode.fail ""
                                        )
                                )
                    ]
        }


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        lang =
            languageFromFlags flags

        loadManCmd =
            Http.get
                { url = "../public/locales/" ++ String.slice 0 2 lang ++ "/help.md"
                , expect = Http.expectString GotHelp
                }

        loadTransCmd =
            Http.get
                { url = "../public/locales/" ++ String.slice 0 2 lang ++ "/translations.json"
                , expect = Http.expectJson GotTranslations I18Next.translationsDecoder
                }

        du =
            durationUnitFromFlags flags
    in
    ( { timelineState =
            Timeline.init []
                (startDateFromFlags flags)
                |> Timeline.canEditGroups False
                |> Timeline.canSortGroups False
                |> Timeline.setLanguage lang
      , error = []
      , errorId = 0
      , box =
            { width = 1000
            , height = 500
            }
      , bounce = Bounce.init
      , fields = Dict.empty
      , groupsField = Dict.empty
      , options = Options (Time.millisToPosix 0) 0 0 38 Horizontal False du
      , records = Dict.empty
      , selectStates = Dict.empty
      , focus = ""
      , showInspector = False
      , language = lang
      , help = ""
      , showModal = None
      , translations = [ defaultLanguage ]
      , durationUnit = du
      }
      -- , initialSizeCmd
    , Cmd.batch
        [ Task.perform (\size -> sizeToMsg (round size.viewport.width) (round size.viewport.height)) Browser.Dom.getViewport
        , loadManCmd
        , loadTransCmd
        ]
    )


view : Model -> { title : String, body : List (Html.Html Msg) }
view model =
    { title = "WeSchedule"
    , body =
        [ Html.node "style" [] [ Html.text Timeline.styles ]
        , Html.node "style" [] [ Html.text (styles ++ Segment.styles) ]
        , Timeline.view model.timelineState model.box
            |> Html.map TimelineMsg
        , Html.div
            [ HA.class "controls"
            , HA.style "position" "absolute"
            , HA.style "top" "5px"
            ]
            [ Html.button [ Html.Events.onClick (ShowModal Help) ]
                [ Phosphor.questionMark Bold
                    |> Phosphor.withSize 14
                    |> Phosphor.withSizeUnit "px"
                    |> Phosphor.toHtml [ HA.style "vertical-align" "sub" ]
                ]
            , Html.button [ Html.Events.onClick (ShowModal Settings) ]
                [ Phosphor.gearFine Bold
                    |> Phosphor.withSize 14
                    |> Phosphor.withSizeUnit "px"
                    |> Phosphor.toHtml [ HA.style "vertical-align" "sub" ]
                ]
            ]
        , if model.showInspector == False || Timeline.Models.selectionIsEmpty model.timelineState.selection then
            Html.text ""

          else
            inspectorView model model.fields
        , case model.showModal of
            Help ->
                Html.div
                    [ HA.class "modal-background"
                    , Html.Events.onClick (ShowModal None)
                    ]
                    [ Html.div
                        [ HA.class "modal help" ]
                        [ Html.button [ HA.class "close-button", Html.Events.onClick (ShowModal None) ]
                            [ Phosphor.x Bold
                                |> Phosphor.withSize 14
                                |> Phosphor.withSizeUnit "px"
                                |> Phosphor.toHtml [ HA.style "vertical-align" "sub" ]
                            ]
                        , Markdown.toHtml [] model.help
                        ]
                    ]

            Settings ->
                settingsView model

            None ->
                Html.text ""

            New ->
                Html.div
                    [ HA.style "display" "flex"
                    , HA.style "justify-content" "center"
                    ]
                    [ Html.div
                        [ HA.class "modal-background"
                        , Html.Events.onClick (ShowModal None)
                        ]
                        []
                    , Html.div
                        [ HA.class "modal create-new" ]
                        [ Html.button [ HA.class "close-button", Html.Events.onClick (ShowModal None) ]
                            [ Phosphor.x Bold
                                |> Phosphor.withSize 14
                                |> Phosphor.withSizeUnit "px"
                                |> Phosphor.toHtml [ HA.style "vertical-align" "sub" ]
                            ]
                        , Html.h1 [] [ Html.text (T.newMoment model.translations) ]
                        , fieldsView NewMoment model model.groupsField
                        , Html.button
                            [ Html.Events.onClick CreateNew
                            , HA.disabled (validateNewMoment model.timelineState.zone model.durationUnit model.groupsField == Nothing)
                            ]
                            [ Html.text (T.create model.translations) ]
                        ]
                    ]
        , if List.isEmpty model.error then
            Html.text ""

          else
            Html.div [ HA.class "errors" ] <|
                List.map
                    (\( iderr, err ) ->
                        Html.div [ HA.class "error" ]
                            [ Html.text err
                            , Html.button [ Html.Events.onClick (CloseError iderr) ]
                                [ Phosphor.x Regular
                                    |> Phosphor.withSize 14
                                    |> Phosphor.withSizeUnit "px"
                                    |> Phosphor.toHtml [ HA.style "vertical-align" "sub" ]
                                ]
                            ]
                    )
                    model.error
        ]
    }


settingsView : Model -> Html.Html Msg
settingsView model =
    Html.div []
        [ Html.div
            [ HA.class "modal-background"
            , Html.Events.onClick (ShowModal None)
            ]
            []
        , Html.div
            [ HA.class "modal settings" ]
            [ Html.button [ HA.class "close-button", Html.Events.onClick (ShowModal None) ]
                [ Phosphor.x Bold
                    |> Phosphor.withSize 14
                    |> Phosphor.withSizeUnit "px"
                    |> Phosphor.toHtml [ HA.style "vertical-align" "sub" ]
                ]
            , Html.h1 [] [ Html.text (T.settings model.translations) ]
            , Html.label [] [ Html.text (T.timeUnit model.translations) ]
            , Segment.radio UpdateDurationUnit
                model.durationUnit
                [ { value = Hours, label = Html.text (T.hours model.translations) }
                , { value = Minutes, label = Html.text (T.minutes model.translations) }
                ]
            , Html.label [ HA.style "margin-top" "20px" ] [ Html.text (T.timelineDirection model.translations) ]
            , Segment.radio UpdateDirection
                model.timelineState.direction
                [ { value = Horizontal, label = Html.text (T.horizontal model.translations) }
                , { value = Vertical, label = Html.text (T.vertical model.translations) }
                ]
            , Html.label [ HA.style "margin-top" "20px" ]
                [ Html.input
                    [ HA.type_ "checkbox"
                    , HA.checked model.timelineState.wrapText
                    , Html.Events.onCheck UpdateWrap
                    ]
                    []
                , Html.text (T.wrapText model.translations)
                ]
            ]
        ]


languageFromFlags : Value -> String
languageFromFlags flags =
    let
        result =
            Decode.decodeValue (Decode.field "language" Decode.string) flags
    in
    case result of
        Ok language ->
            language

        Err _ ->
            "en"


startDateFromFlags : Value -> Time.Posix
startDateFromFlags flags =
    let
        result =
            Decode.decodeValue (Decode.field "startDate" Decode.int) flags
    in
    case result of
        Ok int ->
            Time.millisToPosix int

        Err _ ->
            Time.millisToPosix 1735686000000


durationUnitFromFlags : Value -> DurationUnit
durationUnitFromFlags flags =
    let
        result =
            Decode.decodeValue (Decode.field "durationUnit" Decode.string) flags
    in
    case result of
        Ok "Hours" ->
            Hours

        Ok "Minutes" ->
            Minutes

        _ ->
            Hours


secondsForDurationUnit : DurationUnit -> Float
secondsForDurationUnit du =
    case du of
        Hours ->
            3600

        Minutes ->
            60


msForDurationUnit : DurationUnit -> Float
msForDurationUnit du =
    case du of
        Hours ->
            3600000

        Minutes ->
            60000


inspectorView : Model -> Dict String ( Field, FieldState ) -> Html.Html Msg
inspectorView model =
    fieldsView Inspector model
        >> List.singleton
        >> Html.div
            [ HA.style "position" "absolute"
            , HA.style "right" "0px"
            , HA.style "top" "0px"
            , HA.style "width" "200px"
            , HA.style "height" ((model.box.height |> String.fromInt) ++ "px")
            , HA.style "background-color" "#f7f7f7"
            , HA.style "font-family" "Arial, Helvetica, sans-serif"
            , HA.style "font-size" "12px"
            , HA.style "overflow-y" "scroll"
            , HA.style "z-index" "2"
            , HA.style "box-shadow" "rgba(0,0,0,0.1) 1px 0px 14px"
            , HA.style "border-left" "1px solid #DDD"
            , HA.style "padding" "10px"
            ]


fieldsView : View -> Model -> Dict String ( Field, FieldState ) -> Html.Html Msg
fieldsView v ({ translations } as model) fields =
    let
        selSize =
            Timeline.Models.selectionSize model.timelineState.selection

        records =
            Timeline.Models.selectionToSet model.timelineState.selection
                |> Set.toList
                |> List.filterMap (\id -> Dict.get id model.records)

        cumul =
            (List.map .amplitude records |> List.sum) / secondsForDurationUnit model.durationUnit

        maybeStart =
            List.map .date records |> List.minimum

        maybeEnd =
            List.map (\r -> r.date + (r.amplitude * 1000 |> round)) records |> List.maximum

        amplitude =
            Maybe.map2 (\start end -> toFloat (end - start) / msForDurationUnit model.durationUnit) maybeStart maybeEnd |> Maybe.withDefault 0

        drawChoices =
            \field choices ->
                case Dict.get field.name model.selectStates of
                    Just selectState ->
                        let
                            selectedItem =
                                Maybe.andThen Field.stringToChoiceId field.str
                                    |> Maybe.map
                                        (\choiceid ->
                                            let
                                                lbl =
                                                    ListX.find (\c -> c.id == choiceid) choices
                                                        |> Maybe.map .label
                                                        |> Maybe.withDefault (Field.choiceIdToString choiceid)
                                            in
                                            Select.basicMenuItem { item = choiceid, label = lbl }
                                        )

                            -- Maybe.andThen (\str -> ListX.find (\c -> c.label == str) choices) field.str
                            -- |> Maybe.map (\c -> Select.basicMenuItem { item = c.id, label = c.label })
                        in
                        Select.view
                            (Select.single selectedItem
                                |> Select.disabled field.field.isFormula
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
                            |> Html.map (SelectMsg v field.name)

                    Nothing ->
                        Html.text ""
    in
    (if v == Inspector then
        [ (if selSize > 1 then
            String.fromInt selSize ++ " " ++ T.momentPlural translations

           else
            String.fromInt selSize ++ " " ++ T.moment translations
          )
            |> Html.text
            |> List.singleton
            |> Html.div []
        , (case model.durationUnit of
            Hours ->
                T.cumulativeDuration translations
                    ++ ((cumul * 10 / 24 |> round |> toFloat) / 10 |> String.fromFloat)
                    ++ " "
                    ++ T.daysShort translations
                    ++ " / "
                    ++ (cumul |> String.fromFloat)
                    ++ " "
                    ++ T.hoursShort translations

            Minutes ->
                T.cumulativeDuration translations
                    ++ ((cumul * 10 / 60 |> round |> toFloat) / 10 |> String.fromFloat)
                    ++ " "
                    ++ T.hoursShort translations
                    ++ " / "
                    ++ (cumul |> String.fromFloat)
                    ++ " "
                    ++ T.minutesShort translations
          )
            |> Html.text
            |> List.singleton
            |> Html.div []
        , (case model.durationUnit of
            Hours ->
                T.timeRange translations
                    ++ String.fromFloat ((amplitude * 10 / 24 |> round |> toFloat) / 10)
                    ++ " "
                    ++ T.daysShort translations
                    ++ " / "
                    ++ String.fromFloat amplitude
                    ++ " "
                    ++ T.hoursShort translations

            Minutes ->
                T.timeRange translations
                    ++ String.fromFloat ((amplitude * 10 / 60 |> round |> toFloat) / 10)
                    ++ " "
                    ++ T.hoursShort translations
                    ++ " / "
                    ++ String.fromFloat amplitude
                    ++ " "
                    ++ T.minutesShort translations
          )
            |> Html.text
            |> List.singleton
            |> Html.div []
        ]

     else
        []
    )
        ++ [ fields
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
                |> List.indexedMap
                    (\index field ->
                        Html.div [ HA.class "field" ]
                            [ Html.label [ HA.for field.name ]
                                [ Html.text field.label
                                , if field.field.id == dureeFieldId then
                                    Html.span [ HA.class "unit" ]
                                        [ Html.text <|
                                            " ("
                                                ++ (case model.durationUnit of
                                                        Hours ->
                                                            T.hoursShort translations

                                                        Minutes ->
                                                            T.minutesShort translations
                                                   )
                                                ++ ")"
                                        ]

                                  else
                                    Html.text ""
                                ]
                            , case field.field.ofType of
                                Field.Choice choices ->
                                    drawChoices field choices

                                Field.Ref choices ->
                                    drawChoices field choices

                                Field.Text True ->
                                    Html.textarea
                                        [ HA.name field.name
                                        , HA.id field.name
                                        , HA.disabled field.field.isFormula
                                        , HA.rows 4
                                        , HA.cols 18
                                        , HA.placeholder
                                            (if field.multi then
                                                "<multiple>"

                                             else
                                                ""
                                            )
                                        , Html.Events.onInput (ChangeText v field.name)
                                        , Html.Events.on "keyup" <|
                                            Decode.andThen
                                                (\key ->
                                                    case ( key, field.str ) of
                                                        ( 27, _ ) ->
                                                            Decode.succeed (CancelChange v field.name)

                                                        _ ->
                                                            Decode.fail ""
                                                )
                                                Html.Events.keyCode
                                        , Html.Events.onFocus (FocusField field.name)
                                        , if field.name == model.focus then
                                            Html.Events.onBlur
                                                (case field.str of
                                                    Just str ->
                                                        ValidateText v field.name str

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
                                        , HA.disabled field.field.isFormula
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
                                                (ValidateText v
                                                    field.name
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
                                            Html.Events.onInput (ChangeText v field.name)
                                        , Html.Events.on "keyup" <|
                                            Decode.andThen
                                                (\key ->
                                                    case ( key, field.str ) of
                                                        ( 27, _ ) ->
                                                            Decode.succeed (CancelChange v field.name)

                                                        ( 13, Just str ) ->
                                                            Decode.succeed (ValidateText v field.name str)

                                                        _ ->
                                                            Decode.fail ""
                                                )
                                                Html.Events.keyCode
                                        , Html.Events.onFocus (FocusField field.name)
                                        , if field.name == model.focus then
                                            Html.Events.onBlur
                                                (case field.str of
                                                    Just str ->
                                                        ValidateText v field.name str

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
        |> Html.div []


sizeToMsg : Int -> Int -> Msg
sizeToMsg w h =
    WindowResize { width = w, height = h }


addError : String -> Model -> Model
addError err model =
    if ListX.find (\( _, e ) -> e == err) model.error == Nothing then
        { model
            | error = ( model.errorId, err ) :: List.take 4 model.error
            , errorId = model.errorId + 1
        }

    else
        model


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
                    ( addError (Decode.errorToString err) model, Cmd.none )

                Ok ids ->
                    let
                        groups =
                            model.timelineState.srcgroups

                        selection =
                            sectionIdsToSelection groups ids
                    in
                    ( { model | timelineState = Timeline.Update.updateSelection selection model.timelineState }
                    , Dict.keys model.fields
                        |> List.head
                        |> Maybe.map (\id -> Task.attempt (\_ -> NoOp) (Browser.Dom.focus id))
                        |> Maybe.withDefault Cmd.none
                    )

        ChangeOptions data ->
            case Decode.decodeValue optionsDecoder data of
                Err err ->
                    ( addError (Decode.errorToString err) model, Cmd.none )

                Ok options ->
                    ( { model
                        | timelineState =
                            Timeline.changeStartAndZoom options.start options.zoom model.timelineState
                                |> Timeline.changeYOffset options.sectionOffsetY
                                |> Timeline.changeLineSize
                                    (if options.direction == Vertical then
                                        options.lineSize * 2

                                     else
                                        options.lineSize
                                    )
                                |> Timeline.changeDirection options.direction
                                |> Timeline.setWrapText options.wrapText
                        , options = options
                        , durationUnit = options.durationUnit
                      }
                    , Cmd.none
                    )

        WindowResize b ->
            ( { model | box = b }, Cmd.none )

        OptionsBounceMsg ->
            ( model, encodeOptions model.options |> updateOptions )

        ChangeText v field str ->
            ( case v of
                Inspector ->
                    { model | fields = Dict.update field (Maybe.map (\( f, _ ) -> ( f, Val str ))) model.fields }

                NewMoment ->
                    { model | groupsField = Dict.update field (Maybe.map (\( f, _ ) -> ( f, Val str ))) model.groupsField }
            , Cmd.none
            )

        ValidateText v field str ->
            case v of
                Inspector ->
                    ( { model
                        | focus =
                            if model.focus == field then
                                ""

                            else
                                model.focus
                        , fields = Dict.update field (Maybe.map (\( f, _ ) -> ( f, Val str ))) model.fields
                      }
                    , makeFieldUpdate model field str
                    )

                NewMoment ->
                    ( { model
                        | focus =
                            if model.focus == field then
                                ""

                            else
                                model.focus
                        , groupsField = updateGroupsField model.timelineState.zone model.translations model.durationUnit field str model.groupsField
                      }
                    , Cmd.none
                    )

        CreateNew ->
            case validateNewMoment model.timelineState.zone model.durationUnit model.groupsField of
                Just cmd ->
                    ( { model | showModal = None }, cmd )

                Nothing ->
                    ( model, Cmd.none )

        CancelChange v field ->
            case v of
                Inspector ->
                    let
                        upd =
                            Dict.update field
                                (Maybe.andThen
                                    (\val ->
                                        Dict.singleton field val
                                            |> fieldsFromSelection model.timelineState.zone model.translations model.durationUnit model.timelineState.selection model.records
                                            |> Dict.get field
                                    )
                                )
                                model.fields
                    in
                    ( { model | fields = upd, focus = "" }, Browser.Dom.blur field |> Task.attempt (always NoOp) )

                NewMoment ->
                    ( { model | focus = "" }, Browser.Dom.blur field |> Task.attempt (always NoOp) )

        NoOp ->
            ( model, Cmd.none )

        SelectMsg v field selectMsg ->
            let
                fields =
                    case v of
                        Inspector ->
                            model.fields

                        NewMoment ->
                            model.groupsField
            in
            case Dict.get field model.selectStates of
                Just selectState ->
                    let
                        ( maybeAction, updatedSelectState, selectCmds ) =
                            Select.update selectMsg selectState

                        ( newDict, updMsg ) =
                            case maybeAction of
                                Just (Select.Select item) ->
                                    ( Dict.update field (Maybe.map <| \( f, _ ) -> ( f, Val (Field.choiceIdToString item) )) fields
                                      -- Dict.get field fields
                                      -- |> Maybe.andThen
                                      --     (\( f, _ ) ->
                                      --         case f.ofType of
                                      --             Field.Choice choices ->
                                      --                 ListX.find (\c -> c.id == item) choices
                                      --             Field.Ref choices ->
                                      --                 ListX.find (\c -> c.id == item) choices
                                      --             _ ->
                                      --                 Nothing
                                      --     )
                                      -- |> Maybe.map
                                      --     (\choice ->
                                      --         Dict.update field (Maybe.map <| \( f, _ ) -> ( f, Val choice.label )) fields
                                      --     )
                                      -- |> Maybe.withDefault fields
                                    , makeFieldUpdate_ model.timelineState.selection field (Field.encodeChoiceId item)
                                    )

                                Just Select.Clear ->
                                    case v of
                                        Inspector ->
                                            ( fields
                                            , makeFieldUpdate_ model.timelineState.selection field (Encode.string "")
                                            )

                                        NewMoment ->
                                            ( Dict.update field (Maybe.map <| \( f, _ ) -> ( f, Error NoValue "" )) fields
                                            , Cmd.none
                                            )

                                _ ->
                                    ( fields, Cmd.none )
                    in
                    case v of
                        Inspector ->
                            ( { model
                                | selectStates = Dict.insert field updatedSelectState model.selectStates
                                , fields = newDict
                              }
                            , Cmd.batch [ updMsg, Cmd.map (SelectMsg v field) selectCmds ]
                            )

                        NewMoment ->
                            let
                                _ =
                                    Dict.get groupFieldId newDict |> Maybe.map Tuple.second
                            in
                            ( { model
                                | selectStates = Dict.insert field updatedSelectState model.selectStates
                                , groupsField = newDict
                              }
                            , Cmd.map (SelectMsg v field) selectCmds
                            )

                Nothing ->
                    ( model, Cmd.none )

        FocusField field ->
            ( { model | focus = field }, Cmd.none )

        CloseError id ->
            ( { model | error = List.filter (\( iderr, _ ) -> iderr /= id) model.error }, Cmd.none )

        AddError str ->
            ( addError str model, Cmd.none )

        GotHelp res ->
            case res of
                Ok str ->
                    ( { model | help = str }, Cmd.none )

                Err _ ->
                    ( addError "Can't load help file : Http error" model, Cmd.none )

        GotTranslations res ->
            case res of
                Ok trans ->
                    ( { model | translations = trans :: model.translations }, Cmd.none )

                Err _ ->
                    ( addError "Can't load translation file : Http error" model, Cmd.none )

        ShowModal modal ->
            ( { model | showModal = modal }, Cmd.none )

        UpdateDirection dir ->
            let
                options =
                    model.options

                size =
                    case dir of
                        Horizontal ->
                            model.options.lineSize

                        Vertical ->
                            model.options.lineSize * 2
            in
            ( { model
                | timelineState =
                    Timeline.changeDirection dir model.timelineState
                        |> Timeline.changeLineSize size
                , options = { options | direction = dir }
              }
            , Bounce.delay 500 OptionsBounceMsg
            )

        UpdateWrap bool ->
            let
                options =
                    model.options
            in
            ( { model
                | timelineState =
                    Timeline.setWrapText bool model.timelineState
                , options = { options | wrapText = bool }
              }
            , Bounce.delay 500 OptionsBounceMsg
            )

        UpdateDurationUnit du ->
            let
                options =
                    model.options
            in
            ( { model
                | durationUnit = du
                , options = { options | durationUnit = du }
                , fields = fieldsFromSelection model.timelineState.zone model.translations du model.timelineState.selection model.records model.fields
              }
            , Bounce.delay 500 OptionsBounceMsg
            )


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

                Timeline.Action.CreateSection maybe from to ->
                    case maybe of
                        Just gid ->
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

                        Nothing ->
                            Cmd.none

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

        ( modal, groupsField ) =
            case action of
                Timeline.Action.CreateSection Nothing start end ->
                    ( New, fieldsFromDates model.timelineState.zone model.translations model.durationUnit start end model.groupsField )

                _ ->
                    ( model.showModal, model.groupsField )
    in
    ( { model
        | timelineState =
            state
                |> Timeline.applyAction action
        , options =
            case action of
                Timeline.Action.ChangeZoom { start, zoom, sectionOffsetY, lineSize } ->
                    Options start
                        zoom
                        sectionOffsetY
                        (if state.direction == Vertical then
                            lineSize / 2

                         else
                            lineSize
                        )
                        model.timelineState.direction
                        model.timelineState.wrapText
                        model.durationUnit

                _ ->
                    model.options
        , fields =
            case action of
                Timeline.Action.SelectSections sel ->
                    if sel == model.timelineState.selection then
                        model.fields

                    else
                        fieldsFromSelection model.timelineState.zone model.translations model.durationUnit sel model.records model.fields

                _ ->
                    model.fields
        , groupsField = groupsField
        , showModal = modal
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
            ( addError (Decode.errorToString err) model, Cmd.none )

        Ok { records, maybeSelection, editable, group, subgroup } ->
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
                                                , isLocked = rec.isLocked
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

                fields =
                    List.indexedMap (\pos field -> ( field.id, ( { field | position = pos }, Error NoSelection "" ) )) editable
                        |> Dict.fromList
                        |> fieldsFromSelection model.timelineState.zone model.translations model.durationUnit newtl.selection recs

                cmd =
                    if maybeSelection /= Nothing then
                        Dict.toList fields
                            |> List.sortBy (\( _, ( f, _ ) ) -> f.position)
                            |> List.map Tuple.first
                            |> ListX.getAt 3
                            |> Maybe.map (\id -> textSelectAndFocus id)
                            |> Maybe.withDefault Cmd.none

                    else
                        Cmd.none

                recs =
                    List.map (\r -> ( String.fromInt r.id, r )) records |> Dict.fromList

                groupsFiltered =
                    List.filterMap identity
                        [ Maybe.map (\g -> { g | id = groupFieldId }) group
                        , Maybe.map (\s -> { s | id = subgroupFieldId }) subgroup
                        ]
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
                    fields
                , groupsField =
                    groupsFiltered
                        |> List.indexedMap (\pos field -> ( field.id, ( { field | position = pos }, Error NoSelection "" ) ))
                        |> Dict.fromList
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
                        (groupsFiltered ++ editable)
                        |> Dict.fromList
                , showInspector = maybeSelection /= Nothing
              }
            , cmd
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

            Err _ ->
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

            Err _ ->
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
                    , setDuree = float * secondsForDurationUnit model.durationUnit
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


fieldsFromDates : Time.Zone -> List I18Next.Translations -> DurationUnit -> Time.Posix -> Time.Posix -> Dict String ( Field, FieldState ) -> Dict String ( Field, FieldState )
fieldsFromDates zone trans durationUnit debut fin fields =
    let
        debutVal =
            debut |> Iso8601.toDateTimeString zone |> Val

        finVal =
            fin |> Iso8601.toDateTimeString zone |> Val

        dureeVal =
            Val (String.fromFloat (toFloat (Moment.durationBetween debut fin |> Moment.fromDuration) / msForDurationUnit durationUnit))
    in
    fields
        |> Dict.insert debutFieldId ( debutField trans, debutVal )
        |> Dict.insert finFieldId ( finField trans, finVal )
        |> Dict.insert dureeFieldId ( dureeField trans, dureeVal )


fieldStateToMaybe fs =
    case fs of
        Val str ->
            Just str

        _ ->
            Nothing


updateGroupsField : Time.Zone -> List I18Next.Translations -> DurationUnit -> String -> String -> Dict String ( Field, FieldState ) -> Dict String ( Field, FieldState )
updateGroupsField zone trans durationUnit field str fields =
    let
        resdebut =
            Dict.get debutFieldId fields
                |> Maybe.andThen (Tuple.second >> fieldStateToMaybe)
                |> Maybe.andThen (\s -> Decode.decodeString DecodeX.datetime ("\"" ++ s ++ "\"") |> Result.toMaybe)

        resfin =
            Dict.get finFieldId fields
                |> Maybe.andThen (Tuple.second >> fieldStateToMaybe)
                |> Maybe.andThen (\s -> Decode.decodeString DecodeX.datetime ("\"" ++ s ++ "\"") |> Result.toMaybe)
    in
    if field == debutFieldId then
        case ( Decode.decodeString DecodeX.datetime ("\"" ++ str ++ "\""), resfin ) of
            ( Ok debut, Just fin ) ->
                let
                    dureeVal =
                        Val (String.fromFloat (toFloat (Moment.durationBetween debut fin |> Moment.fromDuration) / msForDurationUnit durationUnit))
                in
                Dict.insert dureeFieldId ( dureeField trans, dureeVal ) fields

            _ ->
                fields

    else if field == finFieldId then
        case ( resdebut, Decode.decodeString DecodeX.datetime ("\"" ++ str ++ "\"") ) of
            ( Just debut, Ok fin ) ->
                let
                    dureeVal =
                        Val (String.fromFloat (toFloat (Moment.durationBetween debut fin |> Moment.fromDuration) / msForDurationUnit durationUnit))
                in
                Dict.insert dureeFieldId ( dureeField trans, dureeVal ) fields

            _ ->
                fields

    else if field == dureeFieldId then
        case ( resdebut, String.toFloat str ) of
            ( Just debut, Just duree ) ->
                let
                    finVal =
                        Time.posixToMillis debut + (duree * msForDurationUnit durationUnit |> round) |> Time.millisToPosix |> Iso8601.toDateTimeString zone |> Val
                in
                Dict.insert finFieldId ( finField trans, finVal ) fields

            _ ->
                fields

    else
        fields


validateNewMoment : Time.Zone -> DurationUnit -> Dict String ( Field, FieldState ) -> Maybe (Cmd Msg)
validateNewMoment zone durationUnit fields =
    let
        mbdebut =
            Dict.get debutFieldId fields
                |> Maybe.andThen (Tuple.second >> fieldStateToMaybe)
                |> Maybe.andThen (\s -> Decode.decodeString DecodeX.datetime ("\"" ++ s ++ "\"") |> Result.toMaybe)

        mbduree =
            Dict.get dureeFieldId fields
                |> Maybe.andThen (Tuple.second >> fieldStateToMaybe)
                |> Maybe.andThen String.toFloat

        mbgroup =
            Dict.get groupFieldId fields
                |> Maybe.andThen (Tuple.second >> fieldStateToMaybe)
                |> Maybe.andThen Field.stringToChoiceId
                |> Maybe.map Field.choiceIdToRawString

        mbsubgroup =
            Dict.get subgroupFieldId fields
                |> Maybe.andThen (Tuple.second >> fieldStateToMaybe)
                |> Maybe.andThen Field.stringToChoiceId
                |> Maybe.map Field.choiceIdToRawString
    in
    case ( mbgroup, mbdebut, mbduree ) of
        ( Just group, Just debut, Just duree ) ->
            let
                args =
                    { groupeId = group
                    , sousGroupeId = mbsubgroup |> Maybe.withDefault ""

                    -- , date = Iso8601.toDateTimeString model.timelineState.zone from
                    , date = Iso8601.toDateTimeString zone debut
                    , duree = duree * secondsForDurationUnit durationUnit |> round
                    }
            in
            Just (createRecord args)

        -- Just Cmd.none
        _ ->
            Nothing


fieldsFromSelection : Time.Zone -> List I18Next.Translations -> DurationUnit -> Timeline.Models.Selection -> Dict String Record -> Dict String ( Field, FieldState ) -> Dict String ( Field, FieldState )
fieldsFromSelection zone trans durationUnit selids allRecords fields =
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
                    Val (String.fromFloat (duree / secondsForDurationUnit durationUnit))

                _ ->
                    Multi
    in
    Dict.map
        (\key ( field, _ ) ->
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
        |> Dict.insert debutFieldId ( debutField trans, debutVal )
        |> (case mbFinVal of
                Just finVal ->
                    Dict.insert finFieldId ( finField trans, finVal )

                Nothing ->
                    Dict.remove finFieldId
           )
        |> Dict.insert dureeFieldId ( dureeField trans, dureeVal )


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


port textSelectAndFocus : String -> Cmd msg


port setRecords : (Value -> msg) -> Sub msg


port setSelection : (Value -> msg) -> Sub msg


port setOptions : (Value -> msg) -> Sub msg


port setError : (String -> msg) -> Sub msg


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
    , isLocked : Bool
    , comment : Maybe String
    }


groupFieldId =
    "_timeline_Group"


subgroupFieldId =
    "_timeline_Subgroup"


debutFieldId =
    "_timeline_Debut"


debutField trans =
    { id = debutFieldId
    , label = T.startDate trans
    , position = -10
    , ofType = Field.DateTime
    , values = Field.ListInt []
    , isFormula = False
    }


finFieldId =
    "_timeline_Fin"


finField trans =
    { id = finFieldId
    , label = T.endDate trans
    , position = -10
    , ofType = Field.DateTime
    , values = Field.ListInt []
    , isFormula = False
    }


dureeFieldId =
    "_timeline_Duree"


dureeField trans =
    { id = dureeFieldId
    , label = T.duration trans
    , position = -5
    , ofType = Field.Float Field.Standard False 0 2
    , values = Field.ListFloat []
    , isFormula = False
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
    { records : List Record, maybeSelection : Maybe (List Int), editable : List Field, group : Maybe Field, subgroup : Maybe Field }


receiveDecoder : Decoder ReceiveData
receiveDecoder =
    Decode.map5 ReceiveData
        (Decode.field "rows" <| Decode.list recordDecoder)
        (Decode.maybe <| Decode.field "selection" (Decode.list (Decode.field "id" Decode.int)))
        (Decode.field "editable"
            (Decode.list (Field.decoder defaultChoice))
        )
        (Decode.field "group" (Decode.maybe (Field.decoder defaultChoice)))
        (Decode.field "subgroup" (Decode.maybe (Field.decoder defaultChoice)))


recordDecoder : Decoder Record
recordDecoder =
    Decode.succeed Record
        |> required "id" Decode.int
        |> required "date" (DecodeX.datetime |> Decode.map Time.posixToMillis)
        |> (required "duree" <|
                Decode.oneOf
                    [ Decode.float

                    {- |> Decode.map ((*) 3600) -}
                    , Decode.null 25200
                    ]
           )
        |> required "groupe" anyDecoder
        |> required "groupeId" anyDecoder
        |> optional "sousGroupe" (Decode.string |> Decode.map Just) Nothing
        |> optional "sousGroupeId" (Decode.string |> Decode.map Just) Nothing
        |> optional "contenu" (Decode.list anyDecoder) []
        |> optional "fields" (Decode.dict anyDecoder) Dict.empty
        |> required "couleur" (Decode.oneOf [ Decode.maybe Decode.string, Decode.nullable Decode.string ] |> Decode.map (Maybe.withDefault ""))
        |> optional "isLocked" Decode.bool False
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
    { start : Time.Posix
    , zoom : Float
    , sectionOffsetY : Float
    , lineSize : Float
    , direction : Direction
    , wrapText : Bool
    , durationUnit : DurationUnit
    }


optionsDecoder : Decoder Options
optionsDecoder =
    Decode.succeed Options
        |> required "start" (Decode.int |> Decode.map Time.millisToPosix)
        |> required "zoom" Decode.float
        |> optional "sectionOffsetY" Decode.float 0
        |> optional "lineSize" Decode.float 38
        |> optional "direction" directionDecoder Horizontal
        |> optional "wrapText" Decode.bool False
        |> optional "durationUnit" durationUnitDecoder Hours


encodeOptions : Options -> Value
encodeOptions options =
    Encode.object
        [ ( "start", Encode.int (Time.posixToMillis options.start) )
        , ( "zoom", Encode.float options.zoom )
        , ( "sectionOffsetY", Encode.float options.sectionOffsetY )
        , ( "lineSize", Encode.float options.lineSize )
        , ( "direction", encodeDirection options.direction )
        , ( "wrapText", Encode.bool options.wrapText )
        , ( "durationUnit", encodeDurationUnit options.durationUnit )
        ]


directionDecoder : Decoder Direction
directionDecoder =
    Decode.string
        |> Decode.map
            (\dir ->
                case dir of
                    "horizontal" ->
                        Horizontal

                    "vertical" ->
                        Vertical

                    _ ->
                        Horizontal
            )


encodeDirection : Direction -> Value
encodeDirection dir =
    case dir of
        Horizontal ->
            Encode.string "horizontal"

        Vertical ->
            Encode.string "vertical"



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

body {
    margin: 0;
    padding: 0;
    background-color: white;
    overflow: hidden;
    color: black;
}

input[type="checkbox"] {
    accent-color: steelblue;
}

.field {
    padding-top: 10px;
}

.field label {
    text-transform: uppercase;
    font-size: 10px;
    color: #262626;
    display: block;

    .unit {
        text-transform: none;
    }
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

.modal-background {
    position: absolute;
    left: 0;
    top: 0;
    width: 100%;
    height: 100%;
    background-color: rgba(0,0,0,.2);
    
}

.modal {
    position: absolute;
    top: 20px;
    background-color: white;
    padding: 10px 30px 30px 30px;
    max-width: 80%;
    max-height: 90%;
    font-family: sans-serif;
    font-size: 14px;
    box-shadow: 0px 10px 24px 0px rgba(0,0,0,0.34);

    h1 {
        font-size: 20px;
    }

    h2 {
        font-size: 16px;
    }

    label {
        text-transform: uppercase;
        font-size: 10px;
        color: #262626;
        display: block;
    }
}

.help {
    left: 40px;
    
}

.help div {
    height: 100%;
    overflow: auto;
}

.settings {
    padding: 10px 30px 20px 30px;
    left: 68px;
}

.create-new {
    > button {
        margin-top: 15px;
    }
}

.errors {
    position: absolute;
    bottom: 0;
    right: 0;
    
}

.error {
    background-color: #F00;
    padding: 8px 4px 8px 16px;
    margin: 5px;
    border: 1px solid #F00;
    font-family: sans-serif;
    font-size: 13px;
    color: white;
}

.error button {
    margin-left: 6px;
    border: none;
    font-size: 10px;
    background: none;
    color: white;
}

.controls  button {
    margin-left: 6px;
    border: none;
    font-size: 10px;
    background-color: #DDD;
    border-radius: 50px;
    padding: 5px;
    cursor: pointer;

    &:hover {
        background-color: #B4CCE1;
    }
}

.close-button {
    display: block;
    position: absolute;
    left: 0;
    top: 3px;
    width: 16px;
    height: 16px;
    cursor: pointer;
    border: none;
    background: none;
    
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
