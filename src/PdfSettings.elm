port module PdfSettings exposing (main)

-- import TimeZone

import Browser
import Css
import Dict exposing (Dict)
import DnDList
import Field exposing (Field)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Styled as Styled
import I18Next
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DecodeX
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeX
import List.Extra as ListX
import PdfSettings.Language exposing (defaultLanguage)
import PdfSettings.Translations as T
import Phosphor exposing (IconWeight(..))
import Platform.Cmd as Cmd
import Select
import Select.Styles as Styles
import Set exposing (Set)
import Time exposing (Posix)
import Timeline.Models exposing (Direction(..))
import View.Segment as Segment


type alias Settings =
    { fromDate : Maybe Posix
    , toDate : Maybe Posix
    , table : Maybe String
    , sort : List Sort
    , fromField : Maybe String
    , toField : Maybe String
    , group : Maybe String
    , subGroup : Maybe String
    , color : Maybe String
    , content : List String
    , layout : Layout
    , papersize : PaperSize
    , vpages : Int
    , hpages : Int
    , groupsFontSize : Float
    , tasksFontSize : Float
    , hoursFontSize : Float
    , orientation : Orientation
    , align : Align
    , multiline : Bool
    , filters : List ( String, Filter )
    }


initialSettings : Settings
initialSettings =
    { fromDate = Nothing
    , toDate = Nothing
    , table = Nothing
    , sort = []
    , fromField = Nothing
    , toField = Nothing
    , group = Nothing
    , subGroup = Nothing
    , color = Nothing
    , content = []
    , layout = Portrait
    , papersize = A4
    , vpages = 1
    , hpages = 1
    , groupsFontSize = 12.0
    , tasksFontSize = 12.0
    , hoursFontSize = 12.0
    , orientation = Horizontal
    , align = Left
    , multiline = False
    , filters = []
    }


type alias Filter =
    { exclude : Bool, values : ValueSet }


type ValueSet
    = SetInt (Set Int)
    | SetFloat (Set Float)
    | SetString (Set String)


clearValueSet vs =
    case vs of
        SetInt _ ->
            SetInt Set.empty

        SetFloat _ ->
            SetFloat Set.empty

        SetString _ ->
            SetString Set.empty


type alias Sort =
    { field : String, direction : Direction }


type Direction
    = Ascending
    | Descending


directionDecoder : Decoder Direction
directionDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Ascending" ->
                        Decode.succeed Ascending

                    "Descending" ->
                        Decode.succeed Descending

                    _ ->
                        Decode.fail "Unknown direction"
            )


encodeDirection : Direction -> Value
encodeDirection dir =
    case dir of
        Ascending ->
            Encode.string "Ascending"

        Descending ->
            Encode.string "Descending"


sortDecoder : Decoder Sort
sortDecoder =
    Decode.map2 Sort
        (Decode.field "field" Decode.string)
        (Decode.field "direction" directionDecoder)


encodeSort : Sort -> Value
encodeSort sort =
    Encode.object
        [ ( "field", Encode.string sort.field )
        , ( "direction", encodeDirection sort.direction )
        ]


filterDecoder : Decoder Filter
filterDecoder =
    Decode.map2 Filter
        (Decode.field "exclude" Decode.bool)
        (Decode.field "type" Decode.string
            |> Decode.andThen
                (\tpstr ->
                    Decode.field "values" <|
                        case tpstr of
                            "Int" ->
                                Decode.list Decode.int |> Decode.map (\l -> Set.fromList l |> SetInt)

                            "Float" ->
                                Decode.list Decode.float |> Decode.map (\l -> Set.fromList l |> SetFloat)

                            _ ->
                                Decode.list Decode.string |> Decode.map (\l -> Set.fromList l |> SetString)
                )
        )


encodeFilter : Filter -> Encode.Value
encodeFilter filter =
    Encode.object
        [ ( "exclude", Encode.bool filter.exclude )
        , ( "values", encodeValues filter.values )
        , ( "type"
          , case filter.values of
                SetFloat _ ->
                    Encode.string "Float"

                SetInt _ ->
                    Encode.string "Int"

                SetString _ ->
                    Encode.string "String"
          )
        ]


encodeValues values =
    case values of
        SetInt set ->
            Encode.set Encode.int set

        SetFloat set ->
            Encode.set Encode.float set

        SetString set ->
            Encode.set Encode.string set


type PaperSize
    = A0
    | A1
    | A2
    | A3
    | A4
    | A5


stringToPaperSize : String -> Maybe PaperSize
stringToPaperSize string =
    case string of
        "A0" ->
            Just A0

        "A1" ->
            Just A1

        "A2" ->
            Just A2

        "A3" ->
            Just A3

        "A4" ->
            Just A4

        "A5" ->
            Just A5

        _ ->
            Nothing


paperSizeToString : PaperSize -> String
paperSizeToString paperSize =
    case paperSize of
        A0 ->
            "A0"

        A1 ->
            "A1"

        A2 ->
            "A2"

        A3 ->
            "A3"

        A4 ->
            "A4"

        A5 ->
            "A5"


paperSizeDecoder : Decoder PaperSize
paperSizeDecoder =
    Decode.string
        |> Decode.andThen
            (stringToPaperSize
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Unknown paper size")
            )


encodePaperSize : PaperSize -> Value
encodePaperSize =
    paperSizeToString >> Encode.string


type Layout
    = Portrait
    | Landscape


stringToLayout : String -> Maybe Layout
stringToLayout string =
    case string of
        "Portrait" ->
            Just Portrait

        "Landscape" ->
            Just Landscape

        _ ->
            Nothing


layoutToString : Layout -> String
layoutToString layout =
    case layout of
        Portrait ->
            "Portrait"

        Landscape ->
            "Landscape"


layoutDecoder : Decoder Layout
layoutDecoder =
    Decode.string
        |> Decode.andThen
            (stringToLayout
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Unknown layout")
            )


encodeLayout : Layout -> Value
encodeLayout =
    layoutToString >> Encode.string


type Orientation
    = Horizontal
    | Vertical


stringToOrientation : String -> Maybe Orientation
stringToOrientation string =
    case string of
        "Horizontal" ->
            Just Horizontal

        "Vertical" ->
            Just Vertical

        _ ->
            Nothing


orientationToString : Orientation -> String
orientationToString orientation =
    case orientation of
        Horizontal ->
            "Horizontal"

        Vertical ->
            "Vertical"


orientationDecoder : Decoder Orientation
orientationDecoder =
    Decode.string
        |> Decode.andThen
            (stringToOrientation
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Unknown orientation")
            )


encodeOrientation : Orientation -> Value
encodeOrientation =
    orientationToString >> Encode.string


type Align
    = Left
    | Middle
    | Right


stringToAlign : String -> Maybe Align
stringToAlign str =
    case str of
        "Left" ->
            Just Left

        "Middle" ->
            Just Middle

        "Right" ->
            Just Right

        _ ->
            Nothing


alignToString : Align -> String
alignToString align =
    case align of
        Left ->
            "Left"

        Middle ->
            "Middle"

        Right ->
            "Right"


alignDecoder : Decoder Align
alignDecoder =
    Decode.string
        |> Decode.andThen
            (stringToAlign
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Unknown align")
            )


encodeAlign : Align -> Value
encodeAlign =
    alignToString >> Encode.string


decodeSettings =
    Decode.succeed Settings
        |> optional "fromDate" (DecodeX.datetime |> Decode.maybe) Nothing
        |> optional "toDate" (DecodeX.datetime |> Decode.maybe) Nothing
        |> optional "table" (Decode.maybe Decode.string) Nothing
        |> optional "sort" (Decode.list sortDecoder) []
        |> optional "fromField" (Decode.maybe Decode.string) Nothing
        |> optional "toField" (Decode.maybe Decode.string) Nothing
        |> optional "group" (Decode.maybe Decode.string) Nothing
        |> optional "subGroup" (Decode.maybe Decode.string) Nothing
        |> optional "color" (Decode.maybe Decode.string) Nothing
        |> optional "content" (Decode.list Decode.string) []
        |> optional "layout" layoutDecoder Portrait
        |> optional "papersize" paperSizeDecoder A4
        |> optional "vpages" Decode.int 1
        |> optional "hpages" Decode.int 1
        |> optional "groupsFontSize" Decode.float 12
        |> optional "tasksFontSize" Decode.float 10
        |> optional "hoursFontSize" Decode.float 8
        |> optional "orientation" orientationDecoder Horizontal
        |> optional "align" alignDecoder Left
        |> optional "multiline" Decode.bool False
        |> optional "filters" (Decode.keyValuePairs filterDecoder) []


encodeSettings : Time.Zone -> Settings -> Encode.Value
encodeSettings zone settings =
    Encode.object
        [ ( "fromDate", EncodeX.maybe Encode.string (Maybe.map (Iso8601.toDateTimeString zone) settings.fromDate) )
        , ( "toDate", EncodeX.maybe Encode.string (Maybe.map (Iso8601.toDateTimeString zone) settings.toDate) )
        , ( "table", EncodeX.maybe Encode.string settings.table )
        , ( "sort", Encode.list encodeSort settings.sort )
        , ( "fromField", EncodeX.maybe Encode.string settings.fromField )
        , ( "toField", EncodeX.maybe Encode.string settings.toField )
        , ( "group", EncodeX.maybe Encode.string settings.group )
        , ( "subGroup", EncodeX.maybe Encode.string settings.subGroup )
        , ( "color", EncodeX.maybe Encode.string settings.color )
        , ( "content", Encode.list Encode.string settings.content )
        , ( "layout", encodeLayout settings.layout )
        , ( "papersize", encodePaperSize settings.papersize )
        , ( "vpages", Encode.int settings.vpages )
        , ( "hpages", Encode.int settings.hpages )
        , ( "groupsFontSize", Encode.float settings.groupsFontSize )
        , ( "tasksFontSize", Encode.float settings.tasksFontSize )
        , ( "hoursFontSize", Encode.float settings.hoursFontSize )
        , ( "orientation", encodeOrientation settings.orientation )
        , ( "align", encodeAlign settings.align )
        , ( "multiline", Encode.bool settings.multiline )
        , ( "filters", Encode.dict identity encodeFilter (Dict.fromList settings.filters) )
        ]



-- ( "title", Encode.string settings.title )


type alias Model =
    { title : String
    , settings : Settings
    , tables : List String
    , fields : Dict String (List Field)
    , form : SettingsForm
    , error : Maybe String
    , zone : Time.Zone
    , lang : List I18Next.Translations
    , panel : Panel
    , contentDnd : DnDList.Model
    , contentAddState : Select.State
    , sortDnd : DnDList.Model
    , sortAddState : Select.State
    , filtersAddState : Select.State
    , filtersEdit : Maybe String
    }


type Panel
    = General
    | Filters


contentSystem : DnDList.System String Msg
contentSystem =
    DnDList.create
        { beforeUpdate = \_ _ list -> list
        , movement = DnDList.Vertical
        , listen = DnDList.OnDrag
        , operation = DnDList.Rotate
        }
        ContentDndMsg


sortSystem : DnDList.System Sort Msg
sortSystem =
    DnDList.create
        { beforeUpdate = \_ _ list -> list
        , movement = DnDList.Vertical
        , listen = DnDList.OnDrag
        , operation = DnDList.Rotate
        }
        SortDndMsg


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


type alias Flags =
    { title : String, settings : Settings, tables : List String, fields : List Field }


decodeFlags =
    Decode.map4 Flags
        (Decode.field "title" Decode.string)
        (Decode.field "settings" decodeSettings)
        (Decode.field "tables" (Decode.list Decode.string))
        (Decode.field "fields" (Decode.list (Field.decoder defaultChoice)))


initialFlags =
    { title = "Default Title", settings = initialSettings, tables = [], fields = [] }


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ jsToElm ReceiveData
                    , contentSystem.subscriptions model.contentDnd
                    , sortSystem.subscriptions model.sortDnd
                    ]
        }


init : Value -> ( Model, Cmd.Cmd Msg )
init flags =
    let
        result =
            Decode.decodeValue decodeFlags flags

        -- zone = TimeZone.europe__paris ()
        zone =
            Time.utc

        { title, settings, tables, fields } =
            Result.withDefault initialFlags result
    in
    ( { title = title
      , settings = settings
      , tables = tables
      , fields =
            Maybe.map (\table -> Dict.singleton table fields) settings.table
                |> Maybe.withDefault Dict.empty
      , form = settingsToForm zone settings
      , error =
            case result of
                Ok _ ->
                    Nothing

                Err err ->
                    Decode.errorToString err |> Just
      , zone = Time.utc
      , lang = [ defaultLanguage ]
      , panel = General
      , contentDnd = contentSystem.model
      , sortDnd = sortSystem.model
      , contentAddState = Select.initState (Select.selectIdentifier "contentAddState")
      , sortAddState = Select.initState (Select.selectIdentifier "sortAddState")
      , filtersAddState = Select.initState (Select.selectIdentifier "filtersAddState")
      , filtersEdit = Nothing
      }
    , Cmd.none
    )


type alias TextInputData =
    { field : String, error : Maybe String }


type alias SettingsForm =
    { fromDate : TextInputData
    , toDate : TextInputData
    , vpages : TextInputData
    , hpages : TextInputData
    , groupsFontSize : TextInputData
    , tasksFontSize : TextInputData
    , hoursFontSize : TextInputData
    , tableSelect : Select.State
    , startSelect : Select.State
    , endSelect : Select.State
    , groupSelect : Select.State
    , subGroupSelect : Select.State
    , colorSelect : Select.State
    }


settingsToForm : Time.Zone -> Settings -> SettingsForm
settingsToForm zone settings =
    { fromDate =
        TextInputData
            (Maybe.map (Iso8601.toDateTimeString zone) settings.fromDate
                |> Maybe.withDefault ""
            )
            Nothing
    , toDate =
        TextInputData
            (Maybe.map (Iso8601.toDateTimeString zone) settings.toDate
                |> Maybe.withDefault ""
            )
            Nothing
    , vpages = TextInputData (String.fromInt settings.vpages) Nothing
    , hpages = TextInputData (String.fromInt settings.hpages) Nothing
    , groupsFontSize = TextInputData (String.fromFloat settings.groupsFontSize) Nothing
    , tasksFontSize = TextInputData (String.fromFloat settings.tasksFontSize) Nothing
    , hoursFontSize = TextInputData (String.fromFloat settings.hoursFontSize) Nothing
    , tableSelect = Select.initState (Select.selectIdentifier "table-select")
    , startSelect = Select.initState (Select.selectIdentifier "start-select")
    , endSelect = Select.initState (Select.selectIdentifier "end-select")
    , groupSelect = Select.initState (Select.selectIdentifier "group-select")
    , subGroupSelect = Select.initState (Select.selectIdentifier "subgroup-select")
    , colorSelect = Select.initState (Select.selectIdentifier "color-select")
    }


validateForm : SettingsForm -> Settings -> ( SettingsForm, Settings )
validateForm form settings =
    let
        fromDate =
            validatePosixOrNothing form.fromDate.field

        toDate =
            validatePosixOrNothing form.toDate.field

        vpages =
            validateInt form.vpages.field

        hpages =
            validateInt form.hpages.field

        groupsFontSize =
            validateFloat form.groupsFontSize.field

        tasksFontSize =
            validateFloat form.tasksFontSize.field

        hoursFontSize =
            validateFloat form.hoursFontSize.field
    in
    ( { fromDate = updateFieldError form.fromDate.field fromDate
      , toDate = updateFieldError form.toDate.field toDate
      , vpages = updateFieldError form.vpages.field vpages
      , hpages = updateFieldError form.hpages.field hpages
      , groupsFontSize = updateFieldError form.groupsFontSize.field groupsFontSize
      , tasksFontSize = updateFieldError form.tasksFontSize.field tasksFontSize
      , hoursFontSize = updateFieldError form.hoursFontSize.field hoursFontSize
      , tableSelect = form.tableSelect
      , startSelect = form.tableSelect
      , endSelect = form.tableSelect
      , groupSelect = form.tableSelect
      , subGroupSelect = form.tableSelect
      , colorSelect = form.tableSelect
      }
    , { settings
        | fromDate = Result.withDefault settings.fromDate fromDate
        , toDate = Result.withDefault settings.toDate toDate
        , vpages = Result.withDefault settings.vpages vpages
        , hpages = Result.withDefault settings.hpages hpages
        , groupsFontSize = Result.withDefault settings.groupsFontSize groupsFontSize
        , tasksFontSize = Result.withDefault settings.tasksFontSize tasksFontSize
        , hoursFontSize = Result.withDefault settings.hoursFontSize hoursFontSize
      }
    )


canGenerate : Settings -> Bool
canGenerate settings =
    settings.table /= Nothing && settings.fromField /= Nothing && settings.toField /= Nothing && settings.group /= Nothing


updateFieldError : String -> Result String res -> TextInputData
updateFieldError val res =
    TextInputData val (errorOrNothing res)


validateInt : String -> Result String Int
validateInt str =
    case String.toInt str of
        Just int ->
            Ok int

        Nothing ->
            Err "Not a valid integer"


validateFloat : String -> Result String Float
validateFloat str =
    case String.toFloat str of
        Just float ->
            Ok float

        Nothing ->
            Err "Not a valid float"


validatePosix : String -> Result String Posix
validatePosix str =
    Decode.decodeString DecodeX.datetime ("\"" ++ str ++ "\"")
        |> Result.mapError (always "Not a valid date-time")


validatePosixOrNothing : String -> Result String (Maybe Posix)
validatePosixOrNothing str =
    if String.isEmpty str then
        Result.Ok Nothing

    else
        validatePosix str
            |> Result.map Just


type Msg
    = UpdateTitle String
    | UpdateFromDate String
    | UpdateToDate String
    | UpdateTable (Select.Msg String)
    | UpdateSort Bool String
    | UpdateFromField (Select.Msg String)
    | UpdateToField (Select.Msg String)
    | UpdateGroup (Select.Msg String)
    | UpdateSubGroup (Select.Msg String)
    | UpdateColor (Select.Msg String)
    | UpdateContent Bool String
    | UpdateLayout Layout
    | UpdatePaperSize String
    | UpdateVPages String
    | UpdateHPages String
    | UpdateGroupsFontSize String
    | UpdateTasksFontSize String
    | UpdateHoursFontSize String
    | UpdateOrientation Orientation
    | UpdateAlign Align
    | UpdateMultiline Bool
    | Blur
    | BlurTitle
    | ReceiveData Value
    | ShowPanel Panel
    | ContentDndMsg DnDList.Msg
    | SortDndMsg DnDList.Msg
    | ContentAddMsg (Select.Msg String)
    | ContentAddShowMsg
    | SortAddMsg (Select.Msg String)
    | SortAddShowMsg
    | ChangeSortDirection String Direction
    | FiltersAddMsg (Select.Msg String)
    | FiltersAddShowMsg
    | UpdateFilters String
    | EditFilter (Maybe String)
    | FilterAll String
    | FilterNone String
    | FilterExclude String Bool
    | FilterAddValues String ValueSet
    | FilterRemoveValues String ValueSet


updateSettings : Settings -> Model -> Model
updateSettings settings model =
    { model | settings = settings }


updateForm : SettingsForm -> Model -> Model
updateForm form model =
    { model | form = form }


noCmd : Model -> ( Model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


withSettingsSend : Model -> ( Model, Cmd msg )
withSettingsSend model =
    ( model, sendSettings model.zone model.settings )


addSettingsSend : ( Model, Cmd msg ) -> ( Model, Cmd msg )
addSettingsSend ( model, cmds ) =
    ( model, Cmd.batch [ cmds, sendSettings model.zone model.settings ] )


errorOrNothing res =
    case res of
        Ok _ ->
            Nothing

        Err str ->
            Just str


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        settings =
            model.settings

        form =
            model.form
    in
    case msg of
        ReceiveData value ->
            let
                ( mdl, cmd ) =
                    init value
            in
            ( { mdl
                | panel = model.panel
                , contentDnd = model.contentDnd
                , sortDnd = model.sortDnd
                , contentAddState = model.contentAddState
                , sortAddState = model.sortAddState
                , filtersAddState = model.filtersAddState
                , filtersEdit = model.filtersEdit
              }
            , if canGenerate mdl.settings then
                Cmd.batch [ cmd, generate (encodeSettings mdl.zone mdl.settings) ]

              else
                cmd
            )

        UpdateTitle title ->
            { model | title = title }
                |> noCmd

        BlurTitle ->
            ( model, sendTitle model.title )

        UpdateFromDate fromDate ->
            updateForm { form | fromDate = TextInputData fromDate form.fromDate.error } model
                |> noCmd

        UpdateToDate toDate ->
            updateForm { form | toDate = TextInputData toDate form.toDate.error } model
                |> noCmd

        UpdateTable selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg form.tableSelect

                table_ =
                    case maybeAction of
                        Just (Select.Select item) ->
                            Just item

                        Just Select.Clear ->
                            Nothing

                        _ ->
                            settings.table

                settings_ =
                    if table_ /= settings.table then
                        { settings
                            | table = table_
                            , fromField = Nothing
                            , toField = Nothing
                            , group = Nothing
                            , subGroup = Nothing
                            , color = Nothing
                            , content = []
                        }

                    else
                        settings
            in
            ( updateForm { form | tableSelect = updatedSelectState } { model | settings = settings_ }
            , Cmd.batch
                [ Cmd.map UpdateTable selectCmds
                , if settings_ == settings then
                    Cmd.none

                  else
                    sendSettings model.zone settings_
                ]
            )

        UpdateSort removeBool field ->
            updateSettings
                { settings
                    | sort =
                        if removeBool then
                            List.filter (\s -> s.field /= field) settings.sort

                        else
                            settings.sort ++ [ { field = field, direction = Ascending } ]
                }
                model
                |> withSettingsSend

        UpdateFromField fromField ->
            updateSelect model
                fromField
                UpdateFromField
                form.startSelect
                (\set data -> { set | fromField = data })
                (\frm data -> { frm | startSelect = data })

        UpdateToField toField ->
            updateSelect model
                toField
                UpdateToField
                form.endSelect
                (\set data -> { set | toField = data })
                (\frm data -> { frm | endSelect = data })

        UpdateGroup group ->
            updateSelect model
                group
                UpdateGroup
                form.groupSelect
                (\set data -> { set | group = data })
                (\frm data -> { frm | groupSelect = data })

        UpdateSubGroup subGroup ->
            updateSelect model
                subGroup
                UpdateSubGroup
                form.subGroupSelect
                (\set data -> { set | subGroup = data })
                (\frm data -> { frm | subGroupSelect = data })

        UpdateColor color ->
            updateSelect model
                color
                UpdateColor
                form.colorSelect
                (\set data -> { set | color = data })
                (\frm data -> { frm | colorSelect = data })

        UpdateContent removeBool field ->
            updateSettings
                { settings
                    | content =
                        if removeBool then
                            ListX.remove field settings.content

                        else
                            settings.content ++ [ field ]
                }
                model
                |> withSettingsSend

        UpdateLayout layout ->
            updateSettings { settings | layout = layout } model
                |> withSettingsSend

        UpdatePaperSize str ->
            updateSettings { settings | papersize = stringToPaperSize str |> Maybe.withDefault A4 } model
                |> withSettingsSend

        UpdateVPages vpages ->
            updateForm { form | vpages = TextInputData vpages form.vpages.error } model
                |> noCmd

        UpdateHPages hpages ->
            updateForm { form | hpages = TextInputData hpages form.hpages.error } model
                |> noCmd

        UpdateGroupsFontSize groupsFontSize ->
            updateForm { form | groupsFontSize = TextInputData groupsFontSize form.groupsFontSize.error } model
                |> noCmd

        UpdateTasksFontSize tasksFontSize ->
            updateForm { form | tasksFontSize = TextInputData tasksFontSize form.tasksFontSize.error } model
                |> noCmd

        UpdateHoursFontSize hoursFontSize ->
            updateForm { form | hoursFontSize = TextInputData hoursFontSize form.hoursFontSize.error } model
                |> noCmd

        UpdateOrientation orient ->
            updateSettings { settings | orientation = orient } model
                |> withSettingsSend

        UpdateAlign align ->
            updateSettings { settings | align = align } model
                |> withSettingsSend

        UpdateMultiline multiline ->
            updateSettings { settings | multiline = multiline } model
                |> withSettingsSend

        Blur ->
            let
                ( form_, settings_ ) =
                    validateForm model.form model.settings
            in
            ( { model | form = form_, settings = settings_ }, sendSettings model.zone settings_ )

        ShowPanel panel ->
            noCmd { model | panel = panel }

        ContentDndMsg dndmsg ->
            let
                ( dnd, content ) =
                    contentSystem.update dndmsg model.contentDnd model.settings.content
            in
            ( { model | contentDnd = dnd, settings = { settings | content = content } }
            , contentSystem.commands dnd
            )
                |> addSettingsSend

        SortDndMsg dndmsg ->
            let
                ( dnd, sort ) =
                    sortSystem.update dndmsg model.sortDnd model.settings.sort
            in
            ( { model | sortDnd = dnd, settings = { settings | sort = sort } }
            , sortSystem.commands dnd
            )
                |> addSettingsSend

        ContentAddMsg selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.contentAddState
            in
            case maybeAction of
                Just (Select.Select item) ->
                    let
                        settings_ =
                            { settings | content = settings.content ++ [ item ] }
                    in
                    ( { model
                        | contentAddState = updatedSelectState
                        , settings = settings_
                      }
                    , Cmd.batch [ Cmd.map ContentAddMsg selectCmds, sendSettings model.zone settings_ ]
                    )

                _ ->
                    ( { model | contentAddState = updatedSelectState }, Cmd.map ContentAddMsg selectCmds )

        ContentAddShowMsg ->
            let
                ( actions, updatedState, cmds ) =
                    Select.update Select.focus model.contentAddState
            in
            ( { model | contentAddState = updatedState }, Cmd.map ContentAddMsg cmds )

        SortAddMsg selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.sortAddState
            in
            case maybeAction of
                Just (Select.Select item) ->
                    let
                        settings_ =
                            { settings | sort = settings.sort ++ [ Sort item Ascending ] }
                    in
                    ( { model
                        | sortAddState = updatedSelectState
                        , settings = settings_
                      }
                    , Cmd.batch [ Cmd.map SortAddMsg selectCmds, sendSettings model.zone settings_ ]
                    )

                _ ->
                    ( { model | sortAddState = updatedSelectState }, Cmd.map SortAddMsg selectCmds )

        SortAddShowMsg ->
            let
                ( actions, updatedState, cmds ) =
                    Select.update Select.focus model.sortAddState
            in
            ( { model | sortAddState = updatedState }, Cmd.map SortAddMsg cmds )

        ChangeSortDirection field direction ->
            updateSettings
                { settings
                    | sort =
                        updateOne
                            (\sort ->
                                if field == sort.field then
                                    Just { sort | direction = direction }

                                else
                                    Nothing
                            )
                            settings.sort
                }
                model
                |> withSettingsSend

        FiltersAddMsg selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.filtersAddState
            in
            case maybeAction of
                Just (Select.Select item) ->
                    let
                        settings_ =
                            Maybe.andThen (\table -> Dict.get table model.fields) settings.table
                                |> Maybe.andThen (ListX.find (\field -> field.id == item))
                                |> Maybe.map
                                    (\field ->
                                        { settings
                                            | filters =
                                                settings.filters
                                                    ++ [ ( item
                                                         , Filter True
                                                            (case field.values of
                                                                Field.ListInt _ ->
                                                                    SetInt Set.empty

                                                                Field.ListFloat _ ->
                                                                    SetFloat Set.empty

                                                                _ ->
                                                                    SetString Set.empty
                                                            )
                                                         )
                                                       ]
                                        }
                                    )
                                |> Maybe.withDefault settings
                    in
                    ( { model
                        | filtersAddState = updatedSelectState
                        , settings = settings_
                      }
                    , Cmd.batch [ Cmd.map SortAddMsg selectCmds, sendSettings model.zone settings_ ]
                    )

                _ ->
                    ( { model | filtersAddState = updatedSelectState }, Cmd.map FiltersAddMsg selectCmds )

        FiltersAddShowMsg ->
            let
                ( actions, updatedState, cmds ) =
                    Select.update Select.focus model.filtersAddState
            in
            ( { model | filtersAddState = updatedState }, Cmd.map FiltersAddMsg cmds )

        UpdateFilters field ->
            updateSettings
                { settings
                    | filters =
                        List.filter (\filter -> Tuple.first filter /= field) settings.filters
                }
                model
                |> withSettingsSend

        EditFilter mbstr ->
            { model | filtersEdit = mbstr }
                |> noCmd

        FilterAll string ->
            updateSettings
                { settings
                    | filters =
                        updateOne
                            (\( field, filter ) ->
                                if field == string then
                                    let
                                        values =
                                            if filter.exclude then
                                                Filter True (clearValueSet filter.values)

                                            else
                                                getValues model field
                                                    |> Maybe.withDefault (clearValueSet filter.values)
                                                    |> Filter False
                                    in
                                    Just ( field, values )

                                else
                                    Nothing
                            )
                            settings.filters
                }
                model
                |> withSettingsSend

        FilterNone string ->
            updateSettings
                { settings
                    | filters =
                        updateOne
                            (\( field, filter ) ->
                                if field == string then
                                    let
                                        values =
                                            if filter.exclude then
                                                getValues model field
                                                    |> Maybe.withDefault (clearValueSet filter.values)
                                                    |> Filter True

                                            else
                                                Filter False (clearValueSet filter.values)
                                    in
                                    Just ( field, values )

                                else
                                    Nothing
                            )
                            settings.filters
                }
                model
                |> withSettingsSend

        FilterExclude string bool ->
            updateSettings
                { settings
                    | filters =
                        updateOne
                            (\( field, filter ) ->
                                if field == string then
                                    let
                                        values =
                                            if filter.exclude == bool then
                                                Filter bool filter.values

                                            else
                                                valueSetDiff
                                                    (getValues model field
                                                        |> Maybe.withDefault (clearValueSet filter.values)
                                                    )
                                                    filter.values
                                                    |> Maybe.withDefault filter.values
                                                    |> Filter bool
                                    in
                                    Just ( field, values )

                                else
                                    Nothing
                            )
                            settings.filters
                }
                model
                |> withSettingsSend

        FilterAddValues string valueSet ->
            updateSettings
                { settings
                    | filters =
                        updateOne
                            (\( field, filter ) ->
                                if field == string then
                                    let
                                        values =
                                            (if filter.exclude then
                                                valueSetDiff filter.values valueSet

                                             else
                                                valueSetUnion filter.values valueSet
                                            )
                                                |> Maybe.withDefault filter.values
                                                |> Filter filter.exclude
                                    in
                                    Just ( field, values )

                                else
                                    Nothing
                            )
                            settings.filters
                }
                model
                |> withSettingsSend

        FilterRemoveValues string valueSet ->
            updateSettings
                { settings
                    | filters =
                        updateOne
                            (\( field, filter ) ->
                                if field == string then
                                    let
                                        values =
                                            (if filter.exclude then
                                                valueSetUnion filter.values valueSet

                                             else
                                                valueSetDiff filter.values valueSet
                                            )
                                                |> Maybe.withDefault filter.values
                                                |> Filter filter.exclude
                                    in
                                    Just ( field, values )

                                else
                                    Nothing
                            )
                            settings.filters
                }
                model
                |> withSettingsSend


valueSetUnion vs1 vs2 =
    case ( vs1, vs2 ) of
        ( SetInt v1, SetInt v2 ) ->
            Just <| SetInt (Set.union v1 v2)

        ( SetFloat v1, SetFloat v2 ) ->
            Just <| SetFloat (Set.union v1 v2)

        ( SetString v1, SetString v2 ) ->
            Just <| SetString (Set.union v1 v2)

        _ ->
            Nothing


valueSetDiff vs1 vs2 =
    case ( vs1, vs2 ) of
        ( SetInt v1, SetInt v2 ) ->
            Just <| SetInt (Set.diff v1 v2)

        ( SetFloat v1, SetFloat v2 ) ->
            Just <| SetFloat (Set.diff v1 v2)

        ( SetString v1, SetString v2 ) ->
            Just <| SetString (Set.diff v1 v2)

        _ ->
            Nothing


getValues : Model -> String -> Maybe ValueSet
getValues ({ settings } as model) field =
    Maybe.andThen (\table -> Dict.get table model.fields) settings.table
        |> Maybe.andThen (ListX.find (\item -> item.id == field))
        |> Maybe.map
            (\f ->
                case f.values of
                    Field.ListInt list ->
                        List.map (\{ value } -> value) list
                            |> Set.fromList
                            |> SetInt

                    Field.ListFloat list ->
                        List.map (\{ value } -> value) list
                            |> Set.fromList
                            |> SetFloat

                    Field.ListString list ->
                        List.map (\{ value } -> value) list
                            |> Set.fromList
                            |> SetString
            )


updateOne : (a -> Maybe a) -> List a -> List a
updateOne upd list =
    updateOneHelper1 upd [] list


updateOneHelper1 upd acc list =
    case list of
        x :: xs ->
            let
                ( bool, acc_ ) =
                    updateOneHelper2 upd x acc
            in
            if bool then
                recomposeList acc_ xs

            else
                updateOneHelper1 upd acc_ xs

        [] ->
            List.reverse acc


updateOneHelper2 upd val acc =
    case upd val of
        Just v ->
            ( True, v :: acc )

        Nothing ->
            ( False, val :: acc )


recomposeList inv tail =
    case inv of
        x :: xs ->
            recomposeList xs (x :: tail)

        [] ->
            tail


updateSelect ({ settings, form } as model) selectMsg toMsg selectState setUpd formUpd =
    let
        ( maybeAction, updatedSelectState, selectCmds ) =
            Select.update selectMsg selectState

        settings_ =
            case maybeAction of
                Just (Select.Select item) ->
                    setUpd settings (Just item)

                Just Select.Clear ->
                    setUpd settings Nothing

                _ ->
                    settings
    in
    ( updateForm (formUpd form updatedSelectState) { model | settings = settings_ }
    , Cmd.batch
        [ Cmd.map toMsg selectCmds
        , if settings_ == settings then
            Cmd.none

          else
            sendSettings model.zone settings_
        ]
    )


sendTitle : String -> Cmd msg
sendTitle title =
    Encode.object [ ( "title", Encode.string title ) ]
        |> elmToJs


sendSettings : Time.Zone -> Settings -> Cmd msg
sendSettings zone settings =
    Encode.object [ ( "settings", encodeSettings zone settings ) ]
        |> elmToJs


port elmToJs : Value -> Cmd msg


port jsToElm : (Value -> msg) -> Sub msg


port generate : Value -> Cmd msg



-- View


fromMaybe : Maybe String -> String
fromMaybe =
    Maybe.withDefault ""


view : Model -> Html Msg
view model =
    Html.div
        [ if model.panel == Filters then
            HA.style "max-width" "400px"

          else
            HA.class ""
        ]
        ([ Html.node "style" [] [ Html.text (styles ++ Segment.styles) ]
         , Segment.menu ShowPanel
            model.panel
            [ { value = General, label = Html.text "Données et aspect" }
            , { value = Filters, label = Html.text "Trier et filtrer" }
            ]
         ]
            ++ (case model.panel of
                    General ->
                        generalView model

                    Filters ->
                        filtersView model
               )
        )


generalView : Model -> List (Html Msg)
generalView ({ settings, form, lang } as model) =
    let
        fields =
            settings.table
                |> Maybe.andThen (\tbl -> Dict.get tbl model.fields)
                |> Maybe.withDefault []

        dateTimeFields =
            List.filter (\field -> field.ofType == Field.DateTime) fields

        choiceFields =
            List.filter
                (\field ->
                    case field.ofType of
                        Field.Choice _ ->
                            True

                        _ ->
                            False
                )
                fields

        fieldsToSelectOptions =
            \fields_ ->
                List.map (\field -> Select.basicMenuItem { item = field.id, label = field.label }) fields_

        fieldToSelection =
            \field_ ->
                field_ |> Maybe.map (\sel -> Select.basicMenuItem { item = sel, label = nameFromKey sel })

        nameFromKey =
            \key ->
                ListX.find (\item -> item.id == key) fields
                    |> Maybe.map .label
                    |> Maybe.withDefault key
    in
    [ Maybe.map (\error -> Html.span [ HA.class "error" ] [ Html.text error ]) model.error
        |> Maybe.withDefault (Html.text "")
    , inputField [ HA.style "width" "100%", HA.style "max-width" "400px" ] "Titre" model.title UpdateTitle BlurTitle
    , Html.hr [] []
    , section "Données"
    , selectInput [ HA.style "max-width" "400px" ]
        (T.table lang ++ "*")
        False
        "Choisir une table"
        form.tableSelect
        (settings.table |> Maybe.map (\sel -> Select.basicMenuItem { item = sel, label = sel }))
        (List.map (\table -> Select.basicMenuItem { item = table, label = table }) model.tables)
        UpdateTable
    , Html.div [ HA.class "horiz" ]
        [ selectInput []
            (T.fromField lang ++ "*")
            False
            "Choisir une colonne"
            form.startSelect
            (fieldToSelection settings.fromField)
            (fieldsToSelectOptions dateTimeFields)
            UpdateFromField
        , selectInput []
            (T.toField lang ++ "*")
            False
            "Choisir une colonne"
            form.endSelect
            (fieldToSelection settings.toField)
            (fieldsToSelectOptions dateTimeFields)
            UpdateToField
        ]
    , Html.div [ HA.class "horiz" ]
        [ selectInput []
            (T.groupField lang ++ "*")
            False
            "Choisir une colonne"
            form.groupSelect
            (fieldToSelection settings.group)
            (fieldsToSelectOptions fields)
            UpdateGroup
        , selectInput []
            (T.subGroupField lang)
            True
            "Choisir une colonne"
            form.subGroupSelect
            (fieldToSelection settings.subGroup)
            (fieldsToSelectOptions fields)
            UpdateSubGroup
        , selectInput []
            (T.colorField lang)
            True
            "Choisir une colonne"
            form.colorSelect
            (fieldToSelection settings.color)
            (fieldsToSelectOptions choiceFields)
            UpdateColor
        ]
    , Html.div [ HA.style "max-width" "400px" ]
        [ optionsField
            { system = contentSystem
            , dnd = model.contentDnd
            , label = "Contenu"
            , options = fields
            , selection = settings.content
            , accessor = identity
            , toHtml = \_ ( _, opt ) -> Html.text opt.label
            , msgRemove = UpdateContent True
            }
        , fieldsPopup
            { options = fields
            , selection = settings.content
            , accessor = identity
            , selectState = model.contentAddState
            , selectMsg = ContentAddMsg
            , showMsg = ContentAddShowMsg
            }
        ]
    , Html.hr [] []
    , section "Période"
    , Html.div [ HA.class "horiz" ]
        [ inputFieldVal (T.fromDate lang) "datetime-local" form.fromDate UpdateFromDate
        , inputFieldVal (T.toDate lang) "datetime-local" form.toDate UpdateToDate
        ]
    , Html.hr [] []
    , section (T.pageLayout lang)
    , Html.div [ HA.class "horiz" ]
        [ label [] (T.orientation lang) <|
            Segment.radio UpdateLayout
                settings.layout
                [ { value = Portrait, label = Html.span [] [ icon [ HA.style "margin-right" "6px" ] Phosphor.file, T.portrait lang |> Html.text ] }
                , { value = Landscape, label = Html.span [] [ icon [ HA.style "margin-right" "6px", HA.style "transform" "rotate(90deg)" ] Phosphor.file, T.landscape lang |> Html.text ] }
                ]
        , dropdownField (T.paperSize lang)
            settings.papersize
            [ ( A0, paperSizeToString A0, "A0" )
            , ( A1, paperSizeToString A1, "A1" )
            , ( A2, paperSizeToString A2, "A2" )
            , ( A3, paperSizeToString A3, "A3" )
            , ( A4, paperSizeToString A4, "A4" )
            , ( A5, paperSizeToString A5, "A5" )
            ]
            UpdatePaperSize
        , inputFieldVal (T.verticalPages lang) "number" form.vpages UpdateVPages
        , inputFieldVal (T.horizontalPages lang) "number" form.hpages UpdateHPages
        ]
    , Html.hr [] []
    , section "Affichage des tâches"
    , Html.div [ HA.class "horiz" ]
        [ label [] "Sens" <|
            Segment.radio UpdateOrientation
                settings.orientation
                [ { value = Horizontal, label = T.horizontal lang |> Html.text }
                , { value = Vertical, label = T.vertical lang |> Html.text }
                ]
        , label [] (T.align lang) <|
            Segment.radio UpdateAlign
                settings.align
                [ { value = Left, label = icon [] Phosphor.textAlignLeft }
                , { value = Middle, label = icon [] Phosphor.textAlignCenter }
                , { value = Right, label = icon [] Phosphor.textAlignRight }
                ]
        , checkboxField (T.multiline lang) settings.multiline UpdateMultiline
        ]
    , Html.hr [] []
    , section (T.textSize lang)
    , Html.div [ HA.class "horiz" ]
        [ inputFieldVal (T.groupFontSize lang) "number" form.groupsFontSize UpdateGroupsFontSize
        , inputFieldVal (T.taskFontSize lang) "number" form.tasksFontSize UpdateTasksFontSize
        , inputFieldVal (T.hourFontSize lang) "number" form.hoursFontSize UpdateHoursFontSize
        ]
    ]


filtersView : Model -> List (Html Msg)
filtersView ({ settings } as model) =
    let
        fields =
            settings.table
                |> Maybe.andThen (\tbl -> Dict.get tbl model.fields)
                |> Maybe.withDefault []

        maybeFilter =
            Maybe.andThen (\id -> ListX.find (\( fid, _ ) -> id == fid) settings.filters) model.filtersEdit
                |> Maybe.map (\( _, flts ) -> flts)

        dict =
            List.map (\opt -> ( opt.id, opt )) fields
                |> Dict.fromList

        selFull =
            List.filterMap
                (\sel ->
                    Dict.get (Tuple.first sel) dict
                        |> Maybe.map (\opt -> ( sel, opt ))
                )
                settings.filters
    in
    [ optionsField
        { system = sortSystem
        , dnd = model.sortDnd
        , label = "Tri"
        , options = fields
        , selection = settings.sort
        , accessor = .field
        , toHtml =
            \_ ( val, opt ) ->
                Html.div
                    [ HE.onClick
                        (ChangeSortDirection val.field
                            (case val.direction of
                                Ascending ->
                                    Descending

                                Descending ->
                                    Ascending
                            )
                        )
                    , HA.style "cursor" "pointer"
                    ]
                    [ if val.direction == Ascending then
                        icon [ HA.style "margin-right" "6px" ] Phosphor.funnelSimple

                      else
                        icon [ HA.style "transform" "scaleY(-1)", HA.style "margin-right" "6px" ] Phosphor.funnelSimple
                    , Html.text opt.label
                    ]
        , msgRemove = UpdateSort True
        }
    , fieldsPopup
        { options = fields
        , selection = settings.sort
        , accessor = .field
        , selectState = model.sortAddState
        , selectMsg = SortAddMsg
        , showMsg = SortAddShowMsg
        }
    , section "Filtres"
    ]
        ++ (List.map
                (\( _, opt ) ->
                    [ Html.div [ HA.class "field" ] <|
                        [ Html.div [ HA.class "option" ]
                            [ Html.span [ HA.class "content" ]
                                [ Html.div
                                    [ HE.onClick <|
                                        EditFilter <|
                                            if model.filtersEdit == Just opt.id then
                                                Nothing

                                            else
                                                Just opt.id
                                    , HA.style "cursor" "pointer"
                                    ]
                                    [ icon [ HA.style "margin-right" "6px" ] Phosphor.funnel
                                    , Html.text opt.label
                                    ]
                                , Html.button [ HA.class "delete", HE.onClick (UpdateFilters opt.id) ] [ icon [ HA.style "margin-right" "6px" ] Phosphor.trash ]
                                ]
                            ]
                        ]
                    , if model.filtersEdit == Just opt.id then
                        filterEditView model maybeFilter settings

                      else
                        Html.text ""
                    ]
                )
                selFull
                |> List.concat
           )
        ++ [ fieldsPopup
                { options = fields
                , selection = settings.filters
                , accessor = Tuple.first
                , selectState = model.filtersAddState
                , selectMsg = FiltersAddMsg
                , showMsg = FiltersAddShowMsg
                }
           ]


filterEditView : Model -> Maybe Filter -> Settings -> Html Msg
filterEditView model maybeFilter settings =
    case ( model.filtersEdit, maybeFilter ) of
        ( Just fieldId, Just filter ) ->
            Html.div [ HA.class "popup" ]
                [ Html.div [ HA.class "horiz" ]
                    [ Html.span [ HA.style "cursor" "pointer", HE.onClick (FilterAll fieldId) ] [ Html.text "Tous" ]
                    , Html.span [] [ Html.text "•" ]
                    , Html.span [ HA.style "cursor" "pointer", HE.onClick (FilterNone fieldId) ] [ Html.text "Aucun" ]
                    ]
                , settings.table
                    |> Maybe.andThen (\table -> Dict.get table model.fields)
                    |> Maybe.andThen (ListX.find (\item -> item.id == fieldId))
                    |> Maybe.andThen
                        (\field ->
                            let
                                checked =
                                    \set ->
                                        if filter.exclude then
                                            \v -> Set.member v set |> not

                                        else
                                            \v -> Set.member v set

                                listToOptions =
                                    \ctor list set ->
                                        List.map
                                            (\v ->
                                                checkbox v.label
                                                    (checked set v.value)
                                                    (\bool ->
                                                        ctor (Set.singleton v.value)
                                                            |> (if bool then
                                                                    FilterAddValues fieldId

                                                                else
                                                                    FilterRemoveValues fieldId
                                                               )
                                                    )
                                            )
                                            list
                                            |> Just
                            in
                            case ( field.values, filter.values ) of
                                ( Field.ListInt list, SetInt set ) ->
                                    listToOptions SetInt list set

                                ( Field.ListFloat list, SetFloat set ) ->
                                    listToOptions SetFloat list set

                                ( Field.ListString list, SetString set ) ->
                                    listToOptions SetString list set

                                _ ->
                                    Nothing
                        )
                    |> Maybe.withDefault []
                    |> Html.div [ HA.style "padding" "16px 0", HA.class "content" ]
                , Html.hr [] []
                , checkbox "Futures valeurs" filter.exclude (FilterExclude fieldId)
                , Html.div [ HA.class "horiz" ]
                    [ Html.button [ HE.onClick (EditFilter Nothing), HA.class "confirm" ] [ Html.text "Fermer" ]
                    , Html.button [ HE.onClick (EditFilter Nothing) ] [ Html.text "Annuler" ]
                    ]
                ]

        _ ->
            Html.text ""



-- Form Helpers


icon attrs icn =
    icn Regular
        |> Phosphor.withSize 16
        |> Phosphor.withSizeUnit "px"
        |> Phosphor.toHtml (HA.style "vertical-align" "sub" :: attrs)


onEnter msg =
    HE.on "keyup"
        (HE.keyCode
            |> Decode.andThen
                (\key ->
                    if key == 13 then
                        Decode.succeed msg

                    else
                        Decode.fail "not enter"
                )
        )


inputField : List (Html.Attribute msg) -> String -> String -> (String -> msg) -> msg -> Html msg
inputField attrs lbl value msgConstructor validateConstructor =
    Html.div [ HA.class "field" ]
        [ label [] lbl <|
            Html.input
                ([ HA.value value
                 , HE.onInput msgConstructor
                 , HE.onBlur validateConstructor
                 ]
                    ++ attrs
                )
                []
        ]


inputFieldVal : String -> String -> TextInputData -> (String -> Msg) -> Html Msg
inputFieldVal lbl typ { field, error } msgConstructor =
    Html.div [ HA.class "field" ]
        [ label [] lbl <|
            Html.input
                [ HA.value field
                , HE.onInput msgConstructor
                , HE.onBlur Blur
                , onEnter Blur
                , HA.type_ typ
                , HA.style "max-width" <|
                    if typ == "number" then
                        "50px"

                    else
                        ""
                , if typ == "number" then
                    HA.style "text-align" "right"

                  else
                    HA.style "text-align" "left"
                ]
                []
        , Html.span [ HA.class "error" ] [ Html.text (fromMaybe error) ]
        ]


checkbox lbl bool ctor =
    Html.div []
        [ Html.label []
            [ Html.input
                [ HA.type_ "checkbox"
                , HA.checked bool
                , HE.onCheck ctor
                ]
                []
            , Html.text lbl
            ]
        ]


dropdownField : String -> val -> List ( val, String, String ) -> (String -> Msg) -> Html Msg
dropdownField lbl selected options msgConstructor =
    Html.div [ HA.class "field" ]
        [ label [] lbl <|
            Html.span [ HA.class "dropdown" ]
                [ Html.select [ HE.onInput msgConstructor ]
                    (List.map (\( opt, optStr, txt ) -> Html.option [ HA.value optStr, HA.selected (opt == selected) ] [ Html.text txt ]) options)
                ]
        ]


checkboxField : String -> Bool -> (Bool -> Msg) -> Html Msg
checkboxField lbl checked msgConstructor =
    Html.label [ HA.class "" ]
        [ Html.div [ HA.class "field" ]
            [ Html.input
                [ HA.type_ "checkbox"
                , HA.checked checked
                , HE.onClick (not checked |> msgConstructor)
                ]
                []
            , Html.text lbl
            ]
        ]


selectInput : List (Html.Attribute msg) -> String -> Bool -> String -> Select.State -> Maybe (Select.MenuItem item) -> List (Select.MenuItem item) -> (Select.Msg item -> msg) -> Html msg
selectInput attrs lbl clearable pholder selstate selection options toMsg =
    label (HA.style "min-width" "200px" :: attrs)
        lbl
        (Select.view
            (Select.single selection
                |> Select.state selstate
                |> Select.menuItems options
                |> Select.setStyles selectStyles
                |> Select.clearable clearable
                |> Select.placeholder pholder
            )
            |> Styled.toUnstyled
            |> Html.map toMsg
        )


optionsField :
    { system : DnDList.System value Msg
    , dnd : DnDList.Model
    , label : String
    , options : List { a | id : String, label : String }
    , selection : List value
    , accessor : value -> String
    , toHtml : Int -> ( value, { a | id : String, label : String } ) -> Html Msg
    , msgRemove : String -> Msg
    }
    -> Html Msg
optionsField config =
    let
        dict =
            List.map (\opt -> ( opt.id, opt )) config.options
                |> Dict.fromList

        selFull =
            List.filterMap
                (\sel ->
                    Dict.get (config.accessor sel) dict
                        |> Maybe.map (\opt -> ( sel, opt ))
                )
                config.selection
    in
    Html.div [ HA.class "field" ]
        [ section config.label
        , Html.div []
            [ List.indexedMap
                (\index (( _, opt ) as item) ->
                    option config.system config.dnd index (config.toHtml index item) opt.id config.msgRemove
                )
                selFull
                |> Html.div []
            , ghostView config.system config.dnd (List.map (Tuple.second >> .label) selFull)
            ]
        ]


option : DnDList.System value Msg -> DnDList.Model -> Int -> Html Msg -> String -> (String -> Msg) -> Html Msg
option system dnd index lbl value msgConstructor =
    let
        itemId : String
        itemId =
            "id-" ++ value
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                Html.div (HA.class "option" :: HA.id itemId :: system.dropEvents index itemId)
                    [ Html.span [ HA.class "content" ] [ lbl ] ]

            else
                Html.div [ HA.id itemId ] [ Html.div [ HA.style "height" "26px" ] [] ]

        Nothing ->
            Html.div [ HA.class "option", HA.id itemId ]
                [ Html.span (HA.class "handle" :: system.dragEvents index itemId) [ icon [] Phosphor.dotsSixVertical ]
                , Html.span [ HA.class "content" ]
                    [ lbl
                    , Html.button [ HA.class "delete", HE.onClick (msgConstructor value) ] [ icon [] Phosphor.trash ]
                    ]
                ]


ghostView : DnDList.System value Msg -> DnDList.Model -> List String -> Html Msg
ghostView system dnd items =
    let
        maybeDragItem : Maybe String
        maybeDragItem =
            system.info dnd
                |> Maybe.andThen (\{ dragIndex } -> items |> List.drop dragIndex |> List.head)
    in
    case maybeDragItem of
        Just item ->
            Html.div (system.ghostStyles dnd)
                [ Html.div [ HA.class "option ghost" ]
                    [ Html.span [ HA.class "handle" ] [ icon [] Phosphor.dotsSixVertical ]
                    , Html.span [ HA.class "content" ]
                        [ Html.text item
                        ]
                    ]
                ]

        Nothing ->
            Html.text ""


fieldsPopup :
    { options : List { a | id : String, label : String }
    , selection : List value
    , accessor : value -> String
    , selectState : Select.State
    , selectMsg : Select.Msg String -> Msg
    , showMsg : Msg
    }
    -> Html Msg
fieldsPopup config =
    let
        valuesSet =
            List.map config.accessor config.selection |> Set.fromList

        optList =
            List.filter (\opt -> Set.member opt.id valuesSet |> not) config.options
    in
    Html.div []
        [ Html.button
            [ HE.onClick config.showMsg
            , HA.style "border" "0"
            , HA.style "background" "none"
            , HA.style "color" "steelblue"
            ]
            [ Html.text "Ajouter une colonne" ]
        , Select.view
            (Select.singleMenu Nothing
                |> Select.menuItems
                    (List.map (\item -> Select.basicMenuItem { item = item.id, label = item.label }) optList)
                |> Select.state config.selectState
                |> Select.setStyles selectStyles
            )
            |> Styled.toUnstyled
            |> Html.map config.selectMsg
        ]


label : List (Html.Attribute msg) -> String -> Html msg -> Html msg
label attrs str widget =
    Html.label [] [ Html.div [ HA.class "label" ] [ Html.text str ], Html.div (HA.class "label_content" :: attrs) [ widget ] ]


section : String -> Html msg
section str =
    Html.h3 [ HA.class "title" ] [ Html.text str ]


selectStyles : Styles.Config
selectStyles =
    Styles.default
        |> Styles.setControlStyles
            (Styles.getControlConfig Styles.default
                |> Styles.setControlMinHeight 30
                |> Styles.setControlBorderRadius 3
                |> Styles.setControlBorderColor (Css.hex "#DDD")
                |> Styles.setControlBorderColorFocus (Css.rgba 0 0 0 0)
                |> Styles.setControlIndicatorPadding 3
            )


styles =
    """
body {
    font-family: sans-serif;
    font-size: 13px;
    color: var(--grist-theme-input-fg, black);
}
.field {
    padding-top: 3px;
    margin-inline-end: 15px;
}

.error {
    font-size: 10px;
    color: red;
}

.label {
    font-size: 10px;
    color: #262626;
    
}

.label_content {
        margin-bottom: 5px;
    }

.title {
    text-transform: uppercase;
    font-size: 11px;
    color: black;
    margin-top: 6px;
    margin-bottom: 4px;
    font-weight: normal;
}

.field > label  input {
    display: block;
    margin-top: 2px;
    color: var(--grist-theme-input-fg, black);
    outline: none;
    height: 28px;
    font-size: 13px;
    border: 1px solid var(--grist-theme-input-border, var(--grist-color-dark-grey));
    border-radius: 3px;
    padding: 0 3px;


}

input[type="checkbox"] {
        width: 15px;
        height: 15px;
        margin-right: 6px;
        vertical-align: sub;

        &:focus {
            outline: 2px solid var(--grist-theme-input-focus);
            outline-offset: 1px;
        }
    }

.field  select {
    margin-top: 1px;
    color: var(--grist-theme-input-fg, black);
    height: 28px;
    font-size: 13px;
    border: 1px solid var(--grist-theme-input-border, var(--grist-color-dark-grey));
    border-radius: 3px;
    width:100%;
    display: inline-block;
    padding-right: 30px;
    padding-left: 7px;
    outline: none;
      -webkit-appearance: none;

    &:focus {
        outline: 2px solid var(--grist-theme-input-focus);
    }
}

button {
    border-radius: 5px;
    border: 1px solid steelblue;
    background-color: transparent;
    padding: 3px 8px;
    cursor: pointer;
    user-select: none;
}

button.confirm {
    border-radius: 5px;
    border: 1px solid steelblue;
    background-color: steelblue;
    color: white;
    padding: 3px 8px;
}

input[type="checkbox"] {
    accent-color: steelblue;
}


.field .option {
    font-size: 12px;
    margin-bottom: 2px;
    margin-left: -10px;
    padding-left: 10px;
    position: relative;
    .content {
        background-color: rgba(0,0,0,.08);
        display: flex;
        padding: 4px;
        align-items: center;
        user-select: none;
        }
    button.delete {
        border: none;
        display: none;
        position: absolute;
        right: -5px;
        background-color: transparent;
        color: grey;

        &:hover {
            color: black;
        }
    }
    .handle {
        position: absolute;
        left: -5px;
        display: none;
        width: 16px;
        height: 16px;
        }
    
    &.ghost .handle {
        display: block;}

    &:hover {
        button {
            display: block;
        }
        .handle {
            display: block;
        }
    }
}




.horiz > * {
    margin-right: 10px;
}
.horiz > div {
    display: inline-block;  
}

.horiz > label {
    display: inline-block;
}


hr {
    border-width: 1px 0 0 0;
    border-style: solid;
    border-color: #bbb;
    width:100%
}

.dropdown {
    display: inline-block;
   position: relative;

   width:100%;

   &:after {
      border-color: grey transparent transparent transparent;
        border-style: solid;
      border-width: 5px;
      content: '';
      position: absolute;
      pointer-events: none;
      top: 14px;
      right: 8px;
      z-index: 1000;
   }

}

.popup {
    background-color: white;
    -webkit-box-shadow: 0px 7px 20px 1px rgba(0, 0, 0, 0.34);
    -moz-box-shadow: 0px 7px 20px 1px rgba(0, 0, 0, 0.34);
    box-shadow: 0px 7px 20px 1px rgba(0, 0, 0, 0.34);
    padding: 16px;
    position: sticky;
    bottom: 5vh;
    max-height: 90vh;
    display: flex;
    flex-direction: column;
    min-height: 100px;

    .content {
        flex-shrink: 1;
        overflow: auto;
    }
}



"""
