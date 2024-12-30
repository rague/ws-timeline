module Field exposing (Align(..), ChoiceId(..), ChoiceRecord, Currency, Field, FieldType(..), NumSign(..), NumberFormat, NumberMode(..), Values(..), choiceIdToRawString, choiceIdToString, currencyFromString, decoder, encodeChoiceId, floatToString, localeForLanguage, standardFloat, standardInt, stringToChoiceId)

import Dict exposing (Dict)
import FormatNumber as FNum
import FormatNumber.Locales as FNL
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as DecodeX
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Money


type alias Field =
    { id : String
    , label : String
    , ofType : FieldType
    , values : Values
    , isFormula : Bool
    }


type alias Currency =
    Money.Currency



-- type Values = VInt (List Int) | VFloat


type FieldType
    = Text Bool -- mulitligne
    | Float NumberFormat
    | Int NumberFormat
    | Bool
    | Date
    | DateTime
    | ToOne String
    | ToMany String
    | Ref (List ChoiceRecord)
    | Choice (List ChoiceRecord)
    | ChoiceList (List ChoiceRecord)
    | Unknow


standardFloat =
    Float { format = Standard, numSign = Minus, wrap = False, align = Right, decimals = Nothing, maxDecimals = Nothing }


standardInt =
    Int { format = Standard, numSign = Minus, wrap = False, align = Right, decimals = Nothing, maxDecimals = Nothing }


type Values
    = ListInt (List { value : Int, label : String })
    | ListFloat (List { value : Float, label : String })
    | ListString (List { value : String, label : String })


type alias NumberFormat =
    { format : NumberMode
    , numSign : NumSign
    , wrap : Bool
    , align : Align
    , decimals : Maybe Int
    , maxDecimals : Maybe Int
    }


type NumberMode
    = Standard
    | Currency (Maybe Money.Currency)
    | Thousands
    | Percent
    | Scientific


type Align
    = Left
    | Center
    | Right


type NumSign
    = Minus
    | Parens


type alias ChoiceRecord =
    { id : ChoiceId
    , label : String
    , textColor : String
    , backgroundColor : String
    , bold : Bool
    , italic : Bool
    , underline : Bool
    , crossedOut : Bool
    }


type ChoiceId
    = ChoiceString String
    | ChoiceInt Int


choiceIdToString : ChoiceId -> String
choiceIdToString cid =
    case cid of
        ChoiceString str ->
            str

        ChoiceInt int ->
            "_Int:" ++ String.fromInt int


choiceIdToRawString : ChoiceId -> String
choiceIdToRawString cid =
    case cid of
        ChoiceString str ->
            str

        ChoiceInt int ->
            String.fromInt int


stringToChoiceId : String -> Maybe ChoiceId
stringToChoiceId str =
    if String.startsWith "_Int:" str then
        String.dropLeft 5 str
            |> String.toInt
            |> Maybe.map ChoiceInt

    else
        ChoiceString str
            |> Just


encodeChoiceId : ChoiceId -> Value
encodeChoiceId cid =
    case cid of
        ChoiceString str ->
            Encode.string str

        ChoiceInt int ->
            Encode.int int


decoder : ChoiceRecord -> Decoder Field
decoder defaultChoice =
    Decode.map5
        (\id label ofType isFormula formula -> { id = id, label = label, ofType = ofType, isFormula = isFormula, formula = formula })
        (Decode.field "colId" Decode.string)
        (Decode.field "label" Decode.string)
        (fieldTypeDecoder defaultChoice)
        (Decode.field "isFormula" Decode.bool)
        (Decode.field "formula" Decode.string)
        |> Decode.andThen
            (\f ->
                Decode.map
                    (\values ->
                        { id = f.id
                        , label = f.label
                        , ofType = f.ofType
                        , values = values
                        , isFormula = f.isFormula && (not <| String.isEmpty f.formula)
                        }
                    )
                    (Decode.maybe (Decode.field "values" (valuesDecoderFor f.ofType))
                        |> Decode.map (Maybe.withDefault (emptyValuesFor f.ofType))
                    )
            )


valuesDecoder decdr vlist =
    Decode.map2 (\value label -> { value = value, label = label })
        (Decode.field "value" decdr)
        (Decode.field "label" Decode.string)
        |> Decode.maybe
        |> Decode.list
        |> Decode.map (List.filterMap identity >> vlist)


valuesDecoderFor ofType =
    case ofType of
        Text _ ->
            valuesDecoder Decode.string ListString

        Float _ ->
            valuesDecoder Decode.float ListFloat

        Int _ ->
            valuesDecoder Decode.int ListInt

        Bool ->
            valuesDecoder Decode.int ListInt

        Date ->
            valuesDecoder Decode.int ListInt

        DateTime ->
            valuesDecoder Decode.int ListInt

        ToOne _ ->
            valuesDecoder Decode.int ListInt

        ToMany _ ->
            valuesDecoder Decode.int ListInt

        Ref _ ->
            valuesDecoder Decode.int ListInt

        Choice _ ->
            valuesDecoder Decode.string ListString

        ChoiceList _ ->
            valuesDecoder Decode.string ListString

        Unknow ->
            Decode.succeed [] |> Decode.map ListInt


emptyValuesFor ofType =
    case ofType of
        Text _ ->
            ListString []

        Float _ ->
            ListFloat []

        Int _ ->
            ListInt []

        Bool ->
            ListInt []

        Date ->
            ListInt []

        DateTime ->
            ListInt []

        ToOne _ ->
            ListInt []

        ToMany _ ->
            ListInt []

        Ref _ ->
            ListInt []

        Choice _ ->
            ListString []

        ChoiceList _ ->
            ListString []

        Unknow ->
            ListString []



-- |> required "values" decodeValues


fieldTypeDecoder : ChoiceRecord -> Decoder FieldType
fieldTypeDecoder defaultChoice =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                let
                    radical =
                        if String.startsWith "Ref:" t then
                            "Ref"

                        else if String.startsWith "DateTime:" t then
                            "DateTime"

                        else
                            t
                in
                case radical of
                    "Text" ->
                        Decode.map Text <|
                            Decode.oneOf [ Decode.at [ "widgetOptions", "wrap" ] Decode.bool, Decode.succeed False ]

                    "Numeric" ->
                        Decode.field "widgetOptions" numberFormatDecoder
                            |> Decode.map Float

                    "Int" ->
                        Decode.field "widgetOptions" numberFormatDecoder
                            |> Decode.map Int

                    "Bool" ->
                        Decode.succeed Bool

                    "Date" ->
                        Decode.succeed Date

                    "DateTime" ->
                        Decode.succeed DateTime

                    "Ref" ->
                        Decode.map Ref
                            (Decode.oneOf
                                [ Decode.field "references" <| Decode.list (refDecoder defaultChoice)
                                , Decode.succeed []
                                ]
                            )

                    "Choice" ->
                        Decode.map2
                            (\chl opts ->
                                List.map
                                    (\ch ->
                                        Dict.get ch opts
                                            |> Maybe.withDefault
                                                { defaultChoice
                                                    | id = ChoiceString ch
                                                    , label = ch
                                                }
                                    )
                                    chl
                                    |> Choice
                            )
                            (Decode.at [ "widgetOptions", "choices" ] (Decode.list Decode.string))
                            (Decode.at [ "widgetOptions", "choiceOptions" ] (Decode.andThen (choicesDecoder defaultChoice) DecodeX.keys))

                    -- "ChoiceList" ->
                    --     Decode.succeed <| FChoiceList []
                    _ ->
                        Decode.succeed Unknow
            )


numberFormatDecoder : Decoder NumberFormat
numberFormatDecoder =
    Decode.map6 NumberFormat
        (Decode.oneOf [ numberModeDecoder, Decode.succeed Standard ])
        (Decode.oneOf [ Decode.field "numSign" numSignDecoder, Decode.succeed Minus ])
        (Decode.oneOf [ Decode.field "wrap" Decode.bool, Decode.succeed False ])
        (Decode.oneOf [ Decode.field "alignment" alignDecoder, Decode.succeed Right ])
        (Decode.maybe (Decode.field "decimals" Decode.int))
        (Decode.maybe (Decode.field "maxDecimals" Decode.int))


numberModeDecoder : Decoder NumberMode
numberModeDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "numMode" Decode.string)
        (Decode.maybe <| Decode.field "currency" Decode.string)
        |> Decode.andThen
            (\( str, maybe ) ->
                case ( str, maybe ) of
                    ( "currency", Just curstr ) ->
                        Decode.succeed (Currency (Money.fromString curstr))

                    ( "currency", Nothing ) ->
                        Decode.succeed (Currency Nothing)

                    ( "decimal", _ ) ->
                        Decode.succeed Thousands

                    ( "percent", _ ) ->
                        Decode.succeed Percent

                    ( "scientific", _ ) ->
                        Decode.succeed Scientific

                    _ ->
                        Decode.succeed Standard
            )


numSignDecoder : Decoder NumSign
numSignDecoder =
    Decode.string
        |> Decode.map
            (\str ->
                if str == "parens" then
                    Parens

                else
                    Minus
            )


alignDecoder : Decoder Align
alignDecoder =
    Decode.string
        |> Decode.map
            (\str ->
                case str of
                    "left" ->
                        Left

                    "center" ->
                        Center

                    _ ->
                        Right
            )


choicesDecoder : ChoiceRecord -> List String -> Decoder (Dict String ChoiceRecord)
choicesDecoder defaultChoice keys =
    List.map (\key -> Decode.field key (choiceDecoder defaultChoice key)) keys
        |> DecodeX.combine
        |> Decode.map (List.map (\c -> ( c.label, c )) >> Dict.fromList)


choiceDecoder : ChoiceRecord -> String -> Decoder ChoiceRecord
choiceDecoder defaultChoice label =
    Decode.succeed ChoiceRecord
        |> hardcoded (ChoiceString label)
        |> hardcoded label
        |> optional "textColor" Decode.string defaultChoice.textColor
        |> optional "fillColor" Decode.string defaultChoice.backgroundColor
        |> optional "fontBold" Decode.bool defaultChoice.bold
        |> optional "fontItalic" Decode.bool defaultChoice.italic
        |> optional "fontUnderline" Decode.bool defaultChoice.underline
        |> optional "fontStrikethrough" Decode.bool defaultChoice.crossedOut


refDecoder : ChoiceRecord -> Decoder ChoiceRecord
refDecoder defaultChoice =
    Decode.succeed ChoiceRecord
        |> required "id" (Decode.map ChoiceInt Decode.int)
        |> required "label" Decode.string
        |> hardcoded defaultChoice.textColor
        |> hardcoded defaultChoice.backgroundColor
        |> hardcoded defaultChoice.bold
        |> hardcoded defaultChoice.italic
        |> hardcoded defaultChoice.underline
        |> hardcoded defaultChoice.crossedOut


localeForLanguage : String -> FNL.Locale
localeForLanguage str =
    case String.left 2 str |> String.toLower of
        "fr" ->
            FNL.frenchLocale

        _ ->
            FNL.usLocale


currencyFromString : String -> Money.Currency
currencyFromString str =
    Money.fromString str
        |> Maybe.withDefault Money.EUR


floatToString : FNL.Locale -> Money.Currency -> NumberFormat -> Float -> String
floatToString locale defaultCurrency format float =
    let
        fper =
            if format.format == Percent then
                float * 100

            else
                float

        float_ =
            Maybe.map
                (\d ->
                    let
                        pow =
                            10 ^ toFloat d
                    in
                    (fper * pow |> round |> toFloat) / pow
                )
                format.maxDecimals
                |> Maybe.withDefault fper
    in
    FNum.format
        { locale
            | decimals =
                case format.decimals of
                    Just d ->
                        FNL.Min d

                    Nothing ->
                        locale.decimals
            , negativePrefix =
                if format.numSign == Parens then
                    "("

                else
                    "-"
            , negativeSuffix =
                if format.numSign == Parens then
                    ")"

                else
                    ""
            , thousandSeparator =
                if format.format == Standard then
                    ""

                else
                    locale.thousandSeparator
        }
        float_
        ++ (case format.format of
                Currency cur ->
                    " " ++ Money.toSymbol (Maybe.withDefault defaultCurrency cur)

                Percent ->
                    " %"

                _ ->
                    ""
           )
