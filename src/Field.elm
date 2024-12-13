module Field exposing (ChoiceId(..), ChoiceRecord, Field, FieldType(..), NumberFormat(..), Values(..), choiceIdToRawString, choiceIdToString, decoder, encodeChoiceId, stringToChoiceId)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as DecodeX
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode


type alias Field =
    { id : String
    , label : String
    , position : Int
    , ofType : FieldType
    , values : Values
    , isFormula : Bool
    }



-- type Values = VInt (List Int) | VFloat


type FieldType
    = Text Bool -- mulitligne
    | Float NumberFormat Bool Int Int
    | Int NumberFormat Bool
    | Bool
    | Date
    | DateTime
    | ToOne String
    | ToMany String
    | Ref (List ChoiceRecord)
    | Choice (List ChoiceRecord)
    | ChoiceList (List ChoiceRecord)
    | Unknow


type Values
    = ListInt (List { value : Int, label : String })
    | ListFloat (List { value : Float, label : String })
    | ListString (List { value : String, label : String })


type NumberFormat
    = Standard
    | Currency String
    | Thousands
    | Exp


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
            "___Int:" ++ String.fromInt int


choiceIdToRawString : ChoiceId -> String
choiceIdToRawString cid =
    case cid of
        ChoiceString str ->
            str

        ChoiceInt int ->
            String.fromInt int


stringToChoiceId : String -> Maybe ChoiceId
stringToChoiceId str =
    if String.startsWith "___Int:" str then
        String.dropLeft 7 str
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
    Decode.map4
        (\id label ofType isFormula -> { id = id, label = label, ofType = ofType, isFormula = isFormula })
        (Decode.field "colId" Decode.string)
        (Decode.field "label" Decode.string)
        (fieldTypeDecoder defaultChoice)
        (Decode.field "isFormula" Decode.bool)
        |> Decode.andThen
            (\f ->
                Decode.map
                    (\values ->
                        { id = f.id
                        , label = f.label
                        , position = 0
                        , ofType = f.ofType
                        , values = values
                        , isFormula = f.isFormula
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

        Float _ _ _ _ ->
            valuesDecoder Decode.float ListFloat

        Int _ _ ->
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

        Float _ _ _ _ ->
            ListFloat []

        Int _ _ ->
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
                        Decode.succeed <| Float Standard False 0 10

                    "Int" ->
                        Decode.succeed <| Int Standard False

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
