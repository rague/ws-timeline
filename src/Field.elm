module Field exposing (ChoiceId(..), ChoiceRecord, Field, FieldType(..), NumberFormat(..), choiceIdToString, decoder, encodeChoiceId)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as DecodeX
import Json.Decode.Pipeline as Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode


type alias Field =
    { id : String
    , label : String
    , position : Int
    , ofType : FieldType
    }


type FieldType
    = Text Bool -- mulitligne
    | Float NumberFormat Bool Int Int
    | Int NumberFormat Bool
    | Bool
    | Date
    | DateTime
    | ToOne String
    | ToMany String
    | Choice (List ChoiceRecord)
    | ChoiceList (List ChoiceRecord)
    | Unknow


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
            String.fromInt int


encodeChoiceId : ChoiceId -> Value
encodeChoiceId cid =
    case cid of
        ChoiceString str ->
            Encode.string str

        ChoiceInt int ->
            Encode.int int


decoder : ChoiceRecord -> Decoder Field
decoder defaultChoice =
    Decode.succeed Field
        |> required "colId" Decode.string
        |> required "label" Decode.string
        |> hardcoded 0
        |> Pipeline.custom (fieldTypeDecoder defaultChoice)


fieldTypeDecoder : ChoiceRecord -> Decoder FieldType
fieldTypeDecoder defaultChoice =
    Decode.andThen
        (\t ->
            let
                radical =
                    if String.startsWith "Ref:" t then
                        "Ref"

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

                "Datetime" ->
                    Decode.succeed DateTime

                "Ref" ->
                    Decode.map Choice
                        (Decode.field "references" <| Decode.list (refDecoder defaultChoice))

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
        (Decode.field "type" Decode.string)


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
