module Timeline.Event exposing
    ( Event
    , clickEvent
    , keyEvent
    , leftButton
    , mouseDownEvent
    , mouseUpEvent
    , moveEvent
    , overEvent
    , wheelEvent, moveY0Event, moveX0Event
    )

import Html exposing (Attribute)
import Html.Events exposing (custom, on)
import Json.Decode as Json exposing (bool, float, int)
import Json.Decode.Pipeline exposing (optional, required)


type alias Event =
    { x : Float
    , y : Float
    , altKey : Bool
    , ctrlKey : Bool
    , metaKey : Bool
    , shiftKey : Bool
    , dx : Float
    , dy : Float
    , button : Int
    , clientX : Float
    , clientY : Float
    }


leftButton : Int
leftButton =
    0



--relPos : MouseEvent -> Position
--relPos ev =
--    Position (ev.clientPos.x - ev.targetPos.x) (ev.clientPos.y - ev.targetPos.y)


eventDecoder : Json.Decoder Event
eventDecoder =
    Json.succeed Event
        |> required "offsetX" float
        |> required "offsetY" float
        |> required "altKey" bool
        |> required "ctrlKey" bool
        |> required "metaKey" bool
        |> required "shiftKey" bool
        |> optional "deltaX" float 0
        |> optional "deltaY" float 0
        |> optional "button" int 0
        |> required "clientX" float
        |> required "clientY" float



--|> custom DOM.boundingClientRect "target"
--(field "target" DOM.boundingClientRect)


wheelEvent : (Event -> msg) -> Attribute msg
wheelEvent message =
    custom "wheel" <| preventDefault (Json.map message eventDecoder)


moveEvent : (Event -> msg) -> Attribute msg
moveEvent message =
    custom "mousemove" <| preventDefault (Json.map message eventDecoder)

moveY0Event : (Event -> msg) -> Attribute msg
moveY0Event message = 
    custom "mousemove" <| preventDefault (Json.map message (Json.map (\evt -> {evt | y = 0}) eventDecoder ))

moveX0Event : (Event -> msg) -> Attribute msg
moveX0Event message = 
    custom "mousemove" <| preventDefault (Json.map message (Json.map (\evt -> {evt | x = 0}) eventDecoder ))


clickEvent : (Event -> msg) -> Attribute msg
clickEvent message =
    on "click" <| Json.map message eventDecoder


mouseDownEvent : (Event -> msg) -> Attribute msg
mouseDownEvent message =
    custom "mousedown" <| stopPropagation (Json.map message eventDecoder)


mouseUpEvent : (Event -> msg) -> Attribute msg
mouseUpEvent message =
    custom "mouseup" <| preventDefault (Json.map message eventDecoder)


overEvent : (Event -> msg) -> Attribute msg
overEvent message =
    custom "mouseover" <| preventDefault (Json.map message eventDecoder)


keyEvent : (Int -> Json.Decoder msg) -> Attribute msg
keyEvent message =
    custom "keydown" <| preventDefault (Json.andThen message (Json.field "keyCode" Json.int))


preventDefault :
    Json.Decoder msg
    ->
        Json.Decoder
            { message : msg
            , stopPropagation : Bool
            , preventDefault : Bool
            }
preventDefault =
    Json.map
        (\msg ->
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )


stopPropagation :
    Json.Decoder msg
    ->
        Json.Decoder
            { message : msg
            , stopPropagation : Bool
            , preventDefault : Bool
            }
stopPropagation =
    Json.map
        (\msg ->
            { message = msg
            , stopPropagation = True
            , preventDefault = False
            }
        )
