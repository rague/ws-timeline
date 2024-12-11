module View.Segment exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type Style
    = Menu
    | Radio


menu =
    view Menu


radio =
    view Radio


view : Style -> (value -> msg) -> value -> List { value : value, label : Html msg } -> Html msg
view style toMsg sel opts =
    Html.div
        [ case style of
            Menu ->
                HA.class "tabs-menu"

            Radio ->
                HA.class "tabs-radio"
        ]
    <|
        List.map
            (\opt ->
                Html.span
                    [ if opt.value == sel then
                        HA.class "selected"

                      else
                        HA.class ""
                    , HE.onClick (toMsg opt.value)
                    ]
                    [ opt.label ]
            )
            opts
