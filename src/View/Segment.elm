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
                HA.class "segment-menu"

            Radio ->
                HA.class "segment-radio"
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


styles =
    """
.segment-radio {
    margin-top: 6px;
    user-select: none;
}

.segment-radio > span {
    cursor: pointer;
    padding: 4px 8px;
    border: 1px solid #BBB;
    border-right: 0px;
    user-select: none;
    &.selected {
        background-color: steelblue;
        color: white;
    }
    &:first-child {
        border-radius: 5px 0 0 5px;
    }
    &:last-child {
        border-right: 1px solid #BBB;
        border-radius: 0 5px 5px 0;
    }

}


.segment-menu {
    width: 100%;
    border-bottom: 1px solid lightgrey;
    user-select: none;
    margin-bottom: 6px
}

.segment-menu > span {
    padding: 12px 8px;
    border-bottom: 1px solid lightgrey;
    display: inline-block;
    color: steelblue;
    cursor: pointer;
    opacity: 0.7;

    position: relative;
    top: 1px;

    &:hover {
        opacity: 1;
    }
    &.selected {
        color: black;
        border-color: black;
        opacity: 1;
    }
}
"""
