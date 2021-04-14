module Shared.UI exposing (..)

import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, i, p, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


viewHorizontalField : Html msg -> Html msg -> Html msg
viewHorizontalField labelElem bodyElem =
    div [ classList [ Bulma.container, Bulma.pb4 ] ]
        [ div [ classList [ Bulma.field, Bulma.isHorizontal ] ]
            [ div [ classList [ Bulma.fieldLabel, Bulma.isNormal ] ]
                [ labelElem ]
            , div [ classList [ Bulma.fieldBody ] ]
                [ p [ classList [ Bulma.field, Bulma.isExpanded ] ]
                    [ bodyElem ]
                ]
            ]
        ]


viewActionBar : String -> msg -> Html msg
viewActionBar title onBack =
    let
        viewTitle =
            span [ class Bulma.isSize4 ]
                [ text title
                ]

        settingButton : Html msg
        settingButton =
            button
                [ classList
                    [ Bulma.button
                    , "is-pulled-right"
                    ]
                ]
                [ span
                    [ class Bulma.icon
                    ]
                    [ i
                        [ classList
                            [ Bulma.fa
                            , "fa-cog"
                            ]
                        ]
                        []
                    ]
                ]
    in
    div
        [ classList
            [ Bulma.container
            , Bulma.px3
            , Bulma.py3
            , Bulma.isVcentered
            , Bulma.isFull
            , Bulma.hasBackgroundLight
            ]
        ]
        [ backButton onBack
        , viewTitle
        , settingButton
        ]


backButton : msg -> Html msg
backButton msg =
    button
        [ classList [ Bulma.button, Bulma.isRounded, Bulma.isLight, Bulma.mr4 ]
        , onClick msg
        ]
        [ span [ classList [ Bulma.icon, Bulma.isSmall ] ]
            [ i [ classList [ "fa", "fa-arrow-left" ] ] [] ]
        ]
