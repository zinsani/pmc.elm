module Shared.UI exposing (..)

import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, i, p, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types exposing (Id(..), MasterMsg(..), Msg(..), PlayerManager)


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


myButton : msg -> String -> String -> Html msg
myButton msg label buttonType =
    button
        [ classList
            [ Bulma.button
            , buttonType
            , Bulma.mx1
            ]
        , onClick msg
        ]
        [ text label
        ]


viewControlButtonGroup : PlayerManager -> Maybe Id -> Html msg
viewControlButtonGroup playerManager playerId =
    div
        [ classList
            [ Bulma.buttons
            , Bulma.hasAddons
            , Bulma.my0
            , Bulma.isFull
            , Bulma.isRight
            ]
        ]
        [ button [ classList [ Bulma.button, Bulma.isSmall ] ]
            [ span [ class Bulma.icon ] [ i [ class "fa fa-undo" ] [] ] ]
        , button [ classList [ Bulma.button, Bulma.isSmall ] ]
            [ span [ classList [ Bulma.icon, Bulma.hasTextDanger ] ]
                [ i [ class "fa fa-stop" ] [] ]
            ]
        , button [ classList [ Bulma.button, Bulma.isSmall ] ]
            [ span [ classList [ Bulma.icon, Bulma.hasTextPrimary ] ]
                [ i [ class "fa fa-play" ] [] ]
            ]
        , button [ classList [ Bulma.button, Bulma.isSmall ] ]
            [ span [ classList [ Bulma.icon, Bulma.hasTextSuccess ] ]
                [ i [ class "fa fa-upload" ] [] ]
            ]
        ]


selectButton : a -> (a -> msg) -> Html msg
selectButton id msg =
    button
        [ classList
            [ Bulma.isPulledRight
            , Bulma.button
            , Bulma.isSmall
            , Bulma.isWhite
            ]
        , onClick (msg id)
        ]
        [ span [ classList [ Bulma.icon ] ]
            [ i [ class "fa fa-chevron-right" ] [] ]
        ]
