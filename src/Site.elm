module Site exposing (..)

import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, h1, i, input, label, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, hidden, placeholder, style, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onDoubleClick, onInput)
import Types exposing (Site, SiteListMsg(..), Sites)


viewSiteList : Sites -> Html SiteListMsg
viewSiteList model =
    div [ class Bulma.container ]
        [ div [ classList [ Bulma.columns, Bulma.isCentered ] ]
            [ div [ classList [ Bulma.column, Bulma.box, Bulma.isHalf ] ]
                [ div [ classList [ Bulma.mb4, Bulma.px3 ] ]
                    [ h1 [ class Bulma.title ] [ text "Site List" ]
                    ]
                , div [ classList [ Bulma.field, Bulma.hasAddons, Bulma.px3 ] ]
                    [ div [ classList [ Bulma.control, Bulma.isExpanded ] ]
                        [ input
                            [ class Bulma.input
                            , type_ "text"
                            , placeholder "Site Name"
                            , onInput InputSiteName
                            , value model.newSiteName
                            ]
                            []
                        ]
                    , div [ class Bulma.control ]
                        [ button
                            [ classList [ Bulma.button, Bulma.isPrimary ]
                            , onClick <| ClickNewSite model.newSiteName
                            ]
                            [ text "New"
                            ]
                        ]
                    ]
                , viewContent model
                , div [ class Bulma.hasTextRight, hidden <| List.isEmpty model.list ]
                    [ label [ classList [ Bulma.checkbox, Bulma.isToggle ] ]
                        [ input
                            [ type_ "checkbox", onCheck ToggleSaveSelection, checked <| model.saveSelection ]
                            []
                        , text "Save Selection"
                        ]
                    ]
                ]
            ]
        ]


viewContent : Sites -> Html SiteListMsg
viewContent model =
    if List.isEmpty model.list then
        div [ classList [ Bulma.mt2, Bulma.px3 ] ] [ text "Welcome to PlayerManagerClient. Why don't you create new site?" ]

    else
        div [ class Bulma.mt2 ]
            [ table [ classList [ Bulma.table, Bulma.isFullwidth ] ]
                [ thead []
                    [ th [ class Bulma.hasTextCentered ]
                        [ text "No."
                        ]
                    , th [ style "width" "60%" ]
                        [ text "Name"
                        ]
                    , th [ class Bulma.hasTextCentered ]
                        [ text "Open"
                        ]
                    ]
                , List.sortBy .id model.list
                    |> List.map
                        (viewSite model.editingSite)
                    |> tbody []
                ]
            ]


viewSite : Maybe Site -> Site -> Html SiteListMsg
viewSite editingSite site =
    tr []
        [ td [ class Bulma.hasTextCentered ] [ text <| String.fromInt site.id ]
        , viewSiteName editingSite site
        , td [ class Bulma.hasTextCentered ]
            [ div [ class Bulma.field ]
                [ div [ class Bulma.control ]
                    [ button
                        [ classList [ Bulma.mr1, Bulma.button, Bulma.isPrimary, Bulma.isSmall ]
                        , onClick <| ClickOpenSite site.id
                        ]
                        [ span [ class Bulma.icon ]
                            [ i [ class "fa fa-search-plus" ] []
                            ]
                        ]
                    , button
                        [ classList [ Bulma.button, Bulma.isDanger, Bulma.isSmall ]
                        , onClick <| ClickDeleteSite site.id
                        ]
                        [ span [ class Bulma.icon ]
                            [ i [ class "fa fa-trash" ] []
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewSiteName : Maybe Site -> Site -> Html SiteListMsg
viewSiteName editingSite site =
    case editingSite of
        Nothing ->
            viewNormalSiteName site

        Just editing ->
            if editing.id == site.id then
                viewEditingSiteName site

            else
                viewNotEditingSiteName site


viewEditingSiteName : Site -> Html SiteListMsg
viewEditingSiteName site =
    td []
        [ div [ class Bulma.field ]
            [ div [ class Bulma.control ]
                [ input
                    [ onInput EditingSiteName
                    , value site.name
                    , onBlur <| EndEditSite site
                    ]
                    []
                ]
            ]
        ]


viewNotEditingSiteName : Site -> Html msg
viewNotEditingSiteName site =
    td [] [ span [] [ text site.name ] ]


viewNormalSiteName : Site -> Html SiteListMsg
viewNormalSiteName site =
    td [ onDoubleClick <| StartEditSite site.id ] [ span [] [ text site.name ] ]
