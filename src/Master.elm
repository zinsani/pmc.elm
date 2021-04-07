module Master exposing (subscriptions, update, view)

import Api exposing (defaultPlayerManager, siteEncoder)
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, h1, i, input, label, p, section, span, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Types exposing (FetchModel(..), FetchingMsg(..), Id(..), MasterMsg(..), Model(..), Msg(..), Player, PlayerManager, Site)


subscriptions : Site -> Sub MasterMsg
subscriptions _ =
    Sub.none


update : MasterMsg -> ( Site, List PlayerManager ) -> ( Model, Cmd Msg )
update msg ( site, playerManagers ) =
    case msg of
        ClickNewPlayerManager ->
            let
                editingPM =
                    Maybe.withDefault
                        defaultPlayerManager
                        site.lastInputPM
            in
            ( MainPage
                { site | editingPM = Just editingPM }
                playerManagers
            , Cmd.none
            )

        InputPMName name ->
            let
                editingPM =
                    case site.editingPM of
                        Just pm ->
                            Just (Debug.log "editingPM" { pm | name = name })

                        Nothing ->
                            Just defaultPlayerManager
            in
            ( MainPage { site | editingPM = editingPM } playerManagers
            , Cmd.none
            )

        ClickSubmit ->
            case site.editingPM of
                Just editingPM ->
                    case editingPM.id of
                        TempId ->
                            ( MainPage site playerManagers
                            , Api.createId GotNewIdOfPM |> Cmd.map MasterMsg
                            )

                        Id _ ->
                            ( Fetch (UpdateSite site.id)
                            , Api.modifyPlayerManager FetchingSite
                                editingPM
                                site
                                |> Cmd.map FetchingMsg
                            )

                Nothing ->
                    ( MainPage site playerManagers, Cmd.none )

        GotNewIdOfPM newId ->
            case site.editingPM of
                Nothing ->
                    ( MainPage site playerManagers, Cmd.none )

                Just editingPM ->
                    let
                        newPM =
                            { editingPM | id = newId }
                    in
                    ( Fetch (UpdateSite site.id)
                    , Api.createNewPM FetchingSite newPM site
                        |> Cmd.map FetchingMsg
                    )

        SelectPM selectedId ->
            ( MainPage site playerManagers, Cmd.none )

        ClickCancel ->
            ( MainPage { site | editingPM = Nothing } playerManagers
            , Cmd.none
            )

        ClickDeletePM pmId ->
            ( Fetch (UpdateSite site.id)
            , Api.deletePlayerManager FetchingSite pmId site playerManagers
                |> Cmd.map FetchingMsg
            )

        BackToSiteList ->
            ( Fetch FetchSites, Api.fetch () )

        ToggleEditMode ->
            ( MainPage { site | listEditing = not site.listEditing } playerManagers
            , Cmd.none
            )

        ClickNewPlayer ->
            Debug.todo "branch 'ClickNewPlayer' not implemented"

        InputPName _ ->
            Debug.todo "branch 'InputPName _' not implemented"


view : Site -> List PlayerManager -> Html MasterMsg
view site playerManagers =
    case site.editingPM of
        Just editingPM ->
            div [ class Bulma.container ]
                [ viewActionBar
                , section [ class Bulma.section ]
                    [ viewPMEdit editingPM
                    ]
                ]

        Nothing ->
            div [ class Bulma.container ]
                [ viewActionBar
                , section [ class Bulma.section ]
                    [ viewPlayerManagers site playerManagers ]
                ]


viewActionBar : Html MasterMsg
viewActionBar =
    let
        viewTitle =
            span [ class Bulma.isSize4 ]
                [ text "Player Manager Client"
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
        [ backButton BackToSiteList
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


viewPlayerManagers : Site -> List PlayerManager -> Html MasterMsg
viewPlayerManagers site playerManagers =
    let
        addNewButton classes =
            [ Bulma.isPrimary ]
                ++ classes
                |> String.join " "
                |> myButton
                    ClickNewPlayerManager
                    "Add"

        editButton =
            String.join " " [ Bulma.isWarning, Bulma.isSmall ]
                |> myButton ToggleEditMode "Edit"
    in
    case List.length site.playerManagers of
        0 ->
            div [ class Bulma.container ]
                [ p [ class Bulma.isSize5 ]
                    [ text "Please create a new PlayerManager." ]
                , addNewButton []
                ]

        _ ->
            div [ class Bulma.container ]
                [ div
                    [ classList
                        [ Bulma.block
                        ]
                    ]
                    [ table [ classList [ Bulma.table, Bulma.isFullwidth ] ]
                        [ thead []
                            [ td [] [ text "No." ]
                            , td [ style "width" "30%" ] [ text "Name" ]
                            , td [ style "width" "30%" ] [ text "Location" ]
                            , td [ style "width" "30%" ]
                                [ div [ class Bulma.buttons ]
                                    [ addNewButton [ Bulma.isSmall ]
                                    , editButton
                                    ]
                                ]
                            ]
                        , tbody []
                            (playerManagers
                                |> List.filter
                                    (\pm ->
                                        List.any (\a -> a == pm.id)
                                            site.playerManagers
                                    )
                                |> List.indexedMap
                                    (viewPlayerManager
                                        site.listEditing
                                    )
                            )
                        ]
                    ]
                ]


viewPlayerManager : Bool -> Int -> PlayerManager -> Html MasterMsg
viewPlayerManager listEditing index pm =
    let
        head =
            if listEditing then
                div [ class Bulma.control ]
                    [ button
                        [ classList
                            [ Bulma.button
                            , Bulma.isSmall
                            , Bulma.hasTextDanger
                            ]
                        , onClick (ClickDeletePM pm.id)
                        ]
                        [ span [ class Bulma.icon ]
                            [ i [ class "fa fa-trash" ] [] ]
                        ]
                    ]

            else
                span [ class Bulma.isSize6 ]
                    [ 1 + index |> String.fromInt |> text ]
    in
    tr []
        [ td []
            [ head
            ]
        , td []
            [ span [ class Bulma.isSize6 ]
                [ text pm.name ]
            ]
        , td []
            [ span [ class Bulma.isSize6 ]
                [ pm.ipaddress ++ ":" ++ String.fromInt pm.port_ |> text ]
            ]
        , td []
            [ pmControlButtonGroup pm
            ]
        ]


pmControlButtonGroup : PlayerManager -> Html MasterMsg
pmControlButtonGroup pm =
    div [ class Bulma.columns ]
        [ div [ classList [ Bulma.column, Bulma.buttons, Bulma.hasAddons, Bulma.my0, Bulma.isNarrow ] ]
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
            , button
                [ classList
                    [ Bulma.isPulledRight
                    , Bulma.button
                    , Bulma.isSmall
                    , Bulma.isWhite
                    ]
                , onClick (SelectPM pm.id)
                ]
                [ span [ classList [ Bulma.icon ] ]
                    [ i [ class "fa fa-chevron-right" ] [] ]
                ]
            ]
        ]


myButton : MasterMsg -> String -> String -> Html MasterMsg
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


viewPMEdit : PlayerManager -> Html MasterMsg
viewPMEdit pm =
    div [ classList [ Bulma.container ] ]
        [ inputText "Name" pm.name InputPMName
        , div []
            [ myButton ClickSubmit "Submit" Bulma.isPrimary
            , myButton ClickCancel "Cancel" Bulma.isDanger
            ]
        ]


inputText : String -> String -> (String -> msg) -> Html msg
inputText label_ value_ msg =
    div [ class Bulma.field ]
        [ label [ class Bulma.label ]
            [ text label_
            ]
        , div [ class Bulma.control ]
            [ input
                [ classList
                    [ Bulma.input
                    ]
                , type_ "text"
                , onInput msg
                , value value_
                ]
                []
            ]
        ]


playerControlButtonGroup : Player -> Html msg
playerControlButtonGroup player =
    div [ classList [ Bulma.buttons, Bulma.hasAddons ] ]
        [ button [ classList [ Bulma.button, Bulma.isSmall, Bulma.isRounded ] ]
            [ span [ class Bulma.icon ] [ i [ class "fa fa-undo" ] [] ] ]
        , button [ classList [ Bulma.button, Bulma.isSmall, Bulma.isRounded ] ]
            [ span [ classList [ Bulma.icon, Bulma.hasTextDanger ] ]
                [ i [ class "fa fa-stop" ] [] ]
            ]
        , button [ classList [ Bulma.button, Bulma.isSmall, Bulma.isRounded ] ]
            [ span [ classList [ Bulma.icon, Bulma.hasTextPrimary ] ]
                [ i [ class "fa fa-play" ] [] ]
            ]
        , button [ classList [ Bulma.button, Bulma.isSmall, Bulma.isRounded ] ]
            [ span [ classList [ Bulma.icon, Bulma.hasTextSuccess ] ]
                [ i [ class "fa fa-upload" ] [] ]
            ]
        ]