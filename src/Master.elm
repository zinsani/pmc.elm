module Master exposing (subscriptions, update, view)

import Api exposing (defaultPlayerManager)
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, i, label, p, section, span, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Types exposing (FetchModel(..), FetchingMsg(..), Id(..), MasterMsg(..), Model(..), Msg(..), PlayerManager, Site)


subscriptions : Site -> Sub MasterMsg
subscriptions _ =
    Sub.none


update : MasterMsg -> ( Site, List PlayerManager ) -> ( Model, Cmd Msg )
update msg ( site, playerManagers ) =
    case msg of
        ClickNewPM ->
            let
                editingPM =
                    Maybe.withDefault
                        defaultPlayerManager
                        (site.lastInputPM
                        |> Debug.log "editing pm")
            in
            ( PlayerManagerEditPage
                { siteId = site.id, playerManager = editingPM }
            , Cmd.none
            )

        SelectPM selectedId ->
            let
                maybePM =
                    playerManagers
                        |> List.filter (\x -> x.id == selectedId)
                        |> List.head
            in
            case maybePM of
                Just pm ->
                    ( DetailPage
                        { siteId = site.id
                        , playerManager = pm
                        , editingPlayer = Nothing
                        , selectedPlayerId = Nothing
                        , lastInputPlayer = Nothing
                        , listEditing = False
                        }
                    , Cmd.none
                    )

                Nothing ->
                    ( MainPage site playerManagers, Cmd.none )

        ClickDeletePM pmId ->
            ( Fetch (UpdateSite site.id)
            , Api.deletePlayerManager FetchingSite pmId site playerManagers
                |> Cmd.map FetchingMsg
            )

        BackToSiteList ->
            ( Fetch FetchSites, Api.fetch () )

        ToggleEditModeOnMaster ->
            ( MainPage { site | listEditing = not site.listEditing } playerManagers
            , Cmd.none
            )


view : Site -> List PlayerManager -> Html MasterMsg
view site playerManagers =
    -- case site.editingPM of
    --     Just editingPM ->
    --         let
    --             editForm =
    --                 viewPMEdit editingPM
    --                     |> Html.map FormInput
    --         in
    --         div [ class Bulma.container ]
    --             [ viewActionBar site
    --             , section [ class Bulma.section ]
    --                 [ editForm
    --                 ]
    --             ]
    --     Nothing ->
    div [ class Bulma.container ]
        [ viewActionBar site
        , section [ class Bulma.section ]
            [ viewPlayerManagers site playerManagers ]
        ]


viewActionBar : Site -> Html MasterMsg
viewActionBar model =
    let
        viewTitle =
            span [ class Bulma.isSize4 ]
                [ "Site: " ++ model.name |> text
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
            List.concat [ [ Bulma.isPrimary ], classes ]
                |> String.join " "
                |> myButton
                    ClickNewPM
                    "Add"

        editButton =
            String.join " " [ Bulma.isWarning, Bulma.isSmall ]
                |> myButton ToggleEditModeOnMaster "Edit"
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
