module Master exposing (subscriptions, update, view)

import Api exposing (defaultPlayerManager)
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, i, label, p, section, span, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Shared.UI exposing (viewActionBar)
import Types exposing (FetchModel(..), FetchingMsg(..), Id(..), MasterMsg(..), Model(..), Msg(..), PlayerManager, Site, UIMsg(..))


subscriptions : Site -> Sub MasterMsg
subscriptions _ =
    Sub.none


update : MasterMsg -> ( Site, List PlayerManager ) -> ( Model, Cmd Msg )
update msg ( site, playerManagers ) =
    case msg of
        UIMsgOnMaster uiMsg ->
            case uiMsg of
                ClickNew ->
                    let
                        editingPM =
                            Maybe.withDefault
                                defaultPlayerManager
                                (site.lastInputPM
                                    |> Debug.log "editing pm"
                                )
                    in
                    ( PlayerManagerEditPage
                        { siteId = site.id, playerManager = editingPM }
                    , Cmd.none
                    )

                ClickDelete pmId ->
                    ( Fetch (UpdateSite site.id)
                    , Api.deletePlayerManager FetchingSite pmId site.id
                        |> Cmd.map FetchingMsg
                    )

                ClickBack ->
                    ( Fetch FetchSites, Api.fetch () )

                _ ->
                    ( MainPage site playerManagers, Cmd.none )

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

        ToggleEditModeOnMaster ->
            ( MainPage { site | listEditing = not site.listEditing } playerManagers
            , Cmd.none
            )


view : Site -> List PlayerManager -> Html Msg
view site playerManagers =
    div [ class Bulma.container ]
        [ viewActionBar ("Site: " ++ site.name) ClickBack |> Html.map(UIMsgOnMaster >> MasterMsg)
        , section [ class Bulma.section ]
            [ viewPlayerManagers site playerManagers ]
        ]




viewPlayerManagers : Site -> List PlayerManager -> Html Msg
viewPlayerManagers site playerManagers =
    let
        addNewButton classes =
            List.concat [ [ Bulma.isPrimary ], classes ]
                |> String.join " "
                |> myButton
                    ClickNew
                    "Add"
                |> Html.map (UIMsgOnMaster >> MasterMsg)

        editButton =
            String.join " " [ Bulma.isWarning, Bulma.isSmall ]
                |> myButton ToggleEditModeOnMaster "Edit"
                |> Html.map MasterMsg
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
                                [ div [ classList [ Bulma.buttons, Bulma.isRight, Bulma.pr5 ] ]
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


viewPlayerManager : Bool -> Int -> PlayerManager -> Html Msg
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
                        , onClick (ClickDelete pm.id)
                        ]
                        [ span [ class Bulma.icon ]
                            [ i [ class "fa fa-trash" ] [] ]
                        ]
                    ]
                    |> Html.map (UIMsgOnMaster >> MasterMsg)

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
            [ viewControlButtonGroup pm
            ]
        ]


viewControlButtonGroup : PlayerManager -> Html Msg
viewControlButtonGroup pm =
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
        |> Html.map MasterMsg


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
