module MDView exposing (storeModel, subscriptions, update, view)

import Api exposing (defaultPlayerManager, pmModelEncoder)
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, h1, i, input, label, p, section, span, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Types exposing (FetchingModel(..), FetchingMsg(..), Id(..), Model(..), Msg(..), PMModel, PMMsg(..), Player, PlayerManager)


subscriptions : PMModel -> Sub PMMsg
subscriptions _ =
    Sub.none


update : PMMsg -> PMModel -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickNewPlayerManager ->
            let
                editingPM : PlayerManager
                editingPM =
                    case model.lastInputPM of
                        Just existingItem ->
                            Debug.log "AddNewPlayerManager:: existing"
                                existingItem

                        Nothing ->
                            Debug.log "AddNewPlayerManager:: new"
                                defaultPlayerManager
            in
            ( MainPage { model | editingPM = Just editingPM }, Cmd.none )

        InputPMName name ->
            let
                editingPM =
                    case model.editingPM of
                        Just pm ->
                            Just (Debug.log "editingPM" { pm | name = name })

                        Nothing ->
                            Just defaultPlayerManager
            in
            ( MainPage { model | editingPM = editingPM }, Cmd.none )

        -- AddNewPlayerManager pm ->
        --     ( Fetching (FetchingSite model.siteId)
        --     , Api.createNewPM FetchingPMModel pm model
        --         |> Cmd.map FetchingMsg
        --     )
        ClickSubmit ->
            case model.editingPM of
                Just editingPM ->
                    case editingPM.id of
                        TempId ->
                            ( MainPage model
                            , Api.createId GotNewId |> Cmd.map PMMsg
                            )

                        Id _ ->
                            ( Fetching (UpdatingSite model.siteId)
                            , Api.modifyPMList FetchingPMModel
                                (model.pmList
                                    |> List.map
                                        (\pm ->
                                            if pm.id == editingPM.id then
                                                editingPM

                                            else
                                                pm
                                        )
                                )
                                editingPM
                                model
                                |> Cmd.map FetchingMsg
                            )

                Nothing ->
                    ( MainPage model, Cmd.none )

        GotNewId newId ->
            case model.editingPM of
                Nothing ->
                    ( MainPage model, Cmd.none )

                Just editingPM ->
                    let
                        newPM =
                            { editingPM | id = newId }
                    in
                    ( Fetching (UpdatingSite model.siteId)
                    , Api.createNewPM FetchingPMModel newPM model
                        |> Cmd.map FetchingMsg
                    )

        ClickCancel ->
            ( MainPage
                { model
                    | editingPM = Nothing
                }
            , Cmd.none
            )

        ClickDeletePM pmId ->
            ( Fetching (UpdatingSite model.siteId)
            , Api.deletePM FetchingPMModel pmId model |> Cmd.map FetchingMsg
            )

        BackToSiteList ->
            ( Fetching FetchingSiteList, Api.fetch () )

        ToggleEditMode ->
            ( MainPage { model | listEditing = not model.listEditing }, Cmd.none )

        ClickNewPlayer ->
            Debug.todo "branch 'ClickNewPlayer' not implemented"

        InputPName _ ->
            Debug.todo "branch 'InputPName _' not implemented"


storeModel : PMModel -> Cmd msg
storeModel model =
    Api.storePMModel <| pmModelEncoder model


view : PMModel -> Html PMMsg
view model =
    case model.editingPM of
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
                    [ viewPMList model ]
                ]


viewActionBar : Html PMMsg
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


viewPMList : PMModel -> Html PMMsg
viewPMList model =
    let
        addNewButton =
            myButton ClickNewPlayerManager "Add" Bulma.isPrimary
    in
    case List.length model.pmList of
        0 ->
            div [ class Bulma.container ]
                [ p [ class Bulma.isSize5 ]
                    [ text "Please create a new PlayerManager." ]
                , addNewButton
                ]

        _ ->
            div [ class Bulma.container ]
                [ div
                    [ classList
                        [ Bulma.block
                        ]
                    ]
                    [ div [ class Bulma.buttons ]
                        [ addNewButton
                        , myButton ToggleEditMode "Edit" Bulma.isWarning
                        ]
                    , table [ classList [ Bulma.table, Bulma.isFullwidth ] ]
                        [ thead []
                            [ td [] [ text "No." ]
                            , td [ style "width" "35%" ] [ text "Name" ]
                            , td [ style "width" "35%" ] [ text "Location" ]
                            , td [ style "width" "25%" ] [ text "Controls" ]
                            ]
                        , tbody []
                            (model.pmList
                                |> List.indexedMap
                                    (viewPlayerManager
                                        model.listEditing
                                    )
                            )
                        ]
                    ]
                , div [ class Bulma.block ]
                    (viewPlayerList
                        model
                    )
                ]


viewPlayerManager : Bool -> Int -> PlayerManager -> Html PMMsg
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


pmControlButtonGroup : PlayerManager -> Html msg
pmControlButtonGroup pm =
    div [ class Bulma.columns ]
        [ div [ classList [ Bulma.column, Bulma.buttons, Bulma.hasAddons, Bulma.mb0 ] ]
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
        , div [ classList [ Bulma.column, Bulma.isNarrow ] ]
            [ button [ classList [ Bulma.isPulledRight, Bulma.button, Bulma.isSmall, Bulma.isWhite ] ]
                [ span [ classList [ Bulma.icon ] ]
                    [ i [ class "fa fa-chevron-right" ] [] ]
                ]
            ]
        ]


viewPlayerList : PMModel -> List (Html msg)
viewPlayerList model =
    case model.selectedPMId of
        Just selectedPMId ->
            List.map
                (\p ->
                    div [ class Bulma.column ]
                        [ text p.name
                        ]
                )
            <|
                List.filter (\p -> p.parentId == selectedPMId) model.playerList

        Nothing ->
            [ div [ class Bulma.container ]
                [ text "No Player yet" ]
            ]


myButton : PMMsg -> String -> String -> Html PMMsg
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


viewPMEdit : PlayerManager -> Html PMMsg
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
