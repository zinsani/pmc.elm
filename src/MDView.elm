module MDView exposing (storeModel, subscriptions, update, view)

import Api exposing (pmModelEncoder)
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, h1, i, input, label, section, span, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Types exposing (FetchingModel(..), FetchingMsg(..), Id(..), Model(..), Msg(..), PMModel, PMMsg(..), PlayerManager)


subscriptions : PMModel -> Sub PMMsg
subscriptions _ =
    Sub.none


defaultPM : PlayerManager
defaultPM =
    { id = TempId
    , name = "PC-0"
    }


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
                                defaultPM
            in
            ( MainPage { model | editingPM = Just editingPM }, Cmd.none )

        InputPMName name ->
            let
                editingPM =
                    case model.editingPM of
                        Just pm ->
                            Just (Debug.log "editingPM" { pm | name = name })

                        Nothing ->
                            Just defaultPM
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
                            ( Fetching (UpdatingSite model.siteId)
                            , Api.createNewPM FetchingPMModel editingPM model |> Cmd.map FetchingMsg
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

        ClickCancel ->
            ( MainPage
                { model
                    | editingPM = Nothing
                }
            , Cmd.none
            )

        BackToSiteList ->
            ( Fetching FetchingSiteList, Api.fetch () )

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
            section [ class Bulma.section ]
                [ viewActionBar
                , viewPMEdit editingPM
                ]

        Nothing ->
            section [ class Bulma.section ]
                [ viewActionBar
                , viewPMList model
                ]


viewActionBar : Html PMMsg
viewActionBar =
    div
        [ classList
            [ Bulma.columns
            , Bulma.isVcentered
            ]
        ]
        [ div
            [ classList
                [ Bulma.column
                , Bulma.isFull
                , Bulma.hasBackgroundLight
                ]
            ]
            [ div [ class Bulma.isSize4 ]
                [ button
                    [ classList [ Bulma.button, Bulma.isRounded, Bulma.isLight, Bulma.mr4 ]
                    , onClick BackToSiteList
                    ]
                    [ span [ classList [ Bulma.icon, Bulma.isSmall ] ]
                        [ i [ classList [ "fa", "fa-arrow-left" ] ] [] ]
                    ]
                , text "Player Manager Client"
                ]
            , div
                [ classList
                    [ Bulma.block
                    , "is-pulled-right"
                    ]
                ]
                [ button
                    [ classList
                        [ Bulma.button
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
                ]
            ]
        ]


viewPMList : PMModel -> Html PMMsg
viewPMList model =
    case List.length model.pmList of
        0 ->
            div [ class Bulma.container ]
                [ addButton ClickNewPlayerManager "Add"
                ]

        _ ->
            div [ class Bulma.columns ]
                [ List.map
                    viewPlayerManager
                    model.pmList
                    |> (\pmList -> List.append pmList [ addButton ClickNewPlayerManager "Add" ])
                    |> div
                        [ classList
                            [ Bulma.column
                            , Bulma.is3DesktopOnly
                            ]
                        ]
                , div [ class Bulma.column ]
                    (viewPlayerList model)
                ]


viewPlayerManager : PlayerManager -> Html msg
viewPlayerManager pm =
    div
        []
        [ h1 [ class Bulma.title ]
            [ text <| pm.name ]
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


addButton : PMMsg -> String -> Html PMMsg
addButton msg label =
    div [ class Bulma.mt3 ]
        [ button
            [ classList
                [ Bulma.button
                , Bulma.isPrimary
                ]
            , onClick msg
            ]
            [ text label
            ]
        ]


viewPMEdit : PlayerManager -> Html PMMsg
viewPMEdit pm =
    div [ classList [ Bulma.container ] ]
        [ inputText "Name" pm.name InputPMName
        , div []
            [ addButton ClickSubmit "Submit"
            , addButton ClickCancel "Cancel"
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
