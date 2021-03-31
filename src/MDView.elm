module MDView exposing (storeModel, subscriptions, update, view)

import Api exposing (pmModelEncoder)
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, h1, i, input, label, section, span, text)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random
import Task
import Types exposing (Id(..), PMModel, PMMsg(..), Player, PlayerManager, createId, idSeed)


subscriptions : PMModel -> Sub PMMsg
subscriptions model =
    Sub.none


defaultPM : PlayerManager
defaultPM =
    { id = TempId
    , name = "PC-0"
    }


update : PMMsg -> PMModel -> ( PMModel, Cmd PMMsg )
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
            ( { model | editingPM = Just editingPM }, Cmd.none )

        InputPMName name ->
            let
                editingPM =
                    case model.editingPM of
                        Just pm ->
                            Just (Debug.log "editingPM" { pm | name = name })

                        Nothing ->
                            Just defaultPM
            in
            ( { model | editingPM = editingPM }, Cmd.none )

        AddNewPlayerManager pm ->
            let
                newModel =
                    { model
                        | pmList = model.pmList ++ [ pm ]
                        , editingPM = Nothing
                    }
            in
            ( newModel
            , storeModel newModel
            )

        ClickSubmit ->
            case model.editingPM of
                Just editingPM ->
                    case editingPM.id of
                        TempId ->
                            ( model, createIdIfNecessary editingPM AddNewPlayerManager )

                        Id _ ->
                            ( { model | pmList = modifyList editingPM model.pmList }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ClickCancel ->
            ( { model
                | editingPM = Nothing
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


storeModel : PMModel -> Cmd msg
storeModel model =
    Api.storeCache <| pmModelEncoder model


modifyList : { a | id : b } -> List { a | id : b } -> List { a | id : b }
modifyList obj list =
    List.map
        (\x ->
            if obj.id == x.id then
                obj

            else
                x
        )
        list


createIdIfNecessary : { a | id : Id } -> ({ a | id : Id } -> msg) -> Cmd msg
createIdIfNecessary obj msg =
    let
        id =
            if obj.id == TempId then
                Id <| createId idSeed

            else
                obj.id
    in
    Task.perform msg (Task.succeed { obj | id = id })


getPMWithTempId : PlayerManager -> Maybe PlayerManager
getPMWithTempId pm =
    Just { pm | id = TempId }


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


viewActionBar : Html msg
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
            [ span [ class Bulma.isSize4 ] [ text "Player Manager Client" ]
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
            , onClick (Debug.log "Click" msg)
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
