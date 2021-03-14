module MDView exposing (Model, Msg, Player, PlayerManager, getId, init, update, view)

import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, h1, i, section, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List


type alias PlayerManager =
    { id : Id
    , name : String
    }


type alias Player =
    { id : Id
    , name : String
    , parentId : Id
    }


type alias Model =
    { pmList : List PlayerManager
    , playerList : List Player
    , editingPM : Maybe PlayerManager
    , selectedPMId : Maybe Id
    , lastInputPM : Maybe PlayerManager
    }


type Msg
    = SelectPM Id
    | SelectPlayer Id
    | AddNewPlayerManager
    | AddNewPlayer
    | Submit


type Id
    = Id String


getId : String -> Id
getId id =
    Id id


init : Model
init =
    { pmList =
        [ { id = Id "1234"
          , name = "PC-01"
          }
        , { id = Id "2345"
          , name = "PC-02"
          }
        ]
    , playerList =
        [ { id = Id "12341234"
          , name = "Player-01"
          , parentId = Id "1234"
          }
        ]
    , lastInputPM = Nothing
    , editingPM = Nothing
    , selectedPMId = Just (Id "1234")
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddNewPlayerManager ->
            let
                editingPM =
                    case model.lastInputPM of
                        Just existingItem ->
                            Debug.log "AddNewPlayerManager:: existing"
                                Just
                                existingItem

                        Nothing ->
                            Debug.log "AddNewPlayerManager:: new"
                                Just
                                { id = Id "temp"
                                , name = "PC-0"
                                }
            in
            { model | editingPM = editingPM }

        Submit ->
            case model.editingPM of
                Just editingPM ->
                    { model
                        | pmList = List.append model.pmList [ editingPM ]
                        , lastInputPM = Just { editingPM | id = getId "temp" }
                    }

                Nothing ->
                    model

        _ ->
            model


view : Model -> Html Msg
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


viewPMList : Model -> Html Msg
viewPMList model =
    case List.length model.pmList of
        0 ->
            div [ class Bulma.container ]
                [ addButton AddNewPlayerManager "Add"
                ]

        _ ->
            div [ class Bulma.columns ]
                [ List.map
                    viewPlayerManager
                    model.pmList
                    |> (\pmList -> List.append pmList [ addButton AddNewPlayerManager "Add" ])
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


viewPlayerList : Model -> List (Html msg)
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


addButton : Msg -> String -> Html Msg
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


viewPMEdit : PlayerManager -> Html msg
viewPMEdit pm =
    div [ classList [ Bulma.container ] ]
        [ viewPlayerManager pm
        ]
