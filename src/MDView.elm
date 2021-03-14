module MDView exposing (Model, Msg, Player, PlayerManager, getId, init, update, view)

import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, h1, i, section, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


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
    , selectedPMId : Maybe Id
    }


type Msg
    = SelectPM Id
    | SelectPlayer Id
    | AddNewPlayerManager
    | AddNewPlayer


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
    , selectedPMId = Just (Id "1234")
    }


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
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
                [ addButton AddNewPlayerManager
                ]

        _ ->
            div [ class Bulma.columns ]
                [ List.map
                    viewPlayerManager
                    model.pmList
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


addButton : Msg -> Html Msg
addButton msg =
    button
        [ classList
            [ Bulma.button
            , Bulma.isPrimary
            ]
        , onClick msg
        ]
        [ text "Add New PC"
        ]
