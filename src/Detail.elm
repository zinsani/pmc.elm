module Detail exposing (..)

import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, i, p, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick, onInput)
import Types exposing (FetchModel(..), FetchingMsg(..), Id(..), Model(..), Msg(..), Player, PlayerManager)


view : PlayerManager -> Html msg
view pm =
    div []
        [ div [] [ text "actionbar here" ]
        , div [] [ text "player manager detail here" ]
        , div [] [ text "add button" ]
        , table [ class Bulma.table ]
            [ thead [] []
            , tbody []
                (pm.players
                    |> List.map viewPlayer
                )
            ]
        ]


viewPlayer : Player -> Html msg
viewPlayer player =
    div [] [ "player " ++ player.name |> text ]
