module Detail exposing (..)

import Api
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, i, p, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick, onInput)
import Types exposing (DetailMsg(..), FetchModel(..), FetchingMsg(..), Id(..), Model(..), Msg(..), Player, PlayerManager)


update : DetailMsg -> ( Int, PlayerManager ) -> ( Model, Cmd msg )
update msg ( siteId, model ) =
    case msg of
        BackToSite ->
            ( Fetch (FetchSite siteId), Api.fetch () )

        _ ->
            ( DetailPage ( siteId, model ), Cmd.none )


view : PlayerManager -> Html DetailMsg
view model =
    div [ class Bulma.container ]
        [ viewActionBar model
        , div [] [ text "player manager detail here" ]
        , div [] [ text "add button" ]
        , table [ class Bulma.table ]
            [ thead [] []
            , tbody []
                ([ viewPlayerManager model
                 ]
                    ++ (model.players
                            |> List.map viewPlayer
                       )
                )
            ]
        ]


viewActionBar : PlayerManager -> Html DetailMsg
viewActionBar model =
    let
        viewTitle =
            span [ class Bulma.isSize4 ]
                [ "PC: " ++ model.name |> text
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
        [ backButton BackToSite
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


viewPlayerManager : PlayerManager -> Html msg
viewPlayerManager model =
    tr []
        [ td []
            [ div [] [ "PlayerManager" ++ model.name |> text ]
            ]
        ]


viewPlayer : Player -> Html msg
viewPlayer player =
    tr []
        [ td []
            [ div [] [ "player " ++ player.name |> text ]
            ]
        ]
