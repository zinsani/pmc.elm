module Detail exposing (..)

import Api
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, a, button, div, i, input, label, p, span, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (checked, class, disabled, href, readonly, style, type_, value)
import Html.Events exposing (onClick)
import Types exposing (DetailMsg(..), FetchModel(..), FetchingMsg(..), Id(..), InputValue(..), Model(..), Msg(..), PC, Player, PlayerManager, UIMsg(..))


update : DetailMsg -> PC -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UIMsgOnDetail uiMsg ->
            case uiMsg of
                BackToSite ->
                    ( Fetch (FetchSite model.siteId), Api.fetch () )

                ClickEditingPM id ->
                    ( PlayerManagerEditPage { siteId = model.siteId, playerManager = model.playerManager }, Cmd.none )

                ClickDeletePM pmId ->
                    ( Fetch (UpdateSite model.siteId)
                    , Api.deletePlayerManager FetchingSite pmId model.siteId
                        |> Cmd.map FetchingMsg
                    )

                BackToSiteList ->
                    ( Fetch FetchSites, Api.fetch () )

                _ ->
                    ( DetailPage model, Cmd.none )

        GotNewPM newPM ->
            ( Fetch (UpdateSite model.siteId)
            , Api.createNewPM FetchingSite newPM model.siteId
                |> Cmd.map FetchingMsg
            )

        GotModifiedPM newPM ->
            ( Fetch (UpdatePC model.siteId newPM.id)
            , Api.modifyPlayerManager FetchingPC newPM model.siteId
                |> Cmd.map FetchingMsg
            )

        _ ->
            ( DetailPage model, Cmd.none )


view : PC -> Html Msg
view model =
    div [ class Bulma.container ]
        [ viewActionBar model
        , div [] [ viewPlayerManager model.playerManager ]
        , table [ class Bulma.table ]
            [ thead [] []
            , tbody []
                (model.playerManager.players
                    |> List.map viewPlayer
                )
            ]
        ]


viewActionBar : PC -> Html Msg
viewActionBar model =
    let
        viewTitle =
            span [ class Bulma.isSize4 ]
                [ "PC: " ++ model.playerManager.name |> text
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
        |> Html.map (UIMsgOnDetail >> DetailMsg)


backButton : msg -> Html msg
backButton msg =
    button
        [ classList [ Bulma.button, Bulma.isRounded, Bulma.isLight, Bulma.mr4 ]
        , onClick msg
        ]
        [ span [ classList [ Bulma.icon, Bulma.isSmall ] ]
            [ i [ classList [ "fa", "fa-arrow-left" ] ] [] ]
        ]


viewPlayerManager : PlayerManager -> Html Msg
viewPlayerManager model =
    div [ classList [ Bulma.card ] ]
        [ div [ class Bulma.cardHeader ]
            [ div [ class Bulma.cardHeaderTitle ]
                [ div [ classList [ Bulma.tags, Bulma.isSmall ] ]
                    [ span [ class Bulma.mr3 ] [ text "Status" ]
                    , span [ classList [ Bulma.tag, Bulma.isPrimary ] ] [ text "online" ]
                    , span [ classList [ Bulma.tag, Bulma.isDanger ] ] [ text "error" ]
                    ]
                ]
            ]
        , div [ class Bulma.cardContent ]
            [ viewHorizontalField
                (label [ class Bulma.label ] [ text "endpoint" ])
                (input
                    [ class Bulma.input
                    , type_ "text"
                    , value (model.ipaddress ++ ":" ++ String.fromInt model.port_)
                    , readonly True
                    ]
                    []
                )
            , viewHorizontalField
                (label [ class Bulma.label ] [ text "timeout seconds" ])
                (input
                    [ class Bulma.input
                    , type_ "number"
                    , value (String.fromFloat model.timeoutSecondsToStartup)
                    , readonly True
                    ]
                    []
                )
            , viewHorizontalField
                (label [ class Bulma.label ] [ text "source path" ])
                (input
                    [ class Bulma.input
                    , type_ "text"
                    , value model.sourcePath
                    , readonly True
                    ]
                    []
                )
            , viewHorizontalField
                (div [] [])
                (label [ class Bulma.checkbox ]
                    [ input
                        [ class Bulma.mr2
                        , type_ "checkbox"
                        , checked model.minimize
                        , readonly True
                        ]
                        []
                    , text "minimize at startup"
                    ]
                )
            ]
        , div [ class Bulma.cardFooter ]
            [ a [ class Bulma.cardFooterItem, href "#", onClick (ClickEditingPM model.id) ] [ text "Edit" ]
            , a [ class Bulma.cardFooterItem, href "#", onClick (ClickDeletePM model.id) ] [ text "Delete" ]
            ]
        ]
        |> Html.map (UIMsgOnDetail >> DetailMsg)


viewHorizontalField : Html msg -> Html msg -> Html msg
viewHorizontalField labelElem bodyElem =
    div [ classList [ Bulma.container, Bulma.pb4 ] ]
        [ div [ classList [ Bulma.field, Bulma.isHorizontal ] ]
            [ div [ classList [ Bulma.fieldLabel, Bulma.isNormal ] ]
                [ labelElem ]
            , div [ classList [ Bulma.fieldBody ] ]
                [ p [ classList [ Bulma.field, Bulma.isExpanded ] ]
                    [ bodyElem ]
                ]
            ]
        ]


viewProperty : Bool -> String -> InputValue -> Html msg
viewProperty readonlyVal label_ val =
    viewHorizontalField
        (label [ class Bulma.label ] [ text label_ ])
        (case val of
            BoolInput v ->
                label
                    [ classList [ Bulma.checkbox, Bulma.mt2 ] ]
                    [ input
                        [ type_ Bulma.checkbox
                        , class Bulma.control
                        , checked v
                        , disabled readonlyVal
                        ]
                        []
                    , span [ classList [ Bulma.ml2 ] ] [ text label_ ]
                    ]

            StringInput v ->
                input
                    [ class Bulma.input
                    , type_ "text"
                    , value v
                    , readonly readonlyVal
                    ]
                    []

            IntInput v ->
                input
                    [ class Bulma.input
                    , type_ "number"
                    , value (String.fromInt v)
                    , readonly readonlyVal
                    ]
                    []

            FloatInput v ->
                input
                    [ class Bulma.input
                    , type_ "number"
                    , value (String.fromFloat v)
                    , readonly readonlyVal
                    ]
                    []
        )


viewProperty2 : String -> String -> Html msg
viewProperty2 label val =
    div [ classList [ Bulma.columns, Bulma.isVcentered ], style "border-bottom" "1px #d3d3d3 solid" ]
        [ div [ classList [ Bulma.column, Bulma.isNarrow ] ]
            [ text label ]
        , div [ classList [ Bulma.column ] ]
            [ text val ]
        ]


viewPlayer : Player -> Html msg
viewPlayer player =
    tr []
        [ td []
            [ div [] [ "player " ++ player.name |> text ]
            ]
        ]
