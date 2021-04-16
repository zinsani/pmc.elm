module Detail exposing (..)

import Api
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, a, button, div, i, input, label, p, section, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, disabled, href, readonly, style, type_, value)
import Html.Events exposing (onClick)
import Shared.UI exposing (myButton, viewActionBar, viewControlButtonGroup, viewHorizontalField)
import Types exposing (DetailMsg(..), FetchModel(..), FetchingMsg(..), Id(..), InputValue(..), Model(..), Msg(..), PC, Player, PlayerManager, UIMsg(..))


update : DetailMsg -> PC -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UIMsgOnDetailPM uiMsg ->
            case uiMsg of
                ClickDelete pmId ->
                    ( Fetch (UpdateSite model.siteId)
                    , Api.deletePlayerManager FetchingSite pmId model.siteId
                        |> Cmd.map FetchingMsg
                    )

                _ ->
                    ( DetailPage model, Cmd.none )

        UIMsgOnDetailPlayer uiMsg ->
            case uiMsg of
                ClickNew ->
                    let
                        newPlayer =
                            Api.defaultPlayer model.playerManager.id
                    in
                    ( PlayerEditPage
                        { siteId = model.siteId
                        , player = newPlayer
                        , playerManager = model.playerManager
                        }
                    , Cmd.none
                    )

                ClickBack ->
                    ( Fetch (FetchSite model.siteId), Api.fetch () )

                ClickEdit _ ->
                    ( PlayerManagerEditPage { siteId = model.siteId, playerManager = model.playerManager }, Cmd.none )

                ClickDelete playerId ->
                    ( Fetch (UpdatePC model.siteId model.playerManager.id)
                    , Api.deletePlayer FetchingSite playerId model.playerManager model.siteId
                        |> Cmd.map FetchingMsg
                    )

        GotNewPM newPM ->
            ( Fetch (UpdateSite model.siteId)
            , Api.createNewPM FetchingSite newPM model.siteId
                |> Cmd.map FetchingMsg
            )

        GotNewPlayer newPlayer ->
            ( Fetch (UpdatePC model.siteId newPlayer.parentId)
            , Api.createNewPlayer FetchingPC newPlayer model.playerManager model.siteId
                |> Cmd.map FetchingMsg
            )

        SelectPlayer id ->
            ( DetailPage model, Cmd.none )

        ToggleEditModeOnDetail ->
            ( DetailPage { model | listEditing = not model.listEditing }, Cmd.none )

        _ ->
            ( DetailPage model, Cmd.none )


view : PC -> Html Msg
view model =
    let
        actionBar =
            viewActionBar ("PC: " ++ model.playerManager.name) ClickBack
                |> Html.map (UIMsgOnDetailPlayer >> DetailMsg)
    in
    div [ class Bulma.container ]
        [ actionBar
        , section [ class Bulma.section ] [ viewPlayerManager model.playerManager ]
        , viewPlayers model
        ]


addNewButton : List String -> Html Msg
addNewButton classes =
    List.concat [ [ Bulma.isPrimary ], classes ]
        |> String.join " "
        |> myButton
            ClickNew
            "Add"
        |> Html.map (UIMsgOnDetailPlayer >> DetailMsg)


viewPlayers : PC -> Html Msg
viewPlayers model =
    let
        players =
            model.playerManager.players

        editButton =
            String.join " " [ Bulma.isWarning, Bulma.isSmall ]
                |> myButton ToggleEditModeOnDetail "Edit"
                |> Html.map DetailMsg
    in
    if List.isEmpty players then
        div [ class Bulma.container ]
            [ p [ class Bulma.isSize5 ]
                [ text "Please create a new PlayerManager." ]
            , addNewButton []
            ]

    else
        div [ classList [ Bulma.container, Bulma.px3 ] ]
            [ table [ classList [ Bulma.table, Bulma.isFullwidth ] ]
                [ thead []
                    [ th [] [ text "No." ]
                    , th [] [ text "Name" ]
                    , th [] [ text "Local path" ]
                    , th [ classList [ Bulma.hasTextRight, Bulma.pr5 ] ]
                        [ addNewButton [ Bulma.isSmall ]
                        , editButton
                        ]
                    ]
                , tbody []
                    (players
                        |> List.indexedMap (viewPlayer model.listEditing)
                    )
                ]
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
            [ a [ class Bulma.cardFooterItem, href "#", onClick (ClickEdit model.id) ] [ text "Edit" ]
            , a [ class Bulma.cardFooterItem, href "#", onClick (ClickDelete model.id) ] [ text "Delete" ]
            ]
        ]
        |> Html.map (UIMsgOnDetailPM >> DetailMsg)


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


viewPlayer : Bool -> Int -> Player -> Html Msg
viewPlayer listEditing index player =
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
                        , onClick (ClickDelete player.id)
                        ]
                        [ span [ class Bulma.icon ]
                            [ i [ class "fa fa-trash" ] [] ]
                        ]
                    ]
                    |> Html.map (UIMsgOnDetailPlayer >> DetailMsg)

            else
                span [ class Bulma.isSize6 ]
                    [ 1 + index |> String.fromInt |> text ]
    in
    tr []
        [ td [] [ head ]
        , td [] [ player.name |> text ]
        , td [] [ player.directory ++ "/" ++ Maybe.withDefault "{not executable}" player.exeFileName |> text ]
        , td [] [ viewControlButtonGroup player.id SelectPlayer |> Html.map DetailMsg ]
        ]
