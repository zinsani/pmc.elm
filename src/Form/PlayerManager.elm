module Form.PlayerManager exposing (..)

import Api
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, form, i, input, label, p, section, span, text)
import Html.Attributes exposing (checked, class, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Shared.UI exposing (viewActionBar, viewHorizontalField)
import Types exposing (DetailMsg(..), FetchModel(..), FetchingMsg(..), Id(..), Model(..), Msg(..), PMEditMsg(..), PlayerManager, PlayerManagerEdit)



-- PlyaerManager Edit Form


updatePM : PlayerManagerEdit -> PlayerManager -> Model
updatePM model pm =
    PlayerManagerEditPage { model | playerManager = pm }


update : PMEditMsg -> PlayerManagerEdit -> ( Model, Cmd Msg )
update msg model =
    let
        pm =
            model.playerManager
    in
    case msg of
        PMEditName name ->
            ( updatePM model { pm | name = name }, Cmd.none )

        PMEditIpAddress ipaddress ->
            ( updatePM model { pm | ipaddress = ipaddress }, Cmd.none )

        PMEditPort maybePort ->
            case maybePort of
                Just port_ ->
                    ( updatePM model { pm | port_ = port_ }, Cmd.none )

                Nothing ->
                    ( updatePM model pm, Cmd.none )

        PMEditTimeoutSecondsToStartup maybeTimeoutSecondsToStartup ->
            case maybeTimeoutSecondsToStartup of
                Just timeoutSecondsToStartup ->
                    ( updatePM model { pm | timeoutSecondsToStartup = timeoutSecondsToStartup }, Cmd.none )

                Nothing ->
                    ( updatePM model pm, Cmd.none )

        PMEditSourcePath sourcePath ->
            ( updatePM model { pm | sourcePath = sourcePath }, Cmd.none )

        PMEditMinimize minimize ->
            ( updatePM model { pm | minimize = minimize }, Cmd.none )

        PMEditSubmit submitPM ->
            case submitPM.id of
                TempId ->
                    ( DetailPage
                        (Api.defaultPC model.siteId submitPM)
                    , Api.createId (\id -> GotNewPM { submitPM | id = id })
                        |> Cmd.map DetailMsg
                    )

                Id _ ->
                    ( Fetch (UpdatePC model.siteId submitPM.id)
                    , Api.modifyPlayerManager FetchingPC
                        submitPM
                        model.siteId
                        |> Cmd.map FetchingMsg
                    )

        PMEditCancel ->
            case pm.id of
                TempId ->
                    ( Fetch (FetchSite model.siteId), Api.fetch () )

                Id _ ->
                    ( Fetch (FetchPC model.siteId pm.id), Api.fetch () )


view : PlayerManagerEdit -> Html PMEditMsg
view model =
    let
        pm =
            model.playerManager
    in
    div [ class Bulma.container ]
        [ viewActionBar ("PC: " ++ model.playerManager.name) PMEditCancel
        , section [ class Bulma.section ]
            [ form [ class Bulma.container, onSubmit (pm |> PMEditSubmit) ]
                [ viewHorizontalField
                    (label [ class Bulma.label ] [ text "name" ])
                    (input
                        [ class Bulma.input
                        , type_ "text"
                        , value pm.name
                        , onInput PMEditName
                        ]
                        []
                    )
                , viewHorizontalField
                    (label [ class Bulma.label ] [ text "ipaddress" ])
                    (input
                        [ class Bulma.input
                        , type_ "text"
                        , value pm.ipaddress
                        , onInput PMEditIpAddress
                        ]
                        []
                    )
                , viewHorizontalField
                    (label [ class Bulma.label ] [ text "port" ])
                    (input
                        [ class Bulma.input
                        , type_ "number"
                        , value (String.fromInt pm.port_)
                        , onInput (String.toInt >> PMEditPort)
                        ]
                        []
                    )
                , viewHorizontalField
                    (label [ class Bulma.label ] [ text "timeout seconds" ])
                    (input
                        [ class Bulma.input
                        , type_ "number"
                        , value (String.fromFloat pm.timeoutSecondsToStartup)
                        , onInput (String.toFloat >> PMEditTimeoutSecondsToStartup)
                        ]
                        []
                    )
                , viewHorizontalField
                    (label [ class Bulma.label ] [ text "source path" ])
                    (input
                        [ class Bulma.input
                        , type_ "text"
                        , value pm.sourcePath
                        , onInput PMEditSourcePath
                        ]
                        []
                    )
                , viewHorizontalField
                    (div [] [])
                    (label [ class Bulma.checkbox ]
                        [ input
                            [ class Bulma.mr2
                            , type_ "checkbox"
                            , checked pm.minimize
                            , onCheck PMEditMinimize
                            ]
                            []
                        , text "minimize at startup"
                        ]
                    )
                , div
                    [ class Bulma.field ]
                    [ div
                        [ class Bulma.control ]
                        [ button
                            [ classList [ Bulma.button, Bulma.isPrimary ]
                            , type_ "submit"
                            ]
                            [ text "Submit" ]
                        , button
                            [ classList [ Bulma.button, Bulma.isDanger ]
                            , onClick PMEditCancel
                            ]
                            [ text "Cancel" ]
                        ]
                    ]
                ]
            ]
        ]
