module Form.Player exposing (..)

import Api
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, form, input, label, section, text)
import Html.Attributes exposing (checked, class, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Shared.UI exposing (viewActionBar, viewHorizontalField)
import Types exposing (DetailMsg(..), FetchModel(..), FetchingMsg(..), Id(..), Model(..), Msg(..), PEditMsg(..), PMEditMsg(..), Player, PlayerEdit, PlayerOptions)


updatePlayer : PlayerEdit -> Player -> Model
updatePlayer model player =
    PlayerEditPage { model | player = player }


updateOptions : PlayerEdit -> Player -> (PlayerOptions -> PlayerOptions) -> Model
updateOptions model player modifier =
    let
        newOptions =
            modifier player.options
    in
    updatePlayer model { player | options = newOptions }


stringToArray : String -> Maybe (List String)
stringToArray text =
    if text |> String.trim |> String.isEmpty then
        Nothing

    else
        String.split "," text
            |> List.map String.trim
            |> Just


update : PEditMsg -> PlayerEdit -> ( Model, Cmd Msg )
update msg model =
    let
        player =
            model.player
    in
    case msg of
        PEditName name ->
            ( updatePlayer model { player | name = name }, Cmd.none )

        PEditDelaySecondsToStart maybeDelaySeconds ->
            case maybeDelaySeconds of
                Just delaySeconds ->
                    ( updateOptions model player (\options -> { options | delaySecondsToStart = delaySeconds }), Cmd.none )

                Nothing ->
                    ( updatePlayer model player, Cmd.none )

        PEditDirectory directory ->
            ( updatePlayer model { player | directory = directory }, Cmd.none )

        PEditExeFileName exeFileName ->
            ( updatePlayer model
                { player
                    | exeFileName =
                        if String.isEmpty exeFileName then
                            Nothing

                        else
                            Just exeFileName
                }
            , Cmd.none
            )

        PEditSourceDir sourceDir ->
            ( updatePlayer model { player | sourceDir = sourceDir }, Cmd.none )

        PEditParameters parameters ->
            ( updateOptions model
                player
                (\options ->
                    { options
                        | parameters =
                            if String.isEmpty parameters then
                                Nothing

                            else
                                Just parameters
                    }
                )
            , Cmd.none
            )

        PEditExcludeFiles excludeFiles ->
            ( updateOptions model player (\options -> { options | excludeFiles = stringToArray excludeFiles }), Cmd.none )

        PEditExcludeDirectories excludeDirectories ->
            ( updateOptions model player (\options -> { options | excludeDirectories = stringToArray excludeDirectories }), Cmd.none )

        PEditLogDir logDir ->
            ( updateOptions model
                player
                (\options ->
                    { options
                        | logDir =
                            if String.isEmpty logDir then
                                Nothing

                            else
                                Just logDir
                    }
                )
            , Cmd.none
            )

        PEditWatchDogEnabled wdEnabled ->
            ( updateOptions model player (\options -> { options | watchDogEnabled = wdEnabled }), Cmd.none )

        PEditSubmit submitData ->
            let
                parentId =
                    model.playerManager.id
            in
            case submitData.id of
                TempId ->
                    ( DetailPage
                        (Api.defaultPC model.siteId model.playerManager)
                    , Api.createId (\id -> GotNewPlayer { submitData | id = id })
                        |> Cmd.map DetailMsg
                    )

                Id _ ->
                    ( Fetch (UpdatePC model.siteId parentId)
                    , Api.modifyPlayer FetchingPC
                        submitData
                        model.playerManager
                        model.siteId
                        |> Cmd.map FetchingMsg
                    )

        PEditCancel ->
            Debug.todo "branch 'PEditCancel' not implemented"


view : PlayerEdit -> Html PEditMsg
view model =
    let
        player =
            model.player

        joinList : List String -> String
        joinList list =
            String.join "," list
    in
    div [ class Bulma.container ]
        [ viewActionBar ("Plyaer: " ++ player.name) PEditCancel
        , section [ class Bulma.section ]
            [ form [ class Bulma.container, onSubmit (player |> PEditSubmit) ]
                [ viewHorizontalField
                    (label [ class Bulma.label ] [ text "name" ])
                    (input
                        [ class Bulma.input
                        , type_ "text"
                        , value player.name
                        , onInput PEditName
                        ]
                        []
                    )
                , viewHorizontalField
                    (label [ class Bulma.label ] [ text "directory" ])
                    (input
                        [ class Bulma.input
                        , type_ "text"
                        , value player.directory
                        , onInput PEditDirectory
                        ]
                        []
                    )
                , viewHorizontalField
                    (label [ class Bulma.label ] [ text "exe file name" ])
                    (input
                        [ class Bulma.input
                        , type_ "text"
                        , value (Maybe.withDefault "" player.exeFileName)
                        , onInput PEditExeFileName
                        ]
                        []
                    )
                , viewHorizontalField
                    (label [ class Bulma.label ] [ text "source directory" ])
                    (input
                        [ class Bulma.input
                        , type_ "text"
                        , value player.sourceDir
                        , onInput PEditSourceDir
                        ]
                        []
                    )
                , viewHorizontalField
                    (div [] [])
                    (label [ class Bulma.checkbox ]
                        [ input
                            [ class Bulma.mr2
                            , type_ "checkbox"
                            , checked player.options.watchDogEnabled
                            , onCheck PEditWatchDogEnabled
                            ]
                            []
                        , text "watch dog enabled"
                        ]
                    )
                , viewHorizontalField
                    (label [ class Bulma.label ] [ text "exclude files" ])
                    (input
                        [ class Bulma.input
                        , type_ "text"
                        , value (joinList (Maybe.withDefault [] player.options.excludeFiles))
                        , onInput PEditExcludeFiles
                        ]
                        []
                    )
                , viewHorizontalField
                    (label [ class Bulma.label ] [ text "exclude directories" ])
                    (input
                        [ class Bulma.input
                        , type_ "text"
                        , value (joinList (Maybe.withDefault [] player.options.excludeDirectories))
                        , onInput PEditExcludeDirectories
                        ]
                        []
                    )
                , viewHorizontalField
                    (label [ class Bulma.label ] [ text "log directory" ])
                    (input
                        [ class Bulma.input
                        , type_ "text"
                        , value (Maybe.withDefault "" player.options.logDir)
                        , onInput PEditLogDir
                        ]
                        []
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
                            , onClick PEditCancel
                            ]
                            [ text "Cancel" ]
                        ]
                    ]
                ]
            ]
        ]
