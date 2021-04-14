module Form.Player exposing (..)

import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Types exposing (Model(..), Msg(..), PEditMsg(..), Player, PlayerEdit, PlayerOptions)


updatePlayer : PlayerEdit -> Player -> Model
updatePlayer model player =
    PlayerEditPage { model | player = player }


updateOptions : PlayerEdit -> Player -> (PlayerOptions -> PlayerOptions)  -> Model
updateOptions model player modifier =
    let
        newOptions =
            modifier player.options
    in
    updatePlayer model { player | options = newOptions }


stringToArray : String -> Maybe (List String)
stringToArray text =
    if String.isEmpty text then
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
                    ( updateOptions model player (\options -> { options | delaySecondsToStart = delaySeconds }) , Cmd.none )

                Nothing ->
                    ( updatePlayer model player, Cmd.none )

        PEditDirectory directory ->
            ( updatePlayer model { player | directory = directory }, Cmd.none )

        PEditExeFileName exeFileName ->
            ( updatePlayer model { player | exeFileName = if String.isEmpty exeFileName then Nothing else Just exeFileName }, Cmd.none )

        PEditSourceDir sourceDir ->
            ( updatePlayer model { player | sourceDir = sourceDir }, Cmd.none )

        PEditParameters parameters ->
            ( updateOptions model player (\options -> { options | parameters = if String.isEmpty parameters then Nothing else Just parameters }), Cmd.none )

        PEditExcludeFiles excludeFiles ->
                ( updateOptions model player (\options -> { options | excludeFiles = stringToArray excludeFiles }), Cmd.none )

        PEditExcludeDirectories excludeDirectories ->
                ( updateOptions model player (\options -> { options | excludeDirectories = stringToArray excludeDirectories }), Cmd.none )

        PEditLogDir logDir ->
            ( updateOptions model player (\options -> { options | logDir = if String.isEmpty logDir then Nothing else Just logDir }), Cmd.none )

        PEditSubmit _ ->
            Debug.todo "branch 'PEditSubmit _' not implemented"

        PEditCancel ->
            Debug.todo "branch 'PEditCancel' not implemented"


view : PlayerEdit -> Html msg
view model =
    div
        [ class Bulma.container ]
        [ text ("Form.Player: " ++ model.player.name) ]
