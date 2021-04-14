module Form.Player exposing (..)

import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Types exposing (Model(..), Msg(..), PEditMsg, Player, PlayerEdit)


update : PEditMsg -> PlayerEdit -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( PlayerEditPage model, Cmd.none )


view : PlayerEdit -> Html msg
view model =
    div
        [ class Bulma.container ]
        [ text ("Form.Player: " ++ model.player.name) ]
