module Main exposing (main)

-- import Page

import Browser
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, input, label, main_, node, text)
import Html.Attributes exposing (class, href, placeholder, rel, style, type_)
import Html.Events exposing (onClick, onInput)
import MDView


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type Model
    = MainPage MDView.Model
    | AuthPage AuthModel


type alias AuthModel =
    { password : String
    , error : Maybe String
    }


type Msg
    = MDVMsg MDView.Msg
    | InputPassword String
    | Authenticate


init : Model
init =
    -- Main mainInitModel
    AuthPage
        { error = Nothing
        , password = ""
        }


mainInitModel : MDView.Model
mainInitModel =
    { pmList =
        []
    , playerList =
        []
    , selectedPMId = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case ( model, msg ) of
        ( AuthPage authModel, InputPassword pw ) ->
            AuthPage { authModel | password = pw }

        ( AuthPage authModel, Authenticate ) ->
            case authModel.password of
                "newmedia" ->
                    MainPage mainInitModel

                _ ->
                    AuthPage { authModel | error = Just "Invalid Password" }

        ( AuthPage authModel, _ ) ->
            AuthPage authModel

        ( MainPage mainModel, _ ) ->
            MainPage mainModel


view : Model -> Html Msg
view model =
    let
        content =
            case model of
                AuthPage authModel ->
                    viewAuthPage authModel

                MainPage mdvm ->
                    MDView.view mdvm
                        |> Html.map MDVMsg
    in
    main_ []
        [ node "link"
            [ rel "stylesheet"
            , href "https://cdn.jsdelivr.net/npm/bulma@0.9.0/css/bulma.min.css"
            ]
            []
        , node "link"
            [ rel "stylesheet"
            , href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.2/css/all.min.css"
            ]
            []
        , div [ class Bulma.container ]
            [ content
            ]
        ]


viewAuthPage : { a | error : Maybe String } -> Html Msg
viewAuthPage model =
    div
        [ class
            Bulma.container
        ]
        [ div
            [ classList
                [ Bulma.columns
                , Bulma.isCentered
                , Bulma.mt4
                ]
            ]
            [ div
                [ classList
                    [ Bulma.column
                    , Bulma.isHalf
                    , Bulma.box
                    ]
                ]
                [ div
                    [ classList
                        [ Bulma.isSize3
                        , Bulma.block
                        , Bulma.hasTextCentered
                        ]
                    ]
                    [ text "PlayerManager Client" ]
                , div
                    [ class Bulma.field
                    ]
                    [ label
                        [ class Bulma.label
                        ]
                        [ text "Password" ]
                    , div
                        [ class Bulma.control
                        ]
                        [ input
                            [ class Bulma.input
                            , type_ "password"
                            , placeholder "Input Password"
                            , onInput InputPassword
                            ]
                            []
                        ]
                    ]
                , div
                    [ class Bulma.field ]
                    [ div
                        [ class Bulma.control
                        ]
                        [ button
                            [ classList
                                [ Bulma.button
                                , Bulma.isPrimary
                                ]
                            , onClick Authenticate
                            ]
                            [ text "Enter"
                            ]
                        ]
                    ]
                , div [ class Bulma.hasTextDanger ]
                    [ text
                        (case model.error of
                            Just message ->
                                message

                            Nothing ->
                                ""
                        )
                    ]
                ]
            ]
        ]
