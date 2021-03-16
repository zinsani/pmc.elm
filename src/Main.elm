port module Main exposing (main)
import Json.Decode exposing (Value)

port onStoreChange : (Value -> msg) -> Sub msg

-- import Page

import Browser exposing (document)
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, input, label, main_, node, text)
import Html.Attributes exposing (class, href, placeholder, rel, style, title, type_)
import Html.Events exposing (onClick, onInput)
import MDView


port storeCache : String -> Cmd msg
port onStoreCached : String -> Msg


main : Program (Maybe String) Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : Maybe String -> ( Model, Cmd msg )
init flags =
    let
        initialModel () =
            Debug.log "flags on init" <|
                case flags of
                    Just json ->
                        MDView.decode json
                            |> Debug.log "decoding"
                            |> Result.toMaybe

                    Nothing ->
                        Maybe.Nothing
    in
    case initialModel () of
        Just model ->
            ( MainPage model, Cmd.none )

        Nothing ->
            ( AuthPage
                { error = Nothing
                , password = ""
                }
            , Cmd.none
            )


mainInitModel : MDView.Model
mainInitModel =
    MDView.init


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( model, msg ) of
        ( AuthPage authModel, InputPassword pw ) ->
            ( AuthPage { authModel | password = pw }, Cmd.none )

        ( AuthPage authModel, Authenticate ) ->
            case authModel.password of
                "newmedia" ->
                    ( MainPage mainInitModel, Cmd.none )

                _ ->
                    ( AuthPage { authModel | error = Just "Invalid Password" }, Cmd.none )

        ( AuthPage authModel, _ ) ->
            ( AuthPage authModel, Cmd.none )

        ( MainPage mainModel, MDVMsg mdvMsg ) ->
            ( MainPage (MDView.update mdvMsg mainModel), Cmd.none )

        ( MainPage mainModel, _ ) ->
            ( MainPage mainModel, Cmd.none )


view model =
    let
        content =
            case model of
                AuthPage authModel ->
                    viewAuthPage authModel

                MainPage mdvm ->
                    MDView.view mdvm
                        |> Html.map MDVMsg

        stylesheetBulma : Html msg
        stylesheetBulma =
            node "link"
                [ rel "stylesheet"
                , href "https://cdn.jsdelivr.net/npm/bulma@0.9.0/css/bulma.min.css"
                ]
                []

        stylesheetFontAwesome : Html msg
        stylesheetFontAwesome =
            node "link"
                [ rel "stylesheet"
                , href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.2/css/all.min.css"
                ]
                []
    in
    { title = "PlyaerManager Client"
    , body =
        [ stylesheetBulma
        , stylesheetFontAwesome
        , div [ class Bulma.container ]
            [ content
            ]
        ]
    }


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
