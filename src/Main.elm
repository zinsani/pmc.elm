module Main exposing (main)

import Api exposing (defaultPMModel)
import Bulma.Classes as Bulma
import Html exposing (Html, div, node, text)
import Html.Attributes exposing (class, href, rel, value)
import Json.Decode as Decode exposing (Value)
import MDView
import Site
import Types exposing (Data, FetchingModel(..), FetchingMsg(..), Model(..), Msg(..), PMMsg(..), SiteListMsg(..))


init : Maybe Data -> ( Model, Cmd Msg )
init storedData =
    case storedData of
        Nothing ->
            ( Fetching UpdatingSiteList, Api.initialize FetchingSites |> Cmd.map FetchingMsg )

        Just data ->
            case ( data.sites, data.sites.selected ) of
                ( _, Nothing ) ->
                    ( SiteListPage data.sites, Cmd.none )

                ( _, Just siteId ) ->
                    let
                        selectedPMModel =
                            List.filter (\pm -> pm.siteId == siteId) data.pmModels
                                |> List.head
                                |> Maybe.withDefault (defaultPMModel siteId)
                    in
                    ( MainPage selectedPMModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        maybeSiteId =
            case model of
                Fetching data ->
                    case data of
                        FetchingSite siteId ->
                            Just siteId

                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
    Api.onStoreChange
        (handleFetchedData maybeSiteId)


handleFetchedData : Maybe Int -> Value -> Msg
handleFetchedData maybeSiteId value =
    let
        fetchedData =
            Api.decodeFromChange value
    in
    case fetchedData of
        Ok data ->
            case maybeSiteId of
                Nothing ->
                    FetchedSites data.sites |> FetchingMsg

                Just siteId ->
                    (List.filter (\pm -> pm.siteId == siteId) data.pmModels |> List.head)
                        |> FetchedPMModel
                        |> FetchingMsg

        Err err ->
            FetchingMsg (FetchingError (Decode.errorToString err))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Fetching fetchingModel, FetchingMsg fetchingMsg ) ->
            updateFetching fetchingMsg fetchingModel

        ( SiteListPage sites, SiteListMsg slmsg ) ->
            Site.update slmsg sites

        ( MainPage mainModel, PMMsg mdvMsg ) ->
            MDView.update mdvMsg mainModel

        ( _, _ ) ->
            ( model, Cmd.none )


updateFetching : FetchingMsg -> FetchingModel -> ( Model, Cmd Msg )
updateFetching msg model =
    case ( model, msg ) of
        ( UpdatingSiteList, _ ) ->
            ( Fetching FetchingSiteList, Api.fetch () )

        ( UpdatingSite siteId, m ) ->
            let
                _ =
                    Debug.log "UpdatingSitemsg" m
            in
            ( Fetching (FetchingSite siteId), Api.fetch () )

        ( FetchingSiteList, FetchedSites sites ) ->
            ( SiteListPage sites, Cmd.none )

        ( FetchingSite _, FetchedPMModel maybeNewPMModel ) ->
            case Debug.log "maybeNewPMModel" maybeNewPMModel of
                Just newPMModel ->
                    ( MainPage newPMModel, Cmd.none )

                Nothing ->
                    ( Fetching (FetchingErr "PM Model is not found"), Cmd.none )

        ( FetchingErr err, _ ) ->
            ( Fetching (FetchingErr err), Cmd.none )

        ( _, _ ) ->
            ( Fetching model, Cmd.none )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    let
        content =
            case model of
                SiteListPage data ->
                    Site.view data
                        |> Html.map SiteListMsg

                MainPage mdvm ->
                    MDView.view mdvm
                        |> Html.map PMMsg

                Fetching fetchingState ->
                    div [ class Bulma.container ]
                        [ div [ class Bulma.hasTextCentered ]
                            [ case fetchingState of
                                FetchingErr err ->
                                    text err

                                _ ->
                                    text "..."
                            ]
                        ]

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


main : Program (Maybe Value) Model Msg
main =
    Api.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
