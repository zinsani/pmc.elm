module Main exposing (main)

import Api exposing (defaultPC)
import Bulma.Classes as Bulma
import Detail
import Form.Player as PlayerEdit
import Form.PlayerManager as PMEdit
import Html exposing (Html, div, node, text)
import Html.Attributes exposing (class, href, rel, value)
import Json.Decode as Decode exposing (Value)
import Master
import Site
import Types exposing (Data, FetchModel(..), FetchingMsg(..), Id(..), MasterMsg(..), Model(..), Msg(..), PMEditMsg(..), SitesMsg(..))


init : Maybe Data -> ( Model, Cmd Msg )
init storedData =
    case storedData of
        Nothing ->
            ( Fetch UpdateSites, Api.initialize FetchingSites |> Cmd.map FetchingMsg )

        Just data ->
            case ( List.length data.sites.list, data.sites.selected ) of
                ( _, Nothing ) ->
                    ( SiteListPage data.sites, Cmd.none )

                ( 0, Just siteId ) ->
                    ( SiteListPage data.sites, Cmd.none )

                ( _, Just siteId ) ->
                    let
                        maybeSite =
                            List.filter (\s -> s.id == siteId) data.sites.list
                                |> List.head
                    in
                    case maybeSite of
                        Just site ->
                            ( MainPage site data.playerManagers, Cmd.none )

                        Nothing ->
                            ( SiteListPage data.sites, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        -- _ =
        --     model |> Debug.log "subscription"
        ( maybeSiteId, maybePMId ) =
            case model of
                Fetch data ->
                    case data of
                        FetchSite siteId ->
                            ( Just siteId, Nothing )

                        FetchPC siteId pmId ->
                            ( Just siteId, Just pmId )

                        _ ->
                            ( Nothing, Nothing )

                _ ->
                    ( Nothing, Nothing )
    in
    Api.onStoreChange
        (handleFetchedData maybeSiteId maybePMId)


handleFetchedData : Maybe Int -> Maybe Id -> Value -> Msg
handleFetchedData maybeSiteId maybePMId value =
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
                    let
                        site =
                            List.filter (\s -> s.id == siteId)
                                data.sites.list
                                |> List.head
                    in
                    case maybePMId of
                        Nothing ->
                            FetchedSite site
                                data.playerManagers
                                |> FetchingMsg

                        Just pmId ->
                            FetchedPC
                                (data.playerManagers
                                    |> List.filter (\x -> x.id == pmId)
                                    |> List.head
                                    |> Maybe.map (\x -> defaultPC siteId x)
                                )
                                |> FetchingMsg

        Err err ->
            FetchingMsg (FetchingError (Decode.errorToString err))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Fetch fetchingModel, FetchingMsg fetchingMsg ) ->
            updateFetch fetchingMsg fetchingModel

        ( SiteListPage sites, SitesMsg slmsg ) ->
            Site.update slmsg sites

        ( MainPage site playerManagers, MasterMsg mMsg ) ->
            Master.update mMsg ( site, playerManagers )

        ( PlayerManagerEditPage pmEdit, PMEditMsg pmeMsg ) ->
            PMEdit.update pmeMsg pmEdit

        ( PlayerEditPage pEdit, PEditMsg peMsg ) ->
            PlayerEdit.update peMsg pEdit

        ( DetailPage pc, DetailMsg dMsg ) ->
            Detail.update dMsg pc

        ( _, _ ) ->
            ( model, Cmd.none )


updateFetch : FetchingMsg -> FetchModel -> ( Model, Cmd Msg )
updateFetch msg model =
    case ( model, msg ) of
        ( UpdateSites, _ ) ->
            ( Fetch FetchSites, Api.fetch () )

        ( UpdateSite siteId, _ ) ->
            ( Fetch (FetchSite siteId), Api.fetch () )

        ( UpdatePC siteId pmId, _ ) ->
            ( Fetch (FetchPC siteId pmId), Api.fetch () )

        ( FetchSites, FetchedSites sites ) ->
            ( SiteListPage sites, Cmd.none )

        ( FetchSite _, FetchedSite maybeNewSite playerManagers ) ->
            case Debug.log "maybeNewSite" maybeNewSite of
                Just newSite ->
                    ( MainPage newSite playerManagers, Cmd.none )

                Nothing ->
                    ( Fetch (FetchErr "PM Model is not found"), Cmd.none )

        ( FetchPC sitId pmId, FetchedPC maybePC ) ->
            case maybePC of
                Just newPC ->
                    ( DetailPage newPC, Cmd.none )

                Nothing ->
                    ( Fetch (FetchErr "PC is not found"), Cmd.none )

        ( FetchErr err, _ ) ->
            ( Fetch (FetchErr err), Cmd.none )

        ( _, _ ) ->
            ( Fetch model, Cmd.none )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    let
        content =
            case model of
                SiteListPage data ->
                    Site.view data
                        |> Html.map SitesMsg

                MainPage site playerManagers ->
                    Master.view site playerManagers

                PlayerManagerEditPage pmEdit ->
                    PMEdit.view pmEdit
                        |> Html.map PMEditMsg

                PlayerEditPage pEdit ->
                    PlayerEdit.view pEdit
                        |> Html.map PEditMsg

                Fetch fetchingState ->
                    div [ class Bulma.container ]
                        [ div [ class Bulma.hasTextCentered ]
                            [ case fetchingState of
                                FetchErr err ->
                                    text err

                                _ ->
                                    text "..."
                            ]
                        ]

                DetailPage pc ->
                    Detail.view pc

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
