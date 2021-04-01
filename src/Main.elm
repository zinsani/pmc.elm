module Main exposing (main)

import Api
import Bulma.Classes as Bulma
import Html exposing (Html, div, node)
import Html.Attributes exposing (class, href, rel, selected, value)
import Json.Decode exposing (Value)
import MDView
import Site
import Types exposing (Data, Model(..), Msg(..), PMMsg, Site, SiteListMsg(..), Sites)


init : Maybe Data -> ( Model, Cmd Msg )
init storedData =
    case storedData of
        Nothing ->
            let
                _ =
                    Debug.log "Nothing stored yet"
            in
            ( SiteListPage { sites = Api.emptySiteData, pmModels = [] }, Cmd.none )

        Just data ->
            let
                defaultSiteModel =
                    Debug.log "defaultSiteModel" SiteListPage data
            in
            case ( data.sites, data.sites.selected ) of
                ( _, Nothing ) ->
                    ( defaultSiteModel, Cmd.none )

                ( _, Just siteId ) ->
                    ( defaultSiteModel, Api.loadPMDataOfSite OpenSite siteId data.pmModels )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Api.onStoreChange (\value -> UpdateData <| Api.decodeFromChange value)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( SiteListPage _, UpdateData newData ) ->
            case Debug.log "newData" newData of
                Just newDataFound ->
                    ( SiteListPage newDataFound, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( SiteListPage data, SiteListMsg slmsg ) ->
            let
                ( siteModel, slMsg ) =
                    updateSiteListMsg data slmsg
            in
            ( SiteListPage siteModel, slMsg )

        ( _, OpenSite newPMModel ) ->
            ( MainPage newPMModel, Cmd.none )

        ( MainPage mainModel, PMMsg mdvMsg ) ->
            let
                ( newModel, cmd ) =
                    MDView.update mdvMsg mainModel
            in
            ( MainPage newModel, Cmd.map PMMsg cmd )

        ( _, _ ) ->
            ( model, Cmd.none )


updateSiteListMsg : Data -> SiteListMsg -> ( Data, Cmd Msg )
updateSiteListMsg data slmsg =
    let
        sites =
            data.sites

        ( newSites, cmd ) =
            case slmsg of
                InputSiteName siteName ->
                    ( { sites | newSiteName = siteName }, Cmd.none )

                ClickNewSite siteName ->
                    ( sites, Api.createNewSite siteName data )

                StartEditSite id ->
                    let
                        editingSite =
                            List.filter (\s -> s.id == id) sites.list
                                |> List.head
                    in
                    ( { sites | editingSite = editingSite }, Cmd.none )

                EditingSiteName modifiedName ->
                    case sites.editingSite of
                        Nothing ->
                            ( sites, Cmd.none )

                        Just modified ->
                            let
                                newSite =
                                    { modified | name = modifiedName }
                            in
                            ( { sites | editingSite = Just newSite }
                            , Cmd.none
                            )

                EndEditSite modified ->
                    let
                        newList =
                            List.map
                                (\s ->
                                    if s.id == modified.id then
                                        modified

                                    else
                                        s
                                )
                                sites.list
                    in
                    ( { sites | editingSite = Nothing, list = newList }, Cmd.none )

                ToggleSaveSelection save ->
                    ( { sites | saveSelection = save }, Cmd.none )

                ClickOpenSite siteId ->
                    let
                        selected : Maybe Site
                        selected =
                            List.filter (\s -> s.id == siteId) sites.list
                                |> List.head

                        newSite : Sites
                        newSite =
                            if sites.saveSelection then
                                { sites
                                    | selected = Maybe.map (\s -> s.id) selected
                                }

                            else
                                sites
                    in
                    -- ( newSite, openSite foundSelected )
                    ( newSite, Api.loadPMDataOfSite OpenSite siteId data.pmModels )

                ClickDeleteSite siteId ->
                    ( { sites | list = List.filter (\s -> not <| s.id == siteId) sites.list }, Cmd.none )
    in
    ( { data | sites = newSites }, cmd )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    let
        content =
            case model of
                SiteListPage data ->
                    Site.viewSiteList data.sites
                        |> Html.map SiteListMsg

                MainPage mdvm ->
                    MDView.view mdvm
                        |> Html.map PMMsg

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
