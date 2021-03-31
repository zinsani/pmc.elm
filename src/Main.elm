module Main exposing (main)

import Api
import Browser
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, h1, i, input, label, node, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, hidden, href, placeholder, rel, selected, style, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onDoubleClick, onInput)
import Json.Decode exposing (Value)
import MDView
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
subscriptions model =
    Api.onStoreChange (\value -> UpdateData <| Api.decodeFromChange value)



-- updateModel : Value -> Msg
-- updateModel val =
--     case MDView.decode val of
--         Result.Ok model ->
--             UpdateModel model
--         Result.Err err ->
--             UpdateError <| Decode.errorToString err


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
                    -- ( { sites
                    --     | list = Api.createNewSite siteName sites.list
                    --     , newSiteName = ""
                    --   }
                    -- , Cmd.none
                    -- )
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
                    viewSiteList data.sites
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


viewSiteList : Sites -> Html SiteListMsg
viewSiteList model =
    let
        viewSiteName site =
            case model.editingSite of
                Nothing ->
                    td [ onDoubleClick <| StartEditSite site.id ] [ span [] [ text site.name ] ]

                Just editing ->
                    if editing.id == site.id then
                        td []
                            [ div [ class Bulma.field ]
                                [ div [ class Bulma.control ]
                                    [ input
                                        [ onInput EditingSiteName
                                        , value editing.name
                                        , onBlur <| EndEditSite editing
                                        ]
                                        []
                                    ]
                                ]
                            ]

                    else
                        td [] [ span [] [ text site.name ] ]

        viewSite : Site -> Html SiteListMsg
        viewSite site =
            tr []
                [ td [ class Bulma.hasTextCentered ] [ text <| String.fromInt site.id ]
                , viewSiteName site
                , td [ class Bulma.hasTextCentered ]
                    [ div [ class Bulma.field ]
                        [ div [ class Bulma.control ]
                            [ button
                                [ classList [ Bulma.mr1, Bulma.button, Bulma.isPrimary, Bulma.isSmall ]
                                , onClick <| ClickOpenSite site.id
                                ]
                                [ span [ class Bulma.icon ]
                                    [ i [ class "fa fa-search-plus" ] []
                                    ]
                                ]
                            , button
                                [ classList [ Bulma.button, Bulma.isDanger, Bulma.isSmall ]
                                , onClick <| ClickDeleteSite site.id
                                ]
                                [ span [ class Bulma.icon ]
                                    [ i [ class "fa fa-trash" ] []
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]

        content =
            if List.isEmpty model.list then
                div [ classList [ Bulma.mt2, Bulma.px3 ] ] [ text "Welcome to PlayerManagerClient. Why don't you create new site?" ]

            else
                div [ class Bulma.mt2 ]
                    [ table [ classList [ Bulma.table, Bulma.isFullwidth ] ]
                        [ thead []
                            [ th [ class Bulma.hasTextCentered ]
                                [ text "No."
                                ]
                            , th [ style "width" "60%" ]
                                [ text "Name"
                                ]
                            , th [ class Bulma.hasTextCentered ]
                                [ text "Open"
                                ]
                            ]
                        , List.sortBy .id model.list
                            |> List.map
                                viewSite
                            |> tbody []
                        ]
                    ]
    in
    div [ class Bulma.container ]
        [ div [ classList [ Bulma.columns, Bulma.isCentered ] ]
            [ div [ classList [ Bulma.column, Bulma.box, Bulma.isHalf ] ]
                [ div [ classList [ Bulma.mb4, Bulma.px3 ] ]
                    [ h1 [ class Bulma.title ] [ text "Site List" ]
                    ]
                , div [ classList [ Bulma.field, Bulma.hasAddons, Bulma.px3 ] ]
                    [ div [ classList [ Bulma.control, Bulma.isExpanded ] ]
                        [ input
                            [ class Bulma.input
                            , type_ "text"
                            , placeholder "Site Name"
                            , onInput InputSiteName
                            , value model.newSiteName
                            ]
                            []
                        ]
                    , div [ class Bulma.control ]
                        [ button
                            [ classList [ Bulma.button, Bulma.isPrimary ]
                            , onClick <| ClickNewSite model.newSiteName
                            ]
                            [ text "New"
                            ]
                        ]
                    ]
                , content
                , div [ class Bulma.hasTextRight, hidden <| List.isEmpty model.list ]
                    [ label [ classList [ Bulma.checkbox, Bulma.isToggle ] ]
                        [ input
                            [ type_ "checkbox", onCheck ToggleSaveSelection, checked <| model.saveSelection ]
                            []
                        , text "Save Selection"
                        ]
                    ]
                ]
            ]
        ]


main : Program (Maybe Value) Model Msg
main =
    Api.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
