module Site exposing (..)

import Api
import Bulma.Classes as Bulma
import Bulma.Helpers exposing (classList)
import Html exposing (Html, button, div, h1, i, input, label, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, disabled, hidden, placeholder, style, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onDoubleClick, onInput)
import Types exposing (FetchModel(..), FetchingMsg(..), Model(..), Msg(..), Site, Sites, SitesMsg(..))


view : Sites -> Html SitesMsg
view model =
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
                            , String.length model.newSiteName == 0 |> disabled
                            ]
                            [ text "New"
                            ]
                        ]
                    ]
                , viewContent model
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


viewContent : Sites -> Html SitesMsg
viewContent model =
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
                    |> List.indexedMap
                        (viewSite model.editingSite)
                    |> tbody []
                ]
            ]


viewSite : Maybe Site -> Int -> Site -> Html SitesMsg
viewSite editingSite num site =
    tr []
        [ td [ class Bulma.hasTextCentered ] [ text <| String.fromInt (num + 1) ]
        , viewSiteName editingSite site
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


viewSiteName : Maybe Site -> Site -> Html SitesMsg
viewSiteName editingSite site =
    case editingSite of
        Nothing ->
            viewNormalSiteName site

        Just editing ->
            if editing.id == site.id then
                viewEditingSiteName editing

            else
                viewNotEditingSiteName site


viewEditingSiteName : Site -> Html SitesMsg
viewEditingSiteName site =
    td []
        [ div [ class Bulma.field ]
            [ div [ class Bulma.control ]
                [ input
                    [ onInput EditingSiteName
                    , value site.name
                    , onBlur <| EndEditSite site
                    ]
                    []
                ]
            ]
        ]


viewNotEditingSiteName : Site -> Html msg
viewNotEditingSiteName site =
    td [] [ span [] [ text site.name ] ]


viewNormalSiteName : Site -> Html SitesMsg
viewNormalSiteName site =
    td [ onDoubleClick <| StartEditSite site.id ] [ span [] [ text site.name ] ]


update : SitesMsg -> Sites -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputSiteName siteName ->
            ( SiteListPage { model | newSiteName = siteName }, Cmd.none )

        ClickNewSite siteName ->
            ( Fetch UpdateSites
            , Api.createNewSite FetchingSites siteName model
                |> Cmd.map FetchingMsg
            )

        StartEditSite id ->
            let
                editingSite =
                    List.filter (\s -> s.id == id) model.list
                        |> List.head
            in
            ( SiteListPage { model | editingSite = editingSite }, Cmd.none )

        EditingSiteName modifiedName ->
            let
                newSite =
                    model.editingSite
                        |> Maybe.andThen (\editingSite -> Just { editingSite | name = modifiedName })
            in
            ( SiteListPage { model | editingSite = newSite }
            , Cmd.none
            )

        EndEditSite modified ->
            ( Fetch UpdateSites
            , Api.modifySites
                FetchingSites
                (model.list
                    |> List.map
                        (\s ->
                            if s.id == modified.id then
                                modified

                            else
                                s
                        )
                )
                model
                |> Cmd.map FetchingMsg
            )

        ToggleSaveSelection save ->
            ( SiteListPage { model | saveSelection = save }, Cmd.none )

        ClickOpenSite siteId ->
            ( Fetch (UpdateSite siteId)
            , Api.selectSite FetchingSite siteId model
                |> Cmd.map FetchingMsg
            )

        ClickDeleteSite siteId ->
            ( Fetch UpdateSites
            , Api.modifySites
                FetchingSites
                (model.list |> List.filter (\s -> not <| s.id == siteId))
                model
                |> Cmd.map FetchingMsg
            )
