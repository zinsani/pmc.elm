port module Api exposing (..)

import Browser
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Task
import Types exposing (Data, Id(..), PMModel, Player, PlayerManager, Site, Sites, idToString)


port storeCache : Value -> Cmd msg


port storePMModels : Value -> Cmd msg


port onStoreChange : (Value -> msg) -> Sub msg


createNewSite : String -> Data -> Cmd msg
createNewSite siteName data =
    let
        list =
            data.sites.list

        maxId : Maybe Int
        maxId =
            List.map (\el -> el.id) list
                |> List.maximum

        newId =
            case maxId of
                Just id ->
                    id + 1

                Nothing ->
                    1

        newList =
            List.append list [ { id = newId, name = siteName } ]

        sites =
            data.sites

        newSites =
            Debug.log "newSites" { sites | list = newList }
    in
    { data | sites = newSites }
        -- |> Task.succeed (\_ -> "Waiting...")
        |> storageEncoder
        |> storeCache


emptySiteData : Sites
emptySiteData =
    { list = []
    , selected = Nothing
    , newSiteName = ""
    , editingSite = Nothing
    , saveSelection = False
    }


application :
    { init : Maybe Data -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    }
    -> Program (Maybe Value) model msg
application config =
    let
        init : Maybe Value -> ( model, Cmd msg )
        init flags =
            flags
                |> Maybe.andThen
                    (\v ->
                        Decode.decodeValue Decode.string v
                            |> Result.andThen (Decode.decodeString storageDecoder)
                            |> Result.toMaybe
                    )
                |> config.init
    in
    Browser.document
        { init = init
        , update = config.update
        , subscriptions = config.subscriptions
        , view = config.view
        }


decodeFromChange : Value -> Maybe Data
decodeFromChange val =
    Decode.decodeValue storageDecoder val
        |> Result.toMaybe


storageDecoder : Decoder Data
storageDecoder =
    Decode.map2
        Data
        (Decode.field "sites" sitesDecoder)
        (Decode.field "pmModels" (Decode.list pmModelDecoder))


sitesDecoder =
    Decode.map5
        Sites
        (Decode.field "list" (Decode.list siteDecoder))
        (Decode.maybe (Decode.field "selected" Decode.int))
        (Decode.field "newSiteName" Decode.string)
        (Decode.maybe (Decode.field "editingSite" siteDecoder))
        (Decode.field "saveSelection" Decode.bool)


siteDecoder : Decoder Site
siteDecoder =
    Decode.map2 Site
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)


loadPMDataOfSite : (PMModel -> msg) -> Int -> List PMModel -> Cmd msg
loadPMDataOfSite msg siteId list =
    List.filter (\p -> p.siteId == siteId) list
        |> List.head
        |> Maybe.withDefault (defaultPM siteId)
        |> Task.succeed
        |> Task.perform msg


defaultPM : Int -> PMModel
defaultPM siteId =
    { pmList = []
    , playerList = []
    , lastInputPM = Nothing
    , editingPM = Nothing
    , selectedPMId = Nothing
    , siteId = siteId
    }


encodePM v =
    Encode.object
        [ ( "id", Encode.string "" )
        , ( "name", Encode.string "" )
        ]


encodePlayer v =
    Encode.object
        [ ( "name", Encode.string "" ) ]


defaultModelJson : Value
defaultModelJson =
    Encode.object
        [ ( "pmList"
          , Encode.list encodePM []
          )
        , ( "players"
          , Encode.list encodePlayer []
          )
        ]


storageEncoder : Data -> Value
storageEncoder data =
    Encode.object
        [ ( "sites", sitesEncoder data.sites )
        , ( "pmModels", Encode.list pmModelEncoder data.pmModels )
        ]


sitesEncoder : Sites -> Value
sitesEncoder sites =
    Encode.object
        [ ( "list", Encode.list siteEncoder sites.list )
        , ( "selected"
          , case sites.selected of
                Just selected_ ->
                    Encode.int selected_

                Nothing ->
                    Encode.null
          )
        , ( "saveSelection", Encode.bool sites.saveSelection )
        , ( "newSiteName", Encode.string "" )
        , ( "editingSite", Encode.null )
        ]


siteEncoder : Site -> Value
siteEncoder site =
    Encode.object
        [ ( "id", Encode.int site.id )
        , ( "name", Encode.string site.name )
        ]


pmModelEncoder : PMModel -> Encode.Value
pmModelEncoder model =
    let
        cvtPM : PlayerManager -> Encode.Value
        cvtPM =
            \pm ->
                Encode.object
                    [ ( "id", Encode.string <| idToString pm.id )
                    , ( "name", Encode.string pm.name )
                    ]

        pmList =
            Encode.list
                cvtPM
                model.pmList

        cvtPlayer : Player -> Encode.Value
        cvtPlayer =
            \p ->
                Encode.object
                    [ ( "id", Encode.string <| idToString p.id )
                    , ( "name", Encode.string p.name )
                    , ( "parentId", Encode.string <| idToString p.id )
                    ]

        players =
            Encode.list
                cvtPlayer
                model.playerList
    in
    Encode.object
        [ ( "pmList", pmList )
        , ( "players", players )
        ]


idDecoder : Decoder String -> Decoder Id
idDecoder =
    Decode.map
        (\id_ ->
            if id_ == "temp" then
                TempId

            else
                Id id_
        )


pmDecoder : Decoder PlayerManager
pmDecoder =
    Decode.map2
        PlayerManager
        (Decode.field "id" Decode.string |> idDecoder)
        (Decode.field "name" Decode.string)


playerDecoder : Decoder Player
playerDecoder =
    Decode.map3
        Player
        (Decode.field "id" Decode.string |> idDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "parentId" Decode.string |> idDecoder)


pmModelDecoder : Decoder PMModel
pmModelDecoder =
    Decode.map6 PMModel
        (Decode.field "siteId" Decode.int)
        (Decode.field "pmList" (Decode.list pmDecoder))
        (Decode.field "playerList" (Decode.list playerDecoder))
        (Decode.maybe (Decode.field "editimgPM" pmDecoder))
        (Decode.maybe (Decode.field "selectedPMId" Decode.string |> idDecoder))
        (Decode.maybe (Decode.field "lastInputPM" pmDecoder))
