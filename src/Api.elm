port module Api exposing (..)

import Browser
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Process
import Task
import Types exposing (Data, Id(..), PMModel, Player, PlayerManager, Site, Sites, createId, idSeed, idToString)


port fetch : () -> Cmd msg


port storeData : Value -> Cmd msg


port storeSites : Value -> Cmd msg


port storePMModels : Value -> Cmd msg


port storePMModel : Value -> Cmd msg


port onStoreChange : (Value -> msg) -> Sub msg



-- fetchMsg : (a -> msg) -> Cmd msg
-- fetchMsg msg =
--     fetch ()
--         |> Cmd.map msg


initialize : msg -> Cmd msg
initialize msg =
    Cmd.batch
        [ { sites = defaultSites, pmModels = [] }
            |> storageEncoder
            |> storeData
        , getResultAfter msg 0
        ]


createNewSite : msg -> String -> Sites -> Cmd msg
createNewSite msg siteName sites =
    let
        newId =
            (List.map (\el -> el.id) sites.list
                |> List.maximum
                |> Maybe.withDefault 0
            )
                + 1

        newList =
            sites.list ++ [ { id = newId, name = siteName } ]
    in
    Cmd.batch
        [ { sites | list = newList }
            |> sitesEncoder
            |> storeSites
        , defaultPM newId
            |> pmModelEncoder
            |> storePMModel
        , getResultAfter msg 10
        ]


selectSite : msg -> Int -> Sites -> Cmd msg
selectSite msg siteId sites =
    let
        newSites =
            { sites
                | selected =
                    if sites.saveSelection then
                        Just siteId

                    else
                        Nothing
            }
    in
    Cmd.batch
        [ newSites
            |> sitesEncoder
            |> storeSites
        , getResultAfter msg 0
        ]


getResultAfter : msg -> Float -> Cmd msg
getResultAfter msg waitMs =
    Process.sleep waitMs
        |> Task.perform (\_ -> msg)


modifySiteList : msg -> List Site -> Sites -> Cmd msg
modifySiteList msg modified sites =
    let
        newSites =
            { sites
                | list = modified
                , editingSite = Nothing
            }
    in
    Cmd.batch
        [ newSites
            |> sitesEncoder
            |> storeSites
        , getResultAfter msg 10
        ]


createIdIfNecessary : { a | id : Id } -> { a | id : Id }
createIdIfNecessary obj =
    if obj.id == TempId then
        { obj | id = Id <| createId idSeed }

    else
        obj


createNewPM : msg -> PlayerManager -> PMModel -> Cmd msg
createNewPM msg newPM model =
    let
        pm =
            createIdIfNecessary newPM
    in
    Cmd.batch
        [ { model
            | pmList = model.pmList ++ [ pm ]
            , editingPM = Nothing
            , lastInputPM = Just newPM
          }
            |> pmModelEncoder
            |> storePMModel
        , getResultAfter msg 0
        ]


modifyPMList : msg -> List PlayerManager -> PlayerManager -> PMModel -> Cmd msg
modifyPMList msg modifiedList pm model =
    Cmd.batch
        [ { model
            | pmList = modifiedList
            , editingPM = Nothing
            , lastInputPM = Just { pm | id = TempId }
          }
            |> pmModelEncoder
            |> storePMModel
        , getResultAfter msg 0
        ]


defaultData : Data
defaultData =
    { sites = defaultSites
    , pmModels = []
    }


defaultSites : Sites
defaultSites =
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
                        let
                            res =
                                Decode.decodeValue Decode.string v
                                    |> Result.andThen (Decode.decodeString storageDecoder)
                        in
                        Debug.log "flags parsed" res |> Result.toMaybe
                    )
                |> config.init
    in
    Browser.document
        { init = init
        , update = config.update
        , subscriptions = config.subscriptions
        , view = config.view
        }


decodeFromChange : Value -> Result Decode.Error Data
decodeFromChange val =
    Decode.decodeValue storageDecoder val


storageDecoder : Decoder Data
storageDecoder =
    Decode.map2
        Data
        (Decode.field "sites" sitesDecoder)
        (Decode.field "pmModels" (Decode.list pmModelDecoder))


sitesDecoder : Decoder Sites
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


defaultPM : Int -> PMModel
defaultPM siteId =
    { pmList = []
    , playerList = []
    , lastInputPM = Nothing
    , editingPM = Nothing
    , selectedPMId = Nothing
    , siteId = siteId
    }


idEncoder : Id -> Value
idEncoder id =
    case id of
        Id id_ ->
            Encode.string id_

        TempId ->
            Encode.string "temp"


pmEncoder : PlayerManager -> Value
pmEncoder pm =
    Encode.object
        [ ( "id", idEncoder pm.id )
        , ( "name", Encode.string pm.name )
        ]


playerEncoder : Player -> Value
playerEncoder p =
    Encode.object
        [ ( "name", Encode.string p.name ) ]


defaultModelJson : Value
defaultModelJson =
    Encode.object
        [ ( "pmList"
          , Encode.list pmEncoder []
          )
        , ( "players"
          , Encode.list playerEncoder []
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
        , ( "playerList", players )
        , ( "siteId", Encode.int model.siteId )
        , ( "editingPM", Encode.null )
        , ( "selectedPMId", Encode.null )
        , ( "lastInputPM", Encode.null )
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
        (Decode.maybe (Decode.field "editingPM" pmDecoder))
        (Decode.maybe (Decode.field "selectedPMId" Decode.string |> idDecoder))
        (Decode.maybe (Decode.field "lastInputPM" pmDecoder))
