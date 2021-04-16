port module Api exposing (..)

import Browser
import Hashids exposing (encodeUsingSalt)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Process
import Task
import Time
import Types
    exposing
        ( Data
        , Id(..)
        , PC
        , Player
        , PlayerManager
        , PlayerOptions
        , Site
        , Sites
        )


port fetch : () -> Cmd msg


port storeData : Value -> Cmd msg


port storeSites : Value -> Cmd msg


port storeSite : Value -> Cmd msg


port storePlayerManagers : Value -> Cmd msg


port storePlayerManager : ( Int, Value ) -> Cmd msg


port removePlayerManager : ( Int, Value ) -> Cmd msg


port onStoreChange : (Value -> msg) -> Sub msg


initialize : msg -> Cmd msg
initialize msg =
    Cmd.batch
        [ { sites = defaultSites, playerManagers = [] }
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
            sites.list ++ [ defaultSite newId siteName ]
    in
    Cmd.batch
        [ { sites
            | list = newList
            , listEditing = False
          }
            |> sitesEncoder
            |> storeSites
        , getResultAfter msg 10
        ]


selectSite : msg -> Int -> Sites -> Cmd msg
selectSite msg siteId sites =
    let
        newSites =
            { sites
                | listEditing = False
                , selected =
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


modifySites : msg -> List Site -> Sites -> Cmd msg
modifySites msg modified sites =
    let
        newSites =
            { sites
                | list = modified
                , listEditing = False
                , editingSite = Nothing
            }
    in
    Cmd.batch
        [ newSites
            |> sitesEncoder
            |> storeSites
        , getResultAfter msg 10
        ]


createId : (Id -> msg) -> Cmd msg
createId msg =
    let
        salt =
            "zinsani"

        handler msg_ posix =
            Time.posixToMillis posix
                |> encodeUsingSalt salt
                |> Id
                |> msg_
    in
    Task.perform (handler msg) Time.now


createNewPM : msg -> PlayerManager -> Int -> Cmd msg
createNewPM msg pm siteId =
    Cmd.batch
        [ pm |> pmEncoder |> Tuple.pair siteId |> storePlayerManager
        , getResultAfter msg 0
        ]


modifyPlayerManager : msg -> PlayerManager -> Int -> Cmd msg
modifyPlayerManager msg pm siteId =
    Cmd.batch
        [ pm |> pmEncoder |> Tuple.pair siteId |> storePlayerManager
        , getResultAfter msg 0
        ]


deletePlayerManager : msg -> Id -> Int -> Cmd msg
deletePlayerManager msg pmId siteId =
    Cmd.batch
        [ pmId |> idEncoder |> Tuple.pair siteId |> removePlayerManager
        , getResultAfter msg 0
        ]


createNewPlayer : msg -> Player -> PlayerManager -> Int -> Cmd msg
createNewPlayer msg player pm siteId =
    let
        newPlayers =
            List.concat [ pm.players, [ { player | parentId = pm.id } ] ]
    in
    Cmd.batch
        [ { pm | players = newPlayers } |> pmEncoder |> Tuple.pair siteId |> storePlayerManager
        , getResultAfter msg 0
        ]


modifyPlayer : msg -> Player -> PlayerManager -> Int -> Cmd msg
modifyPlayer msg player pm siteId =
    let
        newPlayers =
            pm.players
                |> List.map
                    (\p ->
                        if p.id == player.id then
                            { player | parentId = pm.id }

                        else
                            p
                    )
    in
    Cmd.batch
        [ { pm | players = newPlayers } |> pmEncoder |> Tuple.pair siteId |> storePlayerManager
        , getResultAfter msg 0
        ]


deletePlayer : msg -> Id -> PlayerManager -> Int -> Cmd msg
deletePlayer msg playerId pm siteId =
    let
        newPlayers =
            pm.players
                |> List.filter
                    (\p -> p.id /= playerId)
    in
    Cmd.batch
        [ { pm | players = newPlayers } |> pmEncoder |> Tuple.pair siteId |> storePlayerManager
        , getResultAfter msg 0
        ]


defaultData : Data
defaultData =
    { sites = defaultSites
    , playerManagers = []
    }


defaultSites : Sites
defaultSites =
    { list = []
    , selected = Nothing
    , newSiteName = ""
    , editingSite = Nothing
    , saveSelection = False
    , listEditing = False
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
        (Decode.field "playerManagers" (Decode.list pmDecoder))


sitesDecoder : Decoder Sites
sitesDecoder =
    Decode.map6
        Sites
        (Decode.field "list" (Decode.list siteDecoder))
        (Decode.maybe (Decode.field "selected" Decode.int))
        (Decode.field "newSiteName" Decode.string)
        (Decode.maybe (Decode.field "editingSite" siteDecoder))
        (Decode.field "saveSelection" Decode.bool)
        (Decode.field "listEditing" Decode.bool)


siteDecoder : Decoder Site
siteDecoder =
    Decode.map8 Site
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "playerManagers" (Decode.list idDecoder))
        (Decode.field "players" (Decode.list idDecoder))
        (Decode.maybe (Decode.field "editingPM" pmDecoder))
        (Decode.maybe (Decode.field "selectedPMId" idDecoder))
        (Decode.maybe (Decode.field "lastInputPM" pmDecoder))
        (Decode.field "listEditing" Decode.bool)


defaultSite : Int -> String -> Site
defaultSite siteId siteName =
    { id = siteId
    , name = siteName
    , playerManagers = []
    , players = []
    , lastInputPM = Nothing
    , editingPM = Nothing
    , selectedPMId = Nothing
    , listEditing = False
    }


defaultPlayerManager : PlayerManager
defaultPlayerManager =
    { id = TempId
    , name = "New PC"
    , timeoutSecondsToStartup = 0
    , minimize = True
    , sourcePath = "\\\\pmc-pc"
    , ipaddress = "192.168.100.1"
    , port_ = 11020
    , players = []
    }


defaultPlayer : Id -> Player
defaultPlayer parentId =
    { id = TempId
    , name = "New Player"
    , parentId = parentId
    , directory = "c:/ezwith/player"
    , exeFileName = Nothing
    , sourceDir = "player"
    , options =
        { parameters = Nothing
        , delaySecondsToStart = 0
        , watchDogEnabled = True
        , excludeFiles = Nothing
        , excludeDirectories = Nothing
        , logDir = Nothing
        }
    }


defaultPC : Int -> PlayerManager -> PC
defaultPC siteId playerManager =
    { siteId = siteId
    , playerManager = playerManager
    , editingPlayer = Nothing
    , selectedPlayerId = Nothing
    , lastInputPlayer = Nothing
    , listEditing = False
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
        , ( "timeoutSecondsToStartup", Encode.float pm.timeoutSecondsToStartup )
        , ( "minimize", Encode.bool pm.minimize )
        , ( "sourcePath", Encode.string pm.sourcePath )
        , ( "ipaddress", Encode.string pm.ipaddress )
        , ( "port", Encode.int pm.port_ )
        , ( "players", Encode.list playerEncoder pm.players )
        ]


playerEncoder : Player -> Value
playerEncoder p =
    Encode.object
        [ ( "id", idEncoder p.id )
        , ( "name", Encode.string p.name )
        , ( "parentId", idEncoder p.parentId )
        , ( "directory", Encode.string p.directory )
        , ( "exeFileName"
          , p.exeFileName
                |> Maybe.map (\exe -> Encode.string exe)
                |> Maybe.withDefault Encode.null
          )
        , ( "sourceDir", Encode.string p.sourceDir )
        , ( "options", playerOptionsEncoder p.options )
        ]


playerOptionsEncoder : PlayerOptions -> Value
playerOptionsEncoder options =
    Encode.object
        [ ( "parameters"
          , options.parameters
                |> Maybe.map (\p -> Encode.string p)
                |> Maybe.withDefault Encode.null
          )
        , ( "delaySecondsToStart", Encode.float options.delaySecondsToStart )
        , ( "watchDogEnabled", Encode.bool options.watchDogEnabled )
        , ( "excludeFiles"
          , options.excludeFiles
                |> Maybe.map (\f -> Encode.list Encode.string f)
                |> Maybe.withDefault Encode.null
          )
        , ( "excludeDirectories"
          , options.excludeDirectories
                |> Maybe.map (\f -> Encode.list Encode.string f)
                |> Maybe.withDefault Encode.null
          )
        ]


defaultModelJson : Value
defaultModelJson =
    Encode.object
        [ ( "playerManagers"
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
        , ( "playerManagers", Encode.list pmEncoder data.playerManagers )
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
        , ( "listEditing", Encode.bool sites.listEditing )
        ]


siteEncoder : Site -> Encode.Value
siteEncoder model =
    let
        playerManagers =
            Encode.list
                idEncoder
                model.playerManagers

        players =
            Encode.list
                idEncoder
                model.players
    in
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "name", Encode.string model.name )
        , ( "playerManagers", playerManagers )
        , ( "players", players )
        , ( "editingPM", Encode.null )
        , ( "selectedPMId", Encode.null )
        , ( "lastInputPM", Encode.null )
        , ( "listEditing", Encode.bool False )
        ]


idDecoder : Decoder Id
idDecoder =
    Decode.string
        |> Decode.map
            (\id_ ->
                if id_ == "temp" then
                    TempId

                else
                    Id id_
            )


pmDecoder : Decoder PlayerManager
pmDecoder =
    Decode.map8
        PlayerManager
        (Decode.field "id" idDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "timeoutSecondsToStartup" Decode.float)
        (Decode.field "minimize" Decode.bool)
        (Decode.field "sourcePath" Decode.string)
        (Decode.field "ipaddress" Decode.string)
        (Decode.field "port" Decode.int)
        (Decode.field "players" (Decode.list playerDecoder))


playerDecoder : Decoder Player
playerDecoder =
    Decode.map7
        Player
        (Decode.field "id" idDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "parentId" idDecoder)
        (Decode.field "directory" Decode.string)
        (Decode.maybe (Decode.field "exeFileName" Decode.string))
        (Decode.field "sourceDir" Decode.string)
        (Decode.field "options"
            (Decode.map6 PlayerOptions
                (Decode.maybe (Decode.field "parameters" Decode.string))
                (Decode.field "delaySecondsToStart" Decode.float)
                (Decode.field "watchDogEnabled" Decode.bool)
                (Decode.maybe (Decode.field "excludeFiles" (Decode.list Decode.string)))
                (Decode.maybe (Decode.field "excludeDirectories" (Decode.list Decode.string)))
                (Decode.maybe (Decode.field "logDir" Decode.string))
            )
        )
