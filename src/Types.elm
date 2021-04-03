module Types exposing (..)

import Random
import UUID


type alias Data =
    { sites : Sites
    , pmModels : List PMModel
    }


type alias Sites =
    { list : List Site
    , selected : Maybe Int
    , newSiteName : String
    , editingSite : Maybe Site
    , saveSelection : Bool
    }


type alias Site =
    { id : Int, name : String }


type Model
    = SiteListPage Sites
    | MainPage PMModel
    | Fetching FetchingModel


type FetchingModel
    = FetchingSiteList
    | FetchingSite Int
    | FetchingErr String
    | UpdatingSiteList
    | UpdatingSite Int



-- = SucceedWithSites Sites
-- | SucceedWithPMList (List PlayerManager)
-- | FailWithSites String
-- | FailWithPMList String


type SiteListMsg
    = ClickNewSite String
    | ClickOpenSite Int
    | ClickDeleteSite Int
    | InputSiteName String
      -- | DeleteSite Int
    | StartEditSite Int
    | EditingSiteName String
    | EndEditSite Site
    | ToggleSaveSelection Bool


type FetchingMsg
    = FetchingSites
    | FetchedSites Sites
    | FetchingPMModel
    | FetchedPMModel (Maybe PMModel)
    | FetchingError String


type Msg
    = SiteListMsg SiteListMsg
    | PMMsg PMMsg
    | FetchingMsg FetchingMsg


type Id
    = Id String
    | TempId


idToString : Id -> String
idToString id =
    case id of
        Id id_ ->
            id_

        TempId ->
            "temp"


idSeed : Random.Seed
idSeed =
    Random.initialSeed 999


createId : Random.Seed -> String
createId seed =
    Random.step UUID.generator seed
        |> Tuple.first
        |> UUID.toRepresentation UUID.Urn



-- |> Debug.log "createId" Tuple.first


type alias PlayerManager =
    { id : Id
    , name : String
    , timeoutSecondsToStartup : Float
    , minimize : Bool
    , sourcePath : String
    , ipaddress : String
    , port_ : Int
    , players : List Player
    }


type alias Player =
    { id : Id
    , name : String
    , parentId : Id
    , directory : String
    , exeFileName : Maybe String
    , sourceDir : String
    , options : PlayerOptions
    }


type alias PlayerOptions =
    { parameters : Maybe String
    , delaySecondsToStart : Float
    , watchDogEnabled : Bool
    , excludeFiles : Maybe (List String)
    , excludeDirectories : Maybe (List String)
    , logDir : Maybe String
    }


type alias PMModel =
    { siteId : Int
    , pmList : List PlayerManager
    , playerList : List Player
    , editingPM : Maybe PlayerManager
    , selectedPMId : Maybe Id
    , lastInputPM : Maybe PlayerManager
    }


type PMMsg
    = ClickNewPlayerManager
      -- | AddNewPlayerManager PlayerManager
    | ClickNewPlayer
    | ClickSubmit
    | ClickCancel
    | InputPMName String
    | InputPName String
    | BackToSiteList
