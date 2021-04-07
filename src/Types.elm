module Types exposing (..)

import Random


type alias Data =
    { sites : Sites
    , playerManagers : List PlayerManager
    }


type alias Sites =
    { list : List Site
    , selected : Maybe Int
    , newSiteName : String
    , editingSite : Maybe Site
    , saveSelection : Bool
    , listEditing : Bool
    }


type alias Site =
    { id : Int
    , name : String
    , playerManagers : List Id
    , players : List Id
    , editingPM : Maybe PlayerManager
    , selectedPMId : Maybe Id
    , lastInputPM : Maybe PlayerManager
    , listEditing : Bool
    }


type Model
    = SiteListPage Sites
    | MainPage Site (List PlayerManager)
    | DetailPage PlayerManager
    | Fetch FetchModel


type FetchModel
    = FetchSites
    | FetchSite Int
    | FetchErr String
    | UpdateSites
    | UpdateSite Int


type SitesMsg
    = ClickNewSite String
    | ClickOpenSite Int
    | ClickDeleteSite Int
    | InputSiteName String
    | StartEditSite Int
    | EditingSiteName String
    | EndEditSite Site
    | ToggleSaveSelection Bool
    | ToggleEditModeOnSites


type FetchingMsg
    = FetchingSites
    | FetchedSites Sites
    | FetchingSite
    | FetchedSite (Maybe Site) (List PlayerManager)
    | FetchingError String


type Msg
    = SitesMsg SitesMsg
    | MasterMsg MasterMsg
    | FetchingMsg FetchingMsg


type Id
    = Id String
    | TempId


type PortErr
    = PortErr String


idToString : Id -> String
idToString id =
    case id of
        Id id_ ->
            id_

        TempId ->
            "temp"


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


type MasterMsg
    = ClickNewPlayerManager
    | ClickNewPlayer
    | ClickSubmit
    | ClickCancel
    | ClickDeletePM Id
    | InputPMName String
    | InputPName String
    | BackToSiteList
    | ToggleEditMode
    | GotNewIdOfPM Id
    | SelectPM Id
