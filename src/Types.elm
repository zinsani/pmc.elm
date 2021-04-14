module Types exposing (..)


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


type alias PC =
    { siteId : Int
    , playerManager : PlayerManager
    , editingPlayer : Maybe Player
    , selectedPlayerId : Maybe Id
    , lastInputPlayer : Maybe Player
    , listEditing : Bool
    }


type alias PlayerManagerEdit =
    { siteId : Int
    , playerManager : PlayerManager
    }


type alias PlayerEdit =
    { siteId : Int
    , player : Player
    }


type Model
    = SiteListPage Sites
    | MainPage Site (List PlayerManager)
    | DetailPage PC
    | PlayerManagerEditPage PlayerManagerEdit
    | PlayerEditPage PlayerEdit
    | Fetch FetchModel


type FetchModel
    = FetchSites
    | FetchSite Int
    | FetchPC Int Id
    | FetchErr String
    | UpdateSites
    | UpdateSite Int
    | UpdatePC Int Id


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


type UIMsg
    = ClickNew
    | ClickEdit Id
    | ClickDelete Id
    | ClickBack


type MasterMsg
    = ToggleEditModeOnMaster
    | SelectPM Id
    | UIMsgOnMaster UIMsg


type DetailMsg
    = GotNewPM PlayerManager
    | GotModifiedPM PlayerManager
    | EndEditingPM
    | InputPName String
    | ToggleEditModeOnDetail
    | GotNewIdOfPlayer Id
    | SelectPlayer Id
    | UIMsgOnDetail UIMsg


type FetchingMsg
    = FetchingSites
    | FetchedSites Sites
    | FetchingSite
    | FetchedSite (Maybe Site) (List PlayerManager)
    | FetchingPC
    | FetchedPC (Maybe PC)
    | FetchingError String


type PMEditMsg
    = PMEditName String
    | PMEditIpAddress String
    | PMEditPort (Maybe Int)
    | PMEditTimeoutSecondsToStartup (Maybe Float)
    | PMEditSourcePath String
    | PMEditMinimize Bool
    | PMEditSubmit PlayerManager
    | PMEditCancel


type PEditMsg
    = PEditName String
    | PEditDirectory String
    | PEditExeFileName String
    | PEditSourceDir String
    | PEditParameters String
    | PEditDelaySecondsToStart (Maybe Float)
    | PEditExcludeFiles String
    | PEditExcludeDirectories String
    | PEditLogDir String
    | PEditSubmit Player
    | PEditCancel


type Msg
    = SitesMsg SitesMsg
    | MasterMsg MasterMsg
    | DetailMsg DetailMsg
    | FetchingMsg FetchingMsg
    | PMEditMsg PMEditMsg
    | PEditMsg PEditMsg


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


type InputValue
    = StringInput String
    | IntInput Int
    | FloatInput Float
    | BoolInput Bool
