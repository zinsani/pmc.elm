module Types exposing (..)

import Json.Decode as Decode
import Random
import UUID exposing (UUID)


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
    = SiteListPage Data
    | MainPage PMModel


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


type Msg
    = SiteListMsg SiteListMsg
    | OpenSite PMModel
    | UpdateData (Maybe Data)
    | PMMsg PMMsg


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
    }


type alias Player =
    { id : Id
    , name : String
    , parentId : Id
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
    = SelectPM Id
    | SelectPlayer Id
    | ClickNewPlayerManager
    | AddNewPlayerManager PlayerManager
    | ClickNewPlayer
    | ClickSubmit
    | ClickCancel
    | InputPMName String
    | InputPName String
    | GotStoreChanged PMModel
    | GotErrWhenParsing Decode.Error
