module Checklist.Model exposing (Flags, Model, Popup(..), initialModel)

import Checklist as Checklist exposing (Checklist)
import Checklist.Messages exposing (Msg)
import Checklist.Types exposing (AttachmentUpload)
import Dict exposing (Dict)
import Json.Decode as D


type alias Flags =
    { procosysPlantId : String
    , parentCommPk : String
    , size : String
    }


type alias Model =
    { procosysPlantId : String
    , parentCommPk : String
    , size : Float
    , apiToken : String
    , checklists : Dict Int Checklist
    , selectedChecklist : Maybe Int
    , requests : Dict Int (List (String -> String -> Cmd Msg))
    , errorMsg : String
    , customCheckItemField : String
    , currentAttachment : Maybe AttachmentUpload
    , popup : Popup
    }


initialModel : Flags -> ( Model, Cmd Msg )
initialModel flags =
    ( { procosysPlantId = flags.procosysPlantId
      , parentCommPk = flags.parentCommPk
      , size = String.toFloat flags.size |> Maybe.withDefault 14
      , apiToken = ""
      , checklists = Dict.empty
      , selectedChecklist = Nothing
      , requests = Dict.empty
      , errorMsg = ""
      , customCheckItemField = ""
      , currentAttachment = Nothing
      , popup = NoPopup
      }
    , Cmd.none
    )


decodeChecklists : String -> List Checklist
decodeChecklists jsonString =
    case D.decodeString (D.list Checklist.decoder) jsonString of
        Ok checklists ->
            checklists

        Err err ->
            []


type Popup
    = NoPopup
    | DeleteAttachmentPopup Checklist Checklist.Attachment
