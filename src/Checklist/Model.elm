module Checklist.Model exposing (Flags, Model, initialModel)

import Checklist as Checklist exposing (Checklist)
import Dict exposing (Dict)
import Json.Decode as D
import Checklist.Messages exposing (Msg)


type alias Flags =
    { procosysPlantId : String
    }


type alias Model =
    { procosysPlantId : String
    , apiToken : String
    , checklists : Dict Int Checklist
    , selectedChecklist : Maybe Int
    , requests : Dict Int (List (String -> String -> Cmd Msg))
    , errorMsg : String
    , customCheckItemField : String
    }


initialModel : Flags -> ( Model, Cmd Msg )
initialModel flags =
    ( { procosysPlantId = flags.procosysPlantId
      , apiToken = ""
      , checklists = Dict.empty
      , selectedChecklist = Nothing
      , requests = Dict.empty
      , errorMsg = ""
      , customCheckItemField = ""
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
