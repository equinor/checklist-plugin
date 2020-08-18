module Checklist.Types exposing (..)

import Bytes exposing (Bytes)
import File exposing (File)


type alias TokenSuccess =
    { refNo : Int
    , token : String
    }


type alias Blob =
    { contentType : String
    , bytes : Bytes
    }


type alias AttachmentUpload =
    { file : File
    , checklistId : Int
    , uri : String
    , name : String
    }
