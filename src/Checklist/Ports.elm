port module Checklist.Ports exposing (fromJs, toJs)

import Json.Encode as E


port toJs : E.Value -> Cmd msg


port fromJs : (E.Value -> msg) -> Sub msg
