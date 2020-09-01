module Checklist.Main exposing (..)

import Browser
import Checklist as Checklist exposing (Checklist)
import Checklist.Messages exposing (Msg(..))
import Checklist.Model as Model exposing (Flags, Model, Popup(..))
import Checklist.Ports as Ports
import Checklist.Types exposing (..)
import Checklist.Update exposing (update)
import Checklist.View as View
import Dict
import Element exposing (..)
import Element.Font as Font
import Equinor.Palette as Palette
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E


view : Model -> Element Msg
view model =
    case model.popup of
        DeleteAttachmentPopup punch attachment ->
            View.deleteAttachmentPopup punch attachment

        NoPopup ->
            if Dict.isEmpty model.checklists then
                el [ Font.size <| round model.size ] (text "No Checklists")

            else
                View.renderChecklists model


main : Program Flags Model Msg
main =
    Browser.element
        { init = Model.initialModel
        , update = update
        , subscriptions = \model -> Ports.fromJs handleJsMsg
        , view = \model -> layout [ width fill, height fill, Font.color Palette.slateBlue, Font.size 14 ] (view model)
        }


handleJsMsg : E.Value -> Msg
handleJsMsg jsValue =
    case D.decodeValue jsValueDecoder jsValue of
        Ok msg ->
            msg

        Err err ->
            DecodeError err


jsValueDecoder : D.Decoder Msg
jsValueDecoder =
    D.field "topic" D.string
        |> D.andThen (\topic -> D.field "payload" (decoder topic))


decoder : String -> D.Decoder Msg
decoder topic =
    case topic of
        "token" ->
            D.map GotToken tokenDecoder

        "checklists" ->
            D.map GotChecklists (D.list Checklist.decoder)

        _ ->
            D.fail "Unknown msg received from Js"


tokenDecoder : D.Decoder TokenSuccess
tokenDecoder =
    D.map2 TokenSuccess
        (D.field "refNo" D.int)
        (D.field "token" D.string)
