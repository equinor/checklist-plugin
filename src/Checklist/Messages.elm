module Checklist.Messages exposing (ApiResult(..), Msg(..))

import Checklist as Checklist exposing (Checklist)
import Checklist.Types exposing (..)
import File exposing (File)
import Http
import Json.Decode as D


type Msg
    = NoOp
      -- Data Handling
    | GotChecklists (List Checklist)
    | GotToken TokenSuccess
    | DecodeError D.Error
      -- Screen Interaction
    | ChecklistPressed Checklist
    | GotApiResult ApiResult
    | NaCheckItemPressed Checklist Checklist.Item
    | OkCheckItemPressed Checklist Checklist.Item
    | SignChecklistButtonPressed Checklist
    | UnsignChecklistButtonPressed Checklist
    | VerifyChecklistButtonPressed Checklist
    | UnverifyChecklistButtonPressed Checklist
    | MetaTableCellInput Checklist Checklist.Item Checklist.Row Checklist.ColumnLabel String
    | MetaTableCellLostFocus Checklist Checklist.Item Checklist.Row Checklist.Cell
    | CommentFieldInput Checklist String
    | CommentFieldLostFocus Checklist String
    | CustomCheckItemInput String
    | AddCustomCheckItemButtonPressed Checklist
    | OkCustomCheckItemPressed Checklist Checklist.CustomItem
    | DeleteCustomCheckItemButtomPressed Checklist Checklist.CustomItem
    | AttachmentPressed Checklist Checklist.Attachment
    | DeleteAttachmentButtonPressed Checklist Checklist.Attachment
    | ConfirmDeleteAttachment Checklist Checklist.Attachment
    | CancelPopupPressed
    | NewAttachmentButtonPressed Checklist
    | AttachmentFileLoaded Int File
    | AttachmentDecoded File Int String String
    | FileNameInputChanged String
    | AddUploadedAttachmentToChecklist Checklist


type ApiResult
    = GotChecklistDetails Int (Result Http.Error Checklist.Details)
    | SetNaResult Checklist (Result Http.Error ())
    | SetOkResult Checklist (Result Http.Error ())
    | ClearResult Checklist (Result Http.Error ())
    | SignChecklistResult Checklist (Result Http.Error ())
    | UnsignChecklistResult Checklist (Result Http.Error ())
    | VerifyChecklistResult Checklist (Result Http.Error ())
    | UnverifyChecklistResult Checklist (Result Http.Error ())
    | UpdateMetaTableCellResult Checklist (Result Http.Error ())
    | CommentChecklistResult Checklist (Result Http.Error ())
    | GotNextCustomItemNo Checklist (Result Http.Error String)
    | AddCustomItemResult Checklist (Result Http.Error ())
    | DeleteCustomItemResult Checklist (Result Http.Error ())
    | GotAttachments Checklist (Result Http.Error (List Checklist.Attachment))
    | GotAttachment Checklist Checklist.Attachment (Result Http.Error Blob)
    | DeleteAttachmentResult Checklist Checklist.Attachment (Result Http.Error ())
    | AddAttachmentResult Checklist AttachmentUpload (Result Http.Error ())
