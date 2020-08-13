module Checklist.Api exposing (addCustomItem, checklistDetails, clearCheckItem, clearCustomCheckItem, clientId, deleteCustomItem, nextCustomItemNo, setCheckItemNa, setCheckItemOk, setCustomCheckItemOk, signChecklist, unSignChecklist, unVerifyChecklist, updateComment, updateMetaTableCell, verifyChecklist)

import Checklist as Checklist
import Http
import Json.Decode as D
import Json.Encode as E
import Checklist.Messages exposing (..)
import Checklist.Types exposing (..)
import Url.Builder exposing (QueryParameter, int, string)


type Environment
    = Development
    | Production


environment : Environment
environment =
    Development


clientId : String
clientId =
    "47641c40-0135-459b-8ab4-459e68dc8d08/.default"


apiVersion : QueryParameter
apiVersion =
    string "api-version" "4.1"


url : List String -> List QueryParameter -> String
url paths queryParams =
    case environment of
        Development ->
            Url.Builder.crossOrigin
                "https://procosyswebapiqp.equinor.com"
                ("api" :: paths)
                (apiVersion :: queryParams)

        Production ->
            Url.Builder.crossOrigin
                "https://procosyswebapi.equinor.com"
                ("api" :: paths)
                (apiVersion :: queryParams)


checklistDetails : Checklist.Checklist -> String -> String -> Cmd Msg
checklistDetails checklist plantId token =
    Http.request
        { method = "GET"
        , url =
            url
                [ "Checklist"
                , case checklist.group of
                    Checklist.CPCL ->
                        "Comm"

                    Checklist.Preservation ->
                        "Preservation"

                    _ ->
                        "MC"
                ]
                [ string "plantId" plantId
                , int "checklistId" checklist.id
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (GotApiResult << GotChecklistDetails checklist.id)
                Checklist.detailsApiDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


nextCustomItemNo : Checklist.Checklist -> String -> String -> Cmd Msg
nextCustomItemNo checklist plantId token =
    Http.request
        { method = "GET"
        , url =
            url
                [ "Checklist", "CustomItem", "NextItemNo" ]
                [ string "plantId" plantId
                , int "checklistId" checklist.id
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect =
            Http.expectString
                (GotApiResult << GotNextCustomItemNo checklist)
        , timeout = Nothing
        , tracker = Nothing
        }


deleteCustomItem : Checklist.Checklist -> Checklist.CustomItem -> String -> String -> Cmd Msg
deleteCustomItem checklist customItem plantId token =
    Http.request
        { method = "DELETE"
        , url =
            url
                [ "Checklist", "CustomItem" ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "CheckListId", E.int checklist.id )
                    , ( "CustomCheckItemId", E.int customItem.id )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << DeleteCustomItemResult checklist)
        , timeout = Nothing
        , tracker = Nothing
        }


setCheckItemNa : Checklist.Checklist -> Checklist.Item -> String -> String -> Cmd Msg
setCheckItemNa checklist checkItem plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "CheckList", "Item", "SetNA" ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "CheckListId", E.int checklist.id )
                    , ( "CheckItemId", E.int checkItem.id )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << SetNaResult checklist)
        , timeout = Nothing
        , tracker = Nothing
        }


setCheckItemOk : Checklist.Checklist -> Checklist.Item -> String -> String -> Cmd Msg
setCheckItemOk checklist checkItem plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "CheckList", "Item", "SetOk" ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "CheckListId", E.int checklist.id )
                    , ( "CheckItemId", E.int checkItem.id )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << SetOkResult checklist)
        , timeout = Nothing
        , tracker = Nothing
        }


setCustomCheckItemOk : Checklist.Checklist -> Checklist.CustomItem -> String -> String -> Cmd Msg
setCustomCheckItemOk checklist checkItem plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "CheckList", "CustomItem", "SetOk" ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "CheckListId", E.int checklist.id )
                    , ( "CustomCheckItemId", E.int checkItem.id )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << SetOkResult checklist)
        , timeout = Nothing
        , tracker = Nothing
        }


clearCheckItem : Checklist.Checklist -> Checklist.Item -> String -> String -> Cmd Msg
clearCheckItem checklist checkItem plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "CheckList", "Item", "Clear" ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "CheckListId", E.int checklist.id )
                    , ( "CheckItemId", E.int checkItem.id )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << ClearResult checklist)
        , timeout = Nothing
        , tracker = Nothing
        }


clearCustomCheckItem : Checklist.Checklist -> Checklist.CustomItem -> String -> String -> Cmd Msg
clearCustomCheckItem checklist checkItem plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "CheckList", "CustomItem", "Clear" ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "CheckListId", E.int checklist.id )
                    , ( "CustomCheckItemId", E.int checkItem.id )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << ClearResult checklist)
        , timeout = Nothing
        , tracker = Nothing
        }


signChecklist : Checklist.Checklist -> String -> String -> Cmd Msg
signChecklist checklist plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "CheckList"
                , case checklist.group of
                    Checklist.CPCL ->
                        "Comm"

                    Checklist.Preservation ->
                        "Preservation"

                    _ ->
                        "MC"
                , "Sign"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.int checklist.id)
        , expect =
            Http.expectWhatever
                (GotApiResult << SignChecklistResult checklist)
        , timeout = Nothing
        , tracker = Nothing
        }


unSignChecklist : Checklist.Checklist -> String -> String -> Cmd Msg
unSignChecklist checklist plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "CheckList"
                , case checklist.group of
                    Checklist.CPCL ->
                        "Comm"

                    Checklist.Preservation ->
                        "Preservation"

                    _ ->
                        "MC"
                , "Unsign"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.int checklist.id)
        , expect =
            Http.expectWhatever
                (GotApiResult << UnsignChecklistResult checklist)
        , timeout = Nothing
        , tracker = Nothing
        }


verifyChecklist : Checklist.Checklist -> String -> String -> Cmd Msg
verifyChecklist checklist plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "CheckList"
                , case checklist.group of
                    Checklist.CPCL ->
                        "Comm"

                    Checklist.Preservation ->
                        "Preservation"

                    _ ->
                        "MC"
                , "Verify"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.int checklist.id)
        , expect =
            Http.expectWhatever
                (GotApiResult << VerifyChecklistResult checklist)

        --Checklist.detailsApiDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


unVerifyChecklist : Checklist.Checklist -> String -> String -> Cmd Msg
unVerifyChecklist checklist plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "CheckList"
                , case checklist.group of
                    Checklist.CPCL ->
                        "Comm"

                    Checklist.Preservation ->
                        "Preservation"

                    _ ->
                        "MC"
                , "Unverify"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.int checklist.id)
        , expect =
            Http.expectWhatever
                (GotApiResult << UnverifyChecklistResult checklist)
        , timeout = Nothing
        , tracker = Nothing
        }


addCustomItem : Checklist.Checklist -> String -> String -> Bool -> String -> String -> Cmd Msg
addCustomItem checklist nextNo itemString isOk plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "CheckList"
                , "CustomItem"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "CheckListId", E.int checklist.id )
                    , ( "ItemNo", E.string nextNo )
                    , ( "Text", E.string itemString )
                    , ( "IsOk", E.bool isOk )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << AddCustomItemResult checklist)
        , timeout = Nothing
        , tracker = Nothing
        }


updateComment : Checklist.Checklist -> String -> String -> String -> Cmd Msg
updateComment checklist comment plantId token =
    Http.request
        { method = "PUT"
        , url =
            url
                [ "CheckList"
                , case checklist.group of
                    Checklist.CPCL ->
                        "Comm"

                    Checklist.Preservation ->
                        "Preservation"

                    _ ->
                        "MC"
                , "Comment"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "CheckListId", E.int checklist.id )
                    , ( "Comment", E.string comment )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << CommentChecklistResult checklist)
        , timeout = Nothing
        , tracker = Nothing
        }


updateMetaTableCell : Checklist.Checklist -> Checklist.Item -> Checklist.Row -> Checklist.Cell -> String -> String -> Cmd Msg
updateMetaTableCell checklist checkItem tableRow cell plantId token =
    Http.request
        { method = "PUT"
        , url =
            url
                [ "CheckList"
                , "Item"
                , "MetaTableCell"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "CheckListId", E.int checklist.id )
                    , ( "CheckItemId", E.int checkItem.id )
                    , ( "RowId", E.int tableRow.id )
                    , ( "ColumnId", E.int cell.columnId )
                    , ( "Value", E.string cell.value )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << UpdateMetaTableCellResult checklist)
        , timeout = Nothing
        , tracker = Nothing
        }
