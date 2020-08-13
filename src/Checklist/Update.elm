module Checklist.Update exposing (update)

import Checklist.Api as Api
import Checklist exposing (Checklist)
import Dict exposing (Dict)
import Http
import Json.Encode as E
import Checklist.Messages exposing (..)
import Checklist.Model exposing (Model)
import Checklist.Ports as Ports
import Svg.Attributes exposing (z)
import Checklist.Types exposing (..)
import Equinor.Types exposing (..)


type alias MC =
    ( Model, Cmd Msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        mc =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            mc

        -- Data Handling
        GotToken tokenSuccess ->
            mc |> sendRequestsWaitingForToken tokenSuccess

        GotChecklists checklists ->
            mc |> setChecklistsTo checklists

        GotApiResult apiResult ->
            mc |> handleApiResult apiResult

        DecodeError err ->
            mc

        -- User Interaction
        ChecklistPressed checklist ->
            if model.selectedChecklist == Just checklist.id then
                mc |> unSelectChecklist

            else
                mc
                    |> selectChecklist checklist
                    |> getChecklistDetails checklist

        NaCheckItemPressed checklist checkItem ->
            let
                apiCall =
                    if checkItem.isNa then
                        Api.clearCheckItem

                    else
                        Api.setCheckItemNa
            in
            mc
                |> apiRequest [ apiCall checklist checkItem ]

        OkCheckItemPressed checklist checkItem ->
            let
                apiCall =
                    if checkItem.isOk then
                        Api.clearCheckItem

                    else
                        Api.setCheckItemOk
            in
            mc
                |> apiRequest [ apiCall checklist checkItem ]

        SignChecklistButtonPressed checklist ->
            mc
                |> apiRequest [ Api.signChecklist checklist ]

        UnsignChecklistButtonPressed checklist ->
            mc
                |> apiRequest [ Api.unSignChecklist checklist ]

        VerifyChecklistButtonPressed checklist ->
            mc
                |> apiRequest [ Api.verifyChecklist checklist ]

        UnverifyChecklistButtonPressed checklist ->
            mc
                |> apiRequest [ Api.unVerifyChecklist checklist ]

        MetaTableCellLostFocus checklist checkItem tableRow cell ->
            mc
                |> apiRequest [ Api.updateMetaTableCell checklist checkItem tableRow cell ]

        MetaTableCellInput checklist checkItem tableRow columnLabel str ->
            let
                updater cl =
                    case cl.details of
                        Loaded _ details ->
                            { cl
                                | details =
                                    Loaded ""
                                        { details
                                            | items =
                                                List.map
                                                    (\item ->
                                                        if item.id == checkItem.id then
                                                            let
                                                                oldTable =
                                                                    item.metaTable
                                                            in
                                                            { item
                                                                | metaTable =
                                                                    { oldTable
                                                                        | rows =
                                                                            List.map
                                                                                (\row ->
                                                                                    if row.id == tableRow.id then
                                                                                        { row
                                                                                            | cells =
                                                                                                List.map
                                                                                                    (\cell ->
                                                                                                        if cell.columnId == columnLabel.id then
                                                                                                            { cell | value = str }

                                                                                                        else
                                                                                                            cell
                                                                                                    )
                                                                                                    row.cells
                                                                                        }

                                                                                    else
                                                                                        row
                                                                                )
                                                                                oldTable.rows
                                                                    }
                                                            }

                                                        else
                                                            item
                                                    )
                                                    details.items
                                        }
                            }

                        _ ->
                            cl
            in
            ( { model
                | checklists =
                    Dict.update checklist.id (Maybe.map updater) model.checklists
              }
            , Cmd.none
            )

        CommentFieldInput checklist str ->
            let
                updater cl =
                    case cl.details of
                        Loaded _ details ->
                            let
                                oldChecklistDetails =
                                    details.checklistDetails
                            in
                            { cl | details = Loaded "" { details | checklistDetails = { oldChecklistDetails | comment = str } } }

                        _ ->
                            cl
            in
            ( { model
                | checklists =
                    Dict.update checklist.id (Maybe.map updater) model.checklists
              }
            , Cmd.none
            )

        CommentFieldLostFocus checklist str ->
            mc
                |> apiRequest [ Api.updateComment checklist str ]

        CustomCheckItemInput str ->
            ( { model | customCheckItemField = str }, Cmd.none )

        AddCustomCheckItemButtonPressed checklist ->
            mc
                |> apiRequest [ Api.nextCustomItemNo checklist ]

        OkCustomCheckItemPressed checklist customItem ->
            let
                apiCall =
                    if customItem.isOk then
                        Api.clearCustomCheckItem

                    else
                        Api.setCustomCheckItemOk
            in
            mc
                |> apiRequest [ apiCall checklist customItem ]

        DeleteCustomCheckItemButtomPressed checklist customItem ->
            mc
                |> apiRequest [ Api.deleteCustomItem checklist customItem ]


setChecklistsTo : List Checklist -> MC -> MC
setChecklistsTo checklists ( m, c ) =
    ( { m | checklists = List.foldl (\checklist dict -> Dict.insert checklist.id checklist dict) Dict.empty checklists }, c )


unSelectChecklist : MC -> MC
unSelectChecklist ( m, c ) =
    ( { m | selectedChecklist = Nothing }, c )


selectChecklist : Checklist -> MC -> MC
selectChecklist checklist ( m, c ) =
    ( { m | selectedChecklist = Just checklist.id }, c )


getChecklistDetails : Checklist -> MC -> MC
getChecklistDetails checklist ( m, c ) =
    let
        updater cl =
            { cl | details = Loading "" Nothing }
    in
    ( { m | checklists = Dict.update checklist.id (Maybe.map updater) m.checklists }, c )
        |> apiRequest [ Api.checklistDetails checklist ]


apiRequest : List (String -> String -> Cmd Msg) -> MC -> MC
apiRequest requests ( m, c ) =
    let
        highestRefNo =
            m.requests
                |> Dict.keys
                |> List.maximum
                |> Maybe.withDefault 0

        nextRef =
            highestRefNo + 1
    in
    ( { m | requests = Dict.insert nextRef requests m.requests }
    , Cmd.batch
        [ c
        , createEvent "getToken"
            (E.object
                [ ( "clientId", E.string Api.clientId )
                , ( "refNo", E.int nextRef )
                ]
            )
        ]
    )


sendRequestsWaitingForToken : TokenSuccess -> MC -> MC
sendRequestsWaitingForToken tokenSuccess ( m, c ) =
    let
        maybeDoReq =
            Dict.get tokenSuccess.refNo m.requests
    in
    ( { m
        | requests = Dict.remove tokenSuccess.refNo m.requests
      }
    , case maybeDoReq of
        Just doReqList ->
            doReqList
                |> List.map (\fn -> fn m.procosysPlantId tokenSuccess.token)
                |> Cmd.batch

        Nothing ->
            Cmd.none
    )


createEvent : String -> E.Value -> Cmd Msg
createEvent topic payload =
    Ports.toJs
        (E.object
            [ ( "topic", E.string topic )
            , ( "payload", payload )
            ]
        )


handleApiResult : ApiResult -> MC -> MC
handleApiResult apiResult ( m, c ) =
    case apiResult of
        GotChecklistDetails id result ->
            let
                updater checklist =
                    { checklist
                        | details =
                            case result of
                                Ok details ->
                                    Loaded "" details

                                Err err ->
                                    DataError "" Nothing
                        , status =
                            case result of
                                Ok details ->
                                    details.checklistDetails.status

                                Err err ->
                                    checklist.status
                    }
            in
            ( { m
                | checklists = Dict.update id (Maybe.map updater) m.checklists
                , customCheckItemField = ""
              }
            , c
            )

        SetNaResult checklist result ->
            case result of
                Ok _ ->
                    ( m, c ) |> apiRequest [ Api.checklistDetails checklist ]

                Err err ->
                    ( m, c )

        SetOkResult checklist result ->
            case result of
                Ok _ ->
                    ( m, c ) |> apiRequest [ Api.checklistDetails checklist ]

                Err err ->
                    ( m, c )

        ClearResult checklist result ->
            case result of
                Ok _ ->
                    ( m, c ) |> apiRequest [ Api.checklistDetails checklist ]

                Err err ->
                    ( m, c )

        SignChecklistResult checklist result ->
            case result of
                Ok _ ->
                    ( m, c ) |> apiRequest [ Api.checklistDetails checklist ]

                Err err ->
                    ( m, c )

        UnsignChecklistResult checklist result ->
            case result of
                Ok _ ->
                    ( m, c ) |> apiRequest [ Api.checklistDetails checklist ]

                Err err ->
                    ( m, c )

        VerifyChecklistResult checklist result ->
            case result of
                Ok _ ->
                    ( m, c ) |> apiRequest [ Api.checklistDetails checklist ]

                Err err ->
                    ( m, c )

        UnverifyChecklistResult checklist result ->
            case result of
                Ok _ ->
                    ( m, c ) |> apiRequest [ Api.checklistDetails checklist ]

                Err err ->
                    ( m, c )

        UpdateMetaTableCellResult checklist result ->
            case result of
                Ok _ ->
                    ( m, c ) |> apiRequest [ Api.checklistDetails checklist ]

                Err err ->
                    ( m, c )

        CommentChecklistResult checklist result ->
            case result of
                Ok _ ->
                    ( m, c ) |> apiRequest [ Api.checklistDetails checklist ]

                Err _ ->
                    ( m, c )

        GotNextCustomItemNo checklist result ->
            case result of
                Ok nextNo ->
                    ( m, c )
                        |> apiRequest [ Api.addCustomItem checklist (String.replace "\"" "" nextNo) m.customCheckItemField False ]

                Err err ->
                    ( m, c )

        AddCustomItemResult checklist result ->
            case result of
                Ok _ ->
                    ( m, c )
                        |> apiRequest [ Api.checklistDetails checklist ]

                Err err ->
                    ( m, c )

        DeleteCustomItemResult checklist result ->
            case result of
                Ok _ ->
                    ( m, c )
                        |> apiRequest [ Api.checklistDetails checklist ]

                Err err ->
                    ( m, c )
