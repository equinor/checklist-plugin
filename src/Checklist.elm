module Checklist exposing (Attachment, Cell, Checklist, ColumnLabel, CustomItem, Details, Group(..), Item, MetaTable, Row, apiDecoder, attachmentDecoder, decoder, detailsApiDecoder, encoder, groupToString)

import Checklist.Types exposing (..)
import Equinor.Data.Procosys.Status as Status exposing (Status(..))
import Equinor.Types exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E


type Group
    = CPCL
    | MCCR
    | Preservation
    | RunningLogs
    | DCCL
    | SignalTag


apiGroupDecoder : D.Decoder Group
apiGroupDecoder =
    nullString
        |> D.andThen
            (\str ->
                case str of
                    "Mechanical Completion Check Record" ->
                        D.succeed MCCR

                    "MCCR" ->
                        D.succeed MCCR

                    "Commissioning Preparatory Check List" ->
                        D.succeed CPCL

                    "CPCL" ->
                        D.succeed CPCL

                    "Preservation" ->
                        D.succeed Preservation

                    "Running Logs" ->
                        D.succeed RunningLogs

                    "DeCommissioning Check List" ->
                        D.succeed DCCL

                    _ ->
                        D.succeed SignalTag
            )


groupDecoder : D.Decoder Group
groupDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "MCCR" ->
                        D.succeed MCCR

                    "CPCL" ->
                        D.succeed CPCL

                    "Preservation" ->
                        D.succeed Preservation

                    "RunningLogs" ->
                        D.succeed RunningLogs

                    "DeCommissioningCheckList" ->
                        D.succeed DCCL

                    _ ->
                        D.succeed SignalTag
            )


groupToString : Group -> String
groupToString group =
    case group of
        MCCR ->
            "MCCR"

        CPCL ->
            "CPCL"

        Preservation ->
            "Preservation"

        RunningLogs ->
            "RunningLogs"

        DCCL ->
            "DeCommissioningCheckList"

        SignalTag ->
            "SignalTag"


type alias Checklist =
    { id : Int
    , group : Group
    , type_ : String
    , tagNo : String
    , responsible : String
    , status : Status
    , commPk : String
    , mcPk : String
    , updatedAt : String
    , register : String
    , description : String
    , sheet : Int
    , subSheet : Int
    , details : WebData Details
    , attachments : WebData (List Attachment)
    , project : String
    }


type alias Details =
    { loopTags : List LoopTag
    , items : List Item
    , customItems : List CustomItem
    , checklistDetails : ChecklistDetails
    }


type alias ChecklistDetails =
    { comment : String
    , signedAt : String
    , signedByFirstName : String
    , signedByLastName : String
    , verifiedAt : String
    , verifiedByFirstName : String
    , verifiedByLastName : String
    , status : Status
    , attachmentCount : Int
    }


type alias Item =
    { id : Int
    , isHeading : Bool
    , isNa : Bool
    , isOk : Bool
    , metaTable : MetaTable
    , sequenceNumber : String
    , text : String
    }


type alias CustomItem =
    { id : Int
    , isOk : Bool
    , itemNo : String
    , text : String
    }


itemDecoder : D.Decoder Item
itemDecoder =
    D.map7 Item
        (D.field "Id" D.int)
        (D.field "IsHeading" D.bool)
        (D.field "IsNotApplicable" D.bool)
        (D.field "IsOk" D.bool)
        (D.field "MetaTable"
            (D.oneOf
                [ metaTableDecoder
                , D.null (MetaTable [] "" [])
                ]
            )
        )
        (D.field "SequenceNumber" D.string)
        (D.field "Text" nullString)


customItemDecoder : D.Decoder CustomItem
customItemDecoder =
    D.map4 CustomItem
        (D.field "Id" D.int)
        (D.field "IsOk" D.bool)
        (D.field "ItemNo" D.string)
        (D.field "Text" nullString)


type alias MetaTable =
    { columnLabels : List ColumnLabel
    , info : String
    , rows : List Row
    }


metaTableDecoder : D.Decoder MetaTable
metaTableDecoder =
    D.map3 MetaTable
        (D.field "ColumnLabels" (D.list columnLabelDecoder))
        (D.field "Info" nullString)
        (D.field "Rows" (D.list rowDecoder))


type alias ColumnLabel =
    { id : Int
    , label : String
    }


columnLabelDecoder : D.Decoder ColumnLabel
columnLabelDecoder =
    D.map2 ColumnLabel
        (D.field "Id" D.int)
        (D.field "Label" nullString)


type alias Row =
    { cells : List Cell
    , id : Int
    , label : String
    }


rowDecoder : D.Decoder Row
rowDecoder =
    D.map3 Row
        (D.field "Cells" (D.list cellDecoder))
        (D.field "Id" D.int)
        (D.field "Label" nullString)


type alias Cell =
    { columnId : Int
    , unit : String
    , value : String
    }


cellDecoder : D.Decoder Cell
cellDecoder =
    D.map3 Cell
        (D.field "ColumnId" D.int)
        (D.field "Unit" nullString)
        (D.field "Value" nullString)


type alias LoopTag =
    String


detailsApiDecoder : D.Decoder Details
detailsApiDecoder =
    D.succeed Details
        |> optional "LoopTags" (D.list loopTagDecoder) []
        |> required "CheckItems" (D.list itemDecoder)
        |> optional "CustomCheckItems" (D.list customItemDecoder) []
        |> required "CheckList" checklistDetails


checklistDetails : D.Decoder ChecklistDetails
checklistDetails =
    D.succeed ChecklistDetails
        |> required "Comment" nullString
        |> required "SignedAt" nullString
        |> required "SignedByFirstName" nullString
        |> required "SignedByLastName" nullString
        |> optional "VerifiedAt" nullString ""
        |> optional "VerifiedByFirstName" nullString ""
        |> optional "VerifiedByLastName" nullString ""
        |> required "Status" statusDecoder
        |> required "AttachmentCount" D.int


loopTagDecoder : D.Decoder LoopTag
loopTagDecoder =
    D.field "TagNo" D.string


apiDecoder : D.Decoder Checklist
apiDecoder =
    D.succeed Checklist
        |> required "Id" D.int
        |> required "TagFormularType__FormularType__FormularGroup__Description" apiGroupDecoder
        |> required "TagFormularType__FormularType__Id" D.string
        |> required "TagFormularType__Tag__TagNo" D.string
        |> required "TagFormularType__Tag__TagNo" D.string
        |> required "Status__Id" statusDecoder
        |> required "TagFormularType__Tag__McPkg__CommPkg__CommPkgNo" nullString
        |> required "TagFormularType__Tag__McPkg__McPkgNo" nullString
        |> required "UpdatedAt" nullString
        |> required "TagFormularType__Tag__Register__Id" nullString
        |> required "TagFormularType__Tag__Description" nullString
        |> required "TagFormularType__SheetNo" nullInt
        |> required "TagFormularType__SubsheetNo" nullInt
        |> hardcoded NotLoaded
        |> hardcoded NotLoaded
        |> required "TagFormularType__Tag__Project__Name" D.string


decoder : D.Decoder Checklist
decoder =
    D.succeed Checklist
        |> required "id" D.int
        |> required "group" groupDecoder
        |> required "type_" D.string
        |> required "tagNo" D.string
        |> required "responsible" D.string
        |> required "status" statusDecoder
        |> required "commPk" D.string
        |> required "mcPk" D.string
        |> required "updatedAt" D.string
        |> required "register" D.string
        |> required "description" D.string
        |> optional "sheet" D.int 0
        |> optional "subSheet" D.int 0
        |> hardcoded NotLoaded
        |> hardcoded NotLoaded
        |> optional "project" D.string ""


statusDecoder : D.Decoder Status
statusDecoder =
    D.oneOf
        [ D.string
            |> D.andThen
                (\str ->
                    D.succeed <|
                        statusFromString str
                )
        , D.null OS
        ]


statusFromString : String -> Status
statusFromString str =
    case str of
        "OK" ->
            OK

        "PA" ->
            PA

        "PB" ->
            PB

        _ ->
            OS


type alias Attachment =
    { id : Int
    , uri : String
    , title : String
    , mimeType : String
    , thumbnailAsBase64 : String
    , hasFile : Bool
    }


nullString : D.Decoder String
nullString =
    D.oneOf
        [ D.string
        , D.null ""
        ]


nullInt : D.Decoder Int
nullInt =
    D.oneOf
        [ D.int
        , D.null 0
        ]


encoder : Checklist -> E.Value
encoder c =
    E.object
        [ ( "id", E.int c.id )
        , ( "group", groupEncoder c.group )
        , ( "type_", E.string c.type_ )
        , ( "tagNo", E.string c.tagNo )
        , ( "responsible", E.string c.responsible )
        , ( "status", Status.encoder c.status )
        , ( "commPk", E.string c.commPk )
        , ( "mcPk", E.string c.mcPk )
        , ( "updatedAt", E.string c.updatedAt )
        , ( "register", E.string c.register )
        , ( "description", E.string c.description )
        , ( "sheet", E.int c.sheet )
        , ( "subSheet", E.int c.subSheet )
        , ( "project", E.string c.project )
        ]


groupEncoder : Group -> E.Value
groupEncoder group =
    E.string <|
        case group of
            MCCR ->
                "MCCR"

            CPCL ->
                "CPCL"

            Preservation ->
                "Preservation"

            RunningLogs ->
                "RunningLogs"

            DCCL ->
                "DeCommissioningCheckList"

            SignalTag ->
                "SignalTag"


attachmentDecoder : D.Decoder Attachment
attachmentDecoder =
    D.succeed Attachment
        |> required "Id" D.int
        |> required "Uri" nullString
        |> required "Title" nullString
        |> required "MimeType" nullString
        |> required "ThumbnailAsBase64" nullString
        |> required "HasFile" D.bool
