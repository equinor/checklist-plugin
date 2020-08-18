module Checklist.View exposing (renderChecklists)

import Checklist as Checklist exposing (Checklist)
import Checklist.Api exposing (checklistDetails)
import Checklist.Messages exposing (Msg(..))
import Checklist.Model exposing (Model)
import Checklist.Types exposing (..)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onLoseFocus)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Equinor.Data.Procosys.Status as Status exposing (Status(..))
import Equinor.Icon as Icon
import Equinor.Palette as Palette exposing (scaledInt)
import Equinor.Types exposing (..)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D


renderChecklists : Float -> Model -> Element Msg
renderChecklists size model =
    let
        groupToString group =
            case group of
                Checklist.MCCR ->
                    "MCCR"

                Checklist.CPCL ->
                    "CPCL"

                Checklist.Preservation ->
                    "Preservation"

                Checklist.RunningLogs ->
                    "runningLogs"

                Checklist.DCCL ->
                    "DCCL"

                Checklist.SignalTag ->
                    "SignalTag"

        updater c mV =
            Just <|
                case mV of
                    Just list ->
                        c :: list

                    Nothing ->
                        [ c ]

        groups =
            model.checklists
                |> Dict.values
                |> List.foldl (\c dict -> Dict.update (groupToString c.group) (updater c) dict) Dict.empty
                |> Dict.toList
    in
    column [ spacing 10, width fill ]
        (groups
            |> List.map
                (\( groupName, groupChecklists ) ->
                    column [ width fill ]
                        [ el [ Font.color Palette.mossGreen, Font.bold ] (text groupName)
                        , groupChecklists
                            |> List.map (renderChecklistItem size model)
                            |> Keyed.column
                                [ width fill
                                , height fill
                                , scrollbarY
                                , Background.color Palette.mistBlue
                                , spacing 1
                                ]
                        ]
                )
        )


spacer =
    el
        [ width fill
        , Border.widthEach { top = 1, left = 0, right = 0, bottom = 0 }
        , Border.color Palette.mistBlue
        , Border.dashed
        ]
        none


renderChecklistItem : Float -> Model -> Checklist.Checklist -> ( String, Element Msg )
renderChecklistItem size model item =
    let
        colors =
            case item.status of
                PA ->
                    Palette.combination Palette.white Palette.red

                PB ->
                    Palette.combination Palette.white Palette.yellow

                OK ->
                    Palette.combination Palette.white Palette.alphaMossGreen

                _ ->
                    Palette.combination Palette.white Palette.grey

        isSelected =
            model.selectedChecklist == Just item.id

        color =
            Palette.white

        statusBadge =
            el
                (colors
                    [ paddingXY 2 1
                    , Border.rounded 4
                    , Font.size (scaledInt size -4)
                    ]
                )
                (item.status |> Status.toString |> text)

        itemType =
            el [ alignRight, clip, Font.size <| scaledInt size -2 ] (text item.type_)

        responsible =
            el [ alignRight, Font.size <| scaledInt size -2 ] (text item.responsible)

        icon =
            el
                [ width (px 30)
                , height (px 30)
                , inFront <| statusBadge
                , clip
                ]
            <|
                html <|
                    iconFromCategory item.register

        tagNo =
            paragraph [ Font.size <| scaledInt size -1, width fill, Font.color Palette.mossGreen ] [ text item.tagNo ]

        tagDescription =
            paragraph [ width fill, Font.size (scaledInt size -2) ] [ text item.description ]
    in
    ( String.fromInt item.id
    , column
        [ width fill
        , Background.color <|
            if isSelected then
                Palette.mistBlue

            else
                Palette.white
        , padding (round <| size / 2)
        ]
        [ row
            [ width fill
            , onClick <| ChecklistPressed item
            , pointer
            ]
            [ icon
            , row [ width fill, spacing (round size * 2) ]
                [ column [ width fill ] [ tagNo, tagDescription ]
                , itemType
                , responsible
                ]
            ]
        , if isSelected then
            case item.details of
                NotLoaded ->
                    text "NotLoaded"

                Loading _ _ ->
                    text "Loading..."

                DataError _ _ ->
                    text "Error getting details"

                Loaded _ details ->
                    let
                        hasUnsignedNormalItems =
                            List.any (\i -> not i.isHeading && not i.isOk && not i.isNa) details.items

                        hasUnsignedCustomItems =
                            List.any (\i -> not i.isOk) details.customItems

                        hasUnsignedItems =
                            hasUnsignedNormalItems || hasUnsignedCustomItems
                    in
                    column [ width fill, height fill, Background.color Palette.white, onClick NoOp ]
                        [ {- text "Loop Tags:"
                             , paragraph [ width fill, spacing 10 ] (details.loopTags |> List.map (\tagNo -> el [] (text tagNo)))
                             ,
                          -}
                          renderChecklistItems size item details
                        , renderCustomChecklistItems size item details model.customCheckItemField
                        , renderCommentField size item details
                        , renderAttachments size model True details item
                        , signatures size item hasUnsignedItems details
                        , if String.isEmpty model.errorMsg then
                            none

                          else
                            paragraph [ width fill, Background.color Palette.alphaYellow, padding 6 ] [ text model.errorMsg ]
                        ]

          else
            none
        ]
    )


renderCommentField : Float -> Checklist -> Checklist.Details -> Element Msg
renderCommentField size checklist details =
    let
        isEnabled =
            String.isEmpty details.checklistDetails.signedAt
    in
    if String.isEmpty details.checklistDetails.comment && not isEnabled then
        none

    else
        column [ width fill ]
            [ el
                [ width fill
                , Background.color Palette.blue
                , Font.color Palette.white
                , padding 8
                , Font.size (scaledInt size -1)
                ]
                (text "Comment")
            , if isEnabled then
                Input.multiline
                    [ width fill
                    , onLoseFocus <| CommentFieldLostFocus checklist details.checklistDetails.comment
                    ]
                    { label = Input.labelHidden ""
                    , onChange = CommentFieldInput checklist
                    , placeholder = Just <| Input.placeholder [] (text "No comment")
                    , spellcheck = True
                    , text = details.checklistDetails.comment
                    }

              else
                details.checklistDetails.comment
                    |> String.lines
                    |> List.map (\txt -> paragraph [ width fill ] [ text txt ])
                    |> column [ width fill, padding 8 ]
            ]


signatures : Float -> Checklist -> Bool -> Checklist.Details -> Element Msg
signatures size checklist hasUnsignedItems details =
    let
        x =
            details.checklistDetails
    in
    column [ width fill ]
        [ if String.isEmpty details.checklistDetails.signedAt then
            none

          else
            el
                [ width fill
                , Background.color Palette.blue
                , Font.color Palette.white
                , padding 8
                , Font.size (scaledInt size -1)
                ]
                (text "Signatures")
        , column [ width fill, padding 10, spacing 2 ]
            [ if String.isEmpty x.signedAt then
                el [ alignRight ] <|
                    signButton size
                        "Sign"
                        (if hasUnsignedItems then
                            Just "There is unsigned items"

                         else
                            Nothing
                        )
                        (SignChecklistButtonPressed checklist)

              else
                row [ width Element.fill ]
                    [ wrappedRow [ width fill, spacingXY 10 0 ]
                        [ el [ Font.bold ] (text "Signed by")
                        , row [ spacing 10 ]
                            [ text (x.signedByFirstName ++ " " ++ x.signedByLastName)
                            , el [ alignRight ] (text <| String.left 10 x.signedAt)
                            ]
                        ]
                    , signButton size
                        "Unsign"
                        (if x.verifiedAt /= "" then
                            Just "Checklist is verified"

                         else
                            Nothing
                        )
                        (UnsignChecklistButtonPressed checklist)
                    ]
            , if checklist.group /= Checklist.CPCL && x.signedAt /= "" then
                if String.isEmpty x.verifiedAt then
                    el [ alignRight ] <|
                        signButton size "Verify" Nothing (VerifyChecklistButtonPressed checklist)

                else
                    row [ width fill, spacingXY 10 0 ]
                        [ wrappedRow [ width fill, spacingXY 10 0 ]
                            [ el [ Font.bold ] (text "Verified by")
                            , row [ spacing 10 ]
                                [ el [ alignRight ] <| text (x.verifiedByFirstName ++ " " ++ x.verifiedByLastName)
                                , el [ alignRight ] (text <| String.left 10 x.verifiedAt)
                                ]
                            ]
                        , signButton size "Unverify" Nothing (UnverifyChecklistButtonPressed checklist)
                        ]

              else
                none
            ]
        ]


signButton : Float -> String -> Maybe String -> Msg -> Element Msg
signButton size name maybeDisabled msg =
    let
        activeAttributes =
            [ pointer
            , onClick msg
            ]

        deactiveAttributes message =
            [ alpha 0.3
            , htmlAttribute <| HA.style "cursor" "not-allowed"
            , htmlAttribute <| HA.title message
            ]
    in
    el
        ([ width <| px <| round <| size * 4
         , height <| px <| round <| size * 2
         , Background.color Palette.blue
         , Font.color Palette.white
         , Border.rounded 10
         ]
            ++ (case maybeDisabled of
                    Just message ->
                        deactiveAttributes message

                    Nothing ->
                        activeAttributes
               )
        )
    <|
        el [ centerX, centerY, Font.size (round size) ] (text name)


iconFromCategory : String -> H.Html msg
iconFromCategory category =
    case category of
        "Circuit/Starter" ->
            Icon.circuit

        "CIRCUIT_AND_STARTER" ->
            Icon.circuit

        "Electrical" ->
            Icon.electrical

        "ELECTRICAL_FIELD" ->
            Icon.electrical

        "Cable" ->
            Icon.cable

        "CABLE" ->
            Icon.cable

        "Instrument" ->
            Icon.instrument

        "INSTRUMENT_FIELD" ->
            Icon.instrument

        "Fire & Gas" ->
            Icon.fireAndGas

        "FIRE_AND_GAS_FIELD" ->
            Icon.fireAndGas

        "Line" ->
            Icon.line_

        "LINE" ->
            Icon.line_

        "Main Equipment" ->
            Icon.tag "M" "none"

        "MAIN_EQUIPMENT" ->
            Icon.tag "M" "none"

        "Telecom" ->
            Icon.telecom

        "TELECOM_FIELD" ->
            Icon.telecom

        "Junction Box" ->
            Icon.junctionBox

        "JUNCTION_BOX" ->
            Icon.junctionBox

        "Special Item" ->
            Icon.tag "SI" "none"

        "SPECIAL_ITEM" ->
            Icon.tag "SI" "none"

        "Heat Tracing Cable" ->
            Icon.heatTrace

        "HEAT_TRACING_CABLE" ->
            Icon.heatTrace

        "Signal" ->
            Icon.signal

        "SIGNAL" ->
            Icon.signal

        "Manual Valve" ->
            Icon.manualValve

        "MANUAL_VALVE" ->
            Icon.manualValve

        "Function" ->
            Icon.function

        "FUNCTION" ->
            Icon.function

        "Ducting" ->
            Icon.ducting

        "DUCTING" ->
            Icon.ducting

        _ ->
            Icon.tag "" "none"


renderChecklistItems : Float -> Checklist -> Checklist.Details -> Element Msg
renderChecklistItems size checklist details =
    column [ width fill ]
        [ row
            [ width fill
            , Background.color Palette.blue
            , Font.color Palette.white
            , paddingXY 8 6
            , Font.size (scaledInt size -1)
            ]
            [ el [] (text "Check items")
            , row [ alignRight, spacing 6 ] [ el [] (text "OK"), el [] (text "N/A") ]
            ]
        , column [ width fill, spacing -1 ] (List.map (renderChecklistCheckItem size checklist details.checklistDetails.signedAt) details.items)
        ]


renderCustomChecklistItems : Float -> Checklist -> Checklist.Details -> String -> Element Msg
renderCustomChecklistItems size checklist details customCheckItemField =
    column [ width fill ]
        [ if List.isEmpty details.customItems then
            none

          else
            row
                [ width fill
                , Background.color Palette.blue
                , Font.color Palette.white
                , paddingXY 8 6
                , Font.size (scaledInt size -1)
                ]
                [ el [] (text "Custom check items")
                , row [ alignRight, spacing 6 ] [ el [] (text "OK"), el [] (text "Del") ]
                ]
        , column [ width fill, spacing -1 ] (List.map (renderCustomChecklistCheckItem size checklist details.checklistDetails.signedAt) details.customItems)
        , if String.isEmpty details.checklistDetails.signedAt then
            row [ width fill, padding 8, spacing 10 ]
                [ Input.text [ width fill ]
                    { label = Input.labelHidden "customItem"
                    , onChange = CustomCheckItemInput
                    , placeholder =
                        Just <|
                            Input.placeholder []
                                (text <|
                                    if List.isEmpty details.customItems then
                                        "Optional: custom check-item"

                                    else
                                        "type to add more"
                                )
                    , text = customCheckItemField
                    }
                , el [ alignRight ] <|
                    signButton size
                        "Add"
                        (if String.isEmpty customCheckItemField then
                            Just "You must type what to check"

                         else
                            Nothing
                        )
                        (AddCustomCheckItemButtonPressed checklist)
                ]

          else
            none
        ]


renderChecklistCheckItem : Float -> Checklist -> String -> Checklist.Item -> Element Msg
renderChecklistCheckItem size checklist signedAt item =
    let
        isDisabled =
            signedAt /= ""
    in
    if item.isHeading then
        item.text
            |> String.lines
            |> List.map (\txt -> paragraph [ Font.center ] [ text txt ])
            |> column
                [ if String.startsWith "NOTE" item.text || String.startsWith "-" item.text then
                    Font.regular

                  else
                    Font.bold
                , if String.startsWith "NOTE" item.text || String.startsWith "-" item.text then
                    Font.size (scaledInt size -3)

                  else
                    Font.size (scaledInt size -2)
                , padding 4
                , centerX
                ]

    else
        row [ width fill, paddingXY 10 2, Border.widthXY 0 1, Border.dashed, Border.color Palette.mistBlue ]
            [ column [ width fill ]
                [ item.text
                    |> String.lines
                    |> List.map (\txt -> paragraph [] [ text txt ])
                    |> column [ width fill, Font.size (scaledInt size -2), padding 4 ]
                , if List.isEmpty item.metaTable.columnLabels then
                    none

                  else
                    renderMetaTable size isDisabled checklist item
                ]
            , row [ centerY, alignRight, spacing 10 ]
                [ checkButton size isDisabled item.isOk (OkCheckItemPressed checklist item)
                , checkButton size isDisabled item.isNa (NaCheckItemPressed checklist item)
                ]
            ]


renderCustomChecklistCheckItem : Float -> Checklist -> String -> Checklist.CustomItem -> Element Msg
renderCustomChecklistCheckItem size checklist signedAt item =
    let
        isDisabled =
            signedAt /= ""

        activeProperties =
            [ pointer
            , onClick <| DeleteCustomCheckItemButtomPressed checklist item
            ]

        notActiveProperties =
            [ alpha 0.3
            , htmlAttribute <| HA.title "Checklist is signed"
            , htmlAttribute <| HA.style "cursor" "not-allowed"
            ]

        deleteButton =
            el
                ([ height <| px <| round <| size
                 , width <| px <| round <| size
                 ]
                    ++ (if isDisabled then
                            notActiveProperties

                        else
                            activeProperties
                       )
                )
            <|
                el [ centerX, centerY ] (text "X")
    in
    row [ width fill, paddingXY 10 2, Border.widthXY 0 1, Border.dashed, Border.color Palette.mistBlue ]
        [ column [ width fill ]
            [ item.text
                |> String.lines
                |> List.map (\txt -> paragraph [] [ text txt ])
                |> column [ width fill, Font.size (scaledInt size -2), padding 4 ]
            ]
        , row [ centerY, alignRight, spacing 10 ]
            [ checkButton size isDisabled item.isOk (OkCustomCheckItemPressed checklist item)
            , deleteButton
            ]
        ]


renderMetaTable : Float -> Bool -> Checklist -> Checklist.Item -> Element Msg
renderMetaTable size isDisabled checklist checkItem =
    table [ width fill ]
        { columns =
            checkItem.metaTable.columnLabels
                |> List.map
                    (\ch ->
                        { header = el [ Font.size (scaledInt size -1) ] (text ch.label)
                        , width = fill
                        , view =
                            \row ->
                                row.cells
                                    |> List.filter (\cell -> cell.columnId == ch.id)
                                    |> List.head
                                    |> Maybe.map (renderCellInput size isDisabled checklist checkItem ch row)
                                    |> Maybe.withDefault none
                        }
                    )
        , data = checkItem.metaTable.rows
        }


renderCellInput : Float -> Bool -> Checklist -> Checklist.Item -> Checklist.ColumnLabel -> Checklist.Row -> Checklist.Cell -> Element Msg
renderCellInput size isDisabled checklist checkItem columnHeader tableRow cell =
    row [ spacing 4, Font.size (round size) ]
        [ if isDisabled then
            el [ Font.color Palette.blue ] (text cell.value)

          else
            Input.text
                [ onLoseFocus <| MetaTableCellLostFocus checklist checkItem tableRow cell ]
                { label = Input.labelHidden ""
                , onChange = MetaTableCellInput checklist checkItem tableRow columnHeader
                , placeholder = Nothing
                , text = cell.value
                }
        , text cell.unit
        ]


checkButton : Float -> Bool -> Bool -> Msg -> Element Msg
checkButton size isDisabled isActive msg =
    el
        ([ height <| px <| round <| size
         , width <| px <| round <| size
         , Border.rounded 1000
         , Border.color Palette.mistBlue
         , Border.width 2
         , Background.color <|
            if isActive then
                Palette.blue

            else
                Palette.white
         ]
            ++ (if isDisabled then
                    [ alpha 0.3
                    , htmlAttribute <| HA.style "cursor" "not-allowed"
                    , htmlAttribute <| HA.title "Checklist is signed"
                    ]

                else
                    [ pointer
                    , onClick msg
                    ]
               )
        )
        none


onClick : msg -> Element.Attribute msg
onClick msg =
    HE.custom "click"
        (D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = False
            }
        )
        |> htmlAttribute


renderAttachments : Float -> Model -> Bool -> Checklist.Details -> Checklist -> Element Msg
renderAttachments size model readOnly details checklist =
    column [ width fill ]
        [ row
            [ width fill
            , Background.color Palette.blue
            , Font.color Palette.white
            , Font.size (scaledInt size -1)
            , paddingXY 8 4
            ]
            [ text <| "Attachments (" ++ String.fromInt details.checklistDetails.attachmentCount ++ ")"
            , el [ padding 6, alignRight, Border.width 1, Border.color Palette.white, Border.rounded 4, pointer, onClick <| NewAttachmentButtonPressed checklist ] (text "Add new")
            ]
        , case model.currentAttachment of
            Just file ->
                row [ width fill, spacing <| round size, padding 4 ]
                    [ image [ width <| px <| round <| size * 4 ] { description = "Thumbnail of new attachment", src = file.uri }
                    , Input.text [ width fill, Input.focusedOnLoad, onEnterKey <| AddUploadedAttachmentToChecklist checklist ]
                        { label = Input.labelHidden "Name"
                        , onChange = FileNameInputChanged
                        , placeholder = Just (Input.placeholder [] (text "Enter name..."))
                        , text = file.name
                        }
                    , el [ padding 6, alignRight, Border.width 1, Background.color Palette.green, Font.color Palette.white, Border.color Palette.blue, Border.rounded 4, pointer, onClick <| AddUploadedAttachmentToChecklist checklist ] (text "Add")
                    ]

            Nothing ->
                none
        , attachmentPreview size model checklist
        ]


onEnterKey msg =
    htmlAttribute <|
        HE.on "keydown"
            (D.field "keyCode" D.int
                |> D.andThen
                    (\keyCode ->
                        if keyCode == 13 then
                            D.succeed msg

                        else
                            D.fail ""
                    )
            )


attachmentPreview : Float -> Model -> Checklist -> Element Msg
attachmentPreview size model checklist =
    case checklist.attachments of
        Loaded _ attachments ->
            attachments
                |> List.map (renderAttachmentItem size checklist)
                |> column [ width fill ]

        Loading _ _ ->
            text "Loading attachments"

        DataError _ _ ->
            text "Problem getting attachments"

        NotLoaded ->
            none


renderAttachmentItem : Float -> Checklist -> Checklist.Attachment -> Element Msg
renderAttachmentItem size checklist a =
    row [ width fill, padding 10, spacing <| round size ]
        [ row
            [ width fill
            , pointer
            , spacing <| round size
            , onClick <| AttachmentPressed checklist a
            ]
            [ if String.isEmpty a.thumbnailAsBase64 then
                el [ width (px 100) ] <| el [ centerX ] (text "no preview")

              else
                image [ width (px 100) ] { description = "preview", src = String.concat [ "data:", a.mimeType, ";base64,", a.thumbnailAsBase64 ] }
            , el [ width fill ] (text a.title)
            ]
        , el [ alignRight, Font.color Palette.energyRed, padding 6, Border.rounded 4, Border.width 1, Border.color Palette.energyRed, pointer, onClick <| DeleteAttachmentButtonPressed checklist a ] (text "X")
        ]
