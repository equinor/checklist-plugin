module View exposing (renderChecklists)

import Data.Checklist as Checklist exposing (Checklist)
import Data.Common as Common exposing (scaledInt)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onLoseFocus)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Icon
import Json.Decode as D
import Messages exposing (Msg(..))
import Palette
import Types exposing (..)


renderChecklists : Float -> Maybe Int -> String -> List Checklist -> Element Msg
renderChecklists size maybeSelected errorMsg checklists =
    let
        groupToString group =
            case group of
                Checklist.MCCR ->
                    "MCCR"

                Checklist.CPCL ->
                    "CPCL"

                Checklist.Preservation ->
                    "Pres"

                Checklist.RunningLogs ->
                    "rLog"

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
            checklists
                |> List.foldl (\c dict -> Dict.update (groupToString c.group) (updater c) dict) Dict.empty
                |> Dict.toList
    in
    column [ spacing -1, width fill ]
        (groups
            |> List.map
                (\( groupName, groupChecklists ) ->
                    column [ width fill ]
                        [ el [ Font.bold, Font.color Palette.mossGreen ] (text groupName)
                        , groupChecklists
                            |> List.map (renderChecklistItem size maybeSelected errorMsg)
                            --|> List.intersperse spacer
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


renderChecklistItem : Float -> Maybe Int -> String -> Checklist.Checklist -> ( String, Element Msg )
renderChecklistItem size maybeSelected errorMsg item =
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
            maybeSelected == Just item.id

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
                (item.status |> Common.statusToString |> text)

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

                Loading ->
                    text "Loading..."

                DataError ->
                    text "Error getting details"

                Loaded details ->
                    let
                        hasUnsignedItems =
                            List.any (\i -> not i.isHeading && not i.isOk && not i.isNa) details.items
                    in
                    column [ width fill, height fill, Background.color Palette.white, onClick NoOp ]
                        [ {- text "Loop Tags:"
                             , paragraph [ width fill, spacing 10 ] (details.loopTags |> List.map (\tagNo -> el [] (text tagNo)))
                             ,
                          -}
                          row
                            [ width fill
                            , Background.color Palette.blue
                            , Font.color Palette.white
                            , paddingXY 8 6
                            , Font.size (scaledInt size -1)
                            ]
                            [ el [] (text "Check items")
                            , row [ alignRight, spacing 6 ] [ el [] (text "OK"), el [] (text "N/A") ]
                            ]
                        , renderChecklistItems size item details
                        , renderCommentField size item details
                        , if String.isEmpty details.checklistDetails.signedAt then
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
                        , signatures size item hasUnsignedItems details
                        , if String.isEmpty errorMsg then
                            none

                          else
                            paragraph [ width fill, Background.color Palette.alphaYellow, padding 6 ] [ text errorMsg ]
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
    column [ width fill, padding 10, spacing 2 ]
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
            wrappedRow [ width fill, spacingXY 10 0 ]
                [ el [ Font.bold ] (text "Signed by")
                , row [ alignRight, spacing 10 ]
                    [ text (x.signedByFirstName ++ " " ++ x.signedByLastName)
                    , el [ alignRight ] (text <| String.left 10 x.signedAt)
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
        , if checklist.group == Checklist.MCCR && x.signedAt /= "" then
            if String.isEmpty x.verifiedAt then
                el [ alignRight ] <|
                    signButton size "Verify" Nothing (VerifyChecklistButtonPressed checklist)

            else
                wrappedRow [ width fill, spacingXY 10 0 ]
                    [ el [ Font.bold ] (text "Verified by")
                    , row [ alignRight, spacing 10 ]
                        [ el [ alignRight ] <| text (x.verifiedByFirstName ++ " " ++ x.verifiedByLastName)
                        , el [ alignRight ] (text <| String.left 10 x.verifiedAt)
                        ]
                    , signButton size "Unverify" Nothing (UnverifyChecklistButtonPressed checklist)
                    ]

          else
            none
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
    column [ width fill, spacing -1 ] (List.map (renderChecklistCheckItem size checklist details.checklistDetails.signedAt) details.items)


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
