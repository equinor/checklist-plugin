module Data.Common exposing (androidAppUrl, colorFromStatus, hexColorFromStatus, iconSize, iosAppUrl, nullInt, nullString, pcsBaseUrl, removeLeadingZeros, scaled, scaledInt, sortByStatus, splitStringTwo, statusToString, worstStatus, worstStatusWithDefault)

import Element exposing (..)
import Element.Font as Font
import Json.Decode as D
import Json.Encode as E
import Equinor.Palette as Palette
import Types exposing (..)


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


colorFromStatus : Maybe Status -> Color
colorFromStatus maybeStatus =
    case maybeStatus of
        Nothing ->
            Palette.darkGrey

        Just status ->
            case status of
                OS ->
                    Palette.white

                PB ->
                    Palette.yellow

                PA ->
                    Palette.red

                OK ->
                    Palette.alphaMossGreen


hexColorFromStatus : Status -> String
hexColorFromStatus status =
    case status of
        OS ->
            Palette.whiteHex

        PB ->
            Palette.yellowHex

        PA ->
            Palette.redHex

        OK ->
            Palette.alphaMossGreenHex


statusToString : Status -> String
statusToString status =
    case status of
        OS ->
            "OS"

        PA ->
            "PA"

        PB ->
            "PB"

        OK ->
            "OK"


worstStatus : List Status -> Maybe Status
worstStatus list =
    worstStatusHelper list Nothing


worstStatusWithDefault : Status -> List Status -> Status
worstStatusWithDefault defaultStatus list =
    case worstStatus (defaultStatus :: list) of
        Just nextStatus ->
            nextStatus

        Nothing ->
            defaultStatus


worstStatusHelper : List Status -> Maybe Status -> Maybe Status
worstStatusHelper list current =
    case list of
        [] ->
            current

        first :: rest ->
            case first of
                OS ->
                    Just OS

                PA ->
                    worstStatusHelper rest
                        (if current == Just OS then
                            current

                         else
                            Just PA
                        )

                PB ->
                    worstStatusHelper rest
                        (if current == Just PA || current == Just OS then
                            current

                         else
                            Just PB
                        )

                OK ->
                    worstStatusHelper rest
                        (if current == Just PA || current == Just PB || current == Just OS then
                            current

                         else
                            Just OK
                        )


sortByStatus : Status -> Int
sortByStatus status =
    case status of
        OS ->
            1

        PB ->
            3

        PA ->
            2

        OK ->
            4


removeLeadingZeros : String -> String
removeLeadingZeros str =
    if String.left 1 str == "0" then
        removeLeadingZeros (String.dropLeft 1 str)

    else
        str


type alias FromTo =
    { from : Int
    , to : Int
    }


splitStringTwo str =
    if String.length str < 20 then
        ( str, "" )

    else
        splitStringTwoHelper "" (String.words str) (String.length str)


splitStringTwoHelper firstString lastWords length =
    case lastWords of
        [] ->
            ( firstString, String.join " " lastWords )

        first :: rest ->
            if String.length firstString > length // 2 then
                ( firstString, String.join " " lastWords )

            else
                splitStringTwoHelper
                    (if firstString == "" then
                        first

                     else
                        firstString ++ " " ++ first
                    )
                    rest
                    length


iosAppUrl =
    --"https://install.appcenter.ms/orgs/InField/apps/Infield"
    "itms-services://?action=download-manifest&url=https://infield.equinor.com/manifest.plist"


androidAppUrl =
    "infield.apk"


pcsBaseUrl : String -> String
pcsBaseUrl plantId =
    String.concat
        [ "https://procosys.equinor.com/"
        , String.replace "PCS$" "" plantId
        ]


iconSize : Float -> Float
iconSize size =
    size * 3


scaled : Float -> (Int -> Float)
scaled size =
    modular size 1.15


scaledInt : Float -> (Int -> Int)
scaledInt size =
    scaled size >> round


type SapFormat
    = HeaderText String
    | NormalText String
