module Clipboard exposing
    ( ClipboardData
    , ClipboardFormat(..)
    , detectFormat
    , parseCsv
    , parseJson
    , parseTsv
    , serializeToTsv
    )

{-| Clipboard parsing and serialization utilities for the spreadsheet.
Handles TSV (from terminals), CSV (from spreadsheets), and JSON arrays.
-}

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E


{-| Parsed clipboard data as a 2D array of strings.
-}
type alias ClipboardData =
    List (List String)


{-| Detected clipboard format.
-}
type ClipboardFormat
    = Tsv
    | Csv
    | JsonArray
    | PlainText


{-| Detect the format of clipboard text.
Heuristics:
- If it starts with '[', try JSON array
- If it contains tabs, it's TSV
- If it contains commas and newlines, it's CSV
- Otherwise, plain text (single cell)
-}
detectFormat : String -> ClipboardFormat
detectFormat text =
    let
        trimmed =
            String.trim text

        hasTab =
            String.contains "\t" text

        hasComma =
            String.contains "," text

        hasNewline =
            String.contains "\n" text
    in
    if String.startsWith "[" trimmed then
        JsonArray

    else if hasTab then
        Tsv

    else if hasComma && hasNewline then
        Csv

    else
        PlainText


{-| Parse tab-separated values.
Each line becomes a row, tabs separate columns.
-}
parseTsv : String -> ClipboardData
parseTsv text =
    text
        |> String.lines
        |> List.filter (not << String.isEmpty)
        |> List.map (String.split "\t")


{-| Parse comma-separated values with proper quote handling.
Handles quoted fields containing commas and escaped quotes ("").
-}
parseCsv : String -> ClipboardData
parseCsv text =
    text
        |> String.lines
        |> List.filter (not << String.isEmpty)
        |> List.map parseCsvRow


{-| Parse a single CSV row with proper quote handling.
-}
parseCsvRow : String -> List String
parseCsvRow row =
    parseCsvRowHelper row "" False []


parseCsvRowHelper : String -> String -> Bool -> List String -> List String
parseCsvRowHelper remaining current inQuotes acc =
    case String.uncons remaining of
        Nothing ->
            List.reverse (String.trim current :: acc)

        Just ( char, rest ) ->
            if char == '"' then
                if inQuotes then
                    -- Check for escaped quote
                    case String.uncons rest of
                        Just ( '"', restAfterQuote ) ->
                            -- Escaped quote ""
                            parseCsvRowHelper restAfterQuote (current ++ "\"") True acc

                        _ ->
                            -- End of quoted section
                            parseCsvRowHelper rest current False acc

                else
                    -- Start of quoted section
                    parseCsvRowHelper rest current True acc

            else if char == ',' && not inQuotes then
                -- Field separator
                parseCsvRowHelper rest "" False (String.trim current :: acc)

            else
                parseCsvRowHelper rest (current ++ String.fromChar char) inQuotes acc


{-| Parse a JSON array of objects or arrays into clipboard data.
Handles both formats:
- `[{"col1": "val1", "col2": "val2"}, ...]` -> rows with values
- `[["val1", "val2"], ...]` -> rows as arrays
-}
parseJson : String -> Result String ClipboardData
parseJson text =
    let
        arrayOfArrays =
            D.list (D.list stringOrValue)

        arrayOfObjects =
            D.list (D.dict stringOrValue)
                |> D.map
                    (\objs ->
                        case objs of
                            [] ->
                                []

                            first :: _ ->
                                let
                                    keys =
                                        first
                                            |> Dict.keys
                                            |> List.sort
                                in
                                List.map
                                    (\obj ->
                                        List.map
                                            (\k -> Dict.get k obj |> Maybe.withDefault "")
                                            keys
                                    )
                                    objs
                    )

        stringOrValue =
            D.oneOf
                [ D.string
                , D.int |> D.map String.fromInt
                , D.float |> D.map String.fromFloat
                , D.bool |> D.map (\b -> if b then "true" else "false")
                , D.null ""
                , D.value |> D.map (E.encode 0)
                ]
    in
    text
        |> D.decodeString
            (D.oneOf
                [ arrayOfArrays
                , arrayOfObjects
                ]
            )
        |> Result.mapError D.errorToString


{-| Serialize a selection to TSV format for copying.
Takes a 2D array of cell values and produces tab-separated text.
-}
serializeToTsv : List (List String) -> String
serializeToTsv rows =
    rows
        |> List.map (String.join "\t")
        |> String.join "\n"
