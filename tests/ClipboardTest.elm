module ClipboardTest exposing (..)

import Clipboard exposing (..)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Clipboard"
        [ describe "detectFormat"
            [ test "detects TSV (tabs present)" <|
                \_ ->
                    detectFormat "a\tb\nc\td"
                        |> Expect.equal Tsv
            , test "detects CSV (commas and newlines, no tabs)" <|
                \_ ->
                    detectFormat "a,b\nc,d"
                        |> Expect.equal Csv
            , test "detects JSON array" <|
                \_ ->
                    detectFormat "[{\"a\": 1}]"
                        |> Expect.equal JsonArray
            , test "detects JSON array with whitespace" <|
                \_ ->
                    detectFormat "  [{\"a\": 1}]  "
                        |> Expect.equal JsonArray
            , test "detects plain text (single value)" <|
                \_ ->
                    detectFormat "hello world"
                        |> Expect.equal PlainText
            , test "prefers TSV over CSV when tabs present" <|
                \_ ->
                    detectFormat "a\tb,c"
                        |> Expect.equal Tsv
            ]
        , describe "parseTsv"
            [ test "parses single row" <|
                \_ ->
                    parseTsv "a\tb\tc"
                        |> Expect.equal [ [ "a", "b", "c" ] ]
            , test "parses multiple rows" <|
                \_ ->
                    parseTsv "a\tb\nc\td"
                        |> Expect.equal [ [ "a", "b" ], [ "c", "d" ] ]
            , test "handles empty cells" <|
                \_ ->
                    parseTsv "a\t\tc"
                        |> Expect.equal [ [ "a", "", "c" ] ]
            , test "filters empty lines" <|
                \_ ->
                    parseTsv "a\tb\n\nc\td"
                        |> Expect.equal [ [ "a", "b" ], [ "c", "d" ] ]
            ]
        , describe "parseCsv"
            [ test "parses simple CSV" <|
                \_ ->
                    parseCsv "a,b,c"
                        |> Expect.equal [ [ "a", "b", "c" ] ]
            , test "parses multiple rows" <|
                \_ ->
                    parseCsv "a,b\nc,d"
                        |> Expect.equal [ [ "a", "b" ], [ "c", "d" ] ]
            , test "handles quoted fields" <|
                \_ ->
                    parseCsv "\"hello, world\",b"
                        |> Expect.equal [ [ "hello, world", "b" ] ]
            , test "handles escaped quotes" <|
                \_ ->
                    parseCsv "\"say \"\"hi\"\"\",b"
                        |> Expect.equal [ [ "say \"hi\"", "b" ] ]
            , test "handles empty cells" <|
                \_ ->
                    parseCsv "a,,c"
                        |> Expect.equal [ [ "a", "", "c" ] ]
            ]
        , describe "parseJson"
            [ test "parses array of arrays" <|
                \_ ->
                    parseJson "[[\"a\", \"b\"], [\"c\", \"d\"]]"
                        |> Expect.equal (Ok [ [ "a", "b" ], [ "c", "d" ] ])
            , test "parses array of objects" <|
                \_ ->
                    parseJson "[{\"x\": 1, \"y\": 2}]"
                        |> Result.map (List.head >> Maybe.map List.sort)
                        |> Expect.equal (Ok (Just [ "1", "2" ]))
            , test "handles mixed types in arrays" <|
                \_ ->
                    parseJson "[[1, \"two\", true, null]]"
                        |> Expect.equal (Ok [ [ "1", "two", "true", "" ] ])
            , test "returns error for invalid JSON" <|
                \_ ->
                    parseJson "not json"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]
        , describe "serializeToTsv"
            [ test "serializes single row" <|
                \_ ->
                    serializeToTsv [ [ "a", "b", "c" ] ]
                        |> Expect.equal "a\tb\tc"
            , test "serializes multiple rows" <|
                \_ ->
                    serializeToTsv [ [ "a", "b" ], [ "c", "d" ] ]
                        |> Expect.equal "a\tb\nc\td"
            , test "handles empty cells" <|
                \_ ->
                    serializeToTsv [ [ "a", "", "c" ] ]
                        |> Expect.equal "a\t\tc"
            , test "handles empty input" <|
                \_ ->
                    serializeToTsv []
                        |> Expect.equal ""
            ]
        ]
