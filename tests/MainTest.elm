module MainTest exposing (..)

import Expect
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Main"
        [ describe "Navigation"
            [ describe "clampIndex"
                [ test "clamps x to lower bound" <|
                    \_ ->
                        clampIndex { maxX = 5, maxY = 10 } { x = -1, y = 5 }
                            |> Expect.equal { x = 0, y = 5 }
                , test "clamps x to upper bound" <|
                    \_ ->
                        clampIndex { maxX = 5, maxY = 10 } { x = 10, y = 5 }
                            |> Expect.equal { x = 5, y = 5 }
                , test "clamps y to lower bound (row 1 minimum)" <|
                    \_ ->
                        clampIndex { maxX = 5, maxY = 10 } { x = 2, y = 0 }
                            |> Expect.equal { x = 2, y = 1 }
                , test "clamps y to upper bound" <|
                    \_ ->
                        clampIndex { maxX = 5, maxY = 10 } { x = 2, y = 15 }
                            |> Expect.equal { x = 2, y = 10 }
                , test "leaves valid index unchanged" <|
                    \_ ->
                        clampIndex { maxX = 5, maxY = 10 } { x = 3, y = 7 }
                            |> Expect.equal { x = 3, y = 7 }
                ]
            , describe "moveSelection"
                [ test "moves right within bounds" <|
                    \_ ->
                        moveSelection { maxX = 5, maxY = 10 } 1 0 { x = 2, y = 5 }
                            |> Expect.equal { a = { x = 3, y = 5 }, b = { x = 3, y = 5 } }
                , test "moves down within bounds" <|
                    \_ ->
                        moveSelection { maxX = 5, maxY = 10 } 0 1 { x = 2, y = 5 }
                            |> Expect.equal { a = { x = 2, y = 6 }, b = { x = 2, y = 6 } }
                , test "moves left within bounds" <|
                    \_ ->
                        moveSelection { maxX = 5, maxY = 10 } -1 0 { x = 2, y = 5 }
                            |> Expect.equal { a = { x = 1, y = 5 }, b = { x = 1, y = 5 } }
                , test "moves up within bounds" <|
                    \_ ->
                        moveSelection { maxX = 5, maxY = 10 } 0 -1 { x = 2, y = 5 }
                            |> Expect.equal { a = { x = 2, y = 4 }, b = { x = 2, y = 4 } }
                , test "clamps move at right edge" <|
                    \_ ->
                        moveSelection { maxX = 5, maxY = 10 } 1 0 { x = 5, y = 5 }
                            |> Expect.equal { a = { x = 5, y = 5 }, b = { x = 5, y = 5 } }
                , test "clamps move at left edge" <|
                    \_ ->
                        moveSelection { maxX = 5, maxY = 10 } -1 0 { x = 0, y = 5 }
                            |> Expect.equal { a = { x = 0, y = 5 }, b = { x = 0, y = 5 } }
                , test "clamps move at top edge" <|
                    \_ ->
                        moveSelection { maxX = 5, maxY = 10 } 0 -1 { x = 2, y = 1 }
                            |> Expect.equal { a = { x = 2, y = 1 }, b = { x = 2, y = 1 } }
                , test "clamps move at bottom edge" <|
                    \_ ->
                        moveSelection { maxX = 5, maxY = 10 } 0 1 { x = 2, y = 10 }
                            |> Expect.equal { a = { x = 2, y = 10 }, b = { x = 2, y = 10 } }
                ]
            , describe "nextSortOrder"
                [ test "Nothing -> Ascending" <|
                    \_ ->
                        nextSortOrder Nothing
                            |> Expect.equal (Just Ascending)
                , test "Ascending -> Descending" <|
                    \_ ->
                        nextSortOrder (Just Ascending)
                            |> Expect.equal (Just Descending)
                , test "Descending -> Nothing" <|
                    \_ ->
                        nextSortOrder (Just Descending)
                            |> Expect.equal Nothing
                ]
            , describe "sortWithOrder"
                [ test "sorts ascending" <|
                    \_ ->
                        sortWithOrder Ascending compare [ 3, 1, 2 ]
                            |> Expect.equal [ 1, 2, 3 ]
                , test "sorts descending" <|
                    \_ ->
                        sortWithOrder Descending compare [ 3, 1, 2 ]
                            |> Expect.equal [ 3, 2, 1 ]
                ]
            , describe "xy helper"
                [ test "creates index with x and y" <|
                    \_ ->
                        xy 3 7
                            |> Expect.equal { x = 3, y = 7 }
                ]
            , describe "rect helper"
                [ test "creates rect from four coordinates" <|
                    \_ ->
                        rect 1 2 3 4
                            |> Expect.equal { a = { x = 1, y = 2 }, b = { x = 3, y = 4 } }
                ]
            , describe "sortWithOrder with strings"
                [ test "sorts strings ascending" <|
                    \_ ->
                        sortWithOrder Ascending compare [ "banana", "apple", "cherry" ]
                            |> Expect.equal [ "apple", "banana", "cherry" ]
                , test "sorts strings descending" <|
                    \_ ->
                        sortWithOrder Descending compare [ "banana", "apple", "cherry" ]
                            |> Expect.equal [ "cherry", "banana", "apple" ]
                ]
            , describe "normalizeRect"
                [ test "normalizes rect with a < b" <|
                    \_ ->
                        normalizeRect { a = { x = 1, y = 2 }, b = { x = 3, y = 4 } }
                            |> Expect.equal { a = { x = 1, y = 2 }, b = { x = 3, y = 4 } }
                , test "normalizes rect with a > b" <|
                    \_ ->
                        normalizeRect { a = { x = 3, y = 4 }, b = { x = 1, y = 2 } }
                            |> Expect.equal { a = { x = 1, y = 2 }, b = { x = 3, y = 4 } }
                , test "normalizes rect with mixed coordinates" <|
                    \_ ->
                        normalizeRect { a = { x = 3, y = 1 }, b = { x = 1, y = 5 } }
                            |> Expect.equal { a = { x = 1, y = 1 }, b = { x = 3, y = 5 } }
                ]
            , describe "expandSelection"
                [ test "expands selection right" <|
                    \_ ->
                        expandSelection { maxX = 5, maxY = 10 } 1 0 { a = { x = 2, y = 3 }, b = { x = 2, y = 3 } }
                            |> Expect.equal { a = { x = 2, y = 3 }, b = { x = 3, y = 3 } }
                , test "expands selection down" <|
                    \_ ->
                        expandSelection { maxX = 5, maxY = 10 } 0 1 { a = { x = 2, y = 3 }, b = { x = 2, y = 3 } }
                            |> Expect.equal { a = { x = 2, y = 3 }, b = { x = 2, y = 4 } }
                , test "clamps expansion at bounds" <|
                    \_ ->
                        expandSelection { maxX = 5, maxY = 10 } 5 0 { a = { x = 2, y = 3 }, b = { x = 2, y = 3 } }
                            |> Expect.equal { a = { x = 2, y = 3 }, b = { x = 5, y = 3 } }
                , test "preserves anchor when expanding" <|
                    \_ ->
                        expandSelection { maxX = 5, maxY = 10 } -1 0 { a = { x = 2, y = 3 }, b = { x = 4, y = 3 } }
                            |> .a
                            |> Expect.equal { x = 2, y = 3 }
                ]
            , describe "selectAll"
                [ test "selects entire table" <|
                    \_ ->
                        selectAll { maxX = 5, maxY = 10 }
                            |> Expect.equal { a = { x = 0, y = 1 }, b = { x = 5, y = 10 } }
                , test "handles single cell table" <|
                    \_ ->
                        selectAll { maxX = 0, maxY = 1 }
                            |> Expect.equal { a = { x = 0, y = 1 }, b = { x = 0, y = 1 } }
                ]
            , describe "rectToIndices"
                [ test "converts single cell rect to list" <|
                    \_ ->
                        rectToIndices { a = { x = 2, y = 3 }, b = { x = 2, y = 3 } }
                            |> Expect.equal [ { x = 2, y = 3 } ]
                , test "converts 2x2 rect to list" <|
                    \_ ->
                        rectToIndices { a = { x = 1, y = 1 }, b = { x = 2, y = 2 } }
                            |> List.length
                            |> Expect.equal 4
                , test "handles reversed rect" <|
                    \_ ->
                        rectToIndices { a = { x = 2, y = 2 }, b = { x = 1, y = 1 } }
                            |> List.length
                            |> Expect.equal 4
                , test "returns cells in row-major order" <|
                    \_ ->
                        rectToIndices { a = { x = 0, y = 1 }, b = { x = 1, y = 2 } }
                            |> Expect.equal
                                [ { x = 0, y = 1 }
                                , { x = 1, y = 1 }
                                , { x = 0, y = 2 }
                                , { x = 1, y = 2 }
                                ]
                ]
            ]
        , describe "Clipboard"
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
        ]
