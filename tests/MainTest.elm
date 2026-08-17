module MainTest exposing (..)

import Array
import Dict
import Expect
import Json.Decode as D
import Json.Encode as E
import Main exposing (..)
import Set
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
            , describe "displayYToDocY / filterAndSortIndexed"
                [ test "identity without sort, filter, or search" <|
                    \_ ->
                        let
                            rows =
                                Array.fromList
                                    [ Dict.fromList [ ( "0", E.string "x" ) ]
                                    , Dict.fromList [ ( "0", E.string "y" ) ]
                                    , Dict.fromList [ ( "0", E.string "z" ) ]
                                    ]
                        in
                        List.map (displayYToDocY "" emptySheet rows) [ 1, 2, 3 ]
                            |> Expect.equal [ 1, 2, 3 ]
                , test "sorted display rows map back to their document rows" <|
                    \_ ->
                        let
                            rows =
                                Array.fromList
                                    [ Dict.fromList [ ( "0", E.string "banana" ) ]
                                    , Dict.fromList [ ( "0", E.string "apple" ) ]
                                    , Dict.fromList [ ( "0", E.string "cherry" ) ]
                                    ]

                            sheet =
                                { emptySheet | sort = Just ( "0", Ascending ) }
                        in
                        -- display order apple, banana, cherry -> document rows 2, 1, 3
                        List.map (displayYToDocY "" sheet rows) [ 1, 2, 3 ]
                            |> Expect.equal [ 2, 1, 3 ]
                , test "search hides rows but preserves original document indices" <|
                    \_ ->
                        let
                            rows =
                                Array.fromList
                                    [ Dict.fromList [ ( "0", E.string "apple" ) ]
                                    , Dict.fromList [ ( "0", E.string "banana" ) ]
                                    , Dict.fromList [ ( "0", E.string "avocado" ) ]
                                    ]
                        in
                        -- only avocado (document row 3) contains "av"
                        ( filterAndSortIndexed "av" emptySheet rows |> Array.map Tuple.first |> Array.toList
                        , displayYToDocY "av" emptySheet rows 1
                        )
                            |> Expect.equal ( [ 2 ], 3 )
                , test "header rows (y <= 0) are never remapped" <|
                    \_ ->
                        let
                            rows =
                                Array.fromList [ Dict.fromList [ ( "0", E.string "a" ) ] ]

                            sheet =
                                { emptySheet | sort = Just ( "0", Descending ) }
                        in
                        List.map (displayYToDocY "" sheet rows) [ 0, -1, -2 ]
                            |> Expect.equal [ 0, -1, -2 ]
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
        , describe "docDecoder"
            [ test "net-http decodes url and interval, headers default to empty" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"net-http","data":[{"url":"https://x.test","interval":60}]}"""
                        |> Expect.equal (Ok (NetHttp { url = "https://x.test", interval = 60, headers = "" }))
            , test "net-http decodes a headers string" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"net-http","data":[{"url":"https://x.test","interval":60,"headers":"X-Key: abc"}]}"""
                        |> Expect.equal (Ok (NetHttp { url = "https://x.test", interval = 60, headers = "X-Key: abc" }))
            , test "net-http without interval is still rejected" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"net-http","data":[{"url":"https://x.test"}]}"""
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            , test "net-socket decodes its url" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"net-socket","data":[{"url":"wss://x.test"}]}"""
                        |> Expect.equal (Ok (NetSocket { url = "wss://x.test" }))
            , test "net-hook decodes" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"net-hook"}"""
                        |> Expect.equal (Ok NetHook)
            , test "portal decodes to Unviewable" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"portal","data":[]}"""
                        |> Expect.equal (Ok (Unviewable "portal"))
            , test "template decodes to Unviewable" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"template"}"""
                        |> Expect.equal (Ok (Unviewable "template"))
            , test "any codex-* type decodes to Unviewable" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"codex-db"}"""
                        |> Expect.equal (Ok (Unviewable "codex-db"))
            , test "an unrecognized type fails to decode" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"wat"}"""
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]
        , describe "Column stats"
            [ describe "civilDays"
                [ test "the unix epoch is day zero" <|
                    \_ -> civilDays 1970 1 1 |> Expect.equal 0
                , test "counts a leap day" <|
                    \_ -> civilDays 2024 3 1 - civilDays 2024 2 28 |> Expect.equal 2
                , test "counts a non-leap February" <|
                    \_ -> civilDays 2026 3 1 - civilDays 2026 2 28 |> Expect.equal 1
                , test "a full non-leap year is 365 days" <|
                    \_ -> civilDays 2027 1 1 - civilDays 2026 1 1 |> Expect.equal 365
                ]
            , describe "parseDay"
                [ test "reads a bare date" <|
                    \_ -> parseDay "1970-01-02" |> Expect.equal (Just 1)
                , test "reads the date out of a timestamp" <|
                    \_ -> parseDay "1970-01-02T13:45:00Z" |> Expect.equal (Just 1)
                , test "rejects text that is not a date" <|
                    \_ -> parseDay "hello" |> Expect.equal Nothing
                , test "rejects an empty string" <|
                    \_ -> parseDay "" |> Expect.equal Nothing
                ]
            , describe "computeTemporalStats"
                [ test "tracks first, last and the distinct days present" <|
                    \_ ->
                        case computeTemporalStats (dateRows [ "2026-01-01", "2026-01-03", "2026-01-01" ]) "d" of
                            Temporal stat ->
                                ( stat.count, ( stat.first, stat.last ) )
                                    |> Expect.equal ( 3, ( Just "2026-01-01", Just "2026-01-03" ) )

                            _ ->
                                Expect.fail "expected a Temporal stat"
                , test "a three-day span holding two distinct days has one gap" <|
                    \_ ->
                        case computeTemporalStats (dateRows [ "2026-01-01", "2026-01-03" ]) "d" of
                            Temporal stat ->
                                Expect.equal 2 (Set.size stat.days)

                            _ ->
                                Expect.fail "expected a Temporal stat"
                , test "ignores values that are not dates" <|
                    \_ ->
                        case computeTemporalStats (dateRows [ "2026-01-01", "nope" ]) "d" of
                            Temporal stat ->
                                Expect.equal 1 stat.count

                            _ ->
                                Expect.fail "expected a Temporal stat"
                ]
            , describe "computeBoolishStats"
                [ test "counts true, false and blank separately" <|
                    \_ ->
                        case computeBoolishStats (boolRows [ E.bool True, E.bool False, E.bool True, E.null ]) "b" of
                            Boolish stat ->
                                ( stat.true, stat.false, stat.blank ) |> Expect.equal ( 2, 1, 1 )

                            _ ->
                                Expect.fail "expected a Boolish stat"
                ]
            ]
        , describe "shortcutGroups"
            [ test "is non-empty and every group has non-empty key/description pairs" <|
                \_ ->
                    shortcutGroups
                        |> Expect.all
                            [ List.isEmpty >> Expect.equal False
                            , List.all
                                (\( group, keys ) ->
                                    (group /= "")
                                        && not (List.isEmpty keys)
                                        && List.all (\( key, description ) -> key /= "" && description /= "") keys
                                )
                                >> Expect.equal True
                            ]
            ]
        ]


dateRows : List String -> Array.Array (Dict.Dict String D.Value)
dateRows =
    List.map (\s -> Dict.singleton "d" (E.string s)) >> Array.fromList


boolRows : List E.Value -> Array.Array (Dict.Dict String D.Value)
boolRows =
    List.map (Dict.singleton "b") >> Array.fromList
