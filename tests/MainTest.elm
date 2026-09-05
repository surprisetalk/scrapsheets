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
                                { emptySheet | sort = [ ( "0", Ascending ) ] }
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
                                { emptySheet | sort = [ ( "0", Descending ) ] }
                        in
                        List.map (displayYToDocY "" sheet rows) [ 0, -1, -2 ]
                            |> Expect.equal [ 0, -1, -2 ]
                ]
            , describe "multi-column sort"
                [ test "a second key breaks ties left by the first" <|
                    \_ ->
                        let
                            rows =
                                Array.fromList
                                    [ Dict.fromList [ ( "0", E.string "b" ), ( "1", E.string "2" ) ]
                                    , Dict.fromList [ ( "0", E.string "a" ), ( "1", E.string "2" ) ]
                                    , Dict.fromList [ ( "0", E.string "a" ), ( "1", E.string "1" ) ]
                                    ]

                            sheet =
                                { emptySheet | sort = [ ( "0", Ascending ), ( "1", Descending ) ] }
                        in
                        -- a/2, a/1, b/2 -> document rows 2, 3, 1
                        filterAndSortIndexed "" sheet rows
                            |> Array.map Tuple.first
                            |> Array.toList
                            |> Expect.equal [ 1, 2, 0 ]
                , test "descending flips the key without reversing ties" <|
                    \_ ->
                        let
                            rows =
                                Array.fromList
                                    [ Dict.fromList [ ( "0", E.string "a" ), ( "1", E.string "first" ) ]
                                    , Dict.fromList [ ( "0", E.string "a" ), ( "1", E.string "second" ) ]
                                    , Dict.fromList [ ( "0", E.string "b" ), ( "1", E.string "third" ) ]
                                    ]

                            sheet =
                                { emptySheet | sort = [ ( "0", Descending ) ] }
                        in
                        -- b first, then the two a rows in their original order
                        filterAndSortIndexed "" sheet rows
                            |> Array.map Tuple.first
                            |> Array.toList
                            |> Expect.equal [ 2, 0, 1 ]
                , test "numeric columns compare as numbers, not text" <|
                    \_ ->
                        let
                            rows =
                                Array.fromList
                                    [ Dict.fromList [ ( "0", E.string "10" ) ]
                                    , Dict.fromList [ ( "0", E.string "9" ) ]
                                    ]

                            sheet =
                                { emptySheet | sort = [ ( "0", Ascending ) ] }
                        in
                        filterAndSortIndexed "" sheet rows
                            |> Array.map Tuple.first
                            |> Array.toList
                            |> Expect.equal [ 1, 0 ]
                , test "a plain click cycles asc, desc, off and drops the other keys" <|
                    \_ ->
                        let
                            step keys =
                                cycleSort False "a" keys
                        in
                        ( step [ ( "b", Ascending ) ]
                        , step [ ( "a", Ascending ), ( "b", Ascending ) ]
                        , step [ ( "a", Descending ), ( "b", Ascending ) ]
                        )
                            |> Expect.equal
                                ( [ ( "a", Ascending ) ]
                                , [ ( "a", Descending ) ]
                                , []
                                )
                , test "shift-click keeps the other keys and appends a new one" <|
                    \_ ->
                        cycleSort True "b" [ ( "a", Ascending ) ]
                            |> Expect.equal [ ( "a", Ascending ), ( "b", Ascending ) ]
                , test "shift-clicking an existing key holds its rank" <|
                    \_ ->
                        cycleSort True "a" [ ( "a", Ascending ), ( "b", Ascending ) ]
                            |> Expect.equal [ ( "a", Descending ), ( "b", Ascending ) ]
                , test "shift-cycling a key off removes only that key" <|
                    \_ ->
                        cycleSort True "a" [ ( "a", Descending ), ( "b", Ascending ) ]
                            |> Expect.equal [ ( "b", Ascending ) ]
                , test "sortRankOf is 1-based and Nothing when the key is unsorted" <|
                    \_ ->
                        ( sortRankOf "a" [ ( "a", Ascending ), ( "b", Descending ) ]
                        , sortRankOf "b" [ ( "a", Ascending ), ( "b", Descending ) ]
                        , sortRankOf "c" [ ( "a", Ascending ) ]
                        )
                            |> Expect.equal ( Just 1, Just 2, Nothing )
                ]
            , describe "rowSplices"
                [ test "inserting above two rows lands blanks at both, undone highest first" <|
                    \_ ->
                        let
                            ( forward, backward ) =
                                rowSplices (\_ -> Just Dict.empty) 0 [ 3, 4 ] identity
                        in
                        ( List.map (.value >> E.encode 0) forward
                        , List.map (.value >> E.encode 0) backward
                        )
                            |> Expect.equal
                                ( [ "[4,0,{}]", "[3,0,{}]" ]
                                , [ "[5,1]", "[3,1]" ]
                                )
                , test "duplicating puts each copy below its source" <|
                    \_ ->
                        let
                            source i =
                                Just (Dict.fromList [ ( "0", E.int i ) ])

                            ( forward, backward ) =
                                rowSplices source 1 [ 3, 4 ] identity
                        in
                        ( List.map (.value >> E.encode 0) forward
                        , List.map (.value >> E.encode 0) backward
                        )
                            |> Expect.equal
                                ( [ "[5,0,{\"0\":4}]", "[4,0,{\"0\":3}]" ]
                                , [ "[6,1]", "[4,1]" ]
                                )
                , test "duplicate indices collapse to one splice" <|
                    \_ ->
                        rowSplices (\_ -> Just Dict.empty) 0 [ 2, 2, 2 ] identity
                            |> Tuple.first
                            |> List.length
                            |> Expect.equal 1
                , test "a source with no row emits nothing" <|
                    \_ ->
                        rowSplices (\_ -> Nothing) 1 [ 1, 2 ] identity
                            |> Expect.equal ( [], [] )
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
        , describe "dropOf"
            (let
                tbl =
                    { cols = namedCols [ "a", "b", "c" ]
                    , rows = Array.repeat 4 Dict.empty
                    }
             in
             [ test "a row let go over another row moves there, in document indices" <|
                \_ ->
                    dropOf (MovingRow 3) (xy 0 1) tbl
                        |> Expect.equal (Just (SheetRowMove 3 1))
             , test "a row let go on the header, or off the table, goes nowhere" <|
                \_ ->
                    [ dropOf (MovingRow 3) (xy 0 0) tbl, dropOf (MovingRow 3) (xy -1 -1) tbl ]
                        |> Expect.equal [ Nothing, Nothing ]
             , test "a row let go where it was is not a move" <|
                \_ ->
                    dropOf (MovingRow 2) (xy 1 2) tbl
                        |> Expect.equal Nothing
             , test "a row that is no longer there goes nowhere" <|
                \_ ->
                    dropOf (MovingRow 9) (xy 0 1) tbl
                        |> Expect.equal Nothing
             , test "a row let go past the last row lands on the last row" <|
                \_ ->
                    dropOf (MovingRow 1) (xy 0 9) tbl
                        |> Expect.equal (Just (SheetRowMove 1 4))
             , test "a column is found by key and dropped by position" <|
                \_ ->
                    dropOf (MovingCol "2") (xy 0 0) tbl
                        |> Expect.equal (Just (SheetColumnMove 2 0))
             ]
            )
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
            , test "alert without a when fires on rows, the way it did before there was one" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"alert","data":[{"code":"","to":"","interval":60}]}"""
                        |> Expect.equal (Ok (Alert { code = "", to = "", interval = 60, digest = False, when = OnRows }))
            , test "alert decodes its when" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"alert","data":[{"code":"","to":"","interval":60,"when":"added"}]}"""
                        |> Expect.equal (Ok (Alert { code = "", to = "", interval = 60, digest = False, when = OnAdded }))
            , test "alert with an unknown when is refused rather than shown as rows" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"alert","data":[{"code":"","to":"","interval":60,"when":"bogus"}]}"""
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            , test "alert with a when that is not a string is refused too, as the server refuses it" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"alert","data":[{"code":"","to":"","interval":60,"when":5}]}"""
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
            , test "chart decodes its source, kind and both axes" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"chart","data":[{"source":"@query:budget-burn","kind":"bar","x":"department","y":"burn_ratio"}]}"""
                        |> Expect.equal (Ok (Chart { source = "@query:budget-burn", kind = Bar, x = "department", y = "burn_ratio" }))
            , test "a chart with no kind is a line, and its axes default to empty" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"chart","data":[{"source":"@query:x"}]}"""
                        |> Expect.equal (Ok (Chart { source = "@query:x", kind = Line, x = "", y = "" }))
            , test "every kind the engine admits decodes to one of its own" <|
                \_ ->
                    chartKinds
                        |> List.map
                            (\k ->
                                D.decodeString docDecoder
                                    ("""{"type":"chart","data":[{"source":"@query:x","kind":\"""" ++ (kindSpec k).name ++ "\"}]}")
                            )
                        |> Expect.equal (List.map (\k -> Ok (Chart { source = "@query:x", kind = k, x = "", y = "" })) chartKinds)
            , test "a kind nobody draws is refused rather than drawn as a line" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"chart","data":[{"source":"@query:x","kind":"scater"}]}"""
                        |> Result.mapError (always "refused")
                        |> Expect.equal (Err "refused")
            , test "dashboard decodes its tiles in order" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"dashboard","data":[{"tiles":["@chart:a","@query:b"]}]}"""
                        |> Expect.equal (Ok (Dashboard [ "@chart:a", "@query:b" ]))
            , test "a dashboard with no tiles is empty rather than a failure" <|
                \_ ->
                    D.decodeString docDecoder """{"type":"dashboard","data":[{}]}"""
                        |> Expect.equal (Ok (Dashboard []))
            ]
        , describe "nameClash"
            [ test "a rename onto another column's name is refused" <|
                \_ ->
                    nameClash (namedCols [ "a", "b", "c" ]) 2 "a"
                        |> Expect.equal (Just "a")
            , test "renaming a column to what it is already called is not a clash" <|
                \_ ->
                    nameClash (namedCols [ "a", "b" ]) 0 "a"
                        |> Expect.equal Nothing
            , test "a name nothing else carries is free" <|
                \_ ->
                    nameClash (namedCols [ "a", "b" ]) 1 "c"
                        |> Expect.equal Nothing
            , test "renaming one of an already-colliding pair is the repair, not a clash" <|
                \_ ->
                    nameClash (namedCols [ "a", "a" ]) 1 "b"
                        |> Expect.equal Nothing
            , test "a blank name collides like any other, since a blank is a name" <|
                \_ ->
                    nameClash (namedCols [ "", "b" ]) 1 ""
                        |> Expect.equal (Just "")
            ]
        , describe "chartPoints"
            [ test "reads the x label and the y number, in the order given" <|
                \_ ->
                    chartPoints (chartTable [ ( "Jan", E.float 3 ), ( "Feb", E.float 1 ) ])
                        |> Expect.equal [ ( "Jan", 3 ), ( "Feb", 1 ) ]
            , test "a y that is not a number is dropped, not read as zero" <|
                \_ ->
                    chartPoints (chartTable [ ( "Jan", E.float 3 ), ( "Feb", E.string "n/a" ), ( "Mar", E.float 2 ) ])
                        |> Expect.equal [ ( "Jan", 3 ), ( "Mar", 2 ) ]
            , test "a y held as a numeric string still counts" <|
                \_ ->
                    chartPoints (chartTable [ ( "Jan", E.string "4.5" ) ])
                        |> Expect.equal [ ( "Jan", 4.5 ) ]
            , test "no plottable rows is an empty chart, not a crash" <|
                \_ ->
                    chartPoints (chartTable [ ( "Jan", E.null ) ])
                        |> Expect.equal []
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
                                        && List.all (\( key, description, _ ) -> key /= "" && description /= "") keys
                                )
                                >> Expect.equal True
                            ]
            ]
        , describe "freshnessDecoder"
            [ test "keys each row by the sheet it is about" <|
                \_ ->
                    D.decodeString freshnessDecoder """[{"sheet_id":"net-http:a","last_run":"2026-08-23T14:02:11.000Z","failures_since_ok":"3"}]"""
                        |> Result.map (Dict.get "net-http:a")
                        |> Expect.equal (Ok (Just (Freshness (Just "2026-08-23T14:02:11.000Z") 3)))
            , test "a sheet that has never run carries no last run" <|
                \_ ->
                    D.decodeString freshnessDecoder """[{"sheet_id":"alert:b","last_run":null,"failures_since_ok":0}]"""
                        |> Result.map (Dict.get "alert:b")
                        |> Expect.equal (Ok (Just (Freshness Nothing 0)))
            , test "a renamed field is an error, not a library where every feed is fine" <|
                \_ ->
                    D.decodeString freshnessDecoder """[{"sheet_id":"net-http:a","last_run":null,"failures":2}]"""
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]
        , describe "freshnessCell"
            [ test "a sheet the read does not answer for shows nothing at all" <|
                \_ -> freshnessCell Nothing |> Expect.equal ""
            , test "a sheet that has never run says so, rather than reading as fine" <|
                \_ -> freshnessCell (Just (Freshness Nothing 0)) |> Expect.equal "never run"
            , test "a good feed shows its last run to the minute" <|
                \_ ->
                    freshnessCell (Just (Freshness (Just "2026-08-23T14:02:11.000Z") 0))
                        |> Expect.equal "2026-08-23 14:02"
            , test "a failing feed shows how many runs since its last good one" <|
                \_ ->
                    freshnessCell (Just (Freshness (Just "2026-08-23T14:02:11.000Z") 4))
                        |> Expect.equal "2026-08-23 14:02 · 4 failed"
            ]
        , describe "Cleaning a column"
            [ test "trim writes only the cells it moves, and its own undo" <|
                \_ ->
                    rewritten String.trim [ " a ", "b", "  " ]
                        |> Expect.equal ( [ ( 1, "a" ), ( 3, "" ) ], [ ( 1, " a " ), ( 3, "  " ) ] )
            , test "a cell the change does not move contributes no patch at all" <|
                \_ -> rewritten String.toUpper [ "A", "B" ] |> Tuple.first |> Expect.equal []
            , test "a cell that is not text is left to the type that owns it" <|
                \_ ->
                    withCleanCol
                        (\col ->
                            cellRewrites col
                                String.trim
                                (Array.fromList [ Dict.fromList [ ( "0", E.int 5 ) ], Dict.fromList [ ( "0", E.string " x " ) ] ])
                                |> Tuple.first
                                |> List.map patchOf
                        )
                        []
                        |> Expect.equal [ ( 2, "x" ) ]
            , test "blank is missing, null or whitespace -- and a zero is not blank" <|
                \_ ->
                    withCleanCol
                        (\col ->
                            blankRows col
                                (Array.fromList
                                    [ Dict.fromList [ ( "0", E.string "x" ) ]
                                    , Dict.fromList [ ( "0", E.string "  " ) ]
                                    , Dict.fromList [ ( "0", E.null ) ]
                                    , Dict.fromList [ ( "1", E.string "elsewhere" ) ]
                                    , Dict.fromList [ ( "0", E.int 0 ) ]
                                    ]
                                )
                        )
                        []
                        |> Expect.equal [ 2, 3, 4 ]
            , test "rows come out highest first and go back lowest first, so each lands where it left" <|
                \_ ->
                    rowDeletions (cleanRows [ "a", "b", "c" ]) [ 1, 3 ]
                        |> Tuple.mapBoth (List.map spliceOf) (List.map spliceOf)
                        |> Expect.equal ( [ "[3,1]", "[1,1]" ], [ "[1,0,{\"0\":\"a\"}]", "[3,0,{\"0\":\"c\"}]" ] )
            , test "a row named twice is removed once" <|
                \_ ->
                    rowDeletions (cleanRows [ "a", "b" ]) [ 2, 2 ]
                        |> Tuple.first
                        |> List.map spliceOf
                        |> Expect.equal [ "[2,1]" ]
            , test "an index with no row behind it deletes nothing, rather than something it cannot put back" <|
                \_ ->
                    -- 0 is the column list and 9 is past the end. Both used to
                    -- splice a row out and hand back an empty undo.
                    rowDeletions (cleanRows [ "a", "b" ]) [ 0, 9, -1, 2 ]
                        |> Tuple.mapBoth (List.map spliceOf) (List.map spliceOf)
                        |> Expect.equal ( [ "[2,1]" ], [ "[2,0,{\"0\":\"b\"}]" ] )
            ]
        , describe "formatNumber"
            [ test "a total that overflowed is not dressed up as money" <|
                \_ ->
                    -- `usd` groups the digits of the string form, so Infinity
                    -- came out "$In,fin,ity.00". Two 1e308 cells in a usd column
                    -- is all the totals row needs.
                    [ reads "usd" (1.0e308 + 1.0e308), reads "usd" -(1.0e308 + 1.0e308) ]
                        |> Expect.equal [ "Infinity", "-Infinity" ]
            , test "a usd column reads as money everywhere it is summed" <|
                \_ ->
                    [ reads "usd" 1234.5, reads "num" 1234.5, reads "percentage" 0.25 ]
                        |> Expect.equal [ "$1,234.50", "1234.5", "25%" ]
            ]
        , describe "paletteCommands"
            [ test "an empty query offers every runnable shortcut, in the order the sheet lists them" <|
                \_ ->
                    paletteCommands Dict.empty ""
                        |> List.map .label
                        |> Expect.equal [ "select all", "copy", "find", "replace", "undo", "redo", "shortcut sheet" ]
            , test "a sheet is matched on its name" <|
                \_ -> paletteCommands paletteShelf "countr" |> List.map .label |> Expect.equal [ "countries" ]
            , test "a sheet is matched on its id too, which is what you remember of a net sheet" <|
                \_ -> paletteCommands paletteShelf "table:us" |> List.map .label |> Expect.equal [ "us states" ]
            , test "a command is matched on the words the shortcut sheet shows" <|
                \_ -> paletteCommands paletteShelf "undo" |> List.map .label |> Expect.equal [ "undo" ]
            , test "a shortcut that only means something against a selection is never offered" <|
                \_ -> paletteCommands Dict.empty "paste" |> List.map .label |> Expect.equal []
            , test "a scratch sheet is not a destination" <|
                \_ ->
                    paletteCommands (libraryOf [ ( "table:draft", "draft", True ) ]) "draft"
                        |> List.map .label
                        |> Expect.equal []
            , test "a trashed sheet is not a destination, or the palette still opens what you threw away" <|
                \_ -> paletteCommands trashedShelf "gone" |> List.map .label |> Expect.equal []
            , test "the library itself is not a destination" <|
                \_ ->
                    paletteCommands (libraryOf [ ( "", "library-root", False ) ]) "library-root"
                        |> List.map .label
                        |> Expect.equal []
            , test "the list is bounded, so a big library is narrowed by typing rather than scrolled" <|
                \_ ->
                    List.range 1 40
                        |> List.map (\n -> ( "table:s" ++ String.fromInt n, "sheet " ++ String.fromInt n, False ))
                        |> libraryOf
                        |> (\shelf -> paletteCommands shelf "sheet")
                        |> List.length
                        |> Expect.equal 12
            ]
        , describe "Hidden columns"
            [ test "moving right steps over a hidden column" <|
                \_ ->
                    hiddenSheet [ "1" ]
                        |> (\sheet -> skipHidden sheet { maxX = 3, maxY = 5 } 1 1)
                        |> Expect.equal 2
            , test "moving left steps over a run of hidden columns" <|
                \_ ->
                    hiddenSheet [ "1", "2" ]
                        |> (\sheet -> skipHidden sheet { maxX = 3, maxY = 5 } -1 2)
                        |> Expect.equal 0
            , test "an edge with nothing visible beyond it stays put" <|
                \_ ->
                    hiddenSheet [ "3" ]
                        |> (\sheet -> skipHidden sheet { maxX = 3, maxY = 5 } 1 3)
                        |> Expect.equal 3
            , test "vertical movement never skips" <|
                \_ ->
                    hiddenSheet [ "1" ]
                        |> (\sheet -> skipHidden sheet { maxX = 3, maxY = 5 } 0 1)
                        |> Expect.equal 1
            , test "every column hidden terminates instead of looping" <|
                \_ ->
                    hiddenSheet [ "0", "1", "2", "3" ]
                        |> (\sheet -> skipHidden sheet { maxX = 3, maxY = 5 } 1 0)
                        |> Expect.equal 0
            ]
        , describe "Column types"
            [ test "a column keeps the spelling the document gave it" <|
                \_ ->
                    typedCols
                        |> List.map .raw
                        |> Expect.equal [ "int", "float", "percentage", "wat" ]
            , test "an alias is read and never written" <|
                \_ ->
                    -- "pct" reads as a percentage so old documents still load, and
                    -- is refused on a write: the engine does not know the word, so
                    -- storing it is how a percent column stops being checked.
                    ( typedCols |> List.map (.typ >> typeName)
                    , List.map knownTypeName [ "pct", "percent", "number", "string", "datetime" ]
                    )
                        |> Expect.equal
                            ( [ "num", "num", "percentage", "unknown" ]
                            , [ False, False, False, False, False ]
                            )
            , test "a type the page cannot read back is not a type it will write" <|
                \_ ->
                    List.map knownTypeName [ "int", "float", "percentage", "enum:a,b", "wat", "enum:", "" ]
                        |> Expect.equal [ True, True, True, True, False, False, False ]
            , test "the refusal offers every type the engine tells apart" <|
                \_ ->
                    -- int, float and num are one Type in the page and three to the
                    -- engine, so offering the Type names told a user to write num
                    -- where int was meant.
                    List.filter (\name -> not (List.member name canonicalTypeNames)) [ "int", "float", "num", "usd" ]
                        |> Expect.equal []
            , test "money rounds to the cent rather than dropping it" <|
                \_ ->
                    List.map usd [ 1.999, 1.994, -1.999, 0, 1234.5 ]
                        |> Expect.equal [ "$2.00", "$1.99", "-$2.00", "$0.00", "$1,234.50" ]
            ]
        , describe "The stored view"
            [ test "a sheet is read back arranged the way it was left" <|
                \_ ->
                    ( arranged.hidden, arranged.widths, Dict.toList arranged.filters )
                        |> Expect.equal ( Set.singleton "1", Dict.fromList [ ( "1", 220 ) ], [ ( "2", TextContains "ill" ) ] )
            , test "rank orders the sort keys, not the order the columns are in" <|
                \_ ->
                    arranged.sort
                        |> Expect.equal [ ( "2", Ascending ), ( "0", Descending ) ]
            , test "a sheet nobody arranged reads as nothing arranged" <|
                \_ ->
                    """{"type":"table","data":[[{"name":"a","type":"text","key":"0"}]]}"""
                        |> D.decodeString viewDecoder
                        |> Expect.equal (Ok emptyView)
            , test "a sheet with no columns at all has no arrangement to lose" <|
                \_ ->
                    """{"type":"library"}"""
                        |> D.decodeString viewDecoder
                        |> Expect.equal (Ok emptyView)
            , test "a sort spelled in a way nobody wrote is no sort, not an error" <|
                \_ ->
                    """{"type":"table","data":[[{"name":"a","type":"text","key":"0","sort":"sideways"}]]}"""
                        |> D.decodeString viewDecoder
                        |> Expect.equal (Ok emptyView)
            , test "an arrangement goes when its column goes" <|
                \_ ->
                    -- An undo used to restore the sort rank a column had when it
                    -- was deleted, colliding with a rank written since; and a
                    -- pushed column reuses a deleted key and inherited the lot.
                    let
                        two =
                            D.decodeString docDecoder
                                """{"type":"table","data":[[{"name":"a","type":"text","key":"0"},{"name":"b","type":"text","key":"1"}]]}"""
                                |> Result.mapError D.errorToString

                        arrangedOnThree =
                            { emptySheet
                                | doc = two
                                , hidden = Set.fromList [ "1", "9" ]
                                , sort = [ ( "9", Ascending ), ( "0", Descending ) ]
                                , widths = Dict.fromList [ ( "1", 220 ), ( "9", 90 ) ]
                            }

                        pruned =
                            pruneView two arrangedOnThree
                    in
                    ( pruned.hidden, pruned.sort, pruned.widths )
                        |> Expect.equal ( Set.singleton "1", [ ( "0", Descending ) ], Dict.singleton "1" 220 )
            , test "the arrangement is keyed the way the table is keyed" <|
                \_ ->
                    -- A column whose name is a JSON number falls back to a key of
                    -- "" when the table decodes it. Read as "1" here, the
                    -- arrangement sat under a key nothing rendered and pruneView
                    -- then deleted it.
                    """{"type":"table","data":[[{"name":2024,"type":"text","key":"1","hidden":true}]]}"""
                        |> D.decodeString viewDecoder
                        |> Result.map .hidden
                        |> Expect.equal (Ok (Set.singleton ""))
            , test "a query's arrangement lives beside its code, keyed by column name" <|
                \_ ->
                    -- A query's rows are computed, so it has no stored columns to
                    -- write on. The view goes in a map beside the type overrides,
                    -- keyed the way those are, and `rank` is what orders the sort
                    -- keys -- a map has no order of its own to borrow.
                    let
                        arrangedQuery =
                            """{"type":"query","data":[{"lang":"sql","code":"select 1","cols":{},
                                "view":{"b":{"sort":"desc","rank":2},
                                        "a":{"sort":"asc","rank":1,"pinned":true,"width":220}}}]}"""
                                |> D.decodeString viewDecoder
                                |> Result.withDefault emptyView
                    in
                    ( arrangedQuery.sort, ( arrangedQuery.pinned, arrangedQuery.widths ) )
                        |> Expect.equal
                            ( [ ( "a", Ascending ), ( "b", Descending ) ]
                            , ( Set.singleton "a", Dict.singleton "a" 220 )
                            )
            , test "a table's view is addressed by position and a query's by name" <|
                \_ ->
                    let
                        paths at =
                            viewPatches at (namedCols [ "a", "b" ]) emptyView { emptyView | hidden = Set.singleton "1" }
                                |> List.map (\p -> E.encode 0 (E.list identity p.path))
                    in
                    ( paths tableHome, paths queryHome )
                        |> Expect.equal
                            ( [ """[0,"1","hidden"]""" ], [ """[0,"view","1","hidden"]""" ] )
            , test "a pinned column reads back pinned" <|
                \_ ->
                    """{"type":"table","data":[[{"name":"a","type":"text","key":"0","pinned":true}]]}"""
                        |> D.decodeString viewDecoder
                        |> Result.map .pinned
                        |> Expect.equal (Ok (Set.singleton "0"))
            , test "a pinned column sits past every sticky column before it" <|
                \_ ->
                    -- Column 0 is sticky whether or not anybody pinned it, so its
                    -- width counts or a pinned column lands underneath it. A
                    -- column nobody pinned does not count, and one that sizes
                    -- itself counts as the width pinning writes for it.
                    pinLeft
                        { emptySheet | pinned = Set.singleton "2", widths = Dict.fromList [ ( "0", 100 ), ( "1", 60 ) ] }
                        (namedCols [ "a", "b", "c" ])
                        |> Dict.toList
                        |> Expect.equal [ ( "0", 0 ), ( "2", 100 ) ]
            , test "a column move is one patch saying where from and where to" <|
                \_ ->
                    -- Not a splice out and a splice back in: what goes back has to
                    -- be the column object the document holds, and `Col` carries
                    -- key, name and type only.
                    ( movePatch [ 0 ] 3 0 |> .action
                    , ( E.encode 0 (E.list identity (movePatch [ 0 ] 3 0).path), E.encode 0 (movePatch [ 0 ] 3 0).value )
                    )
                        |> Expect.equal ( "move", ( "[0]", "[3,0]" ) )
            , test "a row move is the same patch at the root, and its inverse is the undo" <|
                \_ ->
                    ( ( E.encode 0 (E.list identity (movePatch [] 3 1).path), E.encode 0 (movePatch [] 3 1).value )
                    , E.encode 0 (movePatch [] 1 3).value
                    )
                        |> Expect.equal ( ( "[]", "[3,1]" ), "[1,3]" )
            , test "a width too narrow to grab is no width" <|
                \_ ->
                    -- The drag clamps at the same floor. A document can carry any
                    -- integer, and a 0px column cannot be grabbed to widen again.
                    """{"type":"table","data":[[{"name":"a","type":"text","key":"0","width":0},
                        {"name":"b","type":"text","key":"1","width":-40},
                        {"name":"c","type":"text","key":"2","width":31},
                        {"name":"d","type":"text","key":"3","width":32}]]}"""
                        |> D.decodeString viewDecoder
                        |> Result.map .widths
                        |> Expect.equal (Ok (Dict.singleton "3" 32))
            ]
        ]


{-| A three-column sheet carrying every part of an arrangement: a secondary sort
key written before the primary one, a hidden column that was also dragged wider,
and a filter.
-}
arranged : SheetView
arranged =
    """{"type":"table","data":[[
        {"name":"a","type":"text","key":"0","sort":"desc","rank":2},
        {"name":"b","type":"text","key":"1","hidden":true,"width":220},
        {"name":"c","type":"text","key":"2","sort":"asc","rank":1,"filter":"ill"}]]}"""
        |> D.decodeString viewDecoder
        |> Result.withDefault emptyView


{-| Four columns whose declared types cover the cases that used to be rewritten:
two numeric spellings the page normalizes to one type, one that was renamed to a
word nothing else knew, and one nobody knows at all.
-}
typedCols : List Col
typedCols =
    D.decodeString docDecoder
        """{"type":"table","data":[[{"name":"a","type":"int","key":"0"},{"name":"b","type":"float","key":"1"},{"name":"c","type":"percentage","key":"2"},{"name":"d","type":"wat","key":"3"}]]}"""
        |> Result.map
            (\doc ->
                case doc of
                    Tab tbl ->
                        Array.toList tbl.cols

                    _ ->
                        []
            )
        |> Result.withDefault []


{-| A library map from id, name and whether the sheet is scratch. `Library` and
`SheetInfo` are not exposed, so this carries no annotation on purpose.
-}
libraryOf entries =
    entries
        |> List.map
            (\( id, name, scratch ) ->
                ( id, { name = name, tags = [], scratch = scratch, system = False, thumb = E.null, seen = "", trashed = False } )
            )
        |> Dict.fromList


{-| A number as the column named by that type spelling reads it. The type comes
back through `docDecoder`, the way every other fixture here is built, so this
needs no `Type` constructor exposed for one test.
-}
reads spelling v =
    D.decodeString docDecoder
        ("""{"type":"table","data":[[{"name":"a","type":\"""" ++ spelling ++ """\","key":"0"}]]}""")
        |> Result.toMaybe
        |> Maybe.andThen
            (\doc ->
                case doc of
                    Tab tbl ->
                        Array.get 0 tbl.cols

                    _ ->
                        Nothing
            )
        |> Maybe.map (\col -> formatNumber col.typ v)
        |> Maybe.withDefault "no such column"


{-| Every cleaning test runs against one text column keyed "0", which is the key
`cleanRows` writes under. Built through the decoder by `namedCols`, the way the
other fixtures here are, so no test needs a `Col` of its own.
-}
withCleanCol use fallback =
    namedCols [ "c" ] |> Array.get 0 |> Maybe.map use |> Maybe.withDefault fallback


rewritten change values =
    withCleanCol
        (\col -> cellRewrites col change (cleanRows values) |> Tuple.mapBoth (List.map patchOf) (List.map patchOf))
        ( [], [] )


cleanRows values =
    values |> List.map (\v -> Dict.fromList [ ( "0", E.string v ) ]) |> Array.fromList


{-| A cell patch as the row it addresses and the value it writes, which is all
these tests are about. No annotation: `Patch` is not exposed.
-}
patchOf patch =
    ( patch.path |> List.head |> Maybe.andThen (D.decodeValue D.int >> Result.toMaybe) |> Maybe.withDefault -1
    , patch.value |> D.decodeValue D.string |> Result.withDefault "?"
    )


spliceOf patch =
    E.encode 0 patch.value


trashedShelf =
    libraryOf [ ( "table:gone", "gone", False ) ]
        |> Dict.map (\_ v -> { v | trashed = True })


paletteShelf =
    libraryOf
        [ ( "table:countries", "countries", False )
        , ( "table:us-states", "us states", False )
        ]


{-| A four-column table sheet with the named column keys hidden.
-}
hiddenSheet : List String -> Sheet
hiddenSheet keys =
    let
        doc =
            D.decodeString docDecoder
                """{"type":"table","data":[[{"name":"a","type":"text","key":"0"},{"name":"b","type":"text","key":"1"},{"name":"c","type":"text","key":"2"},{"name":"d","type":"text","key":"3"}]]}"""
    in
    { emptySheet | doc = Result.mapError D.errorToString doc, hidden = Set.fromList keys }


dateRows : List String -> Array.Array (Dict.Dict String D.Value)
dateRows =
    List.map (\s -> Dict.singleton "d" (E.string s)) >> Array.fromList


boolRows : List E.Value -> Array.Array (Dict.Dict String D.Value)
boolRows =
    List.map (Dict.singleton "b") >> Array.fromList


{-| A chart sheet always resolves to two columns named x and y, whatever the
query underneath it called them, so a test only has to supply the pairs.
-}
chartTable : List ( String, E.Value ) -> Table
chartTable points =
    -- chartPoints reads the rows by key and never looks at the columns, so
    -- naming them here would only be a second place to keep them in step.
    { cols = Array.empty
    , rows =
        points
            |> List.map (\( x, y ) -> Dict.fromList [ ( "x", E.string x ), ( "y", y ) ])
            |> Array.fromList
    }


{-| Columns as the page actually receives them: decoded from a table document,
rather than built by hand against a constructor only this test would need.
-}
namedCols : List String -> Array.Array Col
namedCols names =
    let
        json =
            names
                |> List.indexedMap (\i name -> E.object [ ( "key", E.string (String.fromInt i) ), ( "name", E.string name ), ( "type", E.string "text" ) ])
                |> E.list identity
    in
    case D.decodeValue docDecoder (E.object [ ( "type", E.string "table" ), ( "data", E.list identity [ json ] ) ]) of
        Ok (Tab table) ->
            table.cols

        _ ->
            Array.empty
