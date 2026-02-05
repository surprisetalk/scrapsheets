module NavigationTest exposing (..)

import Expect
import Navigation exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Navigation"
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
