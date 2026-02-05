module TableOpsTest exposing (..)

import Array
import Dict
import Expect
import Json.Encode as E
import TableOps exposing (..)
import Test exposing (..)


{-| Helper to create test rows.
-}
makeRow : List ( String, String ) -> Dict.Dict String E.Value
makeRow pairs =
    pairs
        |> List.map (\( k, v ) -> ( k, E.string v ))
        |> Dict.fromList


{-| Helper to create test columns.
-}
makeCol : String -> String -> { key : String, name : String, typ : String }
makeCol key name =
    { key = key, name = name, typ = "text" }


suite : Test
suite =
    describe "TableOps"
        [ describe "deleteRow"
            [ test "deletes row at index 1" <|
                \_ ->
                    let
                        rows =
                            Array.fromList
                                [ makeRow [ ( "a", "r1" ) ]
                                , makeRow [ ( "a", "r2" ) ]
                                , makeRow [ ( "a", "r3" ) ]
                                ]

                        result =
                            deleteRow 1 rows
                    in
                    Array.length result
                        |> Expect.equal 2
            , test "deletes correct row" <|
                \_ ->
                    let
                        rows =
                            Array.fromList
                                [ makeRow [ ( "a", "r1" ) ]
                                , makeRow [ ( "a", "r2" ) ]
                                , makeRow [ ( "a", "r3" ) ]
                                ]

                        result =
                            deleteRow 2 rows
                    in
                    Array.get 1 result
                        |> Maybe.andThen (Dict.get "a")
                        |> Maybe.andThen
                            (\v ->
                                case E.encode 0 v of
                                    "\"r3\"" ->
                                        Just True

                                    _ ->
                                        Nothing
                            )
                        |> Expect.equal (Just True)
            , test "does nothing for invalid index (0)" <|
                \_ ->
                    let
                        rows =
                            Array.fromList [ makeRow [ ( "a", "r1" ) ] ]
                    in
                    deleteRow 0 rows
                        |> Array.length
                        |> Expect.equal 1
            , test "does nothing for index out of bounds" <|
                \_ ->
                    let
                        rows =
                            Array.fromList [ makeRow [ ( "a", "r1" ) ] ]
                    in
                    deleteRow 5 rows
                        |> Array.length
                        |> Expect.equal 1
            ]
        , describe "deleteRows"
            [ test "deletes multiple rows" <|
                \_ ->
                    let
                        rows =
                            Array.fromList
                                [ makeRow [ ( "a", "r1" ) ]
                                , makeRow [ ( "a", "r2" ) ]
                                , makeRow [ ( "a", "r3" ) ]
                                , makeRow [ ( "a", "r4" ) ]
                                ]

                        result =
                            deleteRows [ 1, 3 ] rows
                    in
                    Array.length result
                        |> Expect.equal 2
            , test "handles unsorted indices" <|
                \_ ->
                    let
                        rows =
                            Array.fromList
                                [ makeRow [ ( "a", "r1" ) ]
                                , makeRow [ ( "a", "r2" ) ]
                                , makeRow [ ( "a", "r3" ) ]
                                ]

                        result =
                            deleteRows [ 3, 1 ] rows
                    in
                    Array.length result
                        |> Expect.equal 1
            ]
        , describe "deleteColumn"
            [ test "removes column from cols array" <|
                \_ ->
                    let
                        table =
                            { cols =
                                Array.fromList
                                    [ makeCol "0" "A"
                                    , makeCol "1" "B"
                                    , makeCol "2" "C"
                                    ]
                            , rows = Array.fromList [ makeRow [ ( "0", "a" ), ( "1", "b" ), ( "2", "c" ) ] ]
                            }

                        result =
                            deleteColumn 1 table
                    in
                    Array.length result.cols
                        |> Expect.equal 2
            , test "removes column data from rows" <|
                \_ ->
                    let
                        table =
                            { cols =
                                Array.fromList
                                    [ makeCol "0" "A"
                                    , makeCol "1" "B"
                                    ]
                            , rows = Array.fromList [ makeRow [ ( "0", "a" ), ( "1", "b" ) ] ]
                            }

                        result =
                            deleteColumn 1 table
                    in
                    Array.get 0 result.rows
                        |> Maybe.map Dict.keys
                        |> Expect.equal (Just [ "0" ])
            , test "does nothing for invalid index" <|
                \_ ->
                    let
                        table =
                            { cols = Array.fromList [ makeCol "0" "A" ]
                            , rows = Array.fromList [ makeRow [ ( "0", "a" ) ] ]
                            }
                    in
                    deleteColumn 5 table
                        |> .cols
                        |> Array.length
                        |> Expect.equal 1
            ]
        , describe "insertRowAt"
            [ test "inserts row at beginning" <|
                \_ ->
                    let
                        rows =
                            Array.fromList [ makeRow [ ( "a", "r1" ) ] ]

                        result =
                            insertRowAt 1 rows
                    in
                    Array.length result
                        |> Expect.equal 2
            , test "inserts empty row" <|
                \_ ->
                    let
                        rows =
                            Array.fromList [ makeRow [ ( "a", "r1" ) ] ]

                        result =
                            insertRowAt 1 rows
                    in
                    Array.get 0 result
                        |> Maybe.map Dict.isEmpty
                        |> Expect.equal (Just True)
            , test "inserts row at end" <|
                \_ ->
                    let
                        rows =
                            Array.fromList [ makeRow [ ( "a", "r1" ) ] ]

                        result =
                            insertRowAt 2 rows
                    in
                    Array.length result
                        |> Expect.equal 2
            ]
        , describe "insertColumnAt"
            [ test "inserts column at index" <|
                \_ ->
                    let
                        table =
                            { cols = Array.fromList [ makeCol "0" "A" ]
                            , rows = Array.fromList [ makeRow [ ( "0", "a" ) ] ]
                            }

                        result =
                            insertColumnAt 0 "New" "text" table
                    in
                    Array.length result.cols
                        |> Expect.equal 2
            , test "new column has correct name" <|
                \_ ->
                    let
                        table =
                            { cols = Array.fromList [ makeCol "0" "A" ]
                            , rows = Array.fromList [ makeRow [ ( "0", "a" ) ] ]
                            }

                        result =
                            insertColumnAt 0 "New" "text" table
                    in
                    Array.get 0 result.cols
                        |> Maybe.map .name
                        |> Expect.equal (Just "New")
            ]
        ]
