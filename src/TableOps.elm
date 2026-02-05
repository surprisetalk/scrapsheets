module TableOps exposing
    ( deleteColumn
    , deleteColumns
    , deleteRow
    , deleteRows
    , insertColumnAt
    , insertRowAt
    )

{-| Table operations for manipulating rows and columns.
These are pure functions that work on table data structures.
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Encode as E


{-| Delete a single row at the given index.
Row indices are 1-based (0 is the header).
-}
deleteRow : Int -> Array (Dict String E.Value) -> Array (Dict String E.Value)
deleteRow rowIndex rows =
    if rowIndex < 1 || rowIndex > Array.length rows then
        rows

    else
        Array.append
            (Array.slice 0 (rowIndex - 1) rows)
            (Array.slice rowIndex (Array.length rows) rows)


{-| Delete multiple rows at the given indices.
Processes from highest to lowest index to maintain validity.
-}
deleteRows : List Int -> Array (Dict String E.Value) -> Array (Dict String E.Value)
deleteRows indices rows =
    indices
        |> List.sort
        |> List.reverse
        |> List.foldl deleteRow rows


{-| Delete a single column at the given index.
Returns updated (cols, rows) with the column removed from both.
-}
deleteColumn :
    Int
    -> { cols : Array { key : String, name : String, typ : a }, rows : Array (Dict String E.Value) }
    -> { cols : Array { key : String, name : String, typ : a }, rows : Array (Dict String E.Value) }
deleteColumn colIndex table =
    if colIndex < 0 || colIndex >= Array.length table.cols then
        table

    else
        let
            colToRemove =
                Array.get colIndex table.cols
                    |> Maybe.map .key

            newCols =
                Array.append
                    (Array.slice 0 colIndex table.cols)
                    (Array.slice (colIndex + 1) (Array.length table.cols) table.cols)

            newRows =
                case colToRemove of
                    Just key ->
                        Array.map (Dict.remove key) table.rows

                    Nothing ->
                        table.rows
        in
        { cols = newCols, rows = newRows }


{-| Delete multiple columns at the given indices.
-}
deleteColumns :
    List Int
    -> { cols : Array { key : String, name : String, typ : a }, rows : Array (Dict String E.Value) }
    -> { cols : Array { key : String, name : String, typ : a }, rows : Array (Dict String E.Value) }
deleteColumns indices table =
    indices
        |> List.sort
        |> List.reverse
        |> List.foldl deleteColumn table


{-| Insert an empty row at the given index (1-based).
-}
insertRowAt : Int -> Array (Dict String E.Value) -> Array (Dict String E.Value)
insertRowAt rowIndex rows =
    let
        clamped =
            clamp 1 (Array.length rows + 1) rowIndex
    in
    Array.append
        (Array.push Dict.empty (Array.slice 0 (clamped - 1) rows))
        (Array.slice (clamped - 1) (Array.length rows) rows)


{-| Insert an empty column at the given index.
-}
insertColumnAt :
    Int
    -> String
    -> a
    -> { cols : Array { key : String, name : String, typ : a }, rows : Array (Dict String E.Value) }
    -> { cols : Array { key : String, name : String, typ : a }, rows : Array (Dict String E.Value) }
insertColumnAt colIndex name typ table =
    let
        clamped =
            clamp 0 (Array.length table.cols) colIndex

        newKey =
            String.fromInt (Array.length table.cols)

        newCol =
            { key = newKey, name = name, typ = typ }

        newCols =
            Array.append
                (Array.push newCol (Array.slice 0 clamped table.cols))
                (Array.slice clamped (Array.length table.cols) table.cols)
    in
    { cols = newCols, rows = table.rows }
