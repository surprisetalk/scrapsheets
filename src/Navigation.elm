module Navigation exposing
    ( Index
    , Rect
    , SortOrder(..)
    , TableBounds
    , clampIndex
    , expandSelection
    , moveSelection
    , nextSortOrder
    , normalizeRect
    , rect
    , rectToIndices
    , selectAll
    , sortWithOrder
    , xy
    )

{-| Navigation and sorting utilities for the spreadsheet grid.
-}


type alias Index =
    { x : Int, y : Int }


type alias Rect =
    { a : Index, b : Index }


type alias TableBounds =
    { maxX : Int
    , maxY : Int
    }


type SortOrder
    = Ascending
    | Descending


xy : Int -> Int -> Index
xy x y =
    Index x y


rect : Int -> Int -> Int -> Int -> Rect
rect ax ay bx by =
    Rect (xy ax ay) (xy bx by)


{-| Clamp an index to valid table bounds.
x is clamped to [0, maxX], y is clamped to [1, maxY] (row 0 is header).
-}
clampIndex : TableBounds -> Index -> Index
clampIndex bounds idx =
    { x = clamp 0 bounds.maxX idx.x
    , y = clamp 1 bounds.maxY idx.y
    }


{-| Move selection by delta, clamping to bounds.
Returns new selection rect with both corners at the same position.
-}
moveSelection : TableBounds -> Int -> Int -> Index -> Rect
moveSelection bounds dx dy current =
    let
        newIdx =
            clampIndex bounds { x = current.x + dx, y = current.y + dy }
    in
    Rect newIdx newIdx


{-| Cycle through sort orders: Nothing -> Ascending -> Descending -> Nothing
-}
nextSortOrder : Maybe SortOrder -> Maybe SortOrder
nextSortOrder current =
    case current of
        Nothing ->
            Just Ascending

        Just Ascending ->
            Just Descending

        Just Descending ->
            Nothing


{-| Sort a list with a comparison function, respecting sort order.
-}
sortWithOrder : SortOrder -> (a -> a -> Order) -> List a -> List a
sortWithOrder order compare list =
    let
        sorted =
            List.sortWith compare list
    in
    case order of
        Ascending ->
            sorted

        Descending ->
            List.reverse sorted


{-| Normalize a rect so that a is the top-left and b is the bottom-right.
-}
normalizeRect : Rect -> Rect
normalizeRect r =
    { a =
        { x = min r.a.x r.b.x
        , y = min r.a.y r.b.y
        }
    , b =
        { x = max r.a.x r.b.x
        , y = max r.a.y r.b.y
        }
    }


{-| Expand selection by delta from the anchor point.
The anchor (a) stays fixed, and corner b moves.
-}
expandSelection : TableBounds -> Int -> Int -> Rect -> Rect
expandSelection bounds dx dy sel =
    let
        newB =
            clampIndex bounds { x = sel.b.x + dx, y = sel.b.y + dy }
    in
    { sel | b = newB }


{-| Select entire table (all cells).
-}
selectAll : TableBounds -> Rect
selectAll bounds =
    Rect (xy 0 1) (xy bounds.maxX bounds.maxY)


{-| Convert a selection rect to a list of indices (row, column pairs).
Returns list of (y, x) pairs for each cell in the selection.
-}
rectToIndices : Rect -> List Index
rectToIndices r =
    let
        normalized =
            normalizeRect r

        xs =
            List.range normalized.a.x normalized.b.x

        ys =
            List.range normalized.a.y normalized.b.y
    in
    List.concatMap (\y -> List.map (\x -> xy x y) xs) ys
