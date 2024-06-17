module Main exposing (Model, Msg(..), Query(..), Rule, main)

import Array exposing (Array)
import Browser exposing (Document)
import Html as H exposing (Html, text)
import Html.Attributes as A exposing (style)
import Html.Events as A
import Html.Lazy as H
import Process
import Regex exposing (Regex)
import Task


iif : Bool -> a -> a -> a
iif c t f =
    if c then
        t

    else
        f


type Msg
    = WriteCell Int String
    | CellsWritten Rect
    | CellsSelecting Index
    | CellsSelected Index


type alias Cell =
    String


type alias Rule =
    { query : Query
    , rule : Sheet -> Sheet
    , def : RuleDef
    , move : Vector
    }


type RuleDef
    = HttpFetch String
    | JS String
    | JsonPath String
    | Copy
    | Sum
    | Product
    | Mean
    | Max
    | Min


type alias Sheet =
    { cols : Int
    , cells : Array Cell
    }


type alias Index =
    ( Int, Int )


type alias Vector =
    ( Int, Int )


type Query
    = Rect Rect
    | Pattern ()


type alias Rect =
    ( Index, Index )


type alias Model =
    { selected : Query
    , editing : ( Index, Cell )
    , sheet : Sheet
    , rules : List Rule
    , newRule : ( Int, RuleDef )
    , frameDuration : Float
    }


main : Platform.Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = Rect ( ( 8, 7 ), ( 12, 13 ) )
      , editing = ( ( -1, -1 ), "" )
      , sheet = { cols = 10, cells = Array.initialize (10 * 100) (\i -> String.fromInt (modBy 10 i) ++ "," ++ String.fromInt (i // 10)) }
      , rules =
            [ { query = Rect ( ( 0, 0 ), ( 3, 4 ) )
              , rule = \sheet -> { sheet | cells = Array.map (\x -> "(" ++ x ++ ")") sheet.cells }
              , def = JS "TODO"
              , move = ( 3, 4 )
              }
            ]
      , newRule = ( -1, JS "" )
      , frameDuration = 100
      }
    , Task.perform CellsWritten (Task.succeed ( ( 0, 0 ), ( 100, 100 ) ))
    )


inc : Index -> Index
inc =
    translate ( 1, 1 )


translate : Vector -> Index -> Index
translate ( x, y ) ( x_0, y_0 ) =
    ( x_0 + x, y_0 + y )


match : Query -> Index -> Cell -> Bool
match q ( xi, yi ) s =
    case q of
        Rect ( ( xa, ya ), ( xb, yb ) ) ->
            xa <= xi && xi < xb && ya <= yi && yi < yb

        -- TODO
        Pattern _ ->
            False


toFlatIndex : Int -> Index -> Int
toFlatIndex cols ( x, y ) =
    y * cols + x


fromFlatIndex : Int -> Int -> Index
fromFlatIndex cols i =
    ( modBy cols i, i // cols )


intersect : Rect -> Rect -> Rect
intersect ( ( xaa, yaa ), ( xab, yab ) ) ( ( xba, yba ), ( xbb, ybb ) ) =
    ( ( max xaa xba, max yaa yba ), ( min xab xbb, min yab ybb ) )


overlaps : Rect -> Rect -> Bool
overlaps a b =
    intersect a b |> (\( ( xa, ya ), ( xb, yb ) ) -> xa < xb && ya < yb)


crop : Rect -> Sheet -> Sheet
crop r s =
    let
        ( ( xa, ya ), ( xb, yb ) ) =
            intersect r ( ( 0, 0 ), ( s.cols - 1, Array.length s.cells // s.cols - 1 ) )

        cols =
            max 0 (xb - xa)

        rows =
            max 0 (yb - ya)
    in
    { cols = cols, cells = Array.initialize (cols * rows) (\i -> s.cells |> Array.get (ya + (i // cols) * s.cols + xa + modBy (max 1 cols) i) |> Maybe.withDefault "") }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ sheet } as model) =
    case msg of
        WriteCell i x ->
            ( { model | sheet = { sheet | cells = Array.set i x sheet.cells } }
            , Task.perform CellsWritten (Task.succeed ( fromFlatIndex sheet.cols i, Tuple.mapBoth ((+) 1) ((+) 1) (fromFlatIndex sheet.cols i) ))
            )

        CellsWritten r ->
            let
                subsheets : List ( Index, Sheet )
                subsheets =
                    List.concatMap
                        (\{ query, rule, move } ->
                            case query of
                                Rect q ->
                                    iif (overlaps r q) [ ( translate move (Tuple.first q), rule (crop q model.sheet) ) ] []

                                -- TODO
                                Pattern () ->
                                    []
                        )
                        model.rules
            in
            ( { model
                | sheet =
                    { sheet
                        | cells =
                            List.foldl
                                (\( ( x, y ), { cols, cells } ) cells_ ->
                                    Array.foldl
                                        (\( i, cell ) ->
                                            if x + modBy cols i > sheet.cols then
                                                identity

                                            else
                                                Array.set (toFlatIndex sheet.cols ( x, y ) + (i // cols) * sheet.cols + modBy cols i) cell
                                        )
                                        cells_
                                        (Array.indexedMap Tuple.pair cells)
                                )
                                sheet.cells
                                subsheets
                    }
              }
            , Cmd.batch <|
                List.map (Task.perform CellsWritten << (\x -> Task.andThen (always (Task.succeed x)) (Process.sleep model.frameDuration))) <|
                    List.map (\( ( x, y ), { cols, cells } ) -> ( ( x, y ), ( x + modBy cols (Array.length cells), y + Array.length cells // cols * sheet.cols ) )) <|
                        subsheets
            )

        CellsSelecting a ->
            ( { model | selected = Rect ( a, a ) }, Cmd.none )

        CellsSelected b ->
            case model.selected of
                Rect ( a, _ ) ->
                    ( { model | selected = Rect ( a, inc b ) }, Cmd.none )

                Pattern _ ->
                    ( { model | selected = Rect ( b, b ) }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "scrapsheets"
    , body =
        -- { selected : Query
        -- , editing : ( Index, Cell )
        -- , sheet : Sheet
        -- , rules : List Rule
        -- , newRule : ( Int, RuleDef )
        -- , frameDuration : Float
        -- }
        [ H.node "style" [] [ text ".cell:hover { background: #eee;  }" ]
        , H.div
            [ style "display" "grid"
            , style "grid-template-columns" "2fr 1fr"
            , style "user-select" "none"
            , style "-webkit-user-select" "none"
            , style "cursor" "pointer"
            ]
            [ H.lazy
                (H.div
                    [ style "display" "grid"
                    , style "grid-template-columns" (String.repeat model.sheet.cols "1fr ")
                    ]
                    << Array.toList
                    << Array.indexedMap
                        (\i cell ->
                            H.div
                                [ A.class "cell"
                                , style "background" (iif (match model.selected (fromFlatIndex model.sheet.cols i) cell) "#ddd" "")
                                , A.onClick (WriteCell i (cell ++ "!"))
                                , A.onMouseDown (CellsSelecting (fromFlatIndex model.sheet.cols i))
                                , A.onMouseUp (CellsSelected (fromFlatIndex model.sheet.cols i))
                                ]
                                [ text cell ]
                        )
                )
                model.sheet.cells
            , H.div [ style "display" "flex", style "flex-direction" "column" ] <|
                List.map (\rule -> H.div [] [ text (Debug.toString rule) ]) <|
                    model.rules
            ]
        ]
    }
