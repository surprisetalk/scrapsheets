module Main exposing (Model, Msg(..), Query(..), Rule, main)

import Array exposing (Array)
import Browser exposing (Document)
import Html as H exposing (Html, text)
import Html.Attributes as A exposing (style)
import Html.Events as A
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
    { selected : Maybe Query
    , editing : Maybe ( Index, Cell )
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
    ( { selected = Nothing
      , editing = Nothing
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


translate : Vector -> Index -> Index
translate ( x, y ) ( x_0, y_0 ) =
    ( x_0 + x, y_0 + y )


toFlatIndex : Int -> Index -> Int
toFlatIndex cols ( x, y ) =
    y * cols + x


fromFlatIndex : Int -> Int -> Index
fromFlatIndex cols i =
    ( i // cols, modBy cols i )


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


view : Model -> Document Msg
view model =
    { title = "scrapsheets"
    , body =
        [ H.div [ style "display" "grid", style "grid-template-columns" "2fr 1fr" ]
            [ H.div [ style "display" "grid", style "grid-template-columns" (String.repeat model.sheet.cols "1fr ") ] <|
                Array.toList <|
                    Array.indexedMap (\i cell -> H.tr [] [ H.td [ A.onClick (WriteCell i (cell ++ "!")) ] [ text cell ] ]) <|
                        model.sheet.cells
            , H.div [ style "display" "flex", style "flex-direction" "column" ] <|
                List.map (\rule -> H.div [] [ text (Debug.toString rule) ]) <|
                    model.rules
            ]
        ]
    }
