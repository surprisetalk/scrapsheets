module Main exposing (Model, Msg(..), Query(..), Rule, main)

import Array exposing (Array)
import Browser exposing (Document)
import Html as H exposing (Html, text)
import Html.Attributes as A exposing (style)
import Regex exposing (Regex)
import Task
import Time


iif : Bool -> a -> a -> a
iif c t f =
    if c then
        t

    else
        f


type Msg
    = CellsWritten Rect


type alias Cell =
    String


type alias Rule =
    Sheet -> Sheet


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
    , rules : List ( Query, Rule, Vector )
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
      , sheet = { cols = 3, cells = Array.fromList <| [ "a", "b", "c", "d", "e", "f", "g", "h", "i" ] }
      , rules = [ ( Rect ( ( 0, 1 ), ( 1, 1 ) ), \sheet -> { sheet | cells = Array.map String.toUpper sheet.cells }, ( 1, 0 ) ) ]
      }
    , Cmd.none
    )


origin : Rect -> Index
origin query =
    -- TODO
    ( 0, 0 )


translate : Vector -> Index -> Index
translate ( x, y ) ( x_0, y_0 ) =
    ( x_0 + x, y_0 + y )


toFlatIndex : Int -> Index -> Int
toFlatIndex cols ( x, y ) =
    x * cols + y


overlaps : Rect -> Rect -> Bool
overlaps ( ( xaa, yaa ), ( xab, yab ) ) ( ( xba, yba ), ( xbb, ybb ) ) =
    False
        || ((abs (xbb - xba) + abs (xab - xaa)) >= (abs (Maybe.withDefault 0 (List.maximum [ xbb, xba, xab, xaa ])) - abs (Maybe.withDefault 0 (List.minimum [ xbb, xba, xab, xaa ]))))
        || ((abs (ybb - yba) + abs (yab - yaa)) >= (abs (Maybe.withDefault 0 (List.maximum [ ybb, yba, yab, yaa ])) - abs (Maybe.withDefault 0 (List.minimum [ ybb, yba, yab, yaa ]))))


focus : Rect -> Sheet -> Sheet
focus ( ( xa, ya ), ( xb, yb ) ) s =
    { cols = abs (xb - xa), cells = Tuple.second (Array.foldl (\( i, x ) y -> ( i + 1, iif () (Array.push x y) y )) ( 0, Array.empty ) s.cells) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ sheet } as model) =
    case msg of
        CellsWritten r ->
            let
                subsheets : List ( Index, Sheet )
                subsheets =
                    List.concatMap
                        (\( query, rule, vec ) ->
                            case query of
                                Rect q ->
                                    iif (overlaps r q) [ ( translate vec (origin q), rule (focus q model.sheet) ) ] []

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
                List.map (Task.perform CellsWritten << Task.succeed) <|
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
                    Array.map (\cell -> H.tr [] [ H.td [] [ text cell ] ]) <|
                        model.sheet.cells
            , H.div [ style "display" "flex", style "flex-direction" "column" ] <|
                List.map (\rule -> H.div [] [ text (Debug.toString rule) ]) <|
                    model.rules
            ]
        ]
    }
