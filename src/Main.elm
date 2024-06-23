module Main exposing (Model, Msg(..), Query(..), Rule, main)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom as Dom
import Html as H exposing (Html, text)
import Html.Attributes as A exposing (style)
import Html.Events as A
import Html.Lazy as H
import Json.Encode as E
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
    = NoOp
    | WriteCell Int String
    | CellsWritten Rect
    | CellsSelecting Index
    | CellsSelected Index
    | CellEditing Index String
    | FrameDurationUpdated String
    | RuleReverted Int
    | RuleEditing Int Rule Vector
    | RuleSaved Int
    | RuleNew


type alias Cell =
    String


type Rule
    = HttpFetch String
    | JS String
    | JsonPath String
    | Copy
    | Sum
    | Product
    | Mean
    | Median
    | Max
    | Min
    | Nada


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

    -- TODO: make it super easy to transpose the result! maybe in the rule?
    -- TODO: it's useful to transpose before/after applying the rule in different cases
    , rules : Array ( Maybe ( Query, Rule, Vector ), ( Query, Rule, Vector ) )
    , frameDuration : Int
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
      , sheet = { cols = 10, cells = Array.initialize (10 * 100) (\i -> String.fromInt (i // 10)) }
      , rules =
            Array.map (Tuple.pair Nothing) <|
                Array.fromList <|
                    [ ( Rect ( ( 0, 0 ), ( 3, 4 ) )
                      , JS "TODO"
                      , ( 4, 5 )
                      )
                    , ( Rect ( ( 0, 4 ), ( 3, 8 ) )
                      , Copy
                      , ( 5, 6 )
                      )
                    ]
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


apply : Rule -> (Sheet -> ( Sheet, Cmd Msg ))
apply rule sheet =
    case rule of
        HttpFetch x ->
            -- TODO
            ( { cols = 1, cells = Array.empty }, Cmd.none )

        JS x ->
            -- TODO
            ( { sheet | cells = Array.map (always "TODO!") sheet.cells }, Cmd.none )

        JsonPath x ->
            -- TODO
            ( { cols = 1, cells = Array.empty }, Cmd.none )

        Copy ->
            ( sheet, Cmd.none )

        Sum ->
            ( reduceRows (Array.foldl (String.toFloat >> Maybe.withDefault 0 >> (+)) 0 >> String.fromFloat) sheet, Cmd.none )

        Product ->
            ( reduceRows (Array.foldl (String.toFloat >> Maybe.withDefault 1 >> (*)) 1 >> String.fromFloat) sheet, Cmd.none )

        Mean ->
            ( reduceRows (\xs -> String.fromFloat (List.sum (List.filterMap String.toFloat (Array.toList xs)) / toFloat (Array.length xs))) sheet, Cmd.none )

        Median ->
            ( reduceRows (Array.toList >> List.sort >> Array.fromList >> Array.get (sheet.cols // 2) >> Maybe.withDefault "") sheet, Cmd.none )

        Max ->
            ( reduceRows (Array.toList >> List.maximum >> Maybe.withDefault "") sheet, Cmd.none )

        Min ->
            ( reduceRows (Array.toList >> List.minimum >> Maybe.withDefault "") sheet, Cmd.none )

        Nada ->
            ( { sheet | cells = Array.empty }, Cmd.none )


reduceRows : (Array Cell -> Cell) -> Sheet -> Sheet
reduceRows f { cols, cells } =
    { cols = 1
    , cells =
        case compare cols 1 of
            LT ->
                Array.empty

            EQ ->
                Array.fromList [ f cells ]

            GT ->
                Array.initialize (Array.length cells // cols) (\i -> f (Array.slice (i * cols) ((i + 1) * cols) cells))
    }


toFlatIndex : Int -> Index -> Int
toFlatIndex cols ( x, y ) =
    y * cols + x


fromFlatIndex : Int -> Int -> Index
fromFlatIndex cols i =
    ( modBy cols i, i // cols )


intersect : Rect -> Rect -> Rect
intersect ( ( xaa, yaa ), ( xab, yab ) ) ( ( xba, yba ), ( xbb, ybb ) ) =
    ( ( max xaa xba, max yaa yba ), ( min xab xbb, min yab ybb ) )


transpose : Sheet -> Sheet
transpose { cols, cells } =
    let
        len =
            Array.length cells
    in
    { cols = len // cols
    , cells = Array.initialize len (\i -> Array.get (i * cols |> modBy len) cells |> Maybe.withDefault "")
    }


overlaps : Rect -> Rect -> Bool
overlaps a b =
    intersect a b |> (\( ( xa, ya ), ( xb, yb ) ) -> xa < xb && ya < yb)


crop : Rect -> Sheet -> Sheet
crop r s =
    let
        ( ( xa, ya ), ( xb, yb ) ) =
            intersect r ( ( 0, 0 ), ( s.cols, Array.length s.cells // s.cols ) )

        ncols =
            max 0 (xb - xa)

        nrows =
            max 0 (yb - ya)
    in
    { cols = ncols, cells = Array.initialize (ncols * nrows) (\i -> s.cells |> Array.get ((ya + i // ncols) * s.cols + xa + modBy (max 1 ncols) i) |> Maybe.withDefault "") }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ sheet } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FrameDurationUpdated n ->
            ( { model | frameDuration = Maybe.withDefault model.frameDuration <| String.toInt n }, Cmd.none )

        WriteCell i x ->
            ( { model
                | sheet = { sheet | cells = Array.set i x sheet.cells }
                , editing = ( ( -1, -1 ), "" )
              }
            , Task.perform CellsWritten (Task.succeed ( fromFlatIndex sheet.cols i, Tuple.mapBoth ((+) 1) ((+) 1) (fromFlatIndex sheet.cols i) ))
            )

        CellsWritten r ->
            let
                ( subsheets, subtasks ) =
                    List.foldr (\( i, ( s, c ) ) ( a, b ) -> ( ( i, s ) :: a, c :: b )) ( [], [] ) <|
                        List.concatMap
                            (\( query, rule, move ) ->
                                case query of
                                    Rect q ->
                                        iif (overlaps r q) [ ( translate move (Tuple.first q), apply rule (crop q model.sheet) ) ] []

                                    -- TODO
                                    Pattern () ->
                                        []
                            )
                        <|
                            Array.toList <|
                                Array.map Tuple.second model.rules
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
                List.concat
                    [ subtasks
                    , List.map (Task.perform CellsWritten << (\x -> Task.andThen (always (Task.succeed x)) (Process.sleep (toFloat model.frameDuration)))) <|
                        List.map (\( ( x, y ), { cols, cells } ) -> ( ( x, y ), ( x + modBy cols (Array.length cells), y + Array.length cells // cols * sheet.cols ) )) <|
                            subsheets
                    ]
            )

        CellsSelecting a ->
            ( { model | selected = Rect ( a, a ) }, Cmd.none )

        CellsSelected b ->
            case model.selected of
                Rect ( a, _ ) ->
                    ( { model | selected = Rect ( a, inc b ) }, Cmd.none )

                Pattern _ ->
                    ( { model | selected = Rect ( b, b ) }, Cmd.none )

        CellEditing i s ->
            ( { model | editing = ( i, s ) }, Task.attempt (\_ -> NoOp) (Dom.focus "edit") )

        RuleReverted i ->
            -- TODO
            ( model, Cmd.none )

        RuleEditing i rule vec ->
            -- TODO
            ( model, Cmd.none )

        RuleSaved i ->
            -- TODO
            ( model, Cmd.none )

        RuleNew ->
            -- TODO: the default vec should always be the selected width
            ( { model | rules = model.rules |> Array.push ( Just ( model.selected, Nada, ( -1, 0 ) ), ( model.selected, Nada, ( -1, 0 ) ) ) }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "scrapsheets"
    , body =
        -- TODO: rules : List Rule
        [ H.node "style" [] [ text ".cell:hover { background: #eee; }" ]
        , H.div
            [ style "display" "grid"
            , style "grid-template-columns" "2fr 1fr"
            , style "user-select" "none"
            , style "-webkit-user-select" "none"
            , style "cursor" "pointer"
            ]
            [ -- TODO: "resize table" settings
              H.lazy
                (\m ->
                    let
                        sources =
                            m.rules |> Array.map (\( _, ( q, _, _ ) ) -> q) |> Array.toList

                        sinks =
                            m.rules
                                |> Array.map
                                    (\( _, ( q, _, mov ) ) ->
                                        case q of
                                            Rect x ->
                                                x |> Tuple.mapBoth (translate mov) (translate mov) |> Rect

                                            Pattern x ->
                                                Pattern x
                                    )
                                |> Array.toList
                    in
                    H.form
                        [ style "display" "grid"
                        , style "grid-template-columns" (String.repeat m.sheet.cols "1fr ")
                        , A.onSubmit (WriteCell (toFlatIndex m.sheet.cols (Tuple.first m.editing)) (Tuple.second m.editing))
                        ]
                    <|
                        Array.toList <|
                            Array.indexedMap
                                (\i cell ->
                                    let
                                        index =
                                            fromFlatIndex m.sheet.cols i
                                    in
                                    H.div
                                        [ A.class "cell"
                                        , style "background" (iif (match m.selected index cell) "#ddd" "")
                                        , style "color" (iif (List.any (\source -> match source index cell) sources) "blue" "")
                                        , style "font-weight" (iif (List.any (\sink -> match sink index cell) sinks) "700" "")
                                        , A.onClick (CellEditing index cell)
                                        , A.onMouseDown (CellsSelecting index)
                                        , A.onMouseUp (CellsSelected index)
                                        ]
                                        [ if index == Tuple.first m.editing then
                                            H.input
                                                [ A.id "edit"
                                                , style "width" "100%"
                                                , A.value (Tuple.second m.editing)
                                                , A.onInput (CellEditing index)
                                                ]
                                                []

                                          else
                                            text cell
                                        ]
                                )
                            <|
                                m.sheet.cells
                )
                model
            , H.div [ style "display" "flex", style "flex-direction" "column", style "gap" "1rem" ] <|
                List.concat
                    [ List.map
                        (\( query, rule, move ) ->
                            H.div [ style "display" "grid", style "grid-template-columns" "auto auto auto" ]
                                [ H.span []
                                    [ case query of
                                        Rect ( ( xa, ya ), ( xb, yb ) ) ->
                                            text <| String.join "" [ "(", String.fromInt xa, ",", String.fromInt ya, ")-->(", String.fromInt xb, ",", String.fromInt yb, ")" ]

                                        Pattern _ ->
                                            text "TODO"
                                    ]
                                , H.span []
                                    [ case rule of
                                        HttpFetch x ->
                                            text x

                                        JS x ->
                                            text x

                                        JsonPath x ->
                                            text x

                                        Copy ->
                                            text "copy"

                                        Sum ->
                                            text "sum"

                                        Product ->
                                            text "product"

                                        Mean ->
                                            text "mean"

                                        Median ->
                                            text "median"

                                        Max ->
                                            text "max"

                                        Min ->
                                            text "min"

                                        Nada ->
                                            text "nothing"
                                    ]
                                , H.span []
                                    [ case move of
                                        ( x, y ) ->
                                            text <| String.join "" [ "(", String.fromInt x, ",", String.fromInt y, ")" ]
                                    ]
                                ]
                        )
                      <|
                        Array.toList <|
                            Array.map Tuple.second model.rules
                    , [ H.div []
                            [ H.button [ A.onClick RuleNew ] [ text "+ New Rule" ]
                            ]
                      , H.div []
                            [ H.span [] [ text (String.fromInt model.frameDuration) ]
                            , H.input
                                [ A.onInput FrameDurationUpdated
                                , A.type_ "range"
                                , A.min "10"
                                , A.max "1000"
                                , A.value (String.fromInt model.frameDuration)
                                ]
                                []
                            ]
                      ]
                    ]
            ]
        ]
    }
