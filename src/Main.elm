port module Main exposing (Model, Msg(..), Query(..), Rule, main)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events as Browser
import Html as H exposing (Html, text)
import Html.Attributes as A exposing (style)
import Html.Events as A
import Html.Lazy as H
import Json.Decode as D
import Json.Encode as E
import Process
import Regex exposing (Regex)
import Task


port clipboardApply : { code : String, data : Sheet } -> Cmd msg


port clipboardResultReceived : (Sheet -> msg) -> Sub msg


port sheetApply : { code : String, data : Sheet, dest : Index } -> Cmd msg


port sheetResultReceived : ({ data : Sheet, dest : Index } -> msg) -> Sub msg


iif : Bool -> a -> a -> a
iif c t f =
    if c then
        t

    else
        f


type Msg
    = NoOp
    | WriteCell Int String
    | CellsWritten Query
    | CellsSelecting Index
    | CellsSelected Index
    | CellEditing Index String
    | SearchEditing String
    | FrameDurationUpdated String
    | RuleReverted Int
    | RuleEditing Int Rule Vector
    | RuleSaved Int
    | RuleNew
    | KeyPressed Bool String
    | ClipboardCodeEdited String
    | ClipboardResultReceived Sheet
    | SheetResultReceived { data : Sheet, dest : Index }


type alias Cell =
    String


type alias Rule =
    String


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
    | Pattern String


type alias Rect =
    ( Index, Index )


type alias Model =
    { selected : Query
    , editing : ( Index, Cell )
    , sheet : Sheet
    , clipboard : { data : Sheet, code : String, result : Result String Sheet }
    , rules : Array ( Maybe ( Query, Rule, Vector ), ( Query, Rule, Vector ) )
    , frameDuration : Int
    , isCtrlKey : Bool
    }


main : Platform.Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.onKeyDown (D.field "key" D.string |> D.map (KeyPressed True))
                    , Browser.onKeyUp (D.field "key" D.string |> D.map (KeyPressed False))
                    , clipboardResultReceived ClipboardResultReceived
                    , sheetResultReceived SheetResultReceived
                    ]
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = Rect ( ( 8, 7 ), ( 12, 13 ) )
      , editing = ( ( -1, -1 ), "" )
      , sheet = { cols = 10, cells = Array.initialize (10 * 100) (\i -> String.fromInt (i // 10)) }
      , clipboard = emptyClipboard
      , rules =
            Array.map (Tuple.pair Nothing) <|
                Array.fromList <|
                    [ ( Rect ( ( 0, 0 ), ( 3, 4 ) )
                      , "msheet(rrows(sum),mcells(x=>'+'+x))"
                      , ( 4, 5 )
                      )
                    , ( Rect ( ( 0, 5 ), ( 3, 9 ) )
                      , "msheet(squares,wrap(5))"
                      , ( 5, 7 )
                      )
                    , ( Pattern "HELLO"
                      , "_ => sheet(1, ['GOODBYE'])"
                      , ( 0, 0 )
                      )
                    ]
      , frameDuration = 100
      , isCtrlKey = False
      }
    , Task.perform CellsWritten (Task.succeed (Rect ( ( 0, 0 ), ( 100, 100 ) )))
    )


emptyClipboard =
    { data = { cols = 1, cells = Array.empty }, code = "", result = Ok { cols = 1, cells = Array.empty } }


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

        Pattern x ->
            String.startsWith x s


apply : ( Query, Rule, Vector ) -> (Sheet -> ( Sheet, Cmd Msg ))
apply ( q, r, v ) sheet =
    -- TODO: In the future, we should try to move everything from async to sync when possible.
    case q of
        Rect ( a, _ ) ->
            ( { cols = 1, cells = Array.empty }, sheetApply { code = r, data = sheet, dest = translate v a } )

        Pattern x ->
            ( { cols = 1, cells = Array.empty }, sheet.cells |> Array.indexedMap (\i c -> iif (String.startsWith x c) (sheetApply { code = r, data = crop ( fromFlatIndex sheet.cols i, inc (fromFlatIndex sheet.cols i) ) sheet, dest = translate v (fromFlatIndex sheet.cols i) }) Cmd.none) |> Array.toList |> Cmd.batch )


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
    intersect a b |> isArea


isArea : Rect -> Bool
isArea ( ( xa, ya ), ( xb, yb ) ) =
    xa < xb && ya < yb


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


write sheet ( ( x, y ), { cols, cells } ) cells_ =
    Array.foldl
        (\( i, cell ) ->
            if x + modBy cols i >= sheet.cols then
                identity

            else
                Array.set (toFlatIndex sheet.cols ( x, y ) + (i // cols) * sheet.cols + modBy cols i) cell
        )
        cells_
        (Array.indexedMap Tuple.pair cells)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ sheet, clipboard } as model) =
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
            , Task.perform CellsWritten (Task.succeed (Rect ( fromFlatIndex sheet.cols i, Tuple.mapBoth ((+) 1) ((+) 1) (fromFlatIndex sheet.cols i) )))
            )

        CellsWritten w ->
            let
                ( subsheets, subtasks ) =
                    List.foldr (\( i, ( s, c ) ) ( a, b ) -> ( ( i, s ) :: a, c :: b )) ( [], [] ) <|
                        List.concatMap
                            (\( query, rule, move ) ->
                                case ( w, query ) of
                                    ( Rect r, Rect q ) ->
                                        iif (overlaps r q) [ ( translate move (Tuple.first q), apply ( query, rule, move ) (crop q model.sheet) ) ] []

                                    ( Pattern p1, Pattern p2 ) ->
                                        -- TODO: This is buggy
                                        iif (String.startsWith p2 p1) [ ( ( 0, 0 ), apply ( query, rule, move ) model.sheet ) ] []

                                    ( Rect _, Pattern p ) ->
                                        -- TODO: Inefficient to search everything...
                                        [ ( ( 0, 0 ), apply ( query, rule, move ) model.sheet ) ]

                                    ( Pattern p, Rect r ) ->
                                        -- TODO
                                        []
                            )
                        <|
                            Array.toList <|
                                Array.map Tuple.second model.rules
            in
            ( { model | sheet = { sheet | cells = List.foldl (write sheet) sheet.cells subsheets } }
            , Cmd.batch <|
                List.concat
                    [ subtasks
                    , List.map (Task.perform CellsWritten << (\x -> Task.map (always x) (Process.sleep (toFloat model.frameDuration)))) <|
                        List.map (\( ( x, y ), { cols, cells } ) -> Rect ( ( x, y ), ( x + modBy cols (Array.length cells), y + Array.length cells // cols * sheet.cols ) )) <|
                            List.filter (\( _, { cells } ) -> Array.length cells > 0) <|
                                subsheets
                    ]
            )

        CellsSelecting a ->
            ( { model | selected = Rect ( a, a ), editing = iif (a == Tuple.first model.editing) model.editing ( ( -1, -1 ), "" ) }, Cmd.none )

        CellsSelected b ->
            case model.selected of
                Rect ( a, _ ) ->
                    ( { model | selected = Rect ( a, inc b ) }, Cmd.none )

                Pattern _ ->
                    ( { model | selected = Rect ( b, b ) }, Cmd.none )

        CellEditing i s ->
            ( { model | editing = ( i, s ) }, Task.attempt (\_ -> NoOp) (Dom.focus "edit") )

        SearchEditing p ->
            ( { model | selected = Pattern p }, Cmd.none )

        RuleReverted i ->
            -- TODO
            ( model, Cmd.none )

        RuleEditing i rule vec ->
            case Array.get i model.rules of
                Just ( Just ( q, _, _ ), r ) ->
                    ( { model | rules = model.rules |> Array.set i ( Just ( q, rule, vec ), r ) }, Cmd.none )

                Just ( Nothing, ( q, r, m ) ) ->
                    ( { model | selected = q, rules = model.rules |> Array.set i ( Just ( q, rule, vec ), ( q, r, m ) ) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        RuleSaved i ->
            case Array.get i model.rules of
                Just ( Just ( q, r, v ), _ ) ->
                    ( { model | rules = model.rules |> Array.set i ( Nothing, ( q, r, v ) ) }, Task.perform CellsWritten (Task.succeed q) )

                Just ( Nothing, ( q, r, v ) ) ->
                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        RuleNew ->
            let
                move =
                    case model.selected of
                        Rect ( ( xa, _ ), ( xb, _ ) ) ->
                            ( max 1 (xb - xa), 0 )

                        Pattern x ->
                            ( 0, 0 )
            in
            ( { model | rules = model.rules |> Array.push ( Just ( model.selected, "identity", move ), ( model.selected, "_=>sheet(1,[])", move ) ) }, Cmd.none )

        KeyPressed isCtrlKey "Control" ->
            ( { model | isCtrlKey = isCtrlKey }, Cmd.none )

        KeyPressed True "Escape" ->
            ( { model | clipboard = emptyClipboard }, Cmd.none )

        KeyPressed True "c" ->
            if not model.isCtrlKey then
                ( model, Cmd.none )

            else
                case model.selected of
                    Rect r ->
                        ( { model | clipboard = { data = crop r model.sheet, code = "", result = Ok (crop r model.sheet) } }, Cmd.none )

                    Pattern p ->
                        ( { model | clipboard = { data = { cols = 1, cells = Array.filter (String.startsWith p) model.sheet.cells }, code = "", result = Ok { cols = 1, cells = Array.filter (String.startsWith p) model.sheet.cells } } }, Cmd.none )

        KeyPressed True "v" ->
            if not model.isCtrlKey then
                ( model, Cmd.none )

            else
                case model.clipboard.result of
                    Err _ ->
                        ( { model | clipboard = emptyClipboard }, Cmd.none )

                    Ok sheet_ ->
                        case model.selected of
                            Rect ( a, b ) ->
                                ( { model
                                    | clipboard = emptyClipboard
                                    , sheet = { sheet | cells = write sheet ( a, crop ( ( 0, 0 ), translate (Tuple.mapBoth ((-) 0) ((-) 0) a) b ) sheet_ ) model.sheet.cells }
                                  }
                                  -- TODO: get the overlap with the subsheet
                                , Task.perform CellsWritten (Task.succeed (Rect ( a, b )))
                                )

                            Pattern _ ->
                                -- TODO
                                ( { model | clipboard = emptyClipboard }, Cmd.none )

        KeyPressed True "f" ->
            if not model.isCtrlKey then
                ( model, Cmd.none )

            else
                case model.selected of
                    Rect _ ->
                        ( { model | selected = Pattern "" }, Task.attempt (\_ -> NoOp) (Dom.focus "search") )

                    Pattern "" ->
                        ( { model | selected = Rect ( ( 0, 0 ), ( 0, 0 ) ) }, Cmd.none )

                    Pattern _ ->
                        ( { model | selected = Pattern "" }, Task.attempt (\_ -> NoOp) (Dom.focus "search") )

        KeyPressed True "j" ->
            iif (not model.isCtrlKey) ( model, Cmd.none ) ( { model | rules = model.rules |> Array.map (Tuple.mapFirst (Maybe.map (\( q, r, ( x, y ) ) -> ( q, r, ( x, y + 1 ) )))) }, Cmd.none )

        KeyPressed True "k" ->
            iif (not model.isCtrlKey) ( model, Cmd.none ) ( { model | rules = model.rules |> Array.map (Tuple.mapFirst (Maybe.map (\( q, r, ( x, y ) ) -> ( q, r, ( x, y - 1 ) )))) }, Cmd.none )

        KeyPressed True "h" ->
            iif (not model.isCtrlKey) ( model, Cmd.none ) ( { model | rules = model.rules |> Array.map (Tuple.mapFirst (Maybe.map (\( q, r, ( x, y ) ) -> ( q, r, ( x - 1, y ) )))) }, Cmd.none )

        KeyPressed True "l" ->
            iif (not model.isCtrlKey) ( model, Cmd.none ) ( { model | rules = model.rules |> Array.map (Tuple.mapFirst (Maybe.map (\( q, r, ( x, y ) ) -> ( q, r, ( x + 1, y ) )))) }, Cmd.none )

        KeyPressed _ _ ->
            ( model, Cmd.none )

        ClipboardCodeEdited "" ->
            ( { model | clipboard = { clipboard | code = "", result = Ok clipboard.data } }, Cmd.none )

        ClipboardCodeEdited code ->
            ( { model | clipboard = { clipboard | code = code, result = Err "" } }, clipboardApply { code = code, data = clipboard.data } )

        ClipboardResultReceived result ->
            ( { model | clipboard = { clipboard | result = Ok result } }, Cmd.none )

        SheetResultReceived { data, dest } ->
            let
                svec { cols, cells } =
                    fromFlatIndex cols (Array.length cells - 1)
            in
            ( { model | sheet = { sheet | cells = write sheet ( dest, data ) model.sheet.cells } }
            , Task.perform CellsWritten (Task.map (always (Rect ( dest, translate dest (svec data) ))) (Process.sleep (toFloat model.frameDuration)))
            )


view : Model -> Document Msg
view model =
    { title = "scrapsheets"
    , body =
        [ H.node "style"
            []
            [ text ".cell:hover { background: #eee; }"
            , text "* { box-sizing: border-box; }"
            ]
        , H.div
            [ style "display" "grid"
            , style "grid-template-columns" "2fr 1fr"
            , style "user-select" "none"
            , style "-webkit-user-select" "none"
            , style "cursor" "pointer"
            , style "z-index" "0"
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
                                        , style "border" "1px solid #eee"
                                        , style "position" "relative"
                                        ]
                                        [ if index == Tuple.first m.editing then
                                            H.input
                                                [ A.id "edit"
                                                , style "width" "100%"
                                                , style "padding" "0"
                                                , A.value (Tuple.second m.editing)
                                                , A.onInput (CellEditing index)
                                                ]
                                                []

                                          else
                                            H.div
                                                [ style "text-overflow" "ellipsis"
                                                , style "width" "100%"
                                                , style "height" "100%"
                                                , A.onClick (CellEditing index cell)
                                                , A.onMouseDown (CellsSelecting index)
                                                , A.onMouseUp (CellsSelected index)
                                                ]
                                                [ text cell
                                                ]
                                        ]
                                )
                            <|
                                m.sheet.cells
                )
                model
            , H.div [ style "display" "flex", style "flex-direction" "column", style "gap" "1rem" ] <|
                List.concat
                    [ List.concatMap
                        (\( i, isEditing, ( query, rule, move ) ) ->
                            [ H.div
                                (List.concat
                                    [ [ style "display" "grid", style "grid-template-columns" "auto auto auto auto" ]
                                    , iif isEditing [] [ A.onClick (RuleEditing i rule move) ]
                                    ]
                                )
                                [ H.span []
                                    [ case query of
                                        Rect ( ( xa, ya ), ( xb, yb ) ) ->
                                            text <| String.join "" [ "(", String.fromInt xa, ",", String.fromInt ya, ")-->(", String.fromInt xb, ",", String.fromInt yb, ")" ]

                                        Pattern p ->
                                            text ("\"" ++ p ++ "\"")
                                    ]
                                , if isEditing then
                                    H.textarea [ A.onInput (\x -> RuleEditing i x move) ] [ text rule ]

                                  else
                                    H.span [] [ text rule ]
                                , H.span []
                                    [ case move of
                                        ( x, y ) ->
                                            text <| String.join "" [ "(", String.fromInt x, ",", String.fromInt y, ")" ]
                                    ]
                                , if isEditing then
                                    H.button [ A.onClick (RuleSaved i) ] [ text "save" ]

                                  else
                                    text ""
                                ]
                            ]
                        )
                      <|
                        Array.toList <|
                            Array.indexedMap (\i ( a, b ) -> ( i, a /= Nothing, Maybe.withDefault b a )) <|
                                model.rules
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
            , case model.selected of
                Rect _ ->
                    text ""

                Pattern p ->
                    H.input
                        [ A.onInput SearchEditing
                        , A.value p
                        , A.id "search"
                        , style "position" "absolute"
                        , style "bottom" "6rem"
                        , style "right" "2rem"
                        , style "background" "white"
                        , style "border" "1px solid black"
                        , style "box-shadow" "10px 10px rgba(0,0,0,0.5)"
                        ]
                        []
            , let
                clipboard =
                    Result.withDefault model.clipboard.data model.clipboard.result
              in
              if Array.length clipboard.cells > 0 then
                H.div
                    [ style "position" "absolute"
                    , style "bottom" "6rem"
                    , style "right" "2rem"
                    , style "background" "white"
                    , style "border" "1px solid black"
                    , style "display" "flex"
                    , style "flex-direction" "column"
                    , style "box-shadow" "10px 10px rgba(0,0,0,0.5)"
                    , style "padding" "1rem"
                    , style "gap" "1rem"
                    , style "min-width" "50vw"
                    , style "min-height" "33vh"
                    ]
                    [ H.div
                        [ style "display" "grid"
                        , style "grid-template-columns" (String.repeat clipboard.cols "1fr ")
                        ]
                      <|
                        Array.toList <|
                            Array.map (H.div [ style "border" "1px solid black", style "padding" "0 0.25rem" ] << List.singleton << text) <|
                                clipboard.cells
                    , H.textarea
                        [ style "border" (iif (Nothing /= Result.toMaybe model.clipboard.result) "" "1px solid red")
                        , A.onInput ClipboardCodeEdited
                        , A.value model.clipboard.code
                        ]
                        []
                    ]

              else
                text ""
            ]
        ]
    }
