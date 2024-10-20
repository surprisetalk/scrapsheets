module Main exposing (main)

---- IMPORTS ------------------------------------------------------------------

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import Dict exposing (Dict)
import Html as H exposing (Html, code, text)
import Html.Attributes as A exposing (..)
import Html.Events as A exposing (..)
import Html.Lazy as H
import Html.Style as S
import Http exposing (stringResolver)
import Json.Decode as D
import Json.Encode as E
import Parser as P exposing ((|.), (|=), Parser)
import Pratt as P
import Regex exposing (Regex)
import Scrapscript as S exposing (Scrap(..))
import Set exposing (Set)
import Task exposing (Task)
import Time exposing (Month(..))
import Url exposing (Url)



---- HELPERS ------------------------------------------------------------------


ls : a -> List a
ls =
    List.singleton


iif : Bool -> a -> a -> a
iif c t f =
    if c then
        t

    else
        f


flip : (a -> b -> c) -> (b -> a -> c)
flip f a b =
    f b a



---- MAIN ---------------------------------------------------------------------


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



---- SUBSCRIPTIONS ------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    model.sheets
        |> Dict.toList
        |> List.filterMap (\( id, s ) -> Result.map (Tuple.pair ( id, s.code )) s.sheet |> Result.toMaybe)
        |> List.filter (\( _, s ) -> s.every > 0)
        |> List.map (\( ( id, code ), s ) -> Time.every (1000 * toFloat s.every) (\_ -> CodeEditing id code))
        |> Sub.batch



---- MODEL --------------------------------------------------------------------


type alias SheetId =
    Int


type alias Input valid data =
    { valid : valid
    , default : data
    , data : data
    }


type ColType
    = Numbers
    | Strings
    | Images
    | Links
    | Buttons
    | Datepickers
    | Colorpickers
    | Checkboxes
    | Sliders
    | Fields
    | Chart
    | Colors
    | Booleans


type alias Col =
    { label : String
    , typ : ColType
    , data : Array String
    }


type alias Sheet =
    { transpose : Bool
    , rows : Int
    , cols : Array Col
    , every : Int
    }


type alias Scrapsheet =
    { watch : Set SheetId
    , code : String
    , sheet : Result String Sheet
    }


type alias Model =
    -- The output of `code` completely determines the shape and content of `sheet`.
    { sheets : Dict SheetId Scrapsheet

    -- the row beneath the sheet opens up with its code and a pointer and tools
    , shelf : List (List SheetId) -- use empty sheets as row/col spacers
    }



---- INIT ---------------------------------------------------------------------


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    let
        sheets : List String
        sheets =
            [ """
              sheet/empty
                |> sheet/col/numbers  "c1" [ 1, 2, 3 ]
              """
            , """
              sheet/empty
                |> sheet/col/numbers  "c1" [ 1, 2, 3 ]
                |> sheet/col/text     "c2" ["apple","pear","mango"]
                |> sheet/col/checkbox "c3" [ true, false, true ]
                |> sheet/col/sliders  "c4" [ 0, 50, 75 ]
              """
            , """
              sheet/limit 2 s1
              """
            , """
              sheet/join "c1" s0 s1
              """
            , """
              s1 
                |> sheet/reduce 
                   [ #sum
                   , #count
                   , #max
                   , #sum 
                   ]
              """

            -- , """
            --   sheet/filter (r1 -> r1.id == 1) s1
            --   """
            -- , """
            --   sheet/join (r1 -> r2 -> r1.id == r2.id) s1 s2
            --   """
            -- , """
            --   sheet/append s1 s2
            --   """
            -- , """
            --   sheet/union (r -> r.id) s1 s2
            --   """
            -- , """
            --   sheet/intersect (r -> r.id) s1 s2
            --   """
            -- , """
            --   sheet/subtract (r -> r.id) s1 s2
            --   """
            -- , """
            --   sheet/group (r -> r.id) s1
            --   """
            -- , """
            --   sheet/sort (r -> r.id) s1
            --   """
            -- , """
            --   sheet/to-columns
            --   """
            -- , """
            --   sheet/from-columns
            --   """
            -- , """
            --   sheet/http "TODO"
            --   """
            -- , """
            --   sheet/websocket "TODO"
            --   """
            -- , """
            --   sheet/every (sheet/http "TODO")
            --   """
            --
            -- basic memory
            , """
              sheet/union s1 s2
              """
            , """
              sheet/union (sheet/limit 1 s1) self 
                |> sheet/limit 10
              """

            -- TODO: Turn this into a Chart.
            -- monte carlo
            , """
              sheet/union
                (sheet/every 5 
                 (sheet/http "https://taylor.town/random")) 
                self 
                |> sheet/limit 10
              """
            , """
              s7 |> sheet/reduce [ #avg ]
              """

            -- game of life
            , """
              self
              |> (game-of-life 20)
              |> sheet/every 1
              """

            -- , """
            --   "[ todo ]"
            --     |> sheet/from-json pair
            --     |> sheet/col/numbers
            --     |> sheet/col/text
            --   """
            -- , """
            --   text/join "" [ "1,a" , "2,b" , "3,c" ]
            --     |> ( sheet/from-csv (a -> b -> { a, b })
            --          |> sheet/csv/col/numbers
            --          |> sheet/csv/col/text
            --        )
            --   """
            ]
    in
    ( { sheets =
            sheets
                |> List.indexedMap
                    (\i code ->
                        Tuple.pair i
                            { watch = Set.singleton -1
                            , code = String.trim code |> String.replace "             " ""
                            , sheet = Ok { transpose = False, rows = 0, cols = Array.empty, every = 0 }
                            }
                    )
                |> Dict.fromList
      , shelf =
            -- List.indexedMap (\i _ -> [ i ]) sheets
            [ [ 1, 0 ]
            , [ 2, 3, 5, 4 ]
            , [ 6, 6, 6 ]
            , [ 7, 8 ]
            , [ 0 ]
            , [ 0 ]
            , [ 9 ]
            ]
      }
    , Task.succeed -1 |> Task.perform SheetEdited
    )



---- MESSAGES -----------------------------------------------------------------


type Msg
    = SheetCreating
    | CodeEditing SheetId String
    | CodeEdited SheetId (Result String Scrapsheet)
    | DataEditing SheetId ( Int, Int ) String
    | SheetEdited SheetId
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest



---- PARSER -------------------------------------------------------------------


type alias Env =
    Dict SheetId (Maybe Sheet)



---- UPDATE -------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SheetCreating ->
            let
                sheetId : Int
                sheetId =
                    model.sheets |> Dict.keys |> List.maximum |> Maybe.withDefault 0 |> (+) 1
            in
            ( { model
                | shelf = [ sheetId ] :: model.shelf
                , sheets =
                    model.sheets
                        |> Dict.insert sheetId
                            { watch = Set.empty
                            , code = "sheet/from-list\n  [\n  ]"
                            , sheet = Ok { transpose = False, rows = 0, cols = Array.empty, every = 0 }
                            }
              }
            , Cmd.batch
                []
            )

        CodeEditing id code ->
            let
                env : Env
                env =
                    model.sheets |> Dict.map (always (.sheet >> Result.toMaybe))

                tupleMap2 : (a -> c -> e) -> (b -> d -> f) -> ( a, b ) -> ( c, d ) -> ( e, f )
                tupleMap2 e f ( a, b ) ( c, d ) =
                    ( e a c, f b d )

                scrapsheet : Scrap -> Task String ( Set SheetId, Result String Sheet )
                scrapsheet ss =
                    case ss of
                        Var "self" ->
                            env
                                -- TODO: Turn input columns into values (e.g. Checkboxes -> Bools)
                                |> Dict.get id
                                |> Result.fromMaybe "TODO: sheet not found"
                                |> Result.map
                                    (Maybe.withDefault
                                        { transpose = False
                                        , rows = 0
                                        , cols = Array.empty
                                        , every = 0
                                        }
                                    )
                                |> Tuple.pair Set.empty
                                |> Task.succeed

                        Var x ->
                            let
                                sheetId =
                                    x |> String.dropLeft 1 |> String.toInt |> Maybe.withDefault -1

                                static : ColType -> ColType
                                static col_ =
                                    case col_ of
                                        -- TODO: Do the rest of the input types.
                                        Checkboxes ->
                                            Booleans

                                        Sliders ->
                                            Numbers

                                        col ->
                                            col
                            in
                            env
                                |> Dict.get sheetId
                                |> Result.fromMaybe "TODO: sheet not found"
                                |> Result.andThen (Result.fromMaybe "TODO: sheet isn't here")
                                |> Result.map (\sheet -> { sheet | cols = sheet.cols |> Array.map (\col -> { col | typ = static col.typ }) })
                                |> Tuple.pair (Set.singleton sheetId)
                                |> Task.succeed

                        Variant "game-of-life" (Record [ ( "l", Int n ), ( "r", sheet ) ]) ->
                            let
                                gol : Sheet -> Sheet
                                gol s =
                                    let
                                        getCell : Int -> Int -> Bool
                                        getCell i j =
                                            "" /= (s.cols |> Array.get i |> Maybe.andThen (.data >> Array.get j) |> Maybe.withDefault (iif (modBy 20 (i * j) == 0) "✓" ""))
                                    in
                                    { s
                                        | rows = n
                                        , cols =
                                            Array.initialize (n * 2)
                                                (\i ->
                                                    { label = ""
                                                    , typ = Booleans
                                                    , data =
                                                        Array.initialize n
                                                            (\j ->
                                                                case List.sum (List.map2 (\i_ j_ -> iif (getCell (i + i_) (j + j_)) 1 0) [ -1, -1, -1, 0, 0, 1, 1, 1 ] [ -1, 0, 1, -1, 1, -1, 0, 1 ]) of
                                                                    2 ->
                                                                        iif (getCell i j) "✓" ""

                                                                    3 ->
                                                                        "✓"

                                                                    _ ->
                                                                        ""
                                                            )
                                                    }
                                                )
                                    }
                            in
                            scrapsheet sheet
                                |> Task.map (Tuple.mapSecond (Result.map gol))

                        Variant "empty" Hole ->
                            Task.succeed
                                ( Set.empty
                                , Ok
                                    { transpose = False
                                    , rows = 0
                                    , cols = Array.empty
                                    , every = 0
                                    }
                                )

                        Variant "col" (Record [ ( "l", sheet ), ( "r", Record [ ( "l", Text k ), ( "r", col ) ] ) ]) ->
                            Task.map2 (tupleMap2 Set.union (Result.map2 (\a b -> { a | rows = Basics.max (Array.length b.data) a.rows, cols = Array.push b a.cols })))
                                (scrapsheet sheet)
                            <|
                                Task.succeed <|
                                    Tuple.pair Set.empty <|
                                        case col of
                                            Variant t (List xs) ->
                                                Ok
                                                    { label = k
                                                    , data = xs |> List.map S.toString |> Array.fromList
                                                    , typ =
                                                        case t of
                                                            "numbers" ->
                                                                Numbers

                                                            "text" ->
                                                                Strings

                                                            "checkbox" ->
                                                                Checkboxes

                                                            "sliders" ->
                                                                Sliders

                                                            _ ->
                                                                -- TODO: Bad!
                                                                Strings
                                                    }

                                            _ ->
                                                Err "TODO: unknown col"

                        Variant "union" (Record [ ( "l", l ), ( "r", r ) ]) ->
                            -- TODO: This whole implementation is broken and quite bad.
                            Task.map2
                                (tupleMap2 Set.union
                                    (Result.map2
                                        (\b d ->
                                            { transpose = b.transpose || d.transpose
                                            , rows = b.rows + d.rows
                                            , cols =
                                                case ( Array.length b.cols, Array.length d.cols ) of
                                                    ( 0, _ ) ->
                                                        d.cols

                                                    ( _, 0 ) ->
                                                        b.cols

                                                    _ ->
                                                        List.map2 (\b_ d_ -> { b_ | data = Array.append b_.data d_.data }) (Array.toList b.cols) (Array.toList d.cols) |> Array.fromList
                                            , every = Basics.max b.every d.every
                                            }
                                        )
                                    )
                                )
                                (scrapsheet l)
                                (scrapsheet r)

                        Variant "limit" (Record [ ( "l", Int n ), ( "r", sheet ) ]) ->
                            scrapsheet sheet |> Task.map (Tuple.mapSecond (Result.map (\s -> { s | rows = Basics.min n s.rows, cols = s.cols |> Array.map (\col -> { col | data = col.data |> Array.slice 0 n }) })))

                        Variant "every" (Record [ ( "l", Int n ), ( "r", sheet ) ]) ->
                            scrapsheet sheet |> Task.map (Tuple.mapSecond (Result.map (\s -> { s | every = n })))

                        Variant "http" (Text x) ->
                            Http.riskyTask
                                { method = "GET"
                                , headers = []
                                , url = x
                                , body = Http.emptyBody
                                , timeout = Just 5000
                                , resolver =
                                    stringResolver
                                        (\s ->
                                            Ok <|
                                                Tuple.pair Set.empty <|
                                                    case s of
                                                        Http.BadUrl_ err ->
                                                            Err err

                                                        Http.Timeout_ ->
                                                            Err "timeout"

                                                        Http.NetworkError_ ->
                                                            Err "network error"

                                                        Http.BadStatus_ _ err ->
                                                            Err err

                                                        Http.GoodStatus_ _ body ->
                                                            Ok
                                                                { transpose = False
                                                                , rows = 1
                                                                , cols = Array.fromList [ { label = "res", typ = Strings, data = Array.fromList [ body ] } ]
                                                                , every = 0
                                                                }
                                        )
                                }

                        Variant "data" (Record xs) ->
                            -- TODO: This should be a Dic rather than a Record.
                            xs
                                |> List.sortBy Tuple.first
                                |> List.foldr
                                    (\( k, v ) c ->
                                        Result.map2 (flip (::)) c <|
                                            case v of
                                                Variant t (List xs_) ->
                                                    Ok
                                                        { label = k
                                                        , data = xs_ |> List.map S.toString |> Array.fromList
                                                        , typ =
                                                            case t of
                                                                "sheet/col/numbers" ->
                                                                    Numbers

                                                                "sheet/col/text" ->
                                                                    Strings

                                                                "sheet/col/checkbox" ->
                                                                    Checkboxes

                                                                "sheet/col/sliders" ->
                                                                    Sliders

                                                                _ ->
                                                                    -- TODO: Bad!
                                                                    Strings
                                                        }

                                                _ ->
                                                    Err "TODO: record cols"
                                    )
                                    (Ok [])
                                |> Result.map Array.fromList
                                |> Result.map (\cols -> { transpose = False, rows = 10, cols = cols, every = 0 })
                                |> Tuple.pair Set.empty
                                |> Task.succeed

                        Variant "reduce" (Record [ ( "l", List l ), ( "r", r ) ]) ->
                            scrapsheet r
                                |> Task.map
                                    (Tuple.mapSecond
                                        (Result.map
                                            (\s ->
                                                { s
                                                    | rows = 1
                                                    , cols =
                                                        Array.fromList <|
                                                            List.map2
                                                                (\b_ d_ ->
                                                                    { b_
                                                                        | data =
                                                                            Array.fromList <|
                                                                                case d_ of
                                                                                    Variant "sum" Hole ->
                                                                                        b_.data |> Array.toList |> List.filterMap String.toFloat |> List.sum |> String.fromFloat |> List.singleton

                                                                                    Variant "avg" Hole ->
                                                                                        b_.data |> Array.toList |> List.filterMap String.toFloat |> List.sum |> flip (/) (toFloat (Basics.max 1 (Array.length b_.data))) |> String.fromFloat |> List.singleton

                                                                                    Variant "min" Hole ->
                                                                                        b_.data |> Array.toList |> List.minimum |> Maybe.withDefault "" |> List.singleton

                                                                                    Variant "max" Hole ->
                                                                                        b_.data |> Array.toList |> List.maximum |> Maybe.withDefault "" |> List.singleton

                                                                                    Variant "count" Hole ->
                                                                                        b_.data |> Array.filter ((/=) "") |> Array.length |> String.fromInt |> List.singleton

                                                                                    _ ->
                                                                                        []
                                                                    }
                                                                )
                                                                (Array.toList s.cols)
                                                                l
                                                }
                                            )
                                        )
                                    )

                        Variant "join" (Record [ ( "l", Record [ ( "l", Text k ), ( "r", l ) ] ), ( "r", r ) ]) ->
                            Task.map2
                                (tupleMap2 Set.union
                                    (Result.map2
                                        (\b d ->
                                            { transpose = b.transpose || d.transpose
                                            , rows = b.rows
                                            , cols = Array.append b.cols (Array.map (\col -> col) d.cols) -- TODO: Implement this for real.
                                            , every = Basics.max b.every d.every
                                            }
                                        )
                                    )
                                )
                                (scrapsheet l)
                                (scrapsheet r)

                        _ ->
                            Err ("TODO: scrapscript -> sheet: " ++ Debug.toString ss) |> Tuple.pair Set.empty |> Task.succeed

                func : String -> Scrap -> Scrap
                func =
                    Binop "->" << Var

                pair : Scrap -> Scrap -> Scrap
                pair l r =
                    Record [ ( "l", l ), ( "r", r ) ]

                env_ : Dict String Scrap
                env_ =
                    Dict.empty
                        |> Dict.union (model.sheets |> Dict.keys |> List.map (String.fromInt >> (++) "s") |> List.map (\x -> ( x, Var x )) |> Dict.fromList)
                        |> Dict.union ([ "numbers", "text", "checkbox", "sliders" ] |> List.map (\x -> ( "sheet/col/" ++ x, func "k" (func "col" (func "sheet" (Variant "col" (pair (Var "sheet") (pair (Var "k") (Variant x (Var "col"))))))) )) |> Dict.fromList)
                        |> Dict.insert "self" (Var "self")
                        |> Dict.insert "true" (Variant "true" Hole)
                        |> Dict.insert "false" (Variant "false" Hole)
                        |> Dict.insert "sheet/empty" (Variant "empty" Hole)
                        |> Dict.insert "sheet/limit" (func "a" (func "b" (Variant "limit" (pair (Var "a") (Var "b")))))
                        |> Dict.insert "sheet/union" (func "a" (func "b" (Variant "union" (pair (Var "a") (Var "b")))))
                        |> Dict.insert "sheet/every" (func "a" (func "b" (Variant "every" (pair (Var "a") (Var "b")))))
                        |> Dict.insert "sheet/http" (func "a" (Variant "http" (Var "a")))
                        |> Dict.insert "sheet/reduce" (func "a" (func "b" (Variant "reduce" (pair (Var "a") (Var "b")))))
                        |> Dict.insert "sheet/join" (func "a" (func "b" (func "c" (Variant "join" (pair (pair (Var "a") (Var "b")) (Var "c"))))))
                        |> Dict.insert "game-of-life" (func "a" (func "b" (Variant "game-of-life" (pair (Var "a") (Var "b")))))
            in
            ( model
            , code
                |> S.run env_
                |> (\x_ ->
                        case x_ of
                            Ok x ->
                                scrapsheet x

                            Err x ->
                                Task.succeed ( Set.empty, Err x )
                   )
                |> Task.map (\( watch, sheet ) -> { watch = watch, code = code, sheet = sheet })
                |> Task.attempt (CodeEdited id)
            )

        CodeEdited id (Ok ss) ->
            ( { model | sheets = model.sheets |> Dict.insert id ss }
            , Task.succeed id |> Task.perform SheetEdited
            )

        CodeEdited id (Err _) ->
            -- TODO
            ( model, Cmd.none )

        DataEditing id ( i, j ) value ->
            let
                edit : Sheet -> Sheet
                edit sheet =
                    case Array.get i sheet.cols of
                        Just col ->
                            { sheet | cols = sheet.cols |> Array.set i { col | data = col.data |> Array.set j value } }

                        Nothing ->
                            sheet
            in
            ( { model | sheets = model.sheets |> Dict.update id (Maybe.map (\x -> { x | sheet = Result.map edit x.sheet })) }
            , Task.succeed id |> Task.perform SheetEdited
            )

        SheetEdited id ->
            ( model
            , model.sheets
                |> Dict.filter (always (.watch >> Set.member id))
                |> Dict.map (\k v -> Task.succeed () |> Task.perform (\_ -> CodeEditing k v.code))
                |> Dict.values
                |> Cmd.batch
            )

        UrlChanged url ->
            ( model, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            ( model, Cmd.none )

        LinkClicked (Browser.External url) ->
            ( model, Cmd.none )



---- VIEW ---------------------------------------------------------------------


viewSheet : SheetId -> Sheet -> Html Msg
viewSheet id sheet =
    -- TODO: Show some call to action for empty sheets? Or make the region clickable?
    H.table []
        [ H.thead []
            [ H.tr [] <|
                Array.toList <|
                    Array.map (H.th [] << ls << text << .label) <|
                        sheet.cols
            ]
        , H.tbody [] <|
            Array.toList <|
                Array.initialize sheet.rows
                    (\i ->
                        H.tr [] <|
                            Array.toList <|
                                Array.initialize (Array.length sheet.cols)
                                    (\j ->
                                        H.td [] <|
                                            ls <|
                                                Maybe.withDefault (text "") <|
                                                    Maybe.map
                                                        (\( typ, val ) ->
                                                            case ( typ, val ) of
                                                                ( Checkboxes, x ) ->
                                                                    H.input [ A.type_ "checkbox", A.checked (x == "#true"), A.onCheck (\c -> DataEditing id ( j, i ) (iif c "#true" "#false")) ] []

                                                                ( Sliders, x ) ->
                                                                    H.input [ A.type_ "range", A.value x, A.onInput (DataEditing id ( j, i )) ] []

                                                                ( Booleans, "#true" ) ->
                                                                    text "✓"

                                                                ( Booleans, "#false" ) ->
                                                                    text ""

                                                                _ ->
                                                                    text val
                                                        )
                                                    <|
                                                        Maybe.andThen (\col -> col.data |> Array.get i |> Maybe.map (Tuple.pair col.typ)) <|
                                                            Array.get j <|
                                                                sheet.cols
                                    )
                    )
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "scrapsheets"
    , body =
        -- TODO: Sheets that exist but aren't currently on the shelf should sit minimized in the corner or something like buffers waiting to be placed back on the shelf.
        [ H.node "style" [] [ text "body * { box-sizing: border-box; }" ]
        , H.node "style" [] [ text "main { padding-bottom: 10rem; }" ]
        , H.node "style" [] [ text "main > div:first-child > :nth-child(even) > :nth-child(even) { background: #fff; }" ]
        , H.node "style" [] [ text "main > div:first-child > :nth-child(even) > :nth-child(odd) { background: #eee; }" ]
        , H.node "style" [] [ text "main > div:first-child > :nth-child(odd) > :nth-child(even) { background: #ddd; }" ]
        , H.node "style" [] [ text "main > div:first-child > :nth-child(odd) > :nth-child(odd) { background: #fff; }" ]
        , H.node "style" [] [ text "main > div:first-child > div > div { border: 0; box-shadow: 0 4px 4px rgba(0,0,0, 0.35); overflow: hidden; border-radius: 5px; }" ]
        , H.node "style" [] [ text "textarea { background: #fff; border: 0; height: 100%; padding: 0.5rem; }" ]
        , H.node "style" [] [ text "table { border-collapse: collapse; }" ]
        , H.node "style" [] [ text "td, th { text-align: center; border-bottom: 1px solid #ccc; height: 1rem; }" ]
        , H.node "style" [] [ text "th { padding: 0.25rem 0.5rem; background-color: rgba(0,0,0,0.15); }" ]
        , H.node "style" [] [ text "tr > :first-child { padding-left: 0.5rem; }" ]
        , H.node "style" [] [ text "tr > :last-child  { padding-right: 0.5rem; }" ]
        , H.main_ []
            [ H.div [ S.displayFlex, S.flexDirectionColumn, S.gapRem 1.5, S.paddingRem 1 ] <|
                List.append [ H.button [ A.onClick SheetCreating ] [ text "New sheet" ] ] <|
                    List.map (H.div [ S.displayFlex, S.flexDirectionRow, S.gapRem 1.25 ]) <|
                        List.map
                            (List.map
                                (\id ->
                                    Maybe.withDefault (H.div [] [ text "TODO: sheet not found" ]) <|
                                        Maybe.map
                                            (\{ code, sheet } ->
                                                H.div [ S.displayFlex, S.flexDirectionColumn, S.width "100%" ]
                                                    [ case sheet of
                                                        Ok x ->
                                                            viewSheet id x

                                                        Err x ->
                                                            H.div [] [ text ("TODO: error: " ++ x) ]
                                                    , H.textarea [ A.onInput (CodeEditing id), S.width "100%", S.fontFamilyMonospace, code |> String.filter ((==) '\n') |> String.length |> (+) 1 |> A.rows ] [ text code ]
                                                    ]
                                            )
                                        <|
                                            Dict.get id model.sheets
                                )
                            )
                        <|
                            model.shelf
            , H.aside []
                []
            ]
        ]
    }
