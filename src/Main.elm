module Main exposing (main)

---- IMPORTS ------------------------------------------------------------------

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import Dict exposing (Dict)
import Html as H exposing (Html, text)
import Html.Attributes as A exposing (..)
import Html.Events as A exposing (..)
import Html.Lazy as H
import Html.Style as S
import Http
import Json.Decode as D
import Json.Encode as E
import Parser as P exposing ((|.), (|=), Parser)
import Pratt as P
import Regex exposing (Regex)
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
subscriptions _ =
    Sub.none



---- MODEL --------------------------------------------------------------------


type alias SheetId =
    Int


type alias Input valid data =
    { valid : valid
    , default : data
    , data : data
    }


type Col
    = Numbers (Array Float)
    | Strings (Array String)
    | Images (Array String)
    | Links (Array String)
    | Buttons (Array (Input () Time.Posix))
    | Datepickers (Array (Input () Date))
    | Checkboxes (Array (Input () Bool))
    | Sliders (Array (Input ( Float, Float ) Float))
    | Fields (Array (Input Regex String))
    | Chart (Array ( Float, Float ))


type alias Sheet =
    { transpose : Bool
    , rows : Int
    , cols : Array ( String, Col )
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
              sheet/into (a -> b -> c -> { a, b, c })
                |> sheet/col/numbers  [ 1, 2, 3 ]
                |> sheet/col/text     [ "a", "b", "c" ]
                |> sheet/col/checkbox [ true, false, true ]
              """
            , """
              sheet/limit 10 s1
              """
            , """
              sheet/filter (r1 -> r1.id == 1) s1
              """
            , """
              sheet/join (r1 -> r2 -> r1.id == r2.id) s1 s2
              """
            , """
              sheet/append s1 s2
              """
            , """
              sheet/union (r -> r.id) s1 s2
              """
            , """
              sheet/intersect (r -> r.id) s1 s2
              """
            , """
              sheet/subtract (r -> r.id) s1 s2
              """
            , """
              sheet/group (r -> r.id) s1
              """
            , """
              sheet/sort (r -> r.id) s1
              """
            , """
              sheet/to-columns
              """
            , """
              sheet/from-columns
              """
            , """
              sheet/http { ... }
              """
            , """
              sheet/websocket { ... }
              """
            , """
              sheet/every 1
              """
            , """
              sheet/lazy (sheet/http { ... }) s1
              """
              -- button clicks
            , """
              s1 |> sheet/lazy (sheet/filter (r -> r.updated >= now))
              . now = s2 |> sheet/row 0 |> maybe/map (r -> r.now) |> maybe/default +&
              """
              -- basic memory
            , """
              s1 |> sheet/lazy (sheet/union (r -> r.id) self)
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
                            , sheet = Ok { transpose = False, rows = 0, cols = Array.empty }
                            }
                    )
                |> Dict.fromList
      , shelf =
            sheets
                |> List.indexedMap (\i _ -> [ i ])
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


type Scrapscript
    = Number Float
    | Text String
    | Var String
    | Op String Scrapscript Scrapscript
    | Apply Scrapscript Scrapscript
    | Fun (Dict String Scrapscript) Scrapscript Scrapscript
    | Rock String
    | Record (Dict String Scrapscript)
    | Arr (List Scrapscript)
    | Tag String
    | Tagged String Scrapscript
    | Hole


eval : Dict String Scrapscript -> Scrapscript -> Result String Scrapscript
eval env ss =
    case ss of
        Number n ->
            n |> Number |> Ok

        Text x ->
            x |> Text |> Ok

        Var x ->
            env |> Dict.get x |> Result.fromMaybe ("TODO: var not found: " ++ x )

        Op "+" (Number l) (Number r) ->
            l + r |> Number |> Ok

        Op "|>" l r ->
            eval env (Apply r l)

        Op "->" l r ->
            Ok (Fun env l r)

        Op op l r ->
            Err ("TODO: " ++ op)

        Fun e l r ->
            Ok (Fun e l r)

        Record xs ->
          xs |> Dict.foldl (\k v kv -> Result.map2 (Dict.insert k) (eval env v) kv) (Ok Dict.empty) |> Result.map Record

        Arr xs ->
            xs |> List.foldr (\x y -> Result.map2 (::) (eval env x) y) (Ok []) |> Result.map Arr

        (Apply (Apply (Rock "text/join") (Text x)) (Arr xs)) ->
         xs 
         |> List.foldr 
            (\x___ xs_ -> case x___ of
               Text x__ -> xs_ |> Result.map ((::) x__)
               _ -> Err "TODO: text/join error"
            )
            (Ok [])
         |> Result.map (String.join "" >> Text)

        (Apply (Rock "sheet/from-csv") (Fun e l r)) ->
          Err "TODO: sheet/from-csv"
      
        -- TODO: Redo this to actually work properly.
        (Apply (Rock "sheet/into") f) ->
          Ok f
      
        (Apply (Apply (Rock "sheet/col/numbers") x) f) ->
          eval env (Apply f (Tagged "sheet/col/numbers" x))

        (Apply (Apply (Rock "sheet/col/text") x) f) ->
          eval env (Apply f (Tagged "sheet/col/text" x))

        (Apply (Apply (Rock "sheet/col/checkbox") x) f) ->
          eval env (Apply f (Tagged "sheet/col/checkbox" x))

        -- (Apply (Apply (Fun e1 (Var l1) (Fun e2 (Var l2) r)) x1) x2) ->
        --  eval (Dict.union e1 e2 |> Dict.insert l1 x1 |> Dict.insert l2 x2) r
      
        (Apply (Fun e (Var l) r) x) ->
         eval (Dict.insert l x e) r

        (Apply (Apply (Rock "sheet/limit") (Number n)) (Record xs)) ->
          xs 
          |> Dict.map 
             (\_ v -> 
               case v of
                 (Tagged k (Arr x)) -> Tagged k (Arr (List.take (round n) x))
                 (Tagged k _) -> Tagged k (Arr [])
                 _ -> Tagged "" (Arr [])
              )
          |> Record
          |> Ok
      
        (Apply (Apply (Var f) x1) x2) ->
         Result.map3 (\f_ x1_ x2_ -> (Apply (Apply f_ x1_) x2_)) (eval env (Var f)) (eval env x1) (eval env x2) |> Result.andThen (eval env)

        (Apply (Var _) (Var _)) ->
          Err "TODO: Something went wrong."

        (Apply (Var f) x) ->
         Result.map2 Apply (eval env (Var f)) (eval env x) |> Result.andThen (eval env)
      
        (Apply (Tag f) x) ->
            Result.map (Tagged f) (eval env x)

        Apply l r ->
            Result.map2 Apply (eval env l) (eval env r) |> Result.andThen (eval env)

        -- Apply f x ->
        --     Err ("TODO: apply" ++ Debug.toString (Apply f x))

        Rock x ->
            Err ("TODO: rock: " ++ x)

        Tag k ->
            Ok (Tag k)

        Tagged k v ->
          eval env v |> Result.map (Tagged k)

        Hole ->
            Ok Hole


scrapscript : Parser Scrapscript
scrapscript =
    let
        space : Parser ()
        space =
            P.chompIf ((==) ' ')

        var : Parser Scrapscript
        var =
            P.variable
                { start = Char.isLower
                , inner = \c -> Char.isAlphaNum c || c == '-' || c == '/'
                , reserved = Set.empty
                }
                |> P.map Var

        expr : Parser Scrapscript
        expr =
            P.expression
                { oneOf =
                    [ P.float |> P.map Number |> P.literal
                    , P.chompIf ((==) '"')
                        |. P.chompWhile ((/=) '"')
                        |. P.chompIf ((==) '"')
                        |> P.getChompedString
                        |> P.map Text
                        |> P.literal
                    , var |> P.literal
                    , \config ->
                        P.succeed identity
                            |. P.symbol "("
                            |= P.subExpression 0 config
                            |. P.symbol ")"
                    , \config ->
                        P.sequence
                            { start = "{"
                            , separator = ","
                            , end = "}"
                            , spaces = P.spaces
                            , item = P.subExpression 1 config
                            , trailing = P.Forbidden
                            }
                            |> P.andThen
                               ( List.foldl
                                 (\ x xs -> 
                                   case x of
                                     Var k -> P.map (Dict.insert k (Var k)) xs
                                     _ -> P.problem "TODO: record stuff"
                                 )
                                 (P.succeed Dict.empty)
                               )
                            |> P.map Record
                    , \config ->
                        P.sequence
                            { start = "["
                            , separator = ","
                            , end = "]"
                            , spaces = P.spaces
                            , item = P.subExpression 1 config
                            , trailing = P.Forbidden
                            }
                            |> P.map Arr
                    ]
                , andThenOneOf =
                    [ P.infixRight 20 (P.symbol "->")  (Op "->")
                    , P.infixLeft  19 (P.symbol ">>")  (Op ">>")
                    , P.infixLeft  15 (P.symbol "++")  (Op "++")
                    , P.infixLeft  16 (P.symbol "+<")  (Op "+<")
                    , P.infixRight 15 (P.symbol ">+")  (Op ">+")
                    , P.infixLeft  14 (P.symbol ">=")  (Op ">=")
                    , P.infixLeft  14 (P.symbol "<=")  (Op "<=")
                    , P.infixLeft  10 (P.symbol "|>")  (Op "|>")
                    , P.infixLeft  13 (P.symbol "<>")  (Op "<>")
                    , P.infixLeft  13 (P.symbol "==")  (Op "==")
                    , P.infixLeft  18 (P.symbol "//")  (Op "//")
                    , P.infixRight 12 (P.symbol "&&")  (Op "&&")
                    , P.infixRight 11 (P.symbol "||")  (Op "||")
                    , P.infixRight 11 (P.symbol "^^")  (Op "^^")
                    , P.infixLeft  18 (P.symbol "*" )  (Op "*" )
                    , P.infixLeft  17 (P.symbol "+ ")  (Op "+ ")
                    , P.infixLeft  14 (P.symbol "<" )  (Op "<" )
                    , P.infixLeft  14 (P.symbol ">" )  (Op ">" )
                    , P.infixLeft  18 (P.symbol "/" )  (Op "/" )
                    , P.infixRight 94 (P.symbol ".") (Op ".")
                    , P.infixLeft  98 (P.succeed ()) Apply
                    ]
                , spaces = P.spaces
                }
    in
    P.succeed identity
        |. P.spaces
        |= expr
        |. P.spaces
        |. P.end



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
                            , sheet = Ok { transpose = False, rows = 0, cols = Array.empty }
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

                scrapsheet : Scrapscript -> Result String Sheet
                scrapsheet ss =
                    case ss of
                        -- Tagged "sheet" x ->
                        --     Err ("TODO: sheet: " ++ Debug.toString ss)

                        Record xs ->
                          xs 
                                |> Dict.toList 
                                |> List.sortBy Tuple.first
                                |> List.foldr
                                   (\ (k,v) c ->
                                       Result.map2 (flip (::)) c <|
                                       case v of
                                         (Tagged "sheet/col/numbers" (Arr xs_)) ->  xs_
                                           |> List.foldl
                                              (\v_ vs_ -> case v_ of
                                                 Number n -> Result.map ((::) n) vs_
                                                 _ -> Err "TODO: sheet/col/numbers: bad number"
                                              )
                                              (Ok [])
                                           |> Result.map Array.fromList
                                           |> Result.map Numbers
                                           |> Result.map (Tuple.pair k)
                                         (Tagged "sheet/col/text" (Arr xs_)) ->  xs_
                                           |> List.foldl
                                              (\v_ vs_ -> case v_ of
                                                 Text n -> Result.map ((::) n) vs_
                                                 _ -> Err "TODO: sheet/col/text: bad number"
                                              )
                                              (Ok [])
                                           |> Result.map Array.fromList
                                           |> Result.map Strings
                                           |> Result.map (Tuple.pair k)
                                         (Tagged "sheet/col/checkbox" (Arr xs_)) ->  xs_
                                           |> List.foldl
                                              (\v_ vs_ -> case v_ of
                                                 Tagged "true" Hole -> Result.map ((::) { valid = (), default = False, data = True}) vs_
                                                 Tagged "false" Hole -> Result.map ((::) { valid = (), default = False, data = False}) vs_
                                                 _ -> Err "TODO: sheet/col/checkbox: bad boolean"
                                              )
                                              (Ok [])
                                           |> Result.map Array.fromList
                                           |> Result.map Checkboxes
                                           |> Result.map (Tuple.pair k)
                                         _ -> Err "TODO: record cols"
                                   )
                                   (Ok [])
                                |> Result.map Array.fromList
                                |> Result.map (\cols -> { transpose = False, rows = 10, cols = cols })

                        _ ->
                            Err ("TODO: scrapscript -> sheet: " ++ Debug.toString ss)

                env_ : Dict String Scrapscript
                env_ =
                    Dict.empty
                        |> Dict.insert "true" (Tagged "true" Hole)
                        |> Dict.insert "false" (Tagged "false" Hole)
                        |> Dict.insert "sheet" (Tagged "sheet" Hole)
                        |> Dict.union ([ "text/join", "add" ] |> List.map (\x -> ( x, Rock x )) |> Dict.fromList)
                        |> Dict.union ([ "limit", "from-csv", "into", "limit", "filter", "join", "append", "union", "intersect", "subtract", "group", "sort", "to-columns", "from-columns", "http", "websocket", "every", "lazy", "row" ] |> List.map ((++) "sheet/") |> List.map (\x -> ( x, Rock x )) |> Dict.fromList)
                        |> Dict.union ([ "numbers", "text", "checkbox" ] |> List.map ((++) "sheet/col/") |> List.map (\x -> ( x, Rock x )) |> Dict.fromList)
                        |> Dict.union ([ "numbers", "text", "checkbox" ] |> List.map ((++) "sheet/csv/col/") |> List.map (\x -> ( x, Rock x )) |> Dict.fromList)
                        |> Dict.union (model.sheets |> Dict.keys |> List.map (String.fromInt >> (++) "s") |> List.map (\x -> ( x, Rock x )) |> Dict.fromList)

                sheet : Result String Sheet
                sheet =
                    code
                        |> P.run scrapscript
                        |> Result.mapError prettyError
                        |> Result.andThen (eval env_)
                        |> Result.andThen scrapsheet

                prettyError : List P.DeadEnd -> String
                prettyError xs =
                    xs
                        |> List.map
                            (\x ->
                                case x of
                                    _ ->
                                        Debug.toString x
                            )
                        |> String.join ", "
            in
            ( model
            , Task.attempt (CodeEdited id) <|
                Task.succeed
                    { watch = Set.empty
                    , code = code
                    , sheet = sheet
                    }
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
                edit_ : Col -> Col
                edit_ col =
                    Maybe.withDefault col <|
                        case col of
                            Numbers xs ->
                                String.toFloat value |> Maybe.map (\x -> xs |> Array.set j x |> Numbers)

                            _ ->
                                -- TODO
                                Nothing

                edit : Sheet -> Sheet
                edit sheet =
                    case Array.get i sheet.cols of
                        Just col ->
                            { sheet | cols = sheet.cols |> Array.set i (Tuple.mapSecond edit_ col) }

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


viewCell : Int -> Col -> Maybe (Html Msg)
viewCell i col =
    case col of
        Numbers xs ->
            xs |> Array.get i |> Maybe.map (String.fromFloat >> text)

        Strings xs ->
            xs |> Array.get i |> Maybe.map text

        Images xs ->
            xs |> Array.get i |> Maybe.map (\src -> H.img [ A.src src ] [])

        Links xs ->
            xs |> Array.get i |> Maybe.map (\href -> H.a [ A.href href ] [])

        Buttons xs ->
            -- TODO
            Nothing

        Datepickers xs ->
            -- TODO
            Nothing

        Checkboxes xs ->
            -- TODO
            xs |> Array.get i |> Maybe.map (\x -> H.input [ A.type_ "checkbox", A.checked x.data ] [])

        Sliders xs ->
            -- TODO
            Nothing

        Fields xs ->
            -- TODO
            Nothing

        Chart xs ->
            case i of
                0 ->
                    -- TODO: Put everything in row 0 with a rowspan of the whole table
                    Nothing

                _ ->
                    Nothing


viewSheet : Sheet -> Html Msg
viewSheet sheet =
    -- TODO: Show some call to action for empty sheets? Or make the region clickable?
    H.table []
        [ H.thead []
            [ H.tr [] <|
                Array.toList <|
                    Array.map (H.th [] << ls << text << Tuple.first) <|
                        sheet.cols
            ]
        , H.tbody [] <|
            Array.toList <|
                Array.initialize sheet.rows
                    (\i ->
                        H.tr [] <|
                            Array.toList <|
                                Array.initialize (Array.length sheet.cols)
                                    (H.td []
                                        << ls
                                        << Maybe.withDefault (text "")
                                        << Maybe.andThen (viewCell i)
                                        << Maybe.map Tuple.second
                                        << flip Array.get sheet.cols
                                    )
                    )
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "scrapsheets"
    , body =
        -- TODO: Sheets that exist but aren't currently on the shelf should sit minimized in the corner or something like buffers waiting to be placed back on the shelf.
        [ H.node "style" [] [ text "main, * { background: black; color: #ccc; } td, th { text-align: center; }" ]
        , H.main_ []
            [ H.div [ S.displayFlex, S.flexDirectionColumn ] <|
                List.append [ H.button [ A.onClick SheetCreating ] [ text "New sheet" ] ] <|
                    List.map (H.div [ S.displayFlex, S.flexDirectionRow ]) <|
                        List.map
                            (List.map
                                (\id ->
                                    Maybe.withDefault (H.div [] [ text "TODO: sheet not found" ]) <|
                                        Maybe.map
                                            (\{ code, sheet } ->
                                                H.div [ S.displayFlex, S.flexDirectionColumn, S.width "100%" ]
                                                    [ case sheet of
                                                        Ok x ->
                                                            viewSheet x

                                                        Err x ->
                                                            H.div [] [ text ("TODO: error: " ++ x) ]
                                                    , H.textarea [ A.onInput (CodeEditing id), A.value code, S.width "100%", S.fontFamilyMonospace, code |> String.filter ((==) '\n') |> String.length |> (+) 1 |> A.rows ] [ text code ]
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
