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
              sheet/http "TODO"
              """
            , """
              sheet/websocket "TODO"
              """
            , """
              sheet/every (sheet/http "TODO")
              """
            , """
              sheet/lazy (sheet/http "TODO") s1
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

            -- TODO: Turn this into a Chart.
            -- monte carlo
            , """
              sheet/union (r -> r.id) 
                (sheet/every 1 (sheet/http "https://taylor.town/random")) 
                self 
                |> sheet/limit 10
              """

            -- game of life
            , """
              self
              |> sheet/map (TODO: game-of-life)
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

                scrapsheet : Scrap -> Task String (Result String Sheet)
                scrapsheet ss =
                    case ss of
                        Variant "every" x ->
                            scrapsheet x |> Task.map (Result.map (\s -> { s | every = 1 }))

                        Variant "http" (Text x) ->
                            Http.task
                                { method = "GET"
                                , headers = []
                                , url = x
                                , body = Http.emptyBody
                                , timeout = Nothing
                                , resolver =
                                    stringResolver
                                        (\s ->
                                            Ok <|
                                                Ok
                                                    { transpose = False
                                                    , rows = 1
                                                    , cols = Array.fromList [ ( "res", Strings (Array.fromList [ "TODO" ]) ) ]
                                                    , every = 0
                                                    }
                                        )
                                }

                        Variant "data" (Record xs) ->
                            -- TODO: This should be a Dic rather than a Record.
                            xs
                                |> Dict.toList
                                |> List.sortBy Tuple.first
                                |> List.foldr
                                    (\( k, v ) c ->
                                        Result.map2 (flip (::)) c <|
                                            case v of
                                                Variant "sheet/col/numbers" (List xs_) ->
                                                    xs_
                                                        |> List.foldl
                                                            (\v_ vs_ ->
                                                                case v_ of
                                                                    Float n ->
                                                                        Result.map ((::) n) vs_

                                                                    Int n ->
                                                                        Result.map ((::) (toFloat n)) vs_

                                                                    _ ->
                                                                        Err "TODO: sheet/col/numbers: bad number"
                                                            )
                                                            (Ok [])
                                                        |> Result.map Array.fromList
                                                        |> Result.map Numbers
                                                        |> Result.map (Tuple.pair k)

                                                Variant "sheet/col/text" (List xs_) ->
                                                    xs_
                                                        |> List.foldl
                                                            (\v_ vs_ ->
                                                                case v_ of
                                                                    Text n ->
                                                                        Result.map ((::) n) vs_

                                                                    _ ->
                                                                        Err "TODO: sheet/col/text: bad number"
                                                            )
                                                            (Ok [])
                                                        |> Result.map Array.fromList
                                                        |> Result.map Strings
                                                        |> Result.map (Tuple.pair k)

                                                Variant "sheet/col/checkbox" (List xs_) ->
                                                    xs_
                                                        |> List.foldl
                                                            (\v_ vs_ ->
                                                                case v_ of
                                                                    Variant "true" Hole ->
                                                                        Result.map ((::) { valid = (), default = False, data = True }) vs_

                                                                    Variant "false" Hole ->
                                                                        Result.map ((::) { valid = (), default = False, data = False }) vs_

                                                                    _ ->
                                                                        Err "TODO: sheet/col/checkbox: bad boolean"
                                                            )
                                                            (Ok [])
                                                        |> Result.map Array.fromList
                                                        |> Result.map Checkboxes
                                                        |> Result.map (Tuple.pair k)

                                                _ ->
                                                    Err "TODO: record cols"
                                    )
                                    (Ok [])
                                |> Result.map Array.fromList
                                |> Result.map (\cols -> { transpose = False, rows = 10, cols = cols, every = 0 })
                                |> Task.succeed

                        _ ->
                            Err ("TODO: scrapscript -> sheet: " ++ Debug.toString ss) |> Task.succeed

                env_ : Dict String Scrap
                env_ =
                    Dict.empty
                        |> Dict.insert "true" (Variant "true" Hole)
                        |> Dict.insert "false" (Variant "false" Hole)
                        |> Dict.insert "sheet" (Variant "sheet" Hole)
                        |> Dict.union ([ "text/join", "add" ] |> List.map (\x -> ( x, Var x )) |> Dict.fromList)
                        |> Dict.union ([ "limit", "from-csv", "into", "limit", "filter", "join", "append", "union", "intersect", "subtract", "group", "sort", "to-columns", "from-columns", "http", "websocket", "every", "lazy", "row" ] |> List.map ((++) "sheet/") |> List.map (\x -> ( x, Var x )) |> Dict.fromList)
                        |> Dict.union ([ "numbers", "text", "checkbox" ] |> List.map ((++) "sheet/col/") |> List.map (\x -> ( x, Var x )) |> Dict.fromList)
                        |> Dict.union ([ "numbers", "text", "checkbox" ] |> List.map ((++) "sheet/csv/col/") |> List.map (\x -> ( x, Var x )) |> Dict.fromList)
                        |> Dict.union (model.sheets |> Dict.keys |> List.map (String.fromInt >> (++) "s") |> List.map (\x -> ( x, Var x )) |> Dict.fromList)
            in
            ( model
            , (case S.run env_ code of
                Ok x ->
                    Task.succeed x

                Err x ->
                    Task.fail x
              )
                |> Task.andThen scrapsheet
                |> Task.map (\sheet -> { watch = Set.empty, code = code, sheet = sheet })
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
