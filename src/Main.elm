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


type alias EditArray a =
    { edits : Dict Int a
    , data : Array a
    }


type Col
    = Numbers (EditArray Float)
    | Strings (EditArray String)
    | Images (EditArray String)
    | Links (EditArray String)
    | Buttons (EditArray (Input () Time.Posix))
    | Datepickers (EditArray (Input () Date))
    | Checkboxes (EditArray (Input () Bool))
    | Sliders (EditArray (Input ( Float, Float ) Float))
    | Fields (EditArray (Input Regex String))
    | Chart (EditArray ( Float, Float ))


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
    ( { sheets =
            Dict.empty
                |> Dict.insert 1
                    { watch = Set.empty
                    , code = "TODO"
                    , sheet =
                        Ok
                            { transpose = False
                            , rows = 4
                            , cols =
                                Array.fromList
                                    [ ( "Col", Numbers { edits = Dict.empty, data = Array.fromList [ 1, 2, 3, 4 ] } )
                                    , ( "Col", Numbers { edits = Dict.empty, data = Array.fromList [ 5, 6, 7, 8 ] } )
                                    ]
                            }
                    }
      , shelf =
            [ [ 1 ]
            ]
      }
    , Cmd.none
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
    | Var String
    | Op String Scrapscript Scrapscript
    | Apply Scrapscript Scrapscript


scrapscript : Parser Scrapscript
scrapscript =
    let
        expr : Parser Scrapscript
        expr =
            P.expression
                { oneOf =
                    [ P.float |> P.map Number |> P.literal
                    , P.variable
                        { start = Char.isLower
                        , inner = \c -> Char.isAlphaNum c || c == '-' || c == '/'
                        , reserved = Set.empty
                        }
                        |> P.map Var
                        |> P.literal
                    , \config ->
                        P.succeed identity
                            |. P.symbol "("
                            |= P.subExpression 0 config
                            |. P.symbol ")"
                    ]
                , andThenOneOf =
                    [ P.infixRight 2 (P.symbol "|>") (Op "|>")
                    , P.infixRight 2 (P.symbol "->") (Op "->")
                    ]
                , spaces = P.spaces
                }
    in
    P.succeed identity
        |= expr
        |. P.end



{-
   -- TODO: Move all the corresponding elm code into a Sheet module

   -- TODO: Use these as tests

   "[ todo ]"
     |> sheet/from-json pair
     |> sheet/col/numbers
     |> sheet/col/text

   text/join "\n" [ "1,a" , "2,b" , "3,c" ]
     |> sheet/from-csv (a -> b -> { a, b })
     |> sheet/col/numbers
     |> sheet/col/text

   sheet/into (a -> b -> c -> { a, b, c })
     |> sheet/col/numbers  [ 1, 2, 3 ]
     |> sheet/col/text     [ "a", "b", "c" ]
     |> sheet/col/checkbox [ true, false, false ]

   sheet/map2 add s1 s2

   sheet/limit 10 s1

   sheet/filter (r1 -> r1.id == 1) s1

   sheet/join (r1 -> r2 -> r1.id == r2.id) s1 s2

   sheet/append s1 s2

   sheet/union (r -> r.id) s1 s2

   sheet/intersect (r -> r.id) s1 s2

   sheet/subtract (r -> r.id) s1 s2

   sheet/group (r -> r.id) s1

   sheet/sort (r -> r.id) s1

   sheet/to-columns

   sheet/from-columns

   sheet/http { ... }

   sheet/websocket { ... }

   sheet/every 1

   -- TODO: we need some sort of sheet/lazy

   -- button clicks
   s1 |> sheet/filter (r -> abs (now - r.updated) < 1)
   . now = s2 |> sheet/get 0 (r -> r.now)

   -- basic memory
   s1 |> sheet/union (r -> r.id) self

-}
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
                | shelf = model.shelf ++ [ [ sheetId ] ]
                , sheets =
                    model.sheets
                        |> Dict.insert sheetId
                            { watch = Set.empty
                            , code = "sheet\n  [\n  ]"
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

                sheet : Result String Sheet
                sheet =
                    Err "TODO"
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
                                String.toFloat value
                                    |> Maybe.map (\x -> Numbers { xs | edits = xs.edits |> Dict.insert j x })

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
    let
        get : EditArray a -> Maybe a
        get arr =
            case ( Dict.get i arr.edits, Array.get i arr.data ) of
                ( Just x, _ ) ->
                    Just x

                ( _, Just x ) ->
                    Just x

                _ ->
                    Nothing
    in
    case col of
        Numbers xs ->
            xs |> get |> Maybe.map (String.fromFloat >> text)

        Strings xs ->
            xs |> get |> Maybe.map text

        Images xs ->
            xs |> get |> Maybe.map (\src -> H.img [ A.src src ] [])

        Links xs ->
            xs |> get |> Maybe.map (\href -> H.a [ A.href href ] [])

        Buttons xs ->
            -- TODO
            Nothing

        Datepickers xs ->
            -- TODO
            Nothing

        Checkboxes xs ->
            -- TODO
            Nothing

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
        [ H.node "style" [] [ text "" ]
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
                                                H.div [ S.displayFlex, S.flexDirectionColumn ]
                                                    [ case sheet of
                                                        Ok x ->
                                                            viewSheet x

                                                        Err x ->
                                                            H.div [] [ text ("TODO: error: " ++ x) ]
                                                    , H.textarea [ A.onInput (CodeEditing id), A.value code ] [ text code ]
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
