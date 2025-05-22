port module Main exposing (main)

---- NOTES --------------------------------------------------------------------
--
-- TODO: Create an immutable examples book?
--
-- TODO: Each book gets its own creds that aren't transferred with sheets.
-- TODO: Your user and your book both provide cred context for the request?
-- TODO: e.g. copying DB URLs without moving the role/password
--
--
---- IMPORTS ------------------------------------------------------------------

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events as Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import Dict exposing (Dict)
import Html as H exposing (Html, text)
import Html.Attributes as A
import Html.Events as A
import Html.Lazy as H
import Html.Style as S
import Json.Decode as D
import Json.Encode as E
import Parser as P exposing ((|.), (|=), Parser)
import Regex exposing (Regex)
import Set exposing (Set)
import Task exposing (Task)
import Time exposing (Month(..))
import Url exposing (Url)



---- PORTS --------------------------------------------------------------------


port libraryChanged :
    -- TODO:
    () -> Cmd msg


port changeBook : DocMsg SheetInfo -> Cmd msg


port changeDoc : DocMsg (List Patch) -> Cmd msg


port notifyDoc : DocMsg E.Value -> Cmd msg


port docChanged : (DocMsg DocChange -> msg) -> Sub msg


port docNotified : (DocMsg D.Value -> msg) -> Sub msg


type alias DocChange =
    { doc : D.Value
    , handle : D.Value
    , patchInfo : D.Value
    , patches : List D.Value
    }


type alias DocMsg a =
    { bookId : Id
    , sheetId : Id
    , data : a
    }


type alias Patch =
    { action : String
    , path : List D.Value
    , value : D.Value
    }



---- HELPERS ------------------------------------------------------------------


flip : (a -> b -> c) -> (b -> a -> c)
flip f a b =
    f b a



---- MAIN ---------------------------------------------------------------------


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



---- SUBSCRIPTIONS ------------------------------------------------------------


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ docChanged DocChanged
        , Browser.onKeyPress (D.map KeyPressed (D.field "key" D.string))
        ]



---- MODEL --------------------------------------------------------------------


type alias Id =
    String


type alias Model =
    { nav : Nav.Key
    , library : Library
    , sheet : Sheet
    }


type alias Sheet =
    { bookId : Id
    , sheetId : Id
    , search : String
    , newTag : Maybe String
    , select : Rect
    , hover : Index
    , rows : Result String (Array Row)
    , cols : Result String (Array (Col Type))
    , source : Source
    }


type alias Row =
    Dict String D.Value


type alias Library =
    Dict Id Book


type alias Book =
    { dir : String
    , perms : ()
    , peers : Dict Id ()
    , sheets : Dict Id SheetInfo
    }


type alias SheetInfo =
    { name : String
    , tags : List String
    , thumb : Svg
    }


type alias Rect =
    { a : Index, b : Index }


rect : Int -> Int -> Int -> Int -> Rect
rect ax ay bx by =
    Rect (xy ax ay) (xy bx by)


type alias Index =
    { x : Int, y : Int }


xy : Int -> Int -> Index
xy x y =
    Index x y


type alias Svg =
    -- TODO: Generate nice preview Svg based on sheet contents.
    ()


type alias Query =
    ()


type Source
    = Rows { write : Maybe String, cols : Array (Col Formula) }
    | Query { query : Query }
    | Feed Feed


type
    Feed
    -- TODO: Move some of these to the shop as free templates.
    -- TODO: email, settings, databases, git, github, stripe, logs, sheets, tests, code stats, social media keywords
    = Lib
    | Shop
    | Files
    | Rss { opml : () }
    | Http { url : (), params : () }
    | Github {}
    | Stripe {}
    | Bluesky {}
    | Form {}
    | Email {}
    | Calendar {}
    | Webhook {}
    | Code
        { lang : Language
        , code : String
        }


type Language
    = Sql
    | Prql
    | Fql
    | Gql
    | Jq
    | Scrapscript
    | Js
    | Python
    | R
    | Julia
    | J
    | K
    | Apl


type alias Col t =
    { key : String, label : String, t : t }


type Formula
    = Parse Type
    | Exceed String


type
    Type
    -- | Bytes
    -- | Tag
    -- | List
    -- | Date
    -- | Color
    -- | Image
    -- | Subsheet
    -- | Shape2d
    -- | Shape3d
    -- | Vector
    -- | Rows (List Type)
    -- | Doc
    -- | Plot
    -- | Map
    -- | Link
    = Text
    | Number



---- INIT ---------------------------------------------------------------------


type alias Flags =
    { host : String
    , docUrl : String
    , doc : D.Value
    , library : D.Value
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags _ nav =
    ( { nav = nav
      , library =
            flags.library
                |> D.decodeValue
                    (D.dict
                        (D.map4 Book
                            (D.field "dir" D.string)
                            (D.succeed ())
                            (D.succeed Dict.empty)
                            (D.succeed Dict.empty)
                        )
                    )
                |> Result.withDefault Dict.empty
      , sheet =
            { bookId = "234"
            , sheetId = "123"
            , search = "TODO: search"
            , newTag = Nothing
            , cols =
                -- TODO: Calculate these from the source.
                flags.doc
                    |> D.decodeValue
                        (D.map identity
                            (D.field "cols"
                                (D.array
                                    (D.map3 Col
                                        (D.field "key" D.string)
                                        (D.field "label" D.string)
                                        (D.field "type" (D.succeed Text))
                                    )
                                )
                            )
                        )
                    |> Result.mapError D.errorToString
            , hover = xy -1 -1
            , select = Rect (xy -1 -1) (xy -1 -1)
            , rows =
                flags.doc
                    |> D.decodeValue
                        (D.map identity
                            (D.field "rows"
                                (D.list (D.dict D.value) |> D.map Array.fromList)
                            )
                        )
                    |> Result.mapError D.errorToString
            , source =
                Rows
                    { write = Nothing
                    , cols =
                        flags.doc
                            |> D.decodeValue
                                (D.map identity
                                    (D.field "cols"
                                        (D.array
                                            (D.map3 Col
                                                (D.field "key" D.string)
                                                (D.field "label" D.string)
                                                (D.field "type" (D.succeed (Exceed "TODO")))
                                            )
                                        )
                                    )
                                )
                            |> Result.withDefault Array.empty
                    }
            }
      }
    , Cmd.none
    )



---- PARSER -------------------------------------------------------------------


json : D.Decoder (Array (Dict String D.Value))
json =
    D.oneOf
        [ D.fail "array of column arrays" -- TODO
        , D.fail "array of row arrays" -- TODO
        , D.fail "array of objects" -- TODO
        , D.fail "object of arrays" -- TODO
        ]


string : D.Decoder String
string =
    D.oneOf
        [ D.string
        , D.map String.fromInt D.int
        , D.map String.fromFloat D.float
        ]


number : D.Decoder Float
number =
    D.oneOf
        [ D.float
        , D.map toFloat D.int
        , D.andThen (String.toFloat >> Maybe.map D.succeed >> Maybe.withDefault (D.fail "")) D.string
        ]



---- MESSAGES -----------------------------------------------------------------


type Msg
    = NoOp
    | InputChanging Input String
    | SheetEditing SheetEdit
    | KeyPressed String
    | CellMouseClick
    | CellMouseDown
    | CellMouseUp
    | CellHovering Index
    | DocChanged (DocMsg DocChange)
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest


type SheetEdit
    = SheetWrite Index
    | SheetColumnPush
    | SheetRowPush Int


type Input
    = SheetName
    | SheetTag
    | SheetSearch
    | CellWrite
    | ColumnType Int
    | ColumnKey Int
    | ColumnLabel Int



---- UPDATE -------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ sheet } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputChanging SheetName x ->
            -- TODO:
            ( model, Cmd.none )

        InputChanging SheetTag x ->
            -- TODO:
            ( model, Cmd.none )

        InputChanging SheetSearch x ->
            -- TODO:
            ( model, Cmd.none )

        InputChanging CellWrite x ->
            ( { model
                | sheet =
                    { sheet
                        | source =
                            case sheet.source of
                                Rows rows ->
                                    Rows { rows | write = Just x }

                                source ->
                                    source
                    }
              }
            , Cmd.none
            )

        InputChanging (ColumnType i) x ->
            -- TODO:
            ( model, Cmd.none )

        InputChanging (ColumnKey i) x ->
            -- TODO:
            ( model, Cmd.none )

        InputChanging (ColumnLabel i) x ->
            -- TODO:
            ( model, Cmd.none )

        SheetEditing edit ->
            ( { model
                | sheet =
                    { sheet
                        | source =
                            case sheet.source of
                                Rows rows ->
                                    Rows { rows | write = Nothing }

                                source ->
                                    source
                    }
              }
            , changeDoc <|
                DocMsg sheet.bookId sheet.sheetId <|
                    case sheet.source of
                        Rows rows ->
                            case edit of
                                SheetWrite { x, y } ->
                                    -- TODO: what if we have no columns?
                                    sheet.cols
                                        |> Result.withDefault Array.empty
                                        |> Array.get x
                                        |> Maybe.map
                                            (\col ->
                                                [ { action = "cell-put"
                                                  , path = [ E.string "rows", E.int y, E.string col.key ]
                                                  , value = rows.write |> Maybe.withDefault "" |> E.string
                                                  }
                                                ]
                                            )
                                        |> Maybe.withDefault []

                                SheetRowPush i ->
                                    [ { action = "row-insert"
                                      , path = [ E.string "rows", E.int i ]
                                      , value = E.object []
                                      }
                                    ]

                                SheetColumnPush ->
                                    [ { action = "col-insert"
                                      , path = [ E.string "cols", E.int (Array.length (Result.withDefault Array.empty sheet.cols)) ]
                                      , value = E.object [ ( "key", E.string "" ), ( "label", E.string "" ), ( "type", E.string "same" ) ]
                                      }
                                    ]

                        _ ->
                            []
            )

        CellMouseClick ->
            ( { model
                | sheet =
                    { sheet
                        | select = { a = sheet.hover, b = sheet.hover }
                        , source =
                            case sheet.source of
                                Rows rows ->
                                    -- TODO: Fill in cell value.
                                    Rows { rows | write = Just "" }

                                _ ->
                                    sheet.source
                    }
              }
            , Task.attempt (always NoOp) (Dom.focus "new-cell")
            )

        CellMouseDown ->
            let
                select =
                    sheet.select
            in
            ( { model | sheet = { sheet | select = { select | a = sheet.hover } } }, Cmd.none )

        CellMouseUp ->
            let
                select =
                    sheet.select
            in
            ( { model | sheet = { sheet | select = { select | b = sheet.hover } } }, Cmd.none )

        CellHovering hover ->
            ( { model | sheet = { sheet | hover = hover } }, Cmd.none )

        DocChanged change ->
            -- TODO:
            ( { model
                | sheet =
                    { sheet
                        | cols =
                            -- TODO: Calculate these from .source
                            change.data.doc
                                |> D.decodeValue
                                    (D.map identity
                                        (D.field "cols"
                                            (D.array
                                                (D.map3 Col
                                                    (D.field "key" D.string)
                                                    (D.field "label" D.string)
                                                    (D.field "type" (D.succeed Text))
                                                )
                                            )
                                        )
                                    )
                                |> Result.mapError D.errorToString
                        , rows =
                            change.data.doc
                                |> D.decodeValue
                                    (D.map identity
                                        (D.field "rows"
                                            (D.list (D.dict D.value) |> D.map Array.fromList)
                                        )
                                    )
                                |> Result.mapError D.errorToString
                        , source =
                            case sheet.source of
                                Rows rows ->
                                    Rows
                                        { rows
                                            | cols =
                                                change.data.doc
                                                    |> D.decodeValue
                                                        (D.map identity
                                                            (D.field "cols"
                                                                (D.array
                                                                    (D.map3 Col
                                                                        (D.field "key" D.string)
                                                                        (D.field "label" D.string)
                                                                        (D.field "type" (D.succeed (Exceed "TODO")))
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    |> Result.withDefault Array.empty
                                        }

                                _ ->
                                    sheet.source
                    }
              }
            , Cmd.none
            )

        UrlChanged url ->
            -- TODO
            ( model, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            -- TODO: ?q=+any ?q=-any ?q==any
            ( model, Cmd.none )

        LinkClicked (Browser.External url) ->
            -- TODO
            ( model, Cmd.none )

        KeyPressed "Backspace" ->
            ( { model
                | sheet =
                    { sheet
                        | source =
                            case sheet.source of
                                Rows rows ->
                                    Rows { rows | write = Nothing }

                                source ->
                                    source
                    }
              }
            , changeDoc <|
                DocMsg sheet.bookId sheet.sheetId <|
                    case sheet.source of
                        Rows rows ->
                            case ( negate sheet.select.a.y, negate sheet.select.a.x ) of
                                ( 1, 1 ) ->
                                    []

                                ( 1, i ) ->
                                    [ { action = "col-del"
                                      , path = [ E.string "cols", E.int (negate i) ]
                                      , value = E.object []
                                      }
                                    ]

                                ( i, 1 ) ->
                                    [ { action = "row-del"
                                      , path = [ E.string "rows", E.int (negate i) ]
                                      , value = E.object []
                                      }
                                    ]

                                ( y, x ) ->
                                    sheet.cols
                                        |> Result.withDefault Array.empty
                                        |> Array.get (negate x)
                                        |> Maybe.map
                                            (\col ->
                                                [ { action = "cell-put"
                                                  , path = [ E.string "rows", E.int (negate y), E.string col.key ]
                                                  , value = rows.write |> Maybe.withDefault "" |> E.string
                                                  }
                                                ]
                                            )
                                        |> Maybe.withDefault []

                        _ ->
                            []
            )

        KeyPressed _ ->
            ( model, Cmd.none )



---- VIEW ---------------------------------------------------------------------


view : Model -> Browser.Document Msg
view ({ sheet } as model) =
    let
        book : Book
        book =
            model.library |> Dict.get sheet.bookId |> Maybe.withDefault { dir = "", perms = (), peers = Dict.empty, sheets = Dict.empty }

        { name, tags, thumb } =
            book.sheets |> Dict.get sheet.sheetId |> Maybe.withDefault { name = "", tags = [], thumb = () }

        nrows : Int
        nrows =
            Array.length (Result.withDefault Array.empty sheet.rows)
    in
    { title = "scrapsheets"
    , body =
        [ H.node "style" [] [ text "body * { box-sizing: border-box; gap: 1rem; }" ]
        , H.node "style" [] [ text "body { font-family: sans-serif; }" ]
        , H.node "style" [] [ text "td { border: 1px solid black; height: 1rem; }" ]
        , H.node "style" [] [ text "td:hover { background: rgba(0,0,0,0.15); }" ]
        , H.node "style" [] [ text "@media (prefers-color-scheme: dark) { td:hover { background: rgba(255,255,255,0.15); } }" ]
        , H.node "style" [] [ text ".selected { background: rgba(0,0,0,0.1); }" ]
        , H.node "style" [] [ text "@media (prefers-color-scheme: dark) { .selected { background: rgba(255,255,255,0.1); } }" ]
        , H.div [ S.displayFlex, S.flexDirectionRow, S.paddingRem 2, S.paddingTopRem 1, S.gapRem 2, S.userSelectNone, S.cursorPointer, A.style "-webkit-user-select" "none" ]
            [ H.main_ [ S.displayFlex, S.flexDirectionColumn, S.height "100%", S.width "100%" ]
                [ H.div [ S.displayFlex, S.flexDirectionRow, S.justifyContentSpaceBetween ]
                    [ H.div [ S.displayFlex, S.flexDirectionRow ]
                        -- Badges indicate scrapscript news, book notifs, etc.
                        [ H.a [ A.href "/" ] [ text "scrapsheets (2)" ]
                        , text "/"
                        , H.a [ A.href "/taylor" ] [ text "taylor (12)" ]
                        , text "/"
                        , H.a [ A.href "/taylor/personal" ] [ text "personal (7)" ]
                        , text "/"
                        , H.input [ A.value name, A.onInput (InputChanging SheetName) ] []
                        , H.div [ S.displayFlex, S.flexDirectionRow, S.gapRem 0.5 ] <|
                            List.concat
                                [ case sheet.newTag of
                                    Nothing ->
                                        [ H.button [ A.onClick (InputChanging SheetTag "") ] [ text "#" ] ]

                                    Just value ->
                                        [ H.input [ A.value value, A.onInput (InputChanging SheetTag) ] [] ]
                                , List.map (\tag -> H.a [ A.href ("?q=+tag:" ++ tag) ] [ text ("#" ++ tag) ]) tags
                                ]
                        ]
                    , H.div [ S.displayFlex, S.flexDirectionRowReverse ]
                        [ H.a [ A.href "#history" ] [ text "history (1)" ]
                        , H.a [ A.href "#share" ] [ text "share" ]
                        , H.div [ S.displayFlex, S.flexDirectionRowReverse, S.gapRem 0.5 ]
                            [ H.a [ A.href "?following=+" ] [ text "taylor" ]
                            , H.a [ A.href "?following=+sarah" ] [ text "sarah" ]
                            ]
                        ]
                    ]

                -- All current filters should be rendered as text in the searchbar.
                -- This helps people (1) learn the language and (2) indicate that they're searching rather than editing.
                , H.input [ A.value sheet.search, A.onInput (InputChanging SheetSearch), S.width "100%" ] []

                -- TODO: https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed
                , H.table [ S.borderCollapseCollapse, A.onMouseLeave (CellHovering (xy -1 -1)) ]
                    [ H.thead []
                        [ H.tr [] <|
                            List.concat
                                [ [ H.th
                                        [ A.onClick CellMouseUp
                                        , A.onMouseEnter (CellHovering (xy -1 nrows))
                                        , S.verticalAlignBottom
                                        ]
                                        [ text "↕️" ]
                                  ]
                                , List.indexedMap
                                    (\i col ->
                                        H.th
                                            [ A.onClick CellMouseClick
                                            , A.onMouseDown CellMouseDown
                                            , A.onMouseUp CellMouseUp
                                            , A.onMouseEnter (CellHovering (xy i -1))
                                            , S.textAlignLeft
                                            , S.verticalAlignBottom
                                            ]
                                            [ H.div [ S.displayFlex, S.flexDirectionColumn, S.justifyContentFlexEnd, S.gapRem 0 ]
                                                [ H.div [ S.displayFlex, S.flexWrapWrap, S.gapRem 0.5, S.fontSizeSmall ] <|
                                                    case col.t of
                                                        Text ->
                                                            let
                                                                stats : Dict String Int
                                                                stats =
                                                                    sheet.rows
                                                                        |> Result.withDefault Array.empty
                                                                        |> Array.foldl
                                                                            (\a -> Dict.update (Dict.get col.key a |> Maybe.withDefault (E.string "") |> D.decodeValue string |> Result.withDefault "") (Maybe.withDefault 0 >> (+) 1 >> Just))
                                                                            Dict.empty

                                                                blanks : Int
                                                                blanks =
                                                                    stats |> Dict.get "" |> Maybe.withDefault 0

                                                                total : Int
                                                                total =
                                                                    stats |> Dict.values |> List.sum
                                                            in
                                                            List.concat
                                                                [ [ H.a [ A.href "?TODO" ] [ text (String.concat [ "all (", String.fromInt total, ")" ]) ]
                                                                  ]
                                                                , if Dict.size stats > 10 then
                                                                    [ H.a [ A.href "?TODO" ] [ text (String.concat [ "\"\" (", String.fromInt blanks, ")" ]) ]
                                                                    , H.a [ A.href "?TODO" ] [ text (String.concat [ "... (", String.fromInt (total - blanks), ")" ]) ]
                                                                    ]

                                                                  else
                                                                    stats
                                                                        |> Dict.toList
                                                                        |> List.sortBy (Tuple.second >> negate)
                                                                        |> List.map (\( k, v ) -> H.a [ A.href "?TODO" ] [ text (String.concat [ "\"", k, "\" (", String.fromInt v, ")" ]) ])
                                                                ]

                                                        Number ->
                                                            -- TODO: Histogram curve with filter sliders.
                                                            let
                                                                vals : Array (Maybe Float)
                                                                vals =
                                                                    sheet.rows
                                                                        |> Result.withDefault Array.empty
                                                                        |> Array.map (Dict.get col.key >> Maybe.andThen (D.decodeValue number >> Result.toMaybe))

                                                                nums : List Float
                                                                nums =
                                                                    vals |> Array.toList |> List.filterMap identity
                                                            in
                                                            -- TODO: String.left is a bad hack! figure out how to do better rounding.
                                                            List.map (\( k, v ) -> H.span [] [ text (k ++ ": " ++ (String.left 5 <| Maybe.withDefault "_" <| Maybe.map String.fromFloat v)) ]) <|
                                                                [ ( "many", vals |> Array.filter ((/=) Nothing) |> Array.length |> toFloat |> Just )
                                                                , ( "min", List.minimum nums )
                                                                , ( "mean"
                                                                  , if List.length nums > 0 then
                                                                        Just (List.sum nums / toFloat (List.length nums))

                                                                    else
                                                                        Nothing
                                                                  )
                                                                , ( "max", List.maximum nums )
                                                                ]
                                                , H.input [ A.value col.label, A.onInput (InputChanging (ColumnLabel i)) ] []
                                                , H.input [ A.value col.key, A.onInput (InputChanging (ColumnKey i)) ] []
                                                , let
                                                    types : List ( String, Type )
                                                    types =
                                                        [ ( "same", Text ), ( "text/from-float", Number ) ]
                                                  in
                                                  H.select [ A.onInput (InputChanging (ColumnType i)) ] <|
                                                    List.map (\( k, v ) -> H.option [ A.value k, A.selected (v == col.t) ] [ text k ]) <|
                                                        types
                                                ]
                                            ]
                                    )
                                    (Array.toList (Result.withDefault Array.empty sheet.cols))
                                , [ H.th [ A.onClick (SheetEditing SheetColumnPush), S.verticalAlignBottom ] [ text "➡️" ] ]
                                ]
                        ]
                    , H.tbody [] <|
                        Array.toList <|
                            Array.indexedMap
                                (\n row ->
                                    H.tr [] <|
                                        List.concat
                                            [ [ H.th
                                                    [ A.onClick CellMouseClick
                                                    , A.onMouseDown CellMouseDown
                                                    , A.onMouseUp CellMouseUp
                                                    , A.onMouseEnter (CellHovering (xy -1 n))
                                                    ]
                                                    [ text "↔️" ]
                                              ]
                                            , List.indexedMap
                                                (\i col ->
                                                    -- TODO: Don't allow editing if Virtual column.
                                                    H.td
                                                        [ A.onClick CellMouseClick
                                                        , A.onMouseDown CellMouseDown
                                                        , A.onMouseUp CellMouseUp
                                                        , A.onMouseEnter (CellHovering (xy i n))
                                                        , A.classList
                                                            [ ( "selected", (sheet.select /= rect -1 -1 -1 -1) && ((sheet.select.a.x <= i && i <= sheet.select.b.x) || (sheet.select.a.x == -1 && -1 == sheet.select.b.x)) && ((sheet.select.a.y <= n && n <= sheet.select.b.y) || (sheet.select.a.y == -1 && -1 == sheet.select.b.y)) )
                                                            ]
                                                        ]
                                                    <|
                                                        case sheet.source of
                                                            Rows { write } ->
                                                                [ if write /= Nothing && sheet.select == rect i n i n then
                                                                    H.input [ A.id "new-cell", A.value (Maybe.withDefault "" write), A.onInput (InputChanging CellWrite), A.onBlur (SheetEditing (SheetWrite sheet.select.a)), S.width "100%" ] []

                                                                  else
                                                                    row |> Dict.get col.key |> Maybe.withDefault (E.string "") |> D.decodeValue string |> Result.withDefault "TODO: parse error" |> text
                                                                ]

                                                            _ ->
                                                                []
                                                )
                                                (Array.toList (Result.withDefault Array.empty sheet.cols))

                                            -- TODO: Drag this to reorder the row.
                                            , [ H.th [ A.onClick (SheetEditing (SheetRowPush n)), S.textAlignCenter ] [ text "↩️" ] ]
                                            ]
                                )
                            <|
                                -- TODO: Better error.
                                Result.withDefault Array.empty
                                <|
                                    sheet.rows
                    ]
                ]
            , H.aside [ S.displayFlex, S.flexDirectionColumn, S.minWidthRem 15 ]
                -- TODO: This section automatically populates based on context. It's like an inspector that's shows you details on what you're currently doing.
                -- TODO: Prefer .selected and fallback to .hover.
                -- TODO: [ "definition", "scrappy", "share", "history", "problems", "related", "help" ]
                [ H.span [] [ text "definition" ]
                , H.textarea [ A.onInput (always NoOp), S.minHeightRem 10 ]
                    [ text (Debug.toString sheet.cols)
                    ]
                ]
            ]
        ]
    }
