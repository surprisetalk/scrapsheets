port module Main exposing (main)

---- NOTES --------------------------------------------------------------------
--
-- New architecture:
--
--   dict id sheet
--   ; sheet :
--       { name : text
--       , tags : set text
--       , peers :
--           | #private (dict id peer)
--           | #public
--       , table :
--           | #doc doc      -- synced to server via automerge
--           | #feed feed    -- data materialized on server, configs synced via automerge
--           | #query query  -- data materialized on server, query synced via automerge
--       }
--   ; peer :
--       { write : bool
--       , read : bool
--       , share : bool
--       }
--   ; doc :
--       { cols : list { name : text, key : int, type }
--       , rows : list (list json)
--       }
--   ; feed :
--       | #library
--       | #shop
--       | #files
--       | #email {}
--       | #database {} -- the table displays the SCHEMA not a table
--       | #oauth {} -- the table display the SCHEMA not an endpoint
--       | #form {}
--       | #webhook {}
--       | #kv {}
--       | #webdav {}
--       | #crawler {}
--       | #rss { query }
--       | #box { query }
--   ; query :
--       -- queries fail if any sources not shared with you
--       -- row actions/abilities (e.g. delete) are cells/columns too
--       | #from { source }
--       | #join { source }
--       | #filter {}
--       | #select select
--   ; source :
--       | #hole
--       | #doc { id }
--       | #feed { id }
--       | #query { id }
--   ; select :
--       | #columns {}
--       | #chart {}
--       | #app {}
--
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


port libraryChanged : (D.Value -> msg) -> Sub msg


port changeBook : DocMsg { sheetId : Id, data : SheetInfo } -> Cmd msg


port changeSheet : DocMsg (List Patch) -> Cmd msg


port notifySheet : DocMsg E.Value -> Cmd msg


port sheetChanged : (DocMsg DocChange -> msg) -> Sub msg


port sheetNotified : (DocMsg D.Value -> msg) -> Sub msg


port selectSheet : Id -> Cmd msg


type alias DocChange =
    { doc : D.Value
    , handle : D.Value
    , patchInfo : D.Value
    , patches : List D.Value
    }


type alias DocMsg a =
    { sheetId : Id
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


iif : Bool -> a -> a -> a
iif c a b =
    if c then
        a

    else
        b



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
        [ sheetChanged DocChanged
        , libraryChanged LibraryChanged
        , Browser.onKeyPress (D.map KeyPressed (D.field "key" D.string))
        ]



---- MODEL --------------------------------------------------------------------


type alias Id =
    String


type alias Model =
    { nav : Nav.Key
    , library : Dict Id Book
    , sheet : Sheet
    }


type alias Sheet =
    { sheetId : Id
    , search : String
    , newTag : Maybe String
    , select : Rect
    , hover : Index
    , click : Bool
    , rows : Result String (Array Row)
    , cols : Result String (Array (Col Type))
    , source : Result String Source
    , tool : Tool
    }


type alias Row =
    Dict String D.Value


type alias Book =
    -- TODO: To publish/share something, just loan it to a public/open book.
    -- TODO:   The trick is communicating how/when data moves across boundaries.
    -- TODO: Consider adding .queries
    { dir : String
    , peers : Dict Id ()
    , feeds : Dict Id Feed
    , sheets : Dict Id SheetInfo
    }


type alias SheetInfo =
    { name : String
    , tags : List String
    , thumb : Svg
    }


type
    Feed
    -- TODO: Move some of these to the shop as free templates.
    -- TODO:   email, settings, databases, git, github, stripe, logs, sheets, tests, code stats, social media keywords
    -- TODO: When you add feeds, it automatically adds some queries to your library, which you can hide?
    = Http { url : (), params : (), every : () }
    | Rss { opml : () }
    | Form {}
    | Email {}
    | Calendar {}
    | Webhook {}
    | Database {}
    | Code
        { lang : Lang
        , code : String
        }


type Lang
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


type
    Query
    -- TODO: Use PRQL AST?
    = Base Base
    | Queet Id
    | Queed Id
    | Select {}
    | Filter {}
    | Join {}


type Base
    = Lib
    | Shop


type
    Source
    -- TODO: Rename to Concrete vs Virtual
    = Data { write : Maybe String, cols : Array (Col Formula) }
    | Query Query


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
    | Many Type
    | Link


type Tool
    = Settings
    | Hints
    | Stats { following : Maybe () }
    | Share
    | History


tools : Dict String Tool
tools =
    Dict.fromList
        [ ( "settings", Settings )
        , ( "stats", Stats { following = Nothing } )
        , ( "share", Share )
        , ( "hints", Hints )
        , ( "history", History )
        ]



---- INIT ---------------------------------------------------------------------


type alias Flags =
    { sheet : D.Value
    , library : D.Value
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url nav =
    let
        library : Dict Id Book
        library =
            flags.library
                |> D.decodeValue libraryDecoder
                |> Result.withDefault Dict.empty
    in
    ( { nav = nav
      , library = library
      , sheet = decodeSheet library (Maybe.withDefault "" url.fragment) flags.sheet
      }
    , Cmd.none
    )


defaultSheet : Sheet
defaultSheet =
    { sheetId = ""
    , search = ""
    , newTag = Nothing
    , hover = xy -1 -1
    , click = False
    , select = Rect (xy -1 -1) (xy -1 -1)
    , cols = Err "TODO: default sheet"
    , rows = Err "TODO: default sheet"
    , source = Err "TODO: default sheet"
    , tool = Settings
    }


libSheet : Dict Id Book -> Sheet
libSheet library =
    { defaultSheet
        | sheetId = ""
        , cols =
            [ ( "book_id", Link )
            , ( "dir", Text )
            , ( "sheet_id", Link )
            , ( "name", Text )
            , ( "tags", Many Text )
            ]
                |> List.map (\( k, t ) -> Col k k t)
                |> Array.fromList
                |> Ok
        , rows =
            library
                |> Dict.toList
                |> List.concatMap
                    (\( bookId, book ) ->
                        book.sheets
                            |> Dict.toList
                            |> List.map
                                (\( sheetId, sheet ) ->
                                    Dict.fromList
                                        [ ( "book_id", E.string ("/?book_id=" ++ bookId) )
                                        , ( "dir", E.string book.dir )
                                        , ( "sheet_id", E.string ("#" ++ sheetId) )
                                        , ( "name", E.string sheet.name )
                                        , ( "tags", E.list E.string sheet.tags )
                                        ]
                                )
                    )
                |> (::)
                    (Dict.fromList
                        [ ( "book_id", E.string "/" )
                        , ( "dir", E.string "" )
                        , ( "sheet_id", E.string "/" )
                        , ( "name", E.string "library" )
                        , ( "tags", E.list E.string [] )
                        ]
                    )
                |> Array.fromList
                |> Ok
        , source = Ok (Feed Lib)
    }



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


libraryDecoder : D.Decoder (Dict Id Book)
libraryDecoder =
    D.dict
        (D.map4 Book
            (D.field "dir" D.string)
            (D.succeed ())
            (D.succeed Dict.empty)
            (D.field "sheets"
                (D.dict
                    (D.map3 SheetInfo
                        (D.field "name" D.string)
                        (D.field "tags" (D.list D.string))
                        (D.succeed ())
                    )
                )
            )
        )


decodeSheet : Dict Id Book -> Id -> D.Value -> Sheet
decodeSheet library id sheet =
    -- TODO: Consider doing D.Decoder ({cols, rows, source}) instead.
    case id of
        "" ->
            libSheet library

        _ ->
            let
                colsDecoder : D.Decoder (Array (Col Type))
                colsDecoder =
                    -- TODO: Calculate these from the source.
                    D.map identity
                        (D.field "cols"
                            (D.array
                                (D.map3 Col
                                    (D.field "key" D.string)
                                    (D.field "label" D.string)
                                    (D.field "type" (D.succeed Text))
                                )
                            )
                        )

                rowsDecoder : D.Decoder (Array Row)
                rowsDecoder =
                    D.field "rows"
                        (D.list (D.dict D.value) |> D.map Array.fromList)

                sourceDecoder : D.Decoder Source
                sourceDecoder =
                    D.oneOf
                        [ D.map Data
                            (D.map (\cols -> { write = Nothing, cols = cols })
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
                            )
                        ]
            in
            { defaultSheet
                | sheetId = id
                , cols = sheet |> D.decodeValue colsDecoder |> Result.mapError D.errorToString
                , rows = sheet |> D.decodeValue rowsDecoder |> Result.mapError D.errorToString
                , source = sheet |> D.decodeValue sourceDecoder |> Result.mapError D.errorToString
            }



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
    | LibraryChanged D.Value
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
                                Ok (Data rows) ->
                                    Ok (Data { rows | write = Just x })

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
                                Ok (Data rows) ->
                                    Ok (Data { rows | write = Nothing })

                                source ->
                                    source
                    }
              }
            , changeSheet <|
                DocMsg sheet.sheetId <|
                    case sheet.source of
                        Ok (Data rows) ->
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
                                Ok (Data rows) ->
                                    -- TODO: Fill in cell value.
                                    Ok (Data { rows | write = Just "" })

                                _ ->
                                    sheet.source
                    }
              }
            , Task.attempt (always NoOp) (Dom.focus "new-cell")
            )

        CellMouseDown ->
            ( { model | sheet = { sheet | click = True, select = Rect sheet.hover sheet.select.b } }, Cmd.none )

        CellMouseUp ->
            ( { model | sheet = { sheet | click = False, select = Rect sheet.select.a sheet.hover } }, Cmd.none )

        CellHovering hover ->
            let
                select =
                    sheet.select

                select_ =
                    if sheet.click then
                        { select | b = hover }

                    else
                        select
            in
            ( { model | sheet = { sheet | hover = hover, select = select_ } }, Cmd.none )

        DocChanged change ->
            -- TODO:
            ( { model | sheet = decodeSheet model.library change.sheetId change.data.doc }, Cmd.none )

        LibraryChanged libraryData ->
            ( { model
                | library =
                    libraryData
                        |> D.decodeValue libraryDecoder
                        |> Result.withDefault model.library
              }
            , Cmd.none
            )

        UrlChanged url ->
            -- TODO: We should eventually change #:sheetId to /:sheetId, but for now it confuses stupid local webserver.
            case url.fragment of
                Just sheetId ->
                    ( model, selectSheet sheetId )

                Nothing ->
                    ( { model | sheet = libSheet model.library }, selectSheet "" )

        LinkClicked (Browser.Internal url) ->
            -- TODO: ?q=+any ?q=-any ?q==any
            case url.fragment |> Maybe.andThen (flip Dict.get tools) of
                Just tool ->
                    ( { model | sheet = { sheet | tool = tool } }, Cmd.none )

                _ ->
                    ( model, Nav.pushUrl model.nav (Url.toString url) )

        LinkClicked (Browser.External url) ->
            ( model, Nav.load url )

        KeyPressed "Enter" ->
            ( model, Task.attempt (always NoOp) (Dom.blur "new-cell") )

        KeyPressed "Backspace" ->
            ( { model
                | sheet =
                    { sheet
                        | source =
                            case sheet.source of
                                Ok (Data rows) ->
                                    Ok (Data { rows | write = Nothing })

                                source ->
                                    source
                    }
              }
            , changeSheet <|
                DocMsg sheet.sheetId <|
                    case sheet.source of
                        Ok (Data rows) ->
                            -- TODO: Do multiple patches when ranges are selected.
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
        ( bookId, book ) =
            model.library
                |> Dict.filter (\_ -> .sheets >> Dict.keys >> Set.fromList >> Set.member sheet.sheetId)
                |> Dict.toList
                |> List.head
                |> Maybe.withDefault ( "", { dir = "", perms = (), peers = Dict.empty, sheets = Dict.empty } )

        { name, tags, thumb } =
            book.sheets |> Dict.get sheet.sheetId |> Maybe.withDefault { name = "", tags = [], thumb = () }
    in
    { title = "scrapsheets"
    , body =
        [ H.node "style" [] [ text "body * { gap: 1rem; }" ]
        , H.node "style" [] [ text "body { font-family: monospace; }" ]
        , H.node "style" [] [ text "th, td { padding: 0 0.25rem; font-weight: normal; }" ]
        , H.node "style" [] [ text "td { border: 1px solid black; height: 1rem; }" ]
        , H.node "style" [] [ text "@media (prefers-color-scheme: dark) { td { background: rgba(255,255,255,0.05); } }" ]
        , H.node "style" [] [ text "td:hover { background: rgba(0,0,0,0.15); }" ]
        , H.node "style" [] [ text "@media (prefers-color-scheme: dark) { td:hover { background: rgba(255,255,255,0.15); } }" ]
        , H.node "style" [] [ text ".selected { background: rgba(0,0,0,0.1); }" ]
        , H.node "style" [] [ text "@media (prefers-color-scheme: dark) { .selected { background: rgba(255,255,255,0.1); } }" ]
        , H.div [ S.displayFlex, S.flexDirectionRow, S.paddingRem 2, S.paddingTopRem 1, S.gapRem 2, S.userSelectNone, S.cursorPointer, A.style "-webkit-user-select" "none" ]
            [ H.main_ [ S.displayFlex, S.flexDirectionColumn, S.height "100%", S.width "100%" ]
                [ H.div [ S.displayFlex, S.flexDirectionRow, S.justifyContentSpaceBetween ]
                    [ H.div [ S.displayFlex, S.flexDirectionRow, S.gapRem 0.5 ] <|
                        -- Badges indicate scrapscript news, book notifs, etc.
                        List.concat
                            [ [ H.a [ A.href "/", S.fontWeightBold ] [ text "scrapsheets (2)" ]
                              ]
                            , iif (book.dir == "")
                                []
                                [ text "/"
                                , H.a [ A.href ("/?book_id=" ++ bookId) ] [ text (book.dir ++ " (12)") ]
                                ]
                            , iif (sheet.sheetId == "")
                                [ text "/"
                                , H.span [] [ text "library" ]
                                ]
                                [ text "/"
                                , H.a [ A.href "#settings" ] [ text name ]
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
                            ]
                    , H.div [ S.displayFlex, S.flexDirectionRowReverse, S.gapRem 0.5 ]
                        -- TODO: If we need to, we can collapse the tools into a dropdown that only shows the current one.
                        [ H.a [ A.href "#settings" ] [ text "settings" ]
                        , H.a [ A.href "#history" ] [ text "history (2)" ]
                        , H.a [ A.href "#hints" ] [ text "hints (3)" ]
                        , H.a [ A.href "#stats" ] [ text "stats (2)" ]
                        , H.a [ A.href "#share" ] [ text "share (4)" ]
                        , H.div [ S.displayFlex, S.flexDirectionRowReverse, S.gapRem 0.5 ]
                            [ H.a [ A.href "?following=" ] [ text "@tt" ]
                            , H.a [ A.href "?following=123" ] [ text "@sa" ]
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
                            -- TODO: Add additional header rows for stats and column def.
                            (::)
                                (H.th
                                    [ A.onClick (SheetEditing (SheetRowPush -1))
                                    , A.onMouseEnter (CellHovering (xy -1 -1))
                                    , S.textAlignRight
                                    , S.widthRem 0.001
                                    , S.whiteSpaceNowrap
                                    ]
                                    [ text "0" ]
                                )
                            <|
                                List.indexedMap
                                    (\i col ->
                                        H.th
                                            [ A.onClick CellMouseClick
                                            , A.onMouseDown CellMouseDown
                                            , A.onMouseUp CellMouseUp
                                            , A.onMouseEnter (CellHovering (xy i -1))
                                            , S.textAlignLeft
                                            , S.verticalAlignBottom
                                            ]
                                            [ H.a [ A.href "#settings", S.displayInlineBlock, S.width "100%" ]
                                                [ text col.label ]
                                            ]
                                    )
                                <|
                                    Array.toList <|
                                        Result.withDefault Array.empty sheet.cols
                        ]
                    , H.tbody [] <|
                        Array.toList <|
                            Array.indexedMap
                                (\n row ->
                                    H.tr [] <|
                                        (::)
                                            (H.th
                                                [ A.onClick (SheetEditing (SheetRowPush n))
                                                , A.onMouseEnter (CellHovering (xy -1 n))
                                                , S.textAlignRight
                                                , S.widthRem 0.001
                                                , S.whiteSpaceNowrap
                                                ]
                                                -- TODO: The row number needs to be pre-filter.
                                                [ text (String.fromInt (n + 1)) ]
                                            )
                                        <|
                                            List.indexedMap
                                                (\i col ->
                                                    -- TODO: Don't allow editing if Virtual column.
                                                    H.td
                                                        [ A.onClick CellMouseClick
                                                        , A.onMouseDown CellMouseDown
                                                        , A.onMouseUp CellMouseUp
                                                        , A.onMouseEnter (CellHovering (xy i n))
                                                        , S.heightRem 1.25
                                                        , A.classList <|
                                                            let
                                                                { a, b } =
                                                                    sheet.select

                                                                between : number -> number -> number -> Bool
                                                                between a_ b_ i_ =
                                                                    min a_ b_ <= i_ && i_ <= max a_ b_

                                                                eq : number -> number -> number -> Bool
                                                                eq a_ b_ i_ =
                                                                    a_ == i_ && i_ == b_
                                                            in
                                                            [ ( "selected", (sheet.select /= rect -1 -1 -1 -1) && (between a.x b.x i || eq a.x b.x -1) && (between a.y b.y n || eq a.y b.y -1) )
                                                            ]
                                                        ]
                                                    <|
                                                        Maybe.withDefault
                                                            [ row
                                                                |> Dict.get col.key
                                                                |> Maybe.withDefault (E.string "")
                                                                |> D.decodeValue
                                                                    (case col.t of
                                                                        Link ->
                                                                            D.string |> D.map (\href -> H.a [ A.href href, S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.displayInlineBlock, S.maxWidthRem 12 ] [ text href ])

                                                                        _ ->
                                                                            D.map text string
                                                                    )
                                                                |> Result.withDefault (text "TODO: parse error")
                                                            ]
                                                        <|
                                                            case sheet.source of
                                                                Ok (Data { write }) ->
                                                                    if write /= Nothing && sheet.select == rect i n i n then
                                                                        Just [ H.input [ A.id "new-cell", A.value (Maybe.withDefault "" write), A.onInput (InputChanging CellWrite), A.onBlur (SheetEditing (SheetWrite sheet.select.a)), S.width "100%" ] [] ]

                                                                    else
                                                                        Nothing

                                                                _ ->
                                                                    Nothing
                                                )
                                            <|
                                                Array.toList <|
                                                    Result.withDefault Array.empty sheet.cols
                                )
                            <|
                                -- TODO: Better error.
                                Result.withDefault Array.empty sheet.rows
                    ]
                ]
            , H.aside [ S.displayFlex, S.flexDirectionColumn, S.minWidthRem 15 ] <|
                List.concat
                    [ [ H.span [] [ text (String.toLower (Debug.toString sheet.tool)) ]
                      ]
                    , case sheet.tool of
                        -- TODO: Hovering over columns/etc should highlight relevant cells, and vice versa.
                        Settings ->
                            -- TODO:
                            [ H.textarea [ A.onInput (always NoOp), S.minHeightRem 10 ]
                                -- TODO: Link to the column configs.
                                [ text (Debug.toString sheet.source)
                                ]
                            , H.div [ S.displayFlex, S.flexWrapWrap, S.justifyContentEnd ]
                                [ H.button [ A.onClick NoOp ] [ text "new column (C)" ]
                                ]
                            ]

                        Hints ->
                            -- TODO: problems (linting), ideas, related (sources/backlinks)
                            []

                        Stats stats ->
                            -- TODO:
                            []

                        Share ->
                            -- TODO:
                            []

                        History ->
                            -- TODO: contextual history
                            -- TODO: I like the idea of also linking to /:sheetId/history as another sheet from the tool.
                            []
                    ]
            ]
        ]
    }
