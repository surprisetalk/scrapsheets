port module Main exposing (main)

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
import Http
import Json.Decode as D
import Json.Encode as E
import Parser as P exposing ((|.), (|=), Parser)
import Regex exposing (Regex)
import Set exposing (Set)
import Task exposing (Task)
import Time exposing (Month(..))
import Url exposing (Url)
import Url.Parser as UrlP exposing ((</>))



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


result : Result a a -> a
result x =
    case x of
        Ok x_ ->
            x_

        Err x_ ->
            x_



---- PORTS --------------------------------------------------------------------


port changeSheet : SheetMsg (List Patch) -> Cmd msg


port notifySheet : SheetMsg E.Value -> Cmd msg


port sheetChanged : (SheetMsg SheetChange -> msg) -> Sub msg


port sheetNotified : (SheetMsg D.Value -> msg) -> Sub msg


type alias SheetChange =
    { doc : D.Value
    , handle : D.Value
    , patchInfo : D.Value
    , patches : List D.Value
    }


type alias SheetMsg a =
    { id : Id
    , data : a
    }


type alias Patch =
    { action : String
    , path : List D.Value
    , value : D.Value
    }



---- MAIN ---------------------------------------------------------------------


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        , onUrlChange = UrlChange
        , onUrlRequest = LinkClick
        }



---- MODEL --------------------------------------------------------------------


type alias Id =
    String


type alias Library =
    Dict Id SheetInfo


type alias Model =
    { nav : Nav.Key
    , library : Library
    , sheet : Sheet
    }


type alias SheetInfo =
    { name : String
    , tags : List String
    , thumb : Svg
    , peers : Peers
    }


type Peers
    = Private (Set Id)
    | Public


type alias Sheet =
    -- TODO: Add stats here? Or do they go down a level?
    { id : Id
    , name : String
    , tags : List String
    , thumb : Svg
    , search : String
    , tag : Maybe String
    , select : Rect
    , hover : Index
    , drag : Bool
    , write : Maybe String
    , schema : Result String Schema
    , rows : Result String (Array Row)

    -- TODO: Move tool up a level.
    , tool : Tool
    }


type alias Svg =
    -- TODO: Generate nice preview Svg based on sheet contents.
    ()


type Schema
    = Library
    | Doc (Array Col)
    | Net Net
    | Query Query_
    | Codex
    | Portal Args


type alias Query_ =
    { code : String
    , query : String
    , args : Args
    }


type alias Col =
    { key : Int
    , name : String
    , typ : Type
    }


type alias Row =
    Array D.Value


type Net
    = Hook
    | Http { url : String, interval : Int }
    | Socket { url : String }


type alias Args =
    Dict String D.Value


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
    | Boolean
    | Many Type
    | Link
    | Json
    | Timestamp
    | Image


type Tool
    = Settings
    | Hints
    | Stats
    | Share { following : Maybe () }
    | History


tools : Dict String Tool
tools =
    Dict.fromList
        [ ( "settings", Settings )
        , ( "stats", Stats )
        , ( "share", Share { following = Nothing } )
        , ( "hints", Hints )
        , ( "history", History )
        ]



---- PARSER -------------------------------------------------------------------


string : D.Decoder String
string =
    D.oneOf
        [ D.string
        , D.map String.fromInt D.int
        , D.map String.fromFloat D.float
        , D.null "NULL"
        , D.map (String.join ", ") (D.list (D.lazy (\_ -> string)))
        , D.map (String.join ", " << List.map (\( k, v ) -> k ++ ": " ++ v) << Dict.toList) (D.dict (D.lazy (\_ -> string)))
        , D.map (\c -> iif c "true" "false") D.bool
        ]


number : D.Decoder Float
number =
    D.oneOf
        [ D.float
        , D.map toFloat D.int
        , D.andThen (String.toFloat >> Maybe.map D.succeed >> Maybe.withDefault (D.fail "")) D.string
        ]


schemaDecoder : D.Decoder Schema
schemaDecoder =
    let
        types : Dict String Type
        types =
            Dict.fromList
                [ ( "bool", Boolean )
                , ( "number", Number )
                , ( "link", Link )
                , ( "image", Image )
                , ( "timestamp", Timestamp )
                , ( "json", Json )
                , ( "text", Text )
                ]

        col : D.Decoder Col
        col =
            D.map3 Col
                (D.index 2 D.int)
                (D.index 0 D.string)
                (D.index 1 D.string |> D.map (flip Dict.get types >> Maybe.withDefault Text))
    in
    D.field "type" D.string
        |> D.andThen
            (\typ ->
                D.field "data" <|
                    D.index 0 <|
                        case typ of
                            "doc" ->
                                D.map Doc (D.array col)

                            "net-hook" ->
                                D.succeed (Net Hook)

                            "net-socket" ->
                                D.map (\url -> Net (Socket { url = url }))
                                    (D.index 0 D.string)

                            "net-http" ->
                                D.map2 (\url interval -> Net (Http { url = url, interval = interval }))
                                    (D.index 0 D.string)
                                    (D.index 1 D.int)

                            "query" ->
                                D.map Query
                                    (D.map3 Query_
                                        (D.index 0 D.string)
                                        (D.index 1 D.string)
                                        (D.index 2 (D.dict D.value))
                                    )

                            typ_ ->
                                D.fail ("Bad table type: " ++ typ_)
            )


rowsDecoder : D.Decoder (Result String (Array Row))
rowsDecoder =
    D.field "type" D.string
        |> D.andThen
            (\typ ->
                D.field "data" <|
                    case typ of
                        "doc" ->
                            D.map (Ok << Array.slice 1 0) (D.array (D.array D.value))

                        _ ->
                            D.succeed (Err "Loading...")
            )



---- INIT ---------------------------------------------------------------------


type alias Flags =
    -- TODO: Use D.Value and a decoder instead.
    { library :
        List
            { id : String
            , name : String
            , tags : List String
            , peers : List Id
            }
    }


route : Url -> Model -> ( Model, Cmd Msg )
route url ({ sheet } as model) =
    let
        id : Id
        id =
            UrlP.parse (UrlP.top </> UrlP.string) url |> Maybe.withDefault ""

        tool : Tool
        tool =
            tools |> Dict.get (url.fragment |> Maybe.withDefault "") |> Maybe.withDefault Stats
    in
    model.library
        |> Dict.get id
        |> Maybe.map
            (\info ->
                ( { model
                    | sheet =
                        -- TODO: Set the schema based on the ID type.
                        { id = id
                        , name = info.name
                        , tags = info.tags
                        , thumb = info.thumb
                        , search = ""
                        , tag = Nothing
                        , select = Rect (xy -1 -1) (xy -1 -1)
                        , hover = xy -1 -1
                        , drag = False
                        , write = Nothing
                        , schema = Err "Loading..."
                        , rows = Err "Loading..."
                        , tool = tool
                        }
                  }
                , Cmd.none
                )
            )
        |> Maybe.withDefault ( { model | sheet = { sheet | tool = tool } }, Nav.pushUrl model.nav "/" )
        |> iif (id == model.sheet.id) ( { model | sheet = { sheet | tool = tool } }, Cmd.none )


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url nav =
    let
        library : Library
        library =
            flags.library
                |> List.map (\{ id, name, tags, peers } -> ( id, SheetInfo name tags () (Private (Set.fromList peers)) ))
                |> Dict.fromList
    in
    route url { nav = nav, library = library, sheet = librarySheet library }


librarySheet : Library -> Sheet
librarySheet library =
    { id = ""
    , name = ""
    , tags = []
    , thumb = ()
    , search = ""
    , tag = Nothing
    , select = Rect (xy -1 -1) (xy -1 -1)
    , hover = xy -1 -1
    , drag = False
    , write = Nothing
    , schema = Ok Library
    , rows = Err "TODO: list everything in library"
    , tool = Stats
    }



---- MESSAGES -----------------------------------------------------------------


type Msg
    = NoOp
    | UrlChange Url
    | LinkClick Browser.UrlRequest
    | LibraryChange (SheetMsg D.Value)
    | DocChange (SheetMsg SheetChange)
    | SchemaMsg SchemaMsg
    | KeyPress String
    | CellMouseClick
    | CellMouseDown
    | CellMouseUp
    | CellHover Index
    | InputChange Input String


type SchemaMsg
    = DocMsg DocMsg
    | QueryMsg ()


type DocMsg
    = SheetWrite Index
    | SheetRowPush Int
    | SheetColumnPush


type Input
    = SheetName
    | SheetTag
    | SheetSearch
    | CellWrite
    | ColumnType Int
    | ColumnKey Int
    | ColumnLabel Int



---- SUBSCRIPTIONS ------------------------------------------------------------


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ sheetChanged DocChange
        , Browser.onKeyPress (D.map KeyPress (D.field "key" D.string))
        ]



---- UPDATE -------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ sheet } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlChange url ->
            route url model

        LinkClick (Browser.Internal url) ->
            -- TODO: ?q=+any ?q=-any ?q==any
            ( model, Nav.pushUrl model.nav (Url.toString url) )

        LinkClick (Browser.External url) ->
            ( model, Nav.load url )

        LibraryChange data ->
            ( { model
                | library =
                    data.data
                        |> D.decodeValue
                            (D.map4 SheetInfo
                                (D.oneOf [ D.field "name" D.string, D.succeed "" ])
                                (D.oneOf [ D.field "tags" (D.list D.string), D.succeed [] ])
                                (D.succeed ())
                                (D.succeed Public)
                            )
                        |> Result.withDefault (SheetInfo "" [] () Public)
                        |> flip (Dict.insert data.id) model.library
              }
            , Cmd.none
            )

        DocChange data ->
            ( { model
                | sheet =
                    iif (data.id /= sheet.id)
                        sheet
                        { sheet
                            | schema = data.data.doc |> D.decodeValue schemaDecoder |> Result.mapError D.errorToString
                            , rows = data.data.doc |> D.decodeValue rowsDecoder |> Result.mapError D.errorToString |> Result.andThen identity
                        }
              }
            , Cmd.none
            )

        _ ->
            -- TODO:
            ( model, Cmd.none )



--         InputChanging SheetName x ->
--             -- TODO:
--             ( model, Cmd.none )
--
--         InputChanging SheetTag x ->
--             -- TODO:
--             ( model, Cmd.none )
--
--         InputChanging SheetSearch x ->
--             -- TODO:
--             ( model, Cmd.none )
--
--         InputChanging CellWrite x ->
--             ( { model
--                 | sheet =
--                     { sheet
--                         | source =
--                             case sheet.source of
--                                 Ok (Data rows) ->
--                                     Ok (Data { rows | write = Just x })
--
--                                 source ->
--                                     source
--                     }
--               }
--             , Cmd.none
--             )
--
--         InputChanging (ColumnType i) x ->
--             -- TODO:
--             ( model, Cmd.none )
--
--         InputChanging (ColumnKey i) x ->
--             -- TODO:
--             ( model, Cmd.none )
--
--         InputChanging (ColumnLabel i) x ->
--             -- TODO:
--             ( model, Cmd.none )
--
--         SheetEditing edit ->
--             ( { model
--                 | sheet =
--                     { sheet
--                         | source =
--                             case sheet.source of
--                                 Ok (Data rows) ->
--                                     Ok (Data { rows | write = Nothing })
--
--                                 source ->
--                                     source
--                     }
--               }
--             , changeSheet <|
--                 DocMsg sheet.sheetId <|
--                     case sheet.source of
--                         Ok (Data rows) ->
--                             case edit of
--                                 SheetWrite { x, y } ->
--                                     -- TODO: what if we have no columns?
--                                     sheet.cols
--                                         |> Result.withDefault Array.empty
--                                         |> Array.get x
--                                         |> Maybe.map
--                                             (\col ->
--                                                 [ { action = "cell-put"
--                                                   , path = [ E.string "rows", E.int y, E.string col.key ]
--                                                   , value = rows.write |> Maybe.withDefault "" |> E.string
--                                                   }
--                                                 ]
--                                             )
--                                         |> Maybe.withDefault []
--
--                                 SheetRowPush i ->
--                                     [ { action = "row-insert"
--                                       , path = [ E.string "rows", E.int i ]
--                                       , value = E.object []
--                                       }
--                                     ]
--
--                                 SheetColumnPush ->
--                                     [ { action = "col-insert"
--                                       , path = [ E.string "cols", E.int (Array.length (Result.withDefault Array.empty sheet.cols)) ]
--                                       , value = E.object [ ( "key", E.string "" ), ( "label", E.string "" ), ( "type", E.string "same" ) ]
--                                       }
--                                     ]
--
--                         _ ->
--                             []
--             )
--
--         CellMouseClick ->
--             ( { model
--                 | sheet =
--                     { sheet
--                         | select = { a = sheet.hover, b = sheet.hover }
--                         , source =
--                             case sheet.source of
--                                 Ok (Data rows) ->
--                                     -- TODO: Fill in cell value.
--                                     Ok (Data { rows | write = Just "" })
--
--                                 _ ->
--                                     sheet.source
--                     }
--               }
--             , Task.attempt (always NoOp) (Dom.focus "new-cell")
--             )
--
--         CellMouseDown ->
--             ( { model | sheet = { sheet | click = True, select = Rect sheet.hover sheet.select.b } }, Cmd.none )
--
--         CellMouseUp ->
--             ( { model | sheet = { sheet | click = False, select = Rect sheet.select.a sheet.hover } }, Cmd.none )
--
--         CellHovering hover ->
--             let
--                 select =
--                     sheet.select
--
--                 select_ =
--                     if sheet.click then
--                         { select | b = hover }
--
--                     else
--                         select
--             in
--             ( { model | sheet = { sheet | hover = hover, select = select_ } }, Cmd.none )
--
--         KeyPressed "Enter" ->
--             ( model, Task.attempt (always NoOp) (Dom.blur "new-cell") )
--
--         KeyPressed "Backspace" ->
--             ( { model
--                 | sheet =
--                     { sheet
--                         | source =
--                             case sheet.source of
--                                 Ok (Data rows) ->
--                                     Ok (Data { rows | write = Nothing })
--
--                                 source ->
--                                     source
--                     }
--               }
--             , changeSheet <|
--                 DocMsg sheet.sheetId <|
--                     case sheet.source of
--                         Ok (Data rows) ->
--                             -- TODO: Do multiple patches when ranges are selected.
--                             case ( negate sheet.select.a.y, negate sheet.select.a.x ) of
--                                 ( 1, 1 ) ->
--                                     []
--
--                                 ( 1, i ) ->
--                                     [ { action = "col-del"
--                                       , path = [ E.string "cols", E.int (negate i) ]
--                                       , value = E.object []
--                                       }
--                                     ]
--
--                                 ( i, 1 ) ->
--                                     [ { action = "row-del"
--                                       , path = [ E.string "rows", E.int (negate i) ]
--                                       , value = E.object []
--                                       }
--                                     ]
--
--                                 ( y, x ) ->
--                                     sheet.cols
--                                         |> Result.withDefault Array.empty
--                                         |> Array.get (negate x)
--                                         |> Maybe.map
--                                             (\col ->
--                                                 [ { action = "cell-put"
--                                                   , path = [ E.string "rows", E.int (negate y), E.string col.key ]
--                                                   , value = rows.write |> Maybe.withDefault "" |> E.string
--                                                   }
--                                                 ]
--                                             )
--                                         |> Maybe.withDefault []
--
--                         _ ->
--                             []
--             )
--
--         KeyPressed _ ->
--             ( model, Cmd.none )
--
--
--
---- VIEW ---------------------------------------------------------------------


view : Model -> Browser.Document Msg
view ({ sheet } as model) =
    let
        info : SheetInfo
        info =
            model.library
                |> Dict.get sheet.id
                |> Maybe.withDefault { name = "", tags = [], thumb = (), peers = Public }

        cols : Result String (Array Col)
        cols =
            case sheet.schema of
                Ok (Doc doc) ->
                    Ok doc

                Ok Library ->
                    [ ( "sheet_id", Link )
                    , ( "name", Text )
                    , ( "tags", Many Text )
                    ]
                        |> List.indexedMap (\i ( k, t ) -> Col i k t)
                        |> Array.fromList
                        |> Ok

                Ok (Net (Socket ws)) ->
                    Ok (Array.fromList [ Col 0 "Payload" Json ])

                Ok (Net (Http ws)) ->
                    Ok (Array.fromList [ Col 0 "Payload" Json ])

                Ok (Query query) ->
                    Ok (Array.fromList [ Col 0 "Row" Json ])

                Err err ->
                    Err err

                _ ->
                    Err "TODO: unimplemented"
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
        , H.div [ S.displayFlex, S.flexDirectionRow, S.paddingRem 2, S.paddingTopRem 1, S.gapRem 2, S.userSelectNone, S.cursorPointer, A.style "-webkit-user-select" "none", S.maxWidth "100vw", S.maxHeight "100vh" ]
            [ H.main_ [ S.displayFlex, S.flexDirectionColumn, S.height "100%", S.width "100%", S.maxWidth "80vw", S.maxHeight "100vh" ]
                [ H.div [ S.displayFlex, S.flexDirectionRow, S.justifyContentSpaceBetween, S.alignItemsBaseline ]
                    [ H.div [ S.displayFlex, S.flexDirectionRow, S.alignItemsBaseline, S.gapRem 0.5 ] <|
                        -- Badges indicate scrapscript news, book notifs, etc.
                        List.concat
                            [ [ H.a [ A.href "/", S.fontWeightBold ] [ text "scrapsheets", H.sup [] [ text "" ] ]
                              ]
                            , iif (sheet.id == "")
                                [ text "/"
                                , H.span [] [ text "library" ]
                                ]
                                [ text "/"
                                , H.a [ A.href "#settings" ] [ text info.name ]
                                , H.div [ S.displayFlex, S.flexDirectionRow, S.alignItemsBaseline, S.gapRem 0.5 ] <|
                                    List.concat
                                        [ case sheet.tag of
                                            Nothing ->
                                                [ H.button [ A.onClick (InputChange SheetTag "") ] [ text "#" ] ]

                                            Just value ->
                                                [ H.input [ A.value value, A.onInput (InputChange SheetTag) ] [] ]
                                        , List.map (\tag -> H.a [ A.href ("?q=+tag:" ++ tag), S.fontSizeRem 0.7, S.opacity "0.8" ] [ text ("#" ++ tag) ]) info.tags
                                        ]
                                ]
                            ]
                    , H.div [ S.displayFlex, S.flexDirectionRowReverse, S.alignItemsBaseline, S.gapRem 0.5 ]
                        -- TODO: If we need to, we can collapse the tools into a dropdown that only shows the current one.
                        [ H.a [ A.href "#settings" ] [ text "settings", H.sup [] [ text "" ] ]
                        , H.a [ A.href "#history" ] [ text "history", H.sup [] [ text "4" ] ]
                        , H.a [ A.href "#hints" ] [ text "hints", H.sup [] [ text "3" ] ]
                        , H.a [ A.href "#stats" ] [ text "stats", H.sup [] [ text "2" ] ]
                        , H.a [ A.href "#share" ] [ text "share", H.sup [] [ text "4" ] ]

                        -- TODO: This doesn't make sense.
                        , case info.peers of
                            Public ->
                                text ""

                            Private peers ->
                                H.div [ S.displayFlex, S.flexDirectionRowReverse, S.gapRem 0.5 ] <|
                                    List.map (\peer -> H.a [ A.href "?following=" ] [ text peer ]) <|
                                        Set.toList <|
                                            peers
                        ]
                    ]

                -- All current filters should be rendered as text in the searchbar.
                -- This helps people (1) learn the language and (2) indicate that they're searching rather than editing.
                , H.input [ A.value sheet.search, A.onInput (InputChange SheetSearch), S.width "100%" ] []

                -- TODO: https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed
                , H.div [ S.overflowAuto ]
                    [ case cols of
                        Err err ->
                            H.p [] [ text err ]

                        Ok cols_ ->
                            H.table [ S.borderCollapseCollapse, S.width "100%", A.onMouseLeave (CellHover (xy -1 -1)) ]
                                [ H.thead []
                                    [ H.tr [] <|
                                        -- TODO: Add additional header rows for stats and column def.
                                        (::)
                                            (H.th
                                                [ A.onClick (SchemaMsg (DocMsg (SheetRowPush -1)))
                                                , A.onMouseEnter (CellHover (xy -1 -1))
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
                                                        , A.onMouseEnter (CellHover (xy i -1))
                                                        , case col.typ of
                                                            Number ->
                                                                S.textAlignRight

                                                            _ ->
                                                                S.textAlignLeft
                                                        , S.verticalAlignBottom
                                                        ]
                                                        [ H.a [ A.href "#settings", S.displayInlineBlock, S.width "100%" ]
                                                            [ text col.name ]
                                                        ]
                                                )
                                            <|
                                                Array.toList cols_
                                    ]
                                , H.tbody [] <|
                                    Array.toList <|
                                        Array.indexedMap
                                            (\n row ->
                                                H.tr [] <|
                                                    (::)
                                                        (H.th
                                                            [ A.onClick (SchemaMsg (DocMsg (SheetRowPush n)))
                                                            , A.onMouseEnter (CellHover (xy -1 n))
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
                                                                    , A.onMouseEnter (CellHover (xy i n))
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
                                                                            |> Array.get col.key
                                                                            |> Maybe.withDefault (E.string "")
                                                                            |> D.decodeValue
                                                                                (case col.typ of
                                                                                    Link ->
                                                                                        D.string |> D.map (\href -> H.a [ A.href href, S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.displayInlineBlock, S.maxWidthRem 12 ] [ text href ])

                                                                                    Image ->
                                                                                        D.string |> D.map (\src -> H.img [ A.src src, S.width "100%", S.objectFitCover ] [])

                                                                                    Text ->
                                                                                        D.map text string

                                                                                    Boolean ->
                                                                                        D.map (\c -> H.input [ A.type_ "checkbox", A.checked c ] []) D.bool

                                                                                    Number ->
                                                                                        D.map (H.span [ S.textAlignRight ] << List.singleton << text) string

                                                                                    _ ->
                                                                                        D.map text string
                                                                                )
                                                                            |> Result.mapError (D.errorToString >> text)
                                                                            |> result
                                                                        ]
                                                                    <|
                                                                        if sheet.write /= Nothing && sheet.select == rect i n i n then
                                                                            Just [ H.input [ A.id "new-cell", A.value (Maybe.withDefault "" sheet.write), A.onInput (InputChange CellWrite), A.onBlur (SchemaMsg (DocMsg (SheetWrite sheet.select.a))), S.width "100%" ] [] ]

                                                                        else
                                                                            Nothing
                                                            )
                                                        <|
                                                            Array.toList cols_
                                            )
                                        <|
                                            -- TODO: Proper error handling.
                                            Result.withDefault Array.empty
                                            <|
                                                sheet.rows
                                ]
                    ]
                ]
            , H.aside [ S.displayFlex, S.flexDirectionColumn, S.minWidthRem 12, S.maxWidthRem 18, S.maxHeight "100vh", S.overflowHidden, S.overflowYAuto ] <|
                List.concat
                    [ [ H.span [] [ text (String.toLower (Debug.toString sheet.tool)), H.sup [] [ text "" ] ]
                      ]
                    , case sheet.tool of
                        -- TODO: Hovering over columns/etc should highlight relevant cells, and vice versa.
                        Settings ->
                            case sheet.schema of
                                Ok (Query query) ->
                                    [ H.textarea [ A.onInput (always NoOp), S.minHeightRem 10, S.height "100%", S.whiteSpaceNowrap, S.overflowXAuto, S.fontSizeRem 0.75 ]
                                        [ text (String.trim query.query)
                                        ]
                                    ]

                                _ ->
                                    -- TODO:
                                    [ H.textarea [ A.onInput (always NoOp), S.minHeightRem 10, S.height "100%" ]
                                        -- TODO: Link to the column configs.
                                        [ text (Debug.toString sheet.schema)
                                        ]
                                    , H.div [ S.displayFlex, S.flexWrapWrap, S.justifyContentEnd, S.alignItemsBaseline ]
                                        [ H.button [ A.onClick NoOp ] [ text "new column (C)" ]
                                        ]
                                    ]

                        Hints ->
                            -- TODO: problems (linting), ideas, related (sources/backlinks)
                            []

                        Stats ->
                            case cols of
                                Err _ ->
                                    [ H.p [] [ text "No data available" ] ]

                                Ok cols_ ->
                                    cols_
                                        |> Array.toList
                                        |> List.map
                                            (\col ->
                                                let
                                                    values =
                                                        sheet.rows
                                                            -- TODO: Proper error handling.
                                                            |> Result.withDefault Array.empty
                                                            |> Array.toList
                                                            |> List.filterMap (Array.get col.key)

                                                    stats =
                                                        case col.typ of
                                                            Number ->
                                                                let
                                                                    numbers =
                                                                        values
                                                                            |> List.filterMap (D.decodeValue number >> Result.toMaybe)

                                                                    count =
                                                                        List.length numbers

                                                                    min_ =
                                                                        List.minimum numbers

                                                                    max_ =
                                                                        List.maximum numbers

                                                                    mean =
                                                                        if count > 0 then
                                                                            Just (List.sum numbers / toFloat count)

                                                                        else
                                                                            Nothing
                                                                in
                                                                [ ( "Count", String.fromInt count )
                                                                , ( "Min", min_ |> Maybe.map String.fromFloat |> Maybe.withDefault "-" )
                                                                , ( "Max", max_ |> Maybe.map String.fromFloat |> Maybe.withDefault "-" )
                                                                , ( "Mean", mean |> Maybe.map (\m -> String.fromFloat (toFloat (round (m * 100)) / 100)) |> Maybe.withDefault "-" )
                                                                ]

                                                            Text ->
                                                                let
                                                                    strings =
                                                                        values
                                                                            |> List.filterMap (D.decodeValue string >> Result.toMaybe)

                                                                    count =
                                                                        List.length strings

                                                                    avgLength =
                                                                        if count > 0 then
                                                                            List.sum (List.map String.length strings) // count

                                                                        else
                                                                            0

                                                                    -- Count occurrences of each value
                                                                    valueCounts =
                                                                        strings
                                                                            |> List.foldr
                                                                                (\str acc ->
                                                                                    Dict.update str
                                                                                        (\maybeCount ->
                                                                                            case maybeCount of
                                                                                                Nothing ->
                                                                                                    Just 1

                                                                                                Just n ->
                                                                                                    Just (n + 1)
                                                                                        )
                                                                                        acc
                                                                                )
                                                                                Dict.empty

                                                                    -- Get top 3 most frequent values
                                                                    topValues =
                                                                        valueCounts
                                                                            |> Dict.toList
                                                                            |> List.sortBy (Tuple.second >> negate)
                                                                            |> List.take 3
                                                                            |> List.map
                                                                                (\( val, cnt ) ->
                                                                                    if String.length val > 20 then
                                                                                        String.left 17 val ++ "... (" ++ String.fromInt cnt ++ ")"

                                                                                    else
                                                                                        val ++ " (" ++ String.fromInt cnt ++ ")"
                                                                                )
                                                                in
                                                                List.concat
                                                                    [ [ ( "Count", String.fromInt count )
                                                                      , ( "Length", String.fromInt avgLength )
                                                                      ]
                                                                    , if List.isEmpty topValues then
                                                                        []

                                                                      else
                                                                        [ ( "Frequent", String.join " " topValues ) ]
                                                                    ]

                                                            Boolean ->
                                                                let
                                                                    bools =
                                                                        values
                                                                            |> List.filterMap (D.decodeValue D.bool >> Result.toMaybe)

                                                                    count =
                                                                        List.length bools

                                                                    trueCount =
                                                                        bools |> List.filter identity |> List.length

                                                                    falseCount =
                                                                        count - trueCount
                                                                in
                                                                [ ( "Count", String.fromInt count )
                                                                , ( "True", String.fromInt trueCount )
                                                                , ( "False", String.fromInt falseCount )
                                                                ]

                                                            _ ->
                                                                [ ( "Count", String.fromInt (List.length values) ) ]
                                                in
                                                H.div [ S.displayFlex, S.flexWrapWrap, S.alignItemsBaseline, S.gapRem 0.5, S.fontSizeRem 0.65, S.opacity "0.8" ] <|
                                                    H.span [ S.fontWeightBold ] [ text col.name ]
                                                        :: (stats
                                                                |> List.map
                                                                    (\( label, value ) ->
                                                                        H.div [ S.opacity "0.8" ] [ text (label ++ ": " ++ value) ]
                                                                    )
                                                           )
                                            )

                        Share share ->
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
