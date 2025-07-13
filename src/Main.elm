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


type alias Model =
    { nav : Nav.Key
    , library : Dict Id SheetInfo
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
    , table : Result String Table
    , tool : Tool
    }


type alias Svg =
    -- TODO: Generate nice preview Svg based on sheet contents.
    ()


type alias Table =
    { schema : Schema
    , rows : Result String (Array Row)
    }


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


tableDecoder : D.Decoder Table
tableDecoder =
    let
        col : D.Decoder Col
        col =
            D.map3 Col
                (D.field "key" D.int)
                (D.field "name" D.string)
                (D.field "type" D.string
                    |> D.map
                        (\typ_ ->
                            case typ_ of
                                "bool" ->
                                    Boolean

                                "number" ->
                                    Number

                                "link" ->
                                    Link

                                "image" ->
                                    Image

                                "timestamp" ->
                                    Timestamp

                                "json" ->
                                    Json

                                _ ->
                                    Text
                        )
                )
    in
    D.field "type" D.string
        |> D.andThen
            (\typ ->
                case typ of
                    "doc" ->
                        D.map Doc
                            (D.map2 Doc
                                (D.field "cols" (D.array col))
                                (D.field "rows"
                                    (D.array
                                        (D.oneOf
                                            [ D.map (Dict.toList >> List.map (Tuple.mapFirst (Maybe.withDefault -1 << String.toInt)) >> Dict.fromList) (D.dict D.value)
                                            , D.map (Dict.fromList << Array.toIndexedList) (D.array D.value)
                                            ]
                                        )
                                    )
                                )
                            )

                    "net" ->
                        D.field "net" D.string
                            |> D.andThen
                                (\feed ->
                                    case feed of
                                        "websocket" ->
                                            D.map2 (\url rows -> Net (Socket { url = url, rows = Maybe.withDefault Array.empty rows }))
                                                (D.field "url" D.string)
                                                (D.maybe (D.field "rows" (D.array D.value)))

                                        "http" ->
                                            D.map2 (\url rows -> Net (Http { url = url, rows = Maybe.withDefault Array.empty rows }))
                                                (D.field "url" D.string)
                                                (D.maybe (D.field "rows" (D.array D.value)))

                                        feed_ ->
                                            D.fail ("Bad feed type: " ++ feed_)
                                )

                    "query" ->
                        D.map Query
                            (D.map2 Query_
                                (D.field "query" D.string)
                                (D.map (Maybe.withDefault Array.empty) (D.maybe (D.field "rows" (D.array D.value))))
                            )

                    typ_ ->
                        D.fail ("Bad table type: " ++ typ_)
            )



---- INIT ---------------------------------------------------------------------


type alias Flags =
    { sheet : D.Value
    }


route : Url -> Model -> ( Model, Cmd Msg )
route url ({ sheet } as model) =
    -- TODO: Store the ID in the path instead and serve from local server.
    let
        id : Id
        id =
            url.fragment |> Maybe.withDefault ""

        tool : Tool
        tool =
            tools |> Dict.get id |> Maybe.withDefault sheet.tool
    in
    ( { model | sheet = { sheet | tool = tool } }
    , if Dict.member id tools then
        Cmd.none

      else
        Nav.pushUrl model.nav (Url.toString url)
    )


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url nav =
    route url
        { nav = nav
        , library = Dict.empty
        , sheet = { default | table = flags.sheet |> D.decodeValue tableDecoder |> Result.mapError D.errorToString }
        }


default : Sheet
default =
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
    , table = Err "TODO: not loaded"
    , tool = Stats
    }



---- MESSAGES -----------------------------------------------------------------


type Msg
    = NoOp
    | UrlChange Url
    | LinkClick Browser.UrlRequest
    | LibraryChange (SheetMsg D.Value)
    | DocChange (SheetMsg SheetChange)
    | TableMsg TableMsg
    | KeyPress String
    | CellMouseClick
    | CellMouseDown
    | CellMouseUp
    | CellHover Index
    | InputChange Input String


type TableMsg
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
            -- TODO: We should eventually change #:sheetId to /:sheetId, but for now it confuses stupid local webserver.
            let
                id : String
                id =
                    url.fragment |> Maybe.withDefault ""
            in
            ( { model
                | sheet =
                    [ { default | id = "", name = "library", table = Ok Library }
                    , { default | id = "device", name = "device", table = Ok (Feed Device) }
                    , { default | id = "shop", name = "shop", table = Ok (Feed Shop) }
                    ]
                        |> List.map (\s -> ( s.id, s ))
                        |> Dict.fromList
                        |> Dict.get id
                        |> Maybe.withDefault model.sheet
              }
            , Cmd.none
            )

        LinkClick (Browser.Internal url) ->
            -- TODO: ?q=+any ?q=-any ?q==any
            route url model

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
            -- TODO:
            ( { model
                | sheet =
                    { default
                        | id = data.id
                        , table = data.data.doc |> D.decodeValue tableDecoder |> Result.mapError D.errorToString
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

        table : Result String Doc
        table =
            case sheet.table of
                Err err ->
                    Err err

                Ok (Doc doc) ->
                    Ok doc

                Ok Library ->
                    Ok
                        { cols =
                            [ ( "sheet_id", Link )
                            , ( "name", Text )
                            , ( "tags", Many Text )
                            ]
                                |> List.indexedMap (\i ( k, t ) -> Col i k t)
                                |> Array.fromList
                        , rows =
                            model.library
                                |> Dict.toList
                                |> List.sortBy (Tuple.second >> .tags >> String.join ", ")
                                |> List.map
                                    (\( id, s ) ->
                                        Dict.fromList <|
                                            List.indexedMap Tuple.pair
                                                [ E.string ("#" ++ id)
                                                , E.string s.name
                                                , E.list E.string s.tags
                                                ]
                                    )
                                |> (++)
                                    [ Dict.fromList <|
                                        List.indexedMap Tuple.pair
                                            [ E.string "/"
                                            , E.string "library"
                                            , E.list E.string []
                                            ]
                                    , Dict.fromList <|
                                        List.indexedMap Tuple.pair
                                            [ E.string "#shop"
                                            , E.string "shop"
                                            , E.list E.string []
                                            ]
                                    , Dict.fromList <|
                                        List.indexedMap Tuple.pair
                                            [ E.string "#device"
                                            , E.string "device"
                                            , E.list E.string []
                                            ]
                                    ]
                                |> Array.fromList
                        }

                Ok (Feed Shop) ->
                    Ok
                        -- TODO:
                        { cols =
                            Array.fromList
                                [ Col 6 "Buy" Text, Col 0 "Type" Text, Col 1 "Publisher" Text, Col 2 "Name" Text, Col 3 "Downloads" Number, Col 4 "Rating" Number, Col 5 "Price" Number ]
                        , rows =
                            [ [ E.string "data", E.string "ScrapeSheets", E.string "Used Car Database", E.int 15420, E.float 4.7, E.float 4.2, E.string "Buy" ]
                            , [ E.string "data", E.string "DataBroker", E.string "Social Network Connections", E.int 34567, E.float 3.5, E.float 179.99, E.string "Buy" ]
                            , [ E.string "data", E.string "BizTools", E.string "Project Planning Suite", E.int 28456, E.float 4.7, E.float 8.99, E.string "Buy" ]
                            , [ E.string "code", E.string "ScrapeSheets", E.string "Email Validator Pro", E.int 21456, E.float 4.5, E.float 0.75, E.string "Buy" ]
                            , [ E.string "data", E.string "WeatherAPI", E.string "Historical Climate Data", E.int 14567, E.float 4.0, E.float 24.99, E.string "Buy" ]
                            ]
                                |> List.map (List.indexedMap Tuple.pair >> Dict.fromList)
                                |> Array.fromList
                        }

                Ok (Feed (Websocket ws)) ->
                    Ok
                        { cols = Array.fromList [ Col 0 "Payload" Json ]
                        , rows = ws.rows |> Array.map (Dict.singleton 0)
                        }

                Ok (Feed (Http ws)) ->
                    Ok
                        { cols = Array.fromList [ Col 0 "Payload" Json ]
                        , rows = ws.rows |> Array.map (Dict.singleton 0)
                        }

                Ok (Query query) ->
                    Ok
                        { cols = Array.fromList [ Col 0 "Row" Json ]
                        , rows = query.rows |> Array.map (Dict.singleton 0)
                        }

                error ->
                    Err ("Unimplemented: " ++ Debug.toString error)
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
                                        Dict.keys <|
                                            peers
                        ]
                    ]

                -- All current filters should be rendered as text in the searchbar.
                -- This helps people (1) learn the language and (2) indicate that they're searching rather than editing.
                , H.input [ A.value sheet.search, A.onInput (InputChange SheetSearch), S.width "100%" ] []

                -- TODO: https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed
                , H.div [ S.overflowAuto ]
                    [ case table of
                        Err err ->
                            H.p [] [ text err ]

                        Ok doc ->
                            H.table [ S.borderCollapseCollapse, S.width "100%", A.onMouseLeave (CellHover (xy -1 -1)) ]
                                [ H.thead []
                                    [ H.tr [] <|
                                        -- TODO: Add additional header rows for stats and column def.
                                        (::)
                                            (H.th
                                                [ A.onClick (TableMsg (DocMsg (SheetRowPush -1)))
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
                                                Array.toList doc.cols
                                    ]
                                , H.tbody [] <|
                                    Array.toList <|
                                        Array.indexedMap
                                            (\n row ->
                                                H.tr [] <|
                                                    (::)
                                                        (H.th
                                                            [ A.onClick (TableMsg (DocMsg (SheetRowPush n)))
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
                                                                            |> Dict.get col.key
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
                                                                            Just [ H.input [ A.id "new-cell", A.value (Maybe.withDefault "" sheet.write), A.onInput (InputChange CellWrite), A.onBlur (TableMsg (DocMsg (SheetWrite sheet.select.a))), S.width "100%" ] [] ]

                                                                        else
                                                                            Nothing
                                                            )
                                                        <|
                                                            Array.toList doc.cols
                                            )
                                            doc.rows
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
                            case sheet.table of
                                Ok (Query query) ->
                                    [ H.textarea [ A.onInput (always NoOp), S.minHeightRem 10, S.height "100%", S.whiteSpaceNowrap, S.overflowXAuto, S.fontSizeRem 0.75 ]
                                        [ text (String.trim query.query)
                                        ]
                                    ]

                                _ ->
                                    -- TODO:
                                    [ H.textarea [ A.onInput (always NoOp), S.minHeightRem 10, S.height "100%" ]
                                        -- TODO: Link to the column configs.
                                        [ text (Debug.toString sheet.table)
                                        ]
                                    , H.div [ S.displayFlex, S.flexWrapWrap, S.justifyContentEnd, S.alignItemsBaseline ]
                                        [ H.button [ A.onClick NoOp ] [ text "new column (C)" ]
                                        ]
                                    ]

                        Hints ->
                            -- TODO: problems (linting), ideas, related (sources/backlinks)
                            []

                        Stats ->
                            case table of
                                Err _ ->
                                    [ H.p [] [ text "No data available" ] ]

                                Ok doc ->
                                    doc.cols
                                        |> Array.toList
                                        |> List.map
                                            (\col ->
                                                let
                                                    values =
                                                        doc.rows
                                                            |> Array.toList
                                                            |> List.filterMap (Dict.get col.key)

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
