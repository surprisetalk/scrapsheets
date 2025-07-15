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


port librarySynced : (D.Value -> msg) -> Sub msg


port updateLibrary : Idd { name : Maybe String, tags : Maybe (List String) } -> Cmd msg


port changeId : Id -> Cmd msg


port newDoc : E.Value -> Cmd msg


port deleteDoc : String -> Cmd msg


port changeDoc : Idd (List Patch) -> Cmd msg


port notifyDoc : Idd E.Value -> Cmd msg


port docSelected : (Idd { doc : D.Value } -> msg) -> Sub msg


port docChanged : (Idd DocDelta -> msg) -> Sub msg


port docNotified : (Idd D.Value -> msg) -> Sub msg


port docQueried : (Idd D.Value -> msg) -> Sub msg


type alias DocDelta =
    -- TODO: This should only include the deltas and not the full doc.
    { doc : D.Value
    , handle : D.Value
    , patchInfo : D.Value
    , patches : List D.Value
    }


type alias Idd a =
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
    , id : String
    , tool : Tool
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
    { id : Id
    , search : String
    , select : Rect
    , hover : Index
    , drag : Bool
    , write : Maybe String
    , doc : Result String Doc
    , table : Result String Table
    , stats : Result String Stats
    }


type alias Svg =
    -- TODO: Generate nice preview Svg based on sheet contents.
    ()


type Doc
    = Library
    | Tab Table
    | Net Net
    | Query Query_
    | Codex
    | Portal Args
    | Template ( String, Array Row )


type alias Stats =
    ()


type alias Table =
    { cols : Array Col
    , rows : Array Row
    }


type alias Query_ =
    { code : String
    , query : String
    , args : Args
    }


type alias Col =
    { key : String
    , name : String
    , typ : Type
    }


type alias Row =
    Dict String D.Value


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
    | Delete
    | Form (Dict String String)


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


docDecoder : D.Decoder Doc
docDecoder =
    D.field "type" D.string
        |> D.andThen
            (\typ ->
                case typ of
                    "library" ->
                        D.succeed Library

                    "table" ->
                        D.field "data" <|
                            D.map Tab tableDecoder

                    "net-hook" ->
                        D.succeed (Net Hook)

                    "portal" ->
                        D.field "data" <|
                            D.map Portal <|
                                D.fail "TODO: args"

                    "net-socket" ->
                        D.field "data" <|
                            D.index 0 <|
                                D.map (\url -> Net (Socket { url = url }))
                                    (D.field "url" D.string)

                    "net-http" ->
                        D.field "data" <|
                            D.index 0 <|
                                D.map2 (\url interval -> Net (Http { url = url, interval = interval }))
                                    (D.field "url" D.string)
                                    (D.field "interval" D.int)

                    "query" ->
                        D.field "data" <|
                            D.index 0 <|
                                D.map Query
                                    (D.map3 Query_
                                        (D.field "lang" D.string)
                                        (D.field "code" D.string)
                                        -- TODO
                                        (D.field "args" (D.succeed Dict.empty))
                                    )

                    "template" ->
                        D.field "data" <|
                            D.index 0 <|
                                D.map Template <|
                                    D.map2 Tuple.pair
                                        (D.field "type" D.string)
                                        (D.field "data" (D.array rowDecoder))

                    typ_ ->
                        D.fail ("Bad table type: " ++ typ_)
            )


tableDecoder : D.Decoder Table
tableDecoder =
    D.map2 Table
        (D.index 0 (D.array colDecoder))
        (D.map (Array.slice 1 -1 << Array.push Dict.empty) (D.array rowDecoder))


rowDecoder : D.Decoder Row
rowDecoder =
    D.oneOf
        [ D.array D.value |> D.map (Array.toIndexedList >> List.map (\( k, v ) -> ( String.fromInt k, v )) >> Dict.fromList)
        , D.dict D.value
        ]


colDecoder : D.Decoder Col
colDecoder =
    let
        types : Dict String Type
        types =
            Dict.fromList
                [ ( "bool", Boolean )
                , ( "number", Number )
                , ( "link", Link )
                , ( "image", Image )
                , ( "form", Form Dict.empty )
                , ( "timestamp", Timestamp )
                , ( "json", Json )
                , ( "text", Text )
                ]
    in
    D.map3 Col
        (D.field "key" string)
        (D.field "name" D.string)
        (D.field "type" D.string |> D.map (flip Dict.get types >> Maybe.withDefault Text))



---- INIT ---------------------------------------------------------------------


type alias Flags =
    {}


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url nav =
    let
        model : Model
        model =
            route url
                { nav = nav
                , id = ""
                , tool = Stats
                , library = Dict.empty
                , sheet =
                    { id = ""
                    , search = ""
                    , select = Rect (xy -1 -1) (xy -1 -1)
                    , hover = xy -1 -1
                    , drag = False
                    , write = Nothing
                    , doc = Err ""
                    , table = Err ""
                    , stats = Err ""
                    }
                }
    in
    ( model, changeId model.id )


route : Url -> Model -> Model
route url model =
    { model
        | id = UrlP.parse (UrlP.top </> UrlP.string) url |> Maybe.withDefault ""
        , tool = tools |> Dict.get (url.fragment |> Maybe.withDefault "") |> Maybe.withDefault model.tool
    }



---- MESSAGES -----------------------------------------------------------------


type Msg
    = NoOp
    | UrlChange Url
    | LinkClick Browser.UrlRequest
    | LibrarySync D.Value
    | DocSelect (Idd { doc : D.Value })
    | DocChange (Idd DocDelta)
    | DocNotify (Idd D.Value)
    | DocQuery (Idd D.Value)
    | DocMsg DocMsg
    | DocNew
    | DocDelete Id
    | KeyPress String
    | CellMouseClick
    | CellMouseDown
    | CellMouseUp
    | CellHover Index
    | InputChange Input String


type DocMsg
    = TabMsg TabMsg
    | QueryMsg ()


type TabMsg
    = SheetWrite Index
    | SheetRowPush Int
    | SheetColumnPush


type Input
    = SheetName
    | SheetTags
    | SheetSearch
    | CellWrite
    | ColumnType Int
    | ColumnKey Int
    | ColumnLabel Int
    | QueryCode



---- SUBSCRIPTIONS ------------------------------------------------------------


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ librarySynced LibrarySync
        , docSelected DocSelect
        , docChanged DocChange
        , docNotified DocNotify
        , docQueried DocQuery
        , Browser.onKeyPress (D.map KeyPress (D.field "key" D.string))
        ]



---- UPDATE -------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ sheet } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlChange url ->
            ( route url model
            , changeId (route url model).id
            )

        LinkClick (Browser.Internal url) ->
            -- TODO: ?q=+any ?q=-any ?q==any
            ( model, Nav.pushUrl model.nav (Url.toString url) )

        LinkClick (Browser.External url) ->
            ( model, Nav.load url )

        LibrarySync data ->
            ( { model
                | library =
                    data
                        |> D.decodeValue
                            (D.dict
                                (D.map4 SheetInfo
                                    (D.oneOf [ D.field "name" D.string, D.succeed "" ])
                                    (D.oneOf [ D.field "tags" (D.list D.string), D.succeed [] ])
                                    (D.succeed ())
                                    (D.succeed Public)
                                )
                            )
                        |> Result.withDefault model.library
              }
            , Cmd.none
            )

        DocSelect data ->
            ( { model
                | sheet =
                    { id = data.id
                    , search = ""
                    , select = Rect (xy -1 -1) (xy -1 -1)
                    , hover = xy -1 -1
                    , drag = False
                    , write = Nothing
                    , doc = data.data.doc |> D.decodeValue docDecoder |> Result.mapError D.errorToString
                    , table = Err ""
                    , stats = Err ""
                    }
              }
              -- TODO: Fetch table rows depending on type, e.g. portal:123
            , Cmd.none
            )

        DocChange data ->
            ( iif (data.id /= model.sheet.id)
                model
                { model | sheet = { sheet | doc = data.data.doc |> D.decodeValue docDecoder |> Result.mapError D.errorToString } }
              -- TODO: Fetch table rows depending on type, e.g. portal:123
            , Cmd.none
            )

        DocNotify data ->
            -- TODO:
            ( model, Cmd.none )

        DocQuery data ->
            ( iif (data.id /= model.sheet.id)
                model
                { model | sheet = { sheet | table = data.data |> D.decodeValue tableDecoder |> Result.mapError D.errorToString } }
            , Cmd.none
            )

        DocMsg (TabMsg edit) ->
            ( { model | sheet = { sheet | write = Nothing } }
            , changeDoc <|
                Idd sheet.id <|
                    case sheet.doc of
                        Ok (Tab table) ->
                            case edit of
                                SheetWrite { x, y } ->
                                    -- TODO: what if we have no columns?
                                    table.cols
                                        |> Array.get x
                                        |> Maybe.map
                                            (\col ->
                                                [ { action = "set"
                                                  , path = [ E.int (y + 1), E.string col.key ]
                                                  , value = sheet.write |> Maybe.withDefault "" |> E.string
                                                  }
                                                ]
                                            )
                                        |> Maybe.withDefault []

                                SheetRowPush i ->
                                    [ { action = "push"
                                      , path = []
                                      , value = E.list identity [ E.object [] ]
                                      }
                                    ]

                                SheetColumnPush ->
                                    [ { action = "push"
                                      , path = [ E.int 0 ]
                                      , value = E.list identity [ E.object [ ( "name", E.string "" ), ( "type", E.string "text" ), ( "key", E.string (String.fromInt (Array.length table.cols)) ) ] ]
                                      }
                                    ]

                        _ ->
                            []
            )

        DocMsg (QueryMsg _) ->
            -- TODO:
            ( model, Cmd.none )

        DocDelete id ->
            ( model, deleteDoc id )

        DocNew ->
            ( model
            , case sheet.doc of
                Ok (Template ( "table", data )) ->
                    -- TODO: Make this consistent?
                    newDoc <| E.object [ ( "type", E.string "table" ), ( "data", E.list identity [ E.array (E.dict identity identity) data ] ) ]

                Ok (Template ( type_, data )) ->
                    newDoc <| E.object [ ( "type", E.string type_ ), ( "data", E.array (E.dict identity identity) data ) ]

                _ ->
                    Cmd.none
            )

        InputChange SheetName x ->
            ( model, updateLibrary (Idd sheet.id { name = Just x, tags = Nothing }) )

        InputChange SheetTags x ->
            ( model, updateLibrary (Idd sheet.id { name = Nothing, tags = x |> String.split ", " |> List.map String.trim |> Just }) )

        InputChange SheetSearch x ->
            -- TODO:
            ( model, Cmd.none )

        InputChange CellWrite x ->
            ( { model | sheet = { sheet | write = Just x } }, Cmd.none )

        InputChange QueryCode x ->
            ( model
            , changeDoc
                { id = sheet.id
                , data =
                    [ { action = "set"
                      , path = [ E.int 0, E.string "code" ]
                      , value = E.string x
                      }
                    ]
                }
            )

        InputChange (ColumnType i) x ->
            -- TODO:
            ( model, Cmd.none )

        InputChange (ColumnKey i) x ->
            -- TODO:
            ( model, Cmd.none )

        InputChange (ColumnLabel i) x ->
            -- TODO:
            ( model, Cmd.none )

        CellMouseClick ->
            case sheet.doc of
                Ok (Tab _) ->
                    ( { model | sheet = { sheet | write = Just "" } }
                    , Task.attempt (always NoOp) (Dom.focus "new-cell")
                    )

                _ ->
                    ( model, Cmd.none )

        CellMouseDown ->
            ( { model | sheet = { sheet | drag = True, select = Rect sheet.hover sheet.select.b } }, Cmd.none )

        CellMouseUp ->
            ( { model | sheet = { sheet | drag = False, select = Rect sheet.select.a sheet.hover } }, Cmd.none )

        CellHover hover ->
            let
                select =
                    sheet.select

                select_ =
                    iif sheet.drag { select | b = hover } select
            in
            ( { model | sheet = { sheet | hover = hover, select = select_ } }, Cmd.none )

        KeyPress "Enter" ->
            ( model, Task.attempt (always NoOp) (Dom.blur "new-cell") )

        -- KeyPress "Backspace" ->
        --     ( { model | sheet = { sheet | write = Nothing } }
        --     , changeDoc <|
        --         Idd sheet.id <|
        --             case sheet.doc of
        --                 Ok (Tab table) ->
        --                     -- TODO: Do multiple patches when ranges are selected.
        --                     case ( negate sheet.select.a.y, negate sheet.select.a.x ) of
        --                         ( 1, 1 ) ->
        --                             []
        --                         ( 1, i ) ->
        --                             [ { action = "col-del"
        --                               , path = [ E.string "cols", E.int (negate i) ]
        --                               , value = E.object []
        --                               }
        --                             ]
        --                         ( i, 1 ) ->
        --                             [ { action = "row-del"
        --                               , path = [ E.string "rows", E.int (negate i) ]
        --                               , value = E.object []
        --                               }
        --                             ]
        --                         ( y, x ) ->
        --                             table.cols
        --                                 |> Array.get (negate x)
        --                                 |> Maybe.map
        --                                     (\col ->
        --                                         [ { action = "cell-put"
        --                                           , path = [ E.string "rows", E.int (negate y), E.int col.key ]
        --                                           , value = sheet.write |> Maybe.withDefault "" |> E.string
        --                                           }
        --                                         ]
        --                                     )
        --                                 |> Maybe.withDefault []
        --                 _ ->
        --                     []
        --     )
        KeyPress _ ->
            ( model, Cmd.none )



---- VIEW ---------------------------------------------------------------------


view : Model -> Browser.Document Msg
view ({ sheet } as model) =
    -- TODO: Show library sheet if (id == ""), otherwise show loading if (model.id /= model.sheet.id).
    let
        info : SheetInfo
        info =
            model.library
                |> Dict.get sheet.id
                |> Maybe.withDefault { name = "", tags = [], thumb = (), peers = Public }

        table : Result String Table
        table =
            case ( sheet.doc, sheet.table ) of
                ( Ok (Tab tbl), _ ) ->
                    Ok tbl

                ( Ok (Template ( "table", params )), _ ) ->
                    Ok
                        { cols = Array.fromList [ Col "name" "name" Text, Col "type" "type" Text, Col "key" "key" Text ]
                        , rows = params
                        }

                ( Ok (Template ( "query", params )), _ ) ->
                    Ok
                        { cols = Array.fromList [ Col "lang" "lang" Text, Col "code" "code" Text, Col "args" "args" Text ]
                        , rows = params
                        }

                ( Ok (Template ( type_, params )), _ ) ->
                    Err "TODO: template"

                ( Ok Library, _ ) ->
                    Ok
                        { cols =
                            [ ( "sheet_id", Link ), ( "name", Text ), ( "tags", Many Text ), ( "del", Delete ) ]
                                |> List.map (\( k, t ) -> Col k k t)
                                |> Array.fromList
                        , rows =
                            model.library
                                |> Dict.filter (\k _ -> k /= "")
                                |> Dict.toList
                                |> List.map (\( k, v ) -> Dict.fromList [ ( "sheet_id", E.string k ), ( "name", E.string v.name ), ( "tags", E.list E.string v.tags ), ( "del", E.string k ) ])
                                |> Array.fromList
                        }

                ( _, Ok tbl ) ->
                    Ok tbl

                ( Err err1, Err err2 ) ->
                    Err (err1 ++ " " ++ err2)

                ( _, Err err ) ->
                    Err err
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
                        -- Badges indicate scrapscript news, library notifs, etc.
                        List.concat
                            [ [ H.a [ A.href "/", S.fontWeightBold ] [ text "scrapsheets", H.sup [] [ text "" ] ]
                              ]
                            , iif (sheet.id == "")
                                [ text "/"
                                , H.span [] [ text "library" ]
                                ]
                                [ text "/"
                                , H.a [ A.href "#settings" ] [ text (iif (String.trim info.name == "") "untitled" info.name) ]
                                , H.div [ S.displayFlex, S.flexDirectionRow, S.alignItemsBaseline, S.gapRem 0.5 ] <|
                                    List.map (\tag -> H.a [ A.href ("/?q=tag:" ++ tag), S.fontSizeRem 0.7, S.opacity "0.8" ] [ text ("#" ++ tag) ]) info.tags
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
                    [ case table of
                        Err err ->
                            H.p [] [ text err ]

                        Ok { cols, rows } ->
                            H.table [ S.borderCollapseCollapse, S.width "100%", A.onMouseLeave (CellHover (xy -1 -1)) ]
                                [ H.thead []
                                    [ H.tr [] <|
                                        -- TODO: Add additional header rows for stats and column def.
                                        (::)
                                            (H.th
                                                [ A.onClick (DocMsg (TabMsg (SheetRowPush -1)))
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
                                                            [ text (iif (col.name == "") "untitled" col.name) ]
                                                        ]
                                                )
                                            <|
                                                Array.toList cols
                                    ]
                                , H.tbody [] <|
                                    Array.toList <|
                                        Array.indexedMap
                                            (\n row ->
                                                H.tr [] <|
                                                    (::)
                                                        (H.th
                                                            [ A.onClick (DocMsg (TabMsg (SheetRowPush n)))
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
                                                                                        D.string |> D.map (\href -> H.a [ A.href ("/" ++ href), S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.displayInlineBlock, S.maxWidthRem 12 ] [ text href ])

                                                                                    Image ->
                                                                                        D.string |> D.map (\src -> H.img [ A.src src, S.width "100%", S.objectFitCover ] [])

                                                                                    Text ->
                                                                                        D.map text string

                                                                                    Boolean ->
                                                                                        D.map (\c -> H.input [ A.type_ "checkbox", A.checked c ] []) D.bool

                                                                                    Number ->
                                                                                        D.map (H.span [ S.textAlignRight ] << List.singleton << text) string

                                                                                    Delete ->
                                                                                        D.string |> D.map (\sheet_id -> H.button [ A.onClick (DocDelete sheet_id) ] [ text "╳" ])

                                                                                    Form form ->
                                                                                        D.map3
                                                                                            (\method action fields ->
                                                                                                -- TODO: Change this to displayGrid
                                                                                                H.form [ A.onSubmit NoOp, S.displayGrid, S.gridTemplateColumns "auto 1fr", S.paddingRem 1 ] <|
                                                                                                    List.concat
                                                                                                        [ List.concatMap
                                                                                                            (\field ->
                                                                                                                [ H.label [] [ text field.label ]
                                                                                                                , H.input [] []
                                                                                                                ]
                                                                                                            )
                                                                                                            fields
                                                                                                        , [ H.span [] []
                                                                                                          , H.button [ A.type_ "submit" ] [ text method ]
                                                                                                          ]
                                                                                                        ]
                                                                                            )
                                                                                            (D.field "method" D.string)
                                                                                            (D.field "action" D.string)
                                                                                            (D.field "fields"
                                                                                                (D.list
                                                                                                    (D.map (\label -> { label = label })
                                                                                                        (D.field "label" D.string)
                                                                                                    )
                                                                                                )
                                                                                            )

                                                                                    _ ->
                                                                                        D.map text string
                                                                                )
                                                                            |> Result.mapError (D.errorToString >> text)
                                                                            |> result
                                                                        ]
                                                                    <|
                                                                        if sheet.write /= Nothing && sheet.select == rect i n i n then
                                                                            Just [ H.input [ A.id "new-cell", A.value (Maybe.withDefault "" sheet.write), A.onInput (InputChange CellWrite), A.onBlur (DocMsg (TabMsg (SheetWrite sheet.select.a))), S.width "100%" ] [] ]

                                                                        else
                                                                            Nothing
                                                            )
                                                        <|
                                                            Array.toList cols
                                            )
                                            rows
                                ]
                    ]
                ]
            , H.aside [ S.displayFlex, S.flexDirectionColumn, S.minWidthRem 12, S.maxWidthRem 18, S.maxHeight "100vh", S.overflowHidden, S.overflowYAuto ] <|
                List.concat
                    [ [ H.span [] [ text (String.toLower (Debug.toString model.tool)), H.sup [] [ text "" ] ]
                      ]
                    , case model.tool of
                        -- TODO: Hovering over columns/etc should highlight relevant cells, and vice versa.
                        Settings ->
                            List.concat
                                [ [ H.label [] [ text "name" ]
                                  , H.input [ A.value info.name, A.onInput (InputChange SheetName) ] []
                                  , H.label [] [ text "tags" ]
                                  , H.input [ A.value (String.join ", " info.tags), A.onInput (InputChange SheetTags) ] []
                                  ]
                                , case sheet.doc of
                                    Ok (Query query) ->
                                        [ H.textarea [ A.onInput (InputChange QueryCode), S.minHeightRem 10, S.height "100%", S.whiteSpaceNowrap, S.overflowXAuto, S.fontSizeRem 0.75 ]
                                            [ text (String.trim query.query)
                                            ]
                                        ]

                                    Ok (Template ( _, _ )) ->
                                        [ H.button [ A.onClick DocNew ] [ text "new sheet (N)" ]
                                        ]

                                    _ ->
                                        -- TODO:
                                        [ H.textarea [ A.onInput (always NoOp), S.minHeightRem 10, S.height "100%" ]
                                            -- TODO: Link to the column configs.
                                            [ text (Debug.toString sheet.doc)
                                            ]
                                        , H.div [ S.displayFlex, S.flexWrapWrap, S.justifyContentEnd, S.alignItemsBaseline ]
                                            [ H.button [ A.onClick (DocMsg (TabMsg SheetColumnPush)) ] [ text "new column (C)" ]
                                            ]
                                        ]
                                ]

                        Hints ->
                            -- TODO: problems (linting), ideas, related (sources/backlinks)
                            []

                        Stats ->
                            -- TODO:
                            []

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
