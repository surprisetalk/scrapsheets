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
import Set exposing (Set)
import Task exposing (Task)
import Url exposing (Url)
import Url.Parser as UrlP exposing ((</>), (<?>))
import Url.Parser.Query as UrlQ



---- HELPERS ------------------------------------------------------------------


ls : a -> List a
ls =
    List.singleton


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


round2 : Float -> Float
round2 =
    (*) 100 >> floor >> toFloat >> flip (/) 100


commas : String -> String
commas =
    String.reverse
        >> String.toList
        >> List.indexedMap
            (\i c ->
                if i > 0 && modBy 3 i == 0 then
                    [ ',', c ]

                else
                    [ c ]
            )
        >> List.concat
        >> String.fromList
        >> String.reverse


usd : Float -> String
usd amount =
    let
        ( intPart, decPart ) =
            case amount |> abs |> round2 |> String.fromFloat |> String.split "." of
                a :: b :: _ ->
                    ( a, String.left 2 <| String.padRight 2 '0' b )

                a :: [] ->
                    ( a, "00" )

                [] ->
                    ( "0", "00" )
    in
    iif (amount < 0) "-" "" ++ "$" ++ commas intPart ++ "." ++ decPart



---- PORTS --------------------------------------------------------------------


port librarySynced : (D.Value -> msg) -> Sub msg


port updateLibrary : Idd { name : Maybe String, tags : Maybe (List String) } -> Cmd msg


port changeId : Id -> Cmd msg


port newDoc : E.Value -> Cmd msg


port deleteDoc : String -> Cmd msg


port changeDoc : Idd (List Patch) -> Cmd msg


port notifyDoc : Idd E.Value -> Cmd msg


port queryDoc : Idd { lang : String, code : String, cols : D.Value } -> Cmd msg


port docSelected : (Idd { doc : D.Value } -> msg) -> Sub msg


port docChanged : (Idd DocDelta -> msg) -> Sub msg


port docNotified : (Idd D.Value -> msg) -> Sub msg


port docQueried : (Idd D.Value -> msg) -> Sub msg


port docErrored : (String -> msg) -> Sub msg


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
    , search : String
    , error : String
    , library : Library
    , sheet : Sheet
    }


type alias SheetInfo =
    { name : String
    , tags : List String
    , scratch : Bool
    , system : Bool
    , thumb : Svg
    , peers : Peers
    }


type Peers
    = Private (Set Id)
    | Public


type alias Sheet =
    { id : Id
    , select : Rect
    , hover : Index
    , drag : Bool
    , write : Maybe String
    , doc : Result String Doc
    , table : Result String Table
    , stats : Result String (Dict String Stat)
    }


type alias Svg =
    -- TODO: Generate nice preview Svg based on sheet contents.
    ()


type Stat
    = Numeric
        { histogram : Dict String Int
        , count : Int
        , sum : Float
        , min : Maybe Float
        , max : Maybe Float
        }
    | Enumerative
        { histogram : Dict String Int
        }
    | Descriptive
        { lengths : Dict Int Int
        , keywords : Dict String Int
        , count : Int
        , sum : Int
        , min : Maybe Int
        , max : Int
        }


type Doc
    = Library
    | Shop
    | Tab Table
    | Net Net
    | Query Query_
    | Codex
    | Portal Args


type alias Table =
    { cols : Array Col
    , rows : Array Row
    }


type Lang
    = Prql
    | Sql
    | Formula
    | Scrapscript
    | Python


langs : Dict String Lang
langs =
    Dict.fromList
        [ ( "prql", Prql )
        , ( "sql", Sql )
        , ( "formula", Formula )
        , ( "scrapscript", Scrapscript )
        , ( "python", Python )
        ]


type alias Query_ =
    { lang : Lang
    , code : String
    , args : Args
    , examples : List String
    , cols : D.Value
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
    Dict String Type


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
    = Unknown
    | Text
    | Number
    | Usd
    | Boolean
    | Many Type
    | Link
    | SheetId
    | Json
    | Timestamp
    | Image
    | Delete
    | Create
    | Form (Dict String String)


typeName : Type -> String
typeName typ =
    case typ of
        Unknown ->
            "unknown"

        Text ->
            "text"

        Number ->
            "num"

        Usd ->
            "usd"

        Boolean ->
            "bool"

        Many typ_ ->
            "list " ++ typeName typ_

        Link ->
            "link"

        SheetId ->
            "sheet_id"

        Json ->
            "json"

        Timestamp ->
            "timestamp"

        Image ->
            "image"

        Create ->
            "create"

        Delete ->
            "delete"

        Form _ ->
            "form"



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


boolean : D.Decoder Bool
boolean =
    D.oneOf
        [ D.bool
        , D.string |> D.map (\c -> String.toLower c == "true" || c == "t" || c == "1")
        , D.int |> D.map ((/=) 0)
        , D.null False
        , D.succeed False
        ]


docDecoder : D.Decoder Doc
docDecoder =
    D.field "type" D.string
        |> D.andThen
            (\typ ->
                case typ of
                    "library" ->
                        D.succeed Library

                    "shop" ->
                        D.succeed Shop

                    "table" ->
                        D.field "data" <|
                            D.map Tab tableDecoder

                    "net-hook" ->
                        D.succeed (Net Hook)

                    "portal" ->
                        D.field "data" <|
                            D.map Portal <|
                                D.succeed Dict.empty

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
                                    (D.map5 Query_
                                        (D.field "lang" langDecoder)
                                        (D.field "code" D.string)
                                        -- TODO
                                        (D.maybe (D.field "args" (D.succeed Dict.empty)) |> D.map (Maybe.withDefault Dict.empty))
                                        (D.maybe (D.field "examples" (D.list D.string)) |> D.map (Maybe.withDefault []))
                                        (D.field "cols" D.value)
                                    )

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
                , ( "num", Number )
                , ( "int", Number )
                , ( "usd", Usd )
                , ( "sheet_id", SheetId )
                , ( "link", Link )
                , ( "image", Image )
                , ( "form", Form Dict.empty )
                , ( "timestamp", Timestamp )
                , ( "datetime", Timestamp )
                , ( "date", Timestamp )
                , ( "json", Json )
                , ( "text", Text )
                , ( "string", Text )
                , ( "create", Create )
                ]
    in
    D.oneOf
        [ D.map3 Col
            (D.field "key" string)
            (D.field "name" D.string)
            (D.field "type" (D.nullable D.string |> D.map (Maybe.andThen (flip Dict.get types) >> Maybe.withDefault Unknown)))
        , D.succeed (Col "" "" Text)
        ]


langDecoder : D.Decoder Lang
langDecoder =
    D.string |> D.andThen (flip Dict.get langs >> Maybe.map D.succeed >> Maybe.withDefault (D.fail "Invalid query language."))


shopDecoder : D.Decoder Table
shopDecoder =
    D.field "data" tableDecoder



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
                , search = ""
                , error = ""
                , library = Dict.empty
                , sheet =
                    { id = ""
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
    url
        |> UrlP.parse
            (UrlP.map
                (\id search -> { model | id = id, search = Maybe.withDefault "" search })
                (UrlP.top
                    </> UrlP.oneOf [ UrlP.string, UrlP.map "" UrlP.top ]
                    <?> UrlQ.string "q"
                 -- </> UrlP.fragment (Maybe.andThen (flip Dict.get tools))
                )
            )
        |> Maybe.withDefault model



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
    | DocError String
    | DocMsg DocMsg
    | DocNew E.Value
    | DocNewQuery
    | DocNewTable
    | DocDelete Id
    | KeyPress String
    | CellMouseClick
    | CellMouseDoubleClick String
    | CellMouseDown
    | CellMouseUp
    | CellHover Index
    | InputChange Input String
    | ShopFetch (Result Http.Error Table)


type DocMsg
    = SheetWrite Index
    | SheetRowPush Int
    | SheetColumnPush
    | CellCheck Index Bool


type Input
    = SheetSearch
    | CellWrite
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
        , docErrored DocError
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
                                (D.map6 SheetInfo
                                    (D.oneOf [ D.field "name" D.string, D.succeed "" ])
                                    (D.oneOf [ D.field "tags" (D.list D.string), D.succeed [] ])
                                    (D.oneOf [ D.field "scratch" D.bool, D.succeed False ])
                                    (D.oneOf [ D.field "system" D.bool, D.succeed False ])
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
                | error = ""
                , sheet =
                    { id = data.id
                    , select = Rect (xy -1 -1) (xy -1 -1)
                    , hover = xy -1 -1
                    , drag = False
                    , write = Nothing
                    , doc = data.data.doc |> D.decodeValue docDecoder |> Result.mapError D.errorToString
                    , table = Err ""
                    , stats = Err ""
                    }
              }
            , case data.data.doc |> D.decodeValue docDecoder of
                Ok Shop ->
                    Http.get
                        { url = "https://api.sheets.scrap.land/shop"
                        , expect = Http.expectJson ShopFetch shopDecoder
                        }

                _ ->
                    Cmd.none
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
                { model | error = "", sheet = { sheet | table = data.data |> D.decodeValue tableDecoder |> Result.mapError D.errorToString } }
            , Cmd.none
            )

        DocError error ->
            ( { model | error = error }
            , Cmd.none
            )

        DocMsg edit ->
            ( { model | sheet = { sheet | write = Nothing } }
            , case sheet.doc of
                Ok Library ->
                    case edit of
                        SheetWrite { x, y } ->
                            let
                                id : String
                                id =
                                    model.library
                                        |> Dict.filter (\k v -> k /= "" && not v.scratch && List.any (String.contains model.search) (k :: v.name :: v.tags))
                                        |> Dict.keys
                                        |> List.drop (y - 1)
                                        |> List.head
                                        |> Maybe.withDefault ""
                            in
                            case Maybe.map .name (Array.get x libraryCols) of
                                Just "name" ->
                                    updateLibrary (Idd id { name = sheet.write, tags = Nothing })

                                Just "tags" ->
                                    updateLibrary (Idd id { name = Nothing, tags = sheet.write |> Maybe.map (String.split ", " >> List.map String.trim) })

                                _ ->
                                    Cmd.none

                        _ ->
                            Cmd.none

                Ok (Tab table) ->
                    changeDoc <|
                        Idd sheet.id <|
                            case edit of
                                SheetWrite { x, y } ->
                                    case max 0 y of
                                        0 ->
                                            Maybe.map2 Tuple.pair sheet.write (Array.get x table.cols)
                                                |> Maybe.map
                                                    (\( write, col ) ->
                                                        [ { action = "set"
                                                          , path = [ E.int 0, E.string (String.fromInt x) ]
                                                          , value =
                                                                case String.fromInt y of
                                                                    "-1" ->
                                                                        E.object [ ( "name", E.string col.name ), ( "type", E.string write ), ( "key", E.string col.key ) ]

                                                                    "0" ->
                                                                        E.object [ ( "name", E.string write ), ( "type", E.string (typeName col.typ) ), ( "key", E.string col.key ) ]

                                                                    _ ->
                                                                        E.object [ ( "name", E.string col.name ), ( "type", E.string (typeName col.typ) ), ( "key", E.string col.key ) ]
                                                          }
                                                        ]
                                                    )
                                                |> Maybe.withDefault []

                                        _ ->
                                            table.cols
                                                |> Array.get x
                                                |> Maybe.map
                                                    (\col ->
                                                        [ { action = "set"
                                                          , path = [ E.int y, E.string col.key ]
                                                          , value = sheet.write |> Maybe.map E.string |> Maybe.withDefault E.null
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
                                      , value = E.list identity [ E.object [ ( "name", E.string "" ), ( "type", E.string "text" ), ( "key", E.int (Array.length table.cols) ) ] ]
                                      }
                                    ]

                                CellCheck i c ->
                                    table.cols
                                        |> Array.get i.x
                                        |> Maybe.map
                                            (\col ->
                                                [ { action = "set"
                                                  , path = [ E.int i.y, E.string col.key ]
                                                  , value = E.bool c
                                                  }
                                                ]
                                            )
                                        |> Maybe.withDefault []

                _ ->
                    Cmd.none
            )

        DocDelete id ->
            ( model, deleteDoc id )

        DocNew x ->
            ( model, newDoc x )

        DocNewTable ->
            ( model, newDoc <| E.object [ ( "type", E.string "table" ), ( "data", E.list identity [ E.list identity [ E.object [ ( "name", E.string "a" ), ( "type", E.string "text" ), ( "key", E.string "a" ) ] ] ] ) ] )

        DocNewQuery ->
            ( model, newDoc <| E.object [ ( "type", E.string "query" ), ( "data", E.list identity [ E.object [ ( "lang", E.string "sql" ), ( "code", E.string "select 1" ) ] ] ) ] )

        ShopFetch x ->
            ( { model | sheet = { sheet | table = Result.mapError (always "Something went wrong.") x } }, Cmd.none )

        InputChange SheetSearch x ->
            ( { model | search = x, sheet = { sheet | table = Err "" } }
            , Cmd.batch
                [ Nav.replaceUrl model.nav ("?q=" ++ Url.percentEncode x)
                , case sheet.doc of
                    Ok (Query query) ->
                        -- TODO: Lang to string.
                        queryDoc (Idd sheet.id { lang = "sql", code = query.code, cols = query.cols })

                    _ ->
                        Cmd.none
                ]
            )

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

        CellMouseDoubleClick write ->
            let
                a =
                    -- TODO: Don't set .write if it's a Boolean or non-editable cell.
                    ( { model | sheet = { sheet | write = Just write } }
                    , Task.attempt (always NoOp) (Dom.focus "new-cell")
                    )
            in
            case sheet.doc of
                Ok Library ->
                    a

                Ok (Tab _) ->
                    a

                _ ->
                    ( model, Cmd.none )

        CellMouseClick ->
            ( { model | sheet = { sheet | drag = False, select = Rect sheet.hover sheet.hover } }, Cmd.none )

        CellMouseDown ->
            ( { model | sheet = { sheet | drag = True, select = Rect sheet.hover sheet.hover } }, Cmd.none )

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

        KeyPress _ ->
            ( model, Cmd.none )



---- VIEW ---------------------------------------------------------------------


libraryCols : Array Col
libraryCols =
    Array.fromList
        [ Col "sheet_id" "" SheetId

        -- , Col "type" "type" Text
        , Col "name" "name" Text
        , Col "tags" "tags" (Many Text)
        , Col "delete" "" Delete
        ]


view : Model -> Browser.Document Msg
view ({ sheet } as model) =
    -- TODO: Show library sheet if (id == ""), otherwise show loading if (model.id /= model.sheet.id).
    let
        info : SheetInfo
        info =
            model.library
                |> Dict.get sheet.id
                |> Maybe.withDefault { name = "", tags = [], scratch = False, system = False, thumb = (), peers = Public }

        computeNumericStats : Array Row -> String -> Stat
        computeNumericStats rows key =
            rows
                |> Array.foldl
                    (\row stat ->
                        row
                            |> Dict.get key
                            |> Maybe.andThen (D.decodeValue number >> Result.toMaybe)
                            |> Maybe.map
                                (\n ->
                                    { histogram = stat.histogram |> Dict.update (String.fromFloat n) (Maybe.withDefault 0 >> (+) 1 >> Just)
                                    , count = stat.count + 1
                                    , sum = stat.sum + n
                                    , min = stat.min |> Maybe.withDefault n |> min n |> Just
                                    , max = stat.max |> Maybe.withDefault n |> max n |> Just
                                    }
                                )
                            |> Maybe.withDefault stat
                    )
                    { histogram = Dict.empty, count = 0, sum = 0, min = Nothing, max = Nothing }
                |> Numeric

        computeTextStats : Array Row -> String -> Stat
        computeTextStats rows key =
            rows
                |> Array.foldl
                    (\row stat ->
                        row
                            |> Dict.get key
                            |> Maybe.andThen (D.decodeValue string >> Result.toMaybe)
                            |> Maybe.map
                                (\s ->
                                    { lengths = stat.lengths |> Dict.update (String.length s) (Maybe.withDefault 0 >> (+) 1 >> Just)
                                    , keywords = s |> String.split " " |> List.foldl (\k -> Dict.update k (Maybe.withDefault 0 >> (+) 1 >> Just)) stat.keywords
                                    , count = stat.count + 1
                                    , sum = stat.sum + String.length s
                                    , min = min (String.length s) (Maybe.withDefault (String.length s) stat.min) |> Just
                                    , max = max (String.length s) stat.max
                                    }
                                )
                            |> Maybe.withDefault stat
                    )
                    { lengths = Dict.empty, keywords = Dict.empty, count = 0, sum = 0, min = Nothing, max = 0 }
                |> Descriptive

        stats : Result String (Array Stat)
        stats =
            case sheet.doc of
                Ok (Tab tbl) ->
                    Ok <|
                        Array.map
                            (\col ->
                                case col.typ of
                                    Number ->
                                        computeNumericStats tbl.rows col.key

                                    Usd ->
                                        computeNumericStats tbl.rows col.key

                                    Text ->
                                        computeTextStats tbl.rows col.key

                                    _ ->
                                        Enumerative { histogram = Dict.empty }
                            )
                            tbl.cols

                x ->
                    Err "TODO: table stats"

        table : Result String Table
        table =
            case ( sheet.doc, sheet.table ) of
                ( Ok (Tab tbl), _ ) ->
                    Ok tbl

                ( Ok Library, _ ) ->
                    Ok
                        { cols =
                            libraryCols
                        , rows =
                            model.library
                                |> Dict.filter (\k v -> k /= "" && not v.scratch && List.any (String.contains model.search) (k :: v.name :: v.tags))
                                |> Dict.toList
                                |> List.map (\( k, v ) -> Dict.fromList [ ( "sheet_id", E.string k ), ( "type", E.string (Maybe.withDefault "" <| List.head <| String.split ":" k) ), ( "name", E.string v.name ), ( "tags", E.list E.string v.tags ), ( "delete", iif v.system E.null (E.string k) ) ])
                                |> Array.fromList
                        }

                ( _, Ok tbl ) ->
                    Ok tbl

                ( Err err1, Err err2 ) ->
                    Err (err1 ++ " " ++ err2)

                ( _, Err err ) ->
                    Err err

        examples : List String
        examples =
            case sheet.doc of
                Ok (Query query) ->
                    query.examples

                _ ->
                    []
    in
    { title = "scrapsheets"
    , body =
        [ H.node "style" [] [ text "body * { gap: 1rem; }" ]
        , H.node "style" [] [ text "body { font-family: sans-serif; font-optical-sizing: auto; height: 100vh; width: 100vw; }" ]
        , H.node "style" [] [ text "table { background: #fff; line-height: 1; }" ]
        , H.node "style" [] [ text "td a, td button { opacity: 0.8; }" ]
        , H.node "style" [] [ text "td a:hover, td button:hover { opacity: 1; }" ]
        , H.node "style" [] [ text "th, td { padding: 0.25rem; padding-bottom: 0.15rem; font-weight: normal; border: 1px solid #aaa; height: 0.8rem; vertical-align: top; }" ]
        , H.node "style" [] [ text "tr > :first-child { border-left: none; padding-left: 0.5rem; }" ]
        , H.node "style" [] [ text "tr > :last-child { border-right: none; padding-right: 0.5rem; }" ]
        , H.node "style" [] [ text "th > *, td > * { max-height: 6rem; text-overlow: ellipsis; }" ]
        , H.node "style" [] [ text "td:hover { background: rgba(0,0,0,0.025); }" ]
        , H.node "style" [] [ text ".r0 { position: sticky; top: -1px; background: #f6f6f6; z-index: 1; border-bottom: 0px; }" ]
        , H.node "style" [] [ text ".r0::after { content: \"\"; display: block; position: absolute; width: 100%; left: 0; bottom: -1px; border-bottom: 1px solid #aaa; }" ]
        , H.node "style" [] [ text ".selected { background: rgba(0,0,0,0.05); }" ]
        , H.node "style" [] [ text "#code { font-family: monospace; background: #fff; }" ]
        , H.node "style" [] [ text "@media (min-width: 768px) { body > div { grid-template-columns: 1fr auto; } #aside { border-left: 1px solid #aaa; } }" ]
        , H.node "style" [] [ text "@media (max-width: 768px) { body > div { grid-template-rows: 1fr auto; } #aside { border-top: 1px solid #aaa; } }" ]
        , H.node "style" [] [ text "@media (max-width: 768px) { #title { display: none; } }" ]
        , H.node "style" [] [ text "#account > * { padding: 0.25rem 0.5rem; border: 1px solid #aaa; border-radius: 2px; font-size: 0.875rem; }" ]

        -- -- TODO: This should probably be part of the main grid.
        -- -- TODO: Actually, move this to the aside of the library.
        -- , H.form [ A.id "account", S.displayGrid, S.gapRem 0.5, S.maxWidth "100vw", S.width "100%", S.gridTemplateColumns "1fr 1fr auto", S.paddingRem 0.5, S.borderTop "1px solid #aaa", S.backgroundColor "#ccc", S.positionAbsolute, S.bottomPx 0, S.zIndex "10" ]
        --     [ H.input [ S.minWidthRem 2, A.placeholder "email", A.type_ "email", A.name "email" ] []
        --     , H.input [ S.minWidthRem 2, A.placeholder "password", A.type_ "password", A.name "password" ] []
        --     , H.button [ A.type_ "submit", S.background "#eee" ] [ text "signup/login" ]
        --     ]
        -- , H.node "style" [] [ text "thead tr td { position: sticky; top: 0; }" ]
        -- , H.node "style" [] [ text "tfoot tr:last-child td { position: sticky; bottom: 0; }" ]
        , H.div [ S.displayGrid, S.gapRem 0, S.userSelectNone, S.cursorPointer, A.style "-webkit-user-select" "none", S.maxWidth "100vw", S.maxHeight "100vh", S.height "100%", S.width "100%" ]
            [ H.main_ [ S.displayFlex, S.flexDirectionColumn, S.width "100%", S.overflowXAuto, S.gapRem 0 ]
                [ H.div [ S.displayFlex, S.flexDirectionRow, S.alignItemsCenter, S.whiteSpaceNowrap, S.gapRem 0.5, S.paddingRem 0.5, S.borderBottom "1px solid #aaa", S.background "#f0f0f0" ] <|
                    List.concat
                        [ [ H.a [ A.href "/", S.fontWeight "900", S.fontSizeRem 1.5, S.heightRem 1, S.lineHeight "0.55" ] [ text "⊞" ]
                          , H.a [ A.href "/", S.fontWeight "900", A.id "title", S.marginLeftRem -0.25 ] [ text "scrapsheets" ]
                          , text "/"
                          ]
                        , [ H.span [] [ text "anon" ]
                          , text "/"
                          ]
                        , iif (sheet.id == "")
                            [ H.span [] [ text "library" ]
                            ]
                            [ H.a [ A.href "#settings" ] [ text (iif (String.trim info.name == "") "untitled" info.name) ]
                            ]

                        -- TODO: This is where we'll put actions and keyboard shortcut hints.
                        -- , [ H.a [ A.href "#", S.marginLeftAuto, S.backgroundColor "#e0e0e0", S.padding "2px 4px" ] [ text "help" ]
                        --   ]
                        ]

                -- TODO: Put recent/saved searches on right.
                , H.div [ S.displayFlex, S.flexDirectionRow, S.justifyContentSpaceBetween, S.gapRem 0, S.borderBottom "1px solid #aaa", S.zIndex "2", S.marginBottomPx -1 ]
                    -- All current filters should be rendered as text in the searchbar.
                    -- This helps people (1) learn the language and (2) indicate that they're searching rather than editing.
                    -- TODO: If no results found, show saved searches and recent searches.
                    [ H.div [ S.displayFlex, S.width "100%", S.height "100%" ]
                        [ H.input [ A.value model.search, A.onInput (InputChange SheetSearch), A.placeholder "search", S.width "100%", S.border "none", S.backgroundColor "#fff", S.padding "0.25rem 0.5rem", S.fontSizeRem 0.875 ] []
                        ]
                    ]

                -- , H.div [ S.displayFlex, S.justifyContentSpaceBetween, S.padding "0.5rem 1rem", S.backgroundColor "#fff" ] <|
                --     [ H.div [ S.displayFlex, S.alignItemsBaseline, S.gapRem 1, S.opacity "0.5" ] <|
                --         List.concat
                --             [ List.map (\x -> H.span [ A.onClick (InputChange SheetSearch x), S.textDecorationUnderline, S.opacity "0.5", S.fontSizeRem 0.6 ] [ text x ]) <| examples
                --             , List.map (\tag -> H.a [ A.href ("/?q=tag:" ++ tag) ] [ text ("#" ++ tag) ]) info.tags
                --             , List.map (\id -> H.a [ A.href ("/" ++ id) ] [ text ("$" ++ id) ]) [ "backlink" ]
                --             , List.map (\id -> H.a [ A.href ("/?following=" ++ id) ] [ text ("@" ++ id) ]) [ "anon" ]
                --             ]
                --     , H.div [ S.displayFlex, S.alignItemsBaseline, S.flexDirectionRowReverse, S.gapRem 1, S.opacity "0.5" ] <|
                --         List.concat
                --             [ case sheet.doc of
                --                 Ok Library ->
                --                     [ H.span [ A.onClick DocNewQuery ] [ text "new query" ]
                --                     , H.span [ A.onClick DocNewTable ] [ text "new table" ]
                --                     ]
                --                 _ ->
                --                     [ H.span [ A.onClick (DocMsg (SheetColumnPush)) ] [ text "⌘C new column" ]
                --                     ]
                --             , List.map (\tag -> H.span [] [ text "⌘F find" ]) [ () ]
                --             ]
                --     ]
                -- TODO: https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed
                , H.div [ S.overflowAuto, S.height "100%", S.backgroundColor "#eee" ]
                    [ case model.error of
                        "" ->
                            text ""

                        error ->
                            H.span [] [ H.button [ A.onClick (DocError "") ] [ text "╳" ], text " ", text error ]
                    , case table of
                        Err "" ->
                            H.div [ S.displayFlex ] [ H.span [ S.textAlignCenter, S.width "100%", S.paddingRem 2, S.opacity "0.5" ] [ text "loading" ] ]

                        Err err ->
                            H.p [] [ text err ]

                        Ok { cols, rows } ->
                            H.table [ S.borderCollapseCollapse, S.width "100%", A.onMouseLeave (CellHover (xy -1 -1)) ]
                                [ H.thead []
                                    -- [ H.tr [] <|
                                    --     H.th [] []
                                    --         :: List.indexedMap
                                    --             -- TODO: Stats.
                                    --             (\i col -> H.th [ S.textAlignLeft, S.paddingBottomRem 1 ] [])
                                    --             (Array.toList cols)
                                    []
                                , H.tbody [] <|
                                    List.concat
                                        [ Array.toList <|
                                            Array.indexedMap
                                                (\n_ row ->
                                                    let
                                                        n =
                                                            -- TODO: Consider moving the header rows to H.thead
                                                            n_ - 2
                                                    in
                                                    H.tr
                                                        [ case String.fromInt n of
                                                            "-2" ->
                                                                S.backgroundColor "#ececec"

                                                            "-1" ->
                                                                S.backgroundColor "#f6f6f6"

                                                            "0" ->
                                                                S.backgroundColor "#f6f6f6"

                                                            _ ->
                                                                S.backgroundColor "#fff"
                                                        , case ( String.fromInt n, stats ) of
                                                            ( "-2", Err _ ) ->
                                                                S.displayNone

                                                            _ ->
                                                                S.displayTableRow
                                                        ]
                                                    <|
                                                        List.indexedMap
                                                            (\i col ->
                                                                H.td
                                                                    [ A.onClick CellMouseClick
                                                                    , A.onDoubleClick <|
                                                                        CellMouseDoubleClick <|
                                                                            case String.fromInt n of
                                                                                "-1" ->
                                                                                    typeName col.typ

                                                                                "0" ->
                                                                                    col.name

                                                                                _ ->
                                                                                    row |> Dict.get col.key |> Maybe.andThen (D.decodeValue string >> Result.toMaybe) |> Maybe.withDefault ""
                                                                    , A.onMouseDown CellMouseDown
                                                                    , A.onMouseUp CellMouseUp
                                                                    , A.onMouseEnter (CellHover (xy i n))
                                                                    , S.heightRem 1.25
                                                                    , S.lineHeight (iif (n == 0) "1.75" "")
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
                                                                        , ( "r0", n == 0 )
                                                                        ]
                                                                    , case col.typ of
                                                                        Create ->
                                                                            S.textAlignRight

                                                                        SheetId ->
                                                                            S.textAlignCenter

                                                                        Boolean ->
                                                                            S.textAlignCenter

                                                                        Usd ->
                                                                            S.textAlignRight

                                                                        Number ->
                                                                            S.textAlignRight

                                                                        Delete ->
                                                                            S.textAlignCenter

                                                                        Form _ ->
                                                                            S.textAlignCenter

                                                                        _ ->
                                                                            S.textAlignLeft
                                                                    , case col.typ of
                                                                        Create ->
                                                                            S.widthRem 10

                                                                        SheetId ->
                                                                            S.widthRem 0.5

                                                                        Boolean ->
                                                                            S.widthRem 0.5

                                                                        Number ->
                                                                            S.widthRem 0.5

                                                                        Usd ->
                                                                            S.widthRem 0.5

                                                                        Delete ->
                                                                            S.widthRem 0.5

                                                                        _ ->
                                                                            S.widthAuto
                                                                    ]
                                                                <|
                                                                    case ( String.fromInt n, sheet.write /= Nothing && sheet.select == rect i n i n ) of
                                                                        ( _, True ) ->
                                                                            [ H.input [ A.id "new-cell", A.value (Maybe.withDefault "" sheet.write), A.onInput (InputChange CellWrite), A.onBlur (DocMsg (SheetWrite sheet.select.a)), S.width "100%", S.height "100%", S.minWidthRem 8 ] [] ]

                                                                        ( "-2", _ ) ->
                                                                            case Maybe.andThen (Array.get i) (Result.toMaybe stats) of
                                                                                Just (Numeric stat) ->
                                                                                    [ H.div [ S.displayGrid, S.gridTemplateColumns "auto auto", S.gapRem 0, S.gridColumnGapRem 0.5, S.justifyContentFlexStart, S.opacity "0.5" ]
                                                                                        [ H.span [] [ text "min" ]
                                                                                        , H.span [] [ text (Maybe.withDefault "" (Maybe.map (String.fromFloat << round2) stat.min)) ]
                                                                                        , H.span [] [ text "max" ]
                                                                                        , H.span [] [ text (Maybe.withDefault "" (Maybe.map (String.fromFloat << round2) stat.max)) ]
                                                                                        , H.span [] [ text "mean" ]
                                                                                        , H.span [] [ text (iif (stat.count == 0) "" (String.fromInt (round (stat.sum / toFloat stat.count)))) ]
                                                                                        , H.span [] [ text "count" ]
                                                                                        , H.span [] [ text (String.fromInt stat.count) ]
                                                                                        ]
                                                                                    ]

                                                                                Just (Descriptive stat) ->
                                                                                    [ H.div [ S.displayGrid, S.gridTemplateColumns "auto auto", S.gapRem 0, S.gridColumnGapRem 0.5, S.justifyContentFlexStart, S.opacity "0.5" ]
                                                                                        [ H.span [] [ text "min" ]
                                                                                        , H.span [] [ text (Maybe.withDefault "" (Maybe.map String.fromInt stat.min)) ]
                                                                                        , H.span [] [ text "max" ]
                                                                                        , H.span [] [ text (String.fromInt stat.max) ]
                                                                                        , H.span [] [ text "mean" ]
                                                                                        , H.span [] [ text (iif (stat.count == 0) "" (String.fromInt (stat.sum // stat.count))) ]
                                                                                        , H.span [] [ text "count" ]
                                                                                        , H.span [] [ text (String.fromInt stat.count) ]
                                                                                        , H.span [] [ text "keywords" ]
                                                                                        , H.span [] [ text (String.join " " (Dict.keys (Dict.filter (\k v -> String.length k >= 4 && v >= 2) stat.keywords))) ]
                                                                                        ]
                                                                                    ]

                                                                                Just (Enumerative stat) ->
                                                                                    -- TODO:
                                                                                    []

                                                                                Nothing ->
                                                                                    []

                                                                        ( "-1", _ ) ->
                                                                            [ H.p [ S.displayBlock, S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.opacity "0.5", S.fontSizeSmall ]
                                                                                [ text (typeName col.typ)
                                                                                ]
                                                                            ]

                                                                        ( "0", _ ) ->
                                                                            case col.name of
                                                                                "" ->
                                                                                    []

                                                                                _ ->
                                                                                    [ H.span [ S.displayBlock, S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.fontWeight "600" ]
                                                                                        [ text col.name
                                                                                        ]
                                                                                    ]

                                                                        _ ->
                                                                            [ row
                                                                                |> Dict.get col.key
                                                                                |> Maybe.withDefault (E.string "")
                                                                                |> D.decodeValue
                                                                                    (D.maybe
                                                                                        (case col.typ of
                                                                                            Unknown ->
                                                                                                D.map text string

                                                                                            SheetId ->
                                                                                                D.string |> D.map (\id -> H.a [ A.href ("/" ++ id), S.overflowVisible, S.whiteSpaceNowrap, S.paddingRightRem 0.5 ] [ text "view" ])

                                                                                            Link ->
                                                                                                D.string |> D.map (\href -> H.a [ A.href href, A.target "_blank", A.rel "noopener noreferrer", S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.wordBreakKeepAll, S.hyphensNone ] [ text "link" ])

                                                                                            Image ->
                                                                                                D.string |> D.map (\src -> H.img [ A.src src ] [])

                                                                                            Text ->
                                                                                                D.map text string

                                                                                            Boolean ->
                                                                                                boolean |> D.map (\c -> H.input [ A.type_ "checkbox", A.checked c, A.onCheck (DocMsg << CellCheck { x = i, y = n }) ] [])

                                                                                            Number ->
                                                                                                D.oneOf
                                                                                                    [ D.map (text << String.fromFloat << round2) number
                                                                                                    , D.map text string
                                                                                                    ]

                                                                                            Usd ->
                                                                                                D.oneOf
                                                                                                    [ D.map (text << usd) number
                                                                                                    , D.map text string
                                                                                                    ]

                                                                                            Delete ->
                                                                                                D.string |> D.map (\sheet_id -> H.button [ A.onClick (DocDelete sheet_id) ] [ text "delete" ])

                                                                                            Create ->
                                                                                                D.value |> D.map (\val -> H.button [ A.onClick (DocNew val) ] [ text "add to library" ])

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
                                                                                    )
                                                                                |> Result.map (Maybe.withDefault (text ""))
                                                                                |> Result.mapError (D.errorToString >> text)
                                                                                |> result
                                                                            ]
                                                            )
                                                            (Array.toList cols)
                                                            ++ [ case sheet.doc of
                                                                    Ok (Tab _) ->
                                                                        H.th
                                                                            [ A.onClick (DocMsg SheetColumnPush)
                                                                            , S.textAlignLeft
                                                                            , S.widthRem 0.001
                                                                            , S.whiteSpaceNowrap
                                                                            , S.opacity "0.5"
                                                                            ]
                                                                            [ text (iif (n == 0) "→" "") ]

                                                                    _ ->
                                                                        text ""
                                                               ]
                                                )
                                            <|
                                                Array.append (Array.initialize 3 (always Dict.empty)) <|
                                                    rows
                                        ]

                                -- TODO: These rows should always be stuck to the bottom of the window.
                                , H.tfoot [] <|
                                    case sheet.doc of
                                        Ok Library ->
                                            List.map
                                                (\( label, msg ) ->
                                                    H.tr [ A.onClick msg ] <|
                                                        (::) (H.td [ S.opacity "0.25" ] [ text label ]) <|
                                                            List.map (\typ -> H.td [ S.opacity "0.25" ] [ text typ ])
                                                                [ "text"
                                                                , "list text"
                                                                , ""
                                                                ]
                                                )
                                                [ ( "table:...", DocNewTable )
                                                , ( "query:...", DocNewQuery )
                                                ]

                                        Ok (Tab _) ->
                                            [ H.tr [ A.onClick (DocMsg (SheetRowPush (Array.length rows))) ] <|
                                                List.concat
                                                    [ List.indexedMap (\i col -> H.td [ S.opacity "0.25" ] [ text (typeName col.typ) ]) <|
                                                        Array.toList cols
                                                    , case sheet.doc of
                                                        Ok (Tab _) ->
                                                            [ H.th
                                                                [ S.widthRem 0.001
                                                                , S.whiteSpaceNowrap
                                                                , S.opacity "0.5"
                                                                ]
                                                                [ text "↴"
                                                                ]
                                                            ]

                                                        _ ->
                                                            []
                                                    ]
                                            ]

                                        _ ->
                                            []
                                ]
                    ]
                ]
            , H.aside [ A.id "aside", S.displayFlex, S.flexDirectionColumn, S.height "100%", S.backgroundColor "#fff" ] <|
                case sheet.doc of
                    Ok (Tab _) ->
                        -- -- TODO: Conversational AI interface.
                        -- [ H.div [ S.displayFlex, S.flexDirectionColumn, S.gapRem 1, S.marginBottomRem 1.5, S.opacity "0.8" ]
                        --     [ H.p [ S.textAlignRight ] [ text "Vivamus dapibus porttitor eros, et semper mi ultricies sit amet." ]
                        --     , H.p [] [ text "Fusce euismod neque et elit vulputate commodo. Donec tempor eu justo vitae porttitor. Integer eget sem faucibus, ullamcorper turpis a, pretium enim." ]
                        --     , H.p [ S.textAlignRight ] [ text "Morbi nec metus pretium, laoreet tortor in, blandit ipsum." ]
                        --     , H.p [] [ text "Interdum et malesuada fames ac ante ipsum primis in faucibus. Cras egestas est dolor, vel euismod urna convallis vitae. " ]
                        --     ]
                        -- , H.textarea [] []
                        -- , H.button [] [ text "send (⌘⏎)" ]
                        -- ]
                        []

                    Ok (Query query) ->
                        [ H.div [ S.minHeightRem 10, S.height "100%", S.width "100%", S.minWidth "25vw", S.displayFlex, S.positionRelative, S.overflowAuto ]
                            [ H.textarea [ A.id "code", A.onInput (InputChange QueryCode), S.height "100%", S.width "100%", S.whiteSpacePre, S.fontSizeRem 0.75, S.border "none", S.backgroundColor "transparent", S.paddingRem 1, S.lineHeightRem 1.5 ]
                                [ text (String.trim query.code)
                                ]

                            -- TODO:
                            -- , H.div [ S.displayFlex, S.alignItemsCenter, S.gapRem 1, S.positionAbsolute, S.bottomRem 1.5, S.rightRem 1.5 ]
                            --     [ H.select []
                            --         [ H.option [] [ text "sql" ]
                            --         ]
                            --     , H.button [ S.backgroundColor "#f0f0f0", S.padding "0.25rem 0.5rem", S.border "1px solid #aaa" ] [ text "submit" ]
                            --     ]
                            ]
                        ]

                    _ ->
                        []
            ]
        ]
    }
