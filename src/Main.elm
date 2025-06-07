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
result res =
    case res of
        Ok x ->
            x

        Err x ->
            x



---- PORTS --------------------------------------------------------------------


port libraryChanged : (SheetMsg D.Value -> msg) -> Sub msg


port changeSheet : SheetMsg (List Patch) -> Cmd msg


port notifySheet : SheetMsg E.Value -> Cmd msg


port sheetChanged : (SheetMsg SheetChange -> msg) -> Sub msg


port sheetNotified : (SheetMsg D.Value -> msg) -> Sub msg


port selectSheet : Id -> Cmd msg


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
    = Private (Dict Id Perm)
    | Public


type alias Perm =
    { read : Bool
    , write : Bool
    , share : Bool
    }


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


type Table
    = Library
    | TableDoc Doc
    | TableFeed Feed
    | TableQuery Query


type alias Doc =
    { cols : Array Col
    , rows : Array Row
    }


type alias Col =
    { key : Int
    , name : String
    , typ : Type
    }


type alias Row =
    Dict Int D.Value


type alias Commit =
    { hash : String
    , author : String
    , email : String
    , date : String
    , message : String
    }


type
    Feed
    -- TODO: Move some of these to the shop as free templates.
    -- TODO:   email, settings, databases, git, github, stripe, logs, sheets, tests, code stats, social media keywords
    -- TODO: When you add feeds, it automatically adds some queries to your library, which you can hide?
    = Device
    | Shop
    | Files
    | Email {}
    | Database {}
    | OAuth {}
    | Form {}
    | Websocket { url : String, rows : Array D.Value }
    | Webhook {}
    | Kv {}
    | Webdav {}
    | Http { url : String, rows : Array D.Value }
    | Git { url : String, rows : Array Commit }
    | Crawler {}
    | Code { lang : Lang, code : String }
    | Rss { query : Query }
    | Box { query : Query }


type alias Query =
    -- -- TODO: queries fail if any sources not shared with you
    -- -- TODO: row actions/abilities (e.g. delete) are cells/columns too
    -- -- TODO: use PRQL AST?
    -- = From { source : Source }
    -- | Join { source : Source }
    -- | Filter {}
    -- | Select Select
    { query : String, rows : Array D.Value }


type Source
    = Hole
    | DocId Id
    | FeedId Id
    | QueryId Id


type Select
    = Columns {}
    | Chart {}
    | App {}


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


json : D.Decoder (Array Row)
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
                        D.map TableDoc
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

                    "feed" ->
                        D.field "feed" D.string
                            |> D.andThen
                                (\feed ->
                                    case feed of
                                        "websocket" ->
                                            D.map2 (\url rows -> TableFeed (Websocket { url = url, rows = Maybe.withDefault Array.empty rows }))
                                                (D.field "url" D.string)
                                                (D.maybe (D.field "rows" (D.array D.value)))

                                        "http" ->
                                            D.map2 (\url rows -> TableFeed (Http { url = url, rows = Maybe.withDefault Array.empty rows }))
                                                (D.field "url" D.string)
                                                (D.maybe (D.field "rows" (D.array D.value)))

                                        "git" ->
                                            D.map2 (\url rows -> TableFeed (Git { url = url, rows = rows }))
                                                (D.field "url" D.string)
                                                (D.field "rows"
                                                    (D.array
                                                        (D.map5 Commit
                                                            (D.field "hash" D.string)
                                                            (D.field "author" D.string)
                                                            (D.field "email" D.string)
                                                            (D.field "date" D.string)
                                                            (D.field "message" D.string)
                                                        )
                                                    )
                                                )

                                        feed_ ->
                                            D.fail ("Bad feed type: " ++ feed_)
                                )

                    "query" ->
                        D.map TableQuery
                            (D.map2 Query
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
    | RepoFetch (Result Http.Error (Array Commit))


type TableMsg
    = DocMsg DocMsg
    | FeedMsg ()
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
        , libraryChanged LibraryChange
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
                    , { default | id = "device", name = "device", table = Ok (TableFeed Device) }
                    , { default | id = "shop", name = "shop", table = Ok (TableFeed Shop) }
                    ]
                        |> List.map (\s -> ( s.id, s ))
                        |> Dict.fromList
                        |> Dict.get id
                        |> Maybe.withDefault model.sheet
              }
            , selectSheet id
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

                Ok (TableDoc doc) ->
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

                Ok (TableFeed Shop) ->
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
                            , [ E.string "data", E.string "RetailData", E.string "Product Catalog Manager", E.int 41234, E.float 4.1, E.float 35.0, E.string "Buy" ]
                            , [ E.string "code", E.string "ScrapeSheets", E.string "Date Formatter Utility", E.int 11234, E.float 4.6, E.float 0.5, E.string "Buy" ]
                            , [ E.string "data", E.string "DataBroker", E.string "Employment History Records", E.int 34890, E.float 3.8, E.float 219.99, E.string "Buy" ]
                            , [ E.string "data", E.string "RealEstate", E.string "Property Listing Database", E.int 13654, E.float 4.1, E.float 11.99, E.string "Buy" ]
                            , [ E.string "data", E.string "DataBroker", E.string "Location Intelligence Data", E.int 67890, E.float 3.6, E.float 249.99, E.string "Buy" ]
                            , [ E.string "data", E.string "FinanceData", E.string "Cryptocurrency Prices", E.int 67891, E.float 4.2, E.float 19.99, E.string "Buy" ]
                            , [ E.string "data", E.string "DataBroker", E.string "Household Demographics Report", E.int 41235, E.float 4.0, E.float 139.99, E.string "Buy" ]
                            , [ E.string "code", E.string "DevUtils", E.string "JSON Validator Tool", E.int 15678, E.float 4.7, E.float 2.49, E.string "Buy" ]
                            , [ E.string "code", E.string "WebDev", E.string "CSS Generator Studio", E.int 19876, E.float 4.4, E.float 1.75, E.string "Buy" ]
                            , [ E.string "data", E.string "GeoTools", E.string "World Cities Database", E.int 12456, E.float 4.1, E.float 15.0, E.string "Buy" ]
                            , [ E.string "data", E.string "HRTools", E.string "Employee Tracker System", E.int 17889, E.float 4.6, E.float 12.5, E.string "Buy" ]
                            , [ E.string "code", E.string "FileTools", E.string "PDF Merger Utility", E.int 44321, E.float 4.6, E.float 4.99, E.string "Buy" ]
                            , [ E.string "data", E.string "GeoTools", E.string "US ZIP Code Database", E.int 26543, E.float 4.2, E.float 9.99, E.string "Buy" ]
                            , [ E.string "data", E.string "OfficeTools", E.string "Invoice Generator Pro", E.int 23150, E.float 4.8, E.float 12.99, E.string "Buy" ]
                            , [ E.string "code", E.string "ScrapeSheets", E.string "Sales Tax Calculator", E.int 8932, E.float 4.2, E.float 1.01, E.string "Buy" ]
                            , [ E.string "data", E.string "SportsData", E.string "NFL Statistics Archive", E.int 38764, E.float 4.0, E.float 22.99, E.string "Buy" ]
                            , [ E.string "data", E.string "DataBroker", E.string "Digital Behavior Insights", E.int 54321, E.float 3.7, E.float 299.99, E.string "Buy" ]
                            , [ E.string "code", E.string "DevUtils", E.string "Form Validator Widget", E.int 7832, E.float 4.5, E.float 0.99, E.string "Buy" ]
                            , [ E.string "data", E.string "HealthData", E.string "Nutrition Facts Database", E.int 29876, E.float 4.3, E.float 18.99, E.string "Buy" ]
                            , [ E.string "data", E.string "Academics", E.string "Student Grade Tracker", E.int 12987, E.float 4.8, E.float 6.99, E.string "Buy" ]
                            , [ E.string "data", E.string "DataBroker", E.string "Consumer Profile Database", E.int 89432, E.float 3.8, E.float 199.99, E.string "Buy" ]
                            , [ E.string "data", E.string "DataBroker", E.string "Credit Profile Reports", E.int 43210, E.float 4.0, E.float 399.99, E.string "Buy" ]
                            , [ E.string "data", E.string "ScrapeSheets", E.string "Personal Budget Tracker", E.int 18924, E.float 4.6, E.float 7.5, E.string "Buy" ]
                            , [ E.string "data", E.string "DataBroker", E.string "Purchase History Analytics", E.int 76543, E.float 3.9, E.float 149.99, E.string "Buy" ]
                            , [ E.string "data", E.string "Fitness", E.string "Workout Log Template", E.int 16789, E.float 4.7, E.float 7.49, E.string "Buy" ]
                            , [ E.string "data", E.string "DataBroker", E.string "Lifestyle Segmentation Data", E.int 56789, E.float 3.9, E.float 169.99, E.string "Buy" ]
                            , [ E.string "data", E.string "ScrapeSheets", E.string "Timesheet Manager", E.int 22334, E.float 4.5, E.float 4.99, E.string "Buy" ]
                            , [ E.string "code", E.string "WebDev", E.string "Advanced Color Picker", E.int 34521, E.float 4.9, E.float 2.99, E.string "Buy" ]
                            , [ E.string "data", E.string "DataBroker", E.string "Property Owner Directory", E.int 45678, E.float 4.2, E.float 129.99, E.string "Buy" ]
                            , [ E.string "data", E.string "EventTools", E.string "Wedding Planner Kit", E.int 8765, E.float 4.9, E.float 15.99, E.string "Buy" ]
                            , [ E.string "code", E.string "TextTools", E.string "String Parser Library", E.int 6543, E.float 4.3, E.float 1.99, E.string "Buy" ]
                            , [ E.string "data", E.string "SocialMedia", E.string "Trend Analytics Dashboard", E.int 33445, E.float 3.9, E.float 49.99, E.string "Buy" ]
                            , [ E.string "data", E.string "DataBroker", E.string "Vehicle Registration Records", E.int 28901, E.float 4.1, E.float 89.99, E.string "Buy" ]
                            , [ E.string "data", E.string "OfficePro", E.string "Expense Report Template", E.int 19823, E.float 4.4, E.float 5.99, E.string "Buy" ]
                            , [ E.string "data", E.string "DataBroker", E.string "Healthcare Consumer Data", E.int 23456, E.float 3.4, E.float 599.99, E.string "Buy" ]
                            , [ E.string "data", E.string "MarketData", E.string "Real-Time Stock Prices", E.int 45670, E.float 4.3, E.float 29.99, E.string "Buy" ]
                            , [ E.string "code", E.string "MathTools", E.string "Statistics Calculator", E.int 9876, E.float 4.8, E.float 3.49, E.string "Buy" ]
                            ]
                                |> List.map (List.indexedMap Tuple.pair >> Dict.fromList)
                                |> Array.fromList
                        }

                Ok (TableFeed (Websocket ws)) ->
                    Ok
                        { cols = Array.fromList [ Col 0 "Payload" Json ]
                        , rows = ws.rows |> Array.map (Dict.singleton 0)
                        }

                Ok (TableFeed (Http ws)) ->
                    Ok
                        { cols = Array.fromList [ Col 0 "Payload" Json ]
                        , rows = ws.rows |> Array.map (Dict.singleton 0)
                        }

                Ok (TableQuery query) ->
                    -- TODO: Derive this from the query structure instead of the hardcoded name.
                    case info.name of
                        "Manifold Bet Stats" ->
                            Ok
                                { cols =
                                    Array.fromList
                                        [ Col 0 "outcome" Text
                                        , Col 1 "total_bets" Number
                                        , Col 2 "total_amount" Number
                                        , Col 3 "avg_amount" Number
                                        , Col 4 "max_amount" Number
                                        , Col 5 "unique_users" Number
                                        , Col 6 "avg_prob_change" Number
                                        , Col 7 "filled_rate" Number
                                        , Col 8 "api_bets" Number
                                        ]
                                , rows =
                                    let
                                        bets : List { outcome : String, amount : Float, userId : String, probBefore : Float, probAfter : Float, isFilled : Bool, isApi : Bool }
                                        bets =
                                            query.rows
                                                |> Array.map
                                                    (\row ->
                                                        { outcome = row |> D.decodeValue (D.at [ "data", "bets", "0", "outcome" ] D.string) |> Result.withDefault ""
                                                        , amount = row |> D.decodeValue (D.at [ "data", "bets", "0", "amount" ] D.float) |> Result.withDefault 0
                                                        , userId = row |> D.decodeValue (D.at [ "data", "bets", "0", "userId" ] D.string) |> Result.withDefault ""
                                                        , probBefore = row |> D.decodeValue (D.at [ "data", "bets", "0", "probBefore" ] D.float) |> Result.withDefault 0
                                                        , probAfter = row |> D.decodeValue (D.at [ "data", "bets", "0", "probAfter" ] D.float) |> Result.withDefault 0
                                                        , isFilled = row |> D.decodeValue (D.at [ "data", "bets", "0", "isFilled" ] D.bool) |> Result.withDefault False
                                                        , isApi = row |> D.decodeValue (D.at [ "data", "bets", "0", "isApi" ] D.bool) |> Result.withDefault False
                                                        }
                                                    )
                                                |> Array.toList

                                        groupedByOutcome : List ( String, List { outcome : String, amount : Float, userId : String, probBefore : Float, probAfter : Float, isFilled : Bool, isApi : Bool } )
                                        groupedByOutcome =
                                            bets
                                                |> List.foldr
                                                    (\bet acc ->
                                                        case acc |> List.filter (Tuple.first >> (==) bet.outcome) |> List.head of
                                                            Just ( outcome, existing ) ->
                                                                acc
                                                                    |> List.filter (Tuple.first >> (/=) bet.outcome)
                                                                    |> (::) ( outcome, bet :: existing )

                                                            Nothing ->
                                                                ( bet.outcome, [ bet ] ) :: acc
                                                    )
                                                    []
                                    in
                                    groupedByOutcome
                                        |> List.map
                                            (\( outcome, outcomeBets ) ->
                                                let
                                                    totalBets =
                                                        List.length outcomeBets

                                                    totalAmount =
                                                        outcomeBets |> List.map .amount |> List.sum

                                                    avgAmount =
                                                        if totalBets > 0 then
                                                            totalAmount / toFloat totalBets

                                                        else
                                                            0

                                                    maxAmount =
                                                        outcomeBets |> List.map .amount |> List.maximum |> Maybe.withDefault 0

                                                    uniqueUsers =
                                                        outcomeBets
                                                            |> List.map .userId
                                                            |> List.foldr
                                                                (\user acc ->
                                                                    if List.member user acc then
                                                                        acc

                                                                    else
                                                                        user :: acc
                                                                )
                                                                []
                                                            |> List.length

                                                    avgProbChange =
                                                        let
                                                            probChanges =
                                                                outcomeBets |> List.map (\bet -> abs (bet.probAfter - bet.probBefore))

                                                            totalChange =
                                                                List.sum probChanges
                                                        in
                                                        if totalBets > 0 then
                                                            totalChange / toFloat totalBets

                                                        else
                                                            0

                                                    filledCount =
                                                        outcomeBets |> List.filter .isFilled |> List.length

                                                    filledRate =
                                                        if totalBets > 0 then
                                                            toFloat filledCount / toFloat totalBets

                                                        else
                                                            0

                                                    apiCount =
                                                        outcomeBets |> List.filter .isApi |> List.length
                                                in
                                                Dict.fromList
                                                    [ ( 0, E.string outcome )
                                                    , ( 1, E.int totalBets )
                                                    , ( 2, E.float (round (totalAmount * 100) |> toFloat |> (\x -> x / 100)) )
                                                    , ( 3, E.float (round (avgAmount * 100) |> toFloat |> (\x -> x / 100)) )
                                                    , ( 4, E.float maxAmount )
                                                    , ( 5, E.int uniqueUsers )
                                                    , ( 6, E.float (round (avgProbChange * 10000) |> toFloat |> (\x -> x / 10000)) )
                                                    , ( 7, E.float (round (filledRate * 100) |> toFloat |> (\x -> x / 100)) )
                                                    , ( 8, E.int apiCount )
                                                    ]
                                            )
                                        |> Array.fromList
                                }

                        "Manifold Bet Latest" ->
                            Ok
                                { cols =
                                    Array.fromList
                                        [ Col 0 "userId" Text
                                        , Col 1 "amount" Number
                                        , Col 2 "outcome" Text
                                        , Col 3 "shares" Number
                                        , Col 4 "probBefore" Number
                                        , Col 5 "probAfter" Number
                                        , Col 6 "contractId" Text
                                        , Col 7 "createdTime" Timestamp
                                        , Col 8 "isFilled" Text
                                        , Col 9 "isRedemption" Text
                                        , Col 10 "isCancelled" Text
                                        , Col 11 "orderAmount" Text
                                        , Col 12 "loanAmount" Text
                                        , Col 13 "limitProb" Text
                                        , Col 14 "expiresAt" Timestamp
                                        , Col 15 "id" Text
                                        , Col 16 "isApi" Text
                                        , Col 17 "silent" Text
                                        , Col 18 "visibility" Text
                                        , Col 19 "matchedBetId" Text
                                        , Col 20 "timestamp" Timestamp
                                        , Col 21 "topic" Text
                                        , Col 22 "type" Text
                                        , Col 23 "creatorFee" Text
                                        , Col 24 "liquidityFee" Text
                                        , Col 25 "platformFee" Text
                                        ]
                                , rows =
                                    query.rows
                                        |> Array.map
                                            (\row ->
                                                Dict.fromList
                                                    [ ( 0, row |> D.decodeValue (D.at [ "data", "bets", "0", "userId" ] D.value) |> Result.withDefault E.null )
                                                    , ( 1, row |> D.decodeValue (D.at [ "data", "bets", "0", "amount" ] D.value) |> Result.withDefault E.null )
                                                    , ( 2, row |> D.decodeValue (D.at [ "data", "bets", "0", "outcome" ] D.value) |> Result.withDefault E.null )
                                                    , ( 3, row |> D.decodeValue (D.at [ "data", "bets", "0", "shares" ] D.value) |> Result.withDefault E.null )
                                                    , ( 4, row |> D.decodeValue (D.at [ "data", "bets", "0", "probBefore" ] D.value) |> Result.withDefault E.null )
                                                    , ( 5, row |> D.decodeValue (D.at [ "data", "bets", "0", "probAfter" ] D.value) |> Result.withDefault E.null )
                                                    , ( 6, row |> D.decodeValue (D.at [ "data", "bets", "0", "contractId" ] D.value) |> Result.withDefault E.null )
                                                    , ( 7, row |> D.decodeValue (D.at [ "data", "bets", "0", "createdTime" ] D.value) |> Result.withDefault E.null )
                                                    , ( 8, row |> D.decodeValue (D.at [ "data", "bets", "0", "isFilled" ] D.value) |> Result.withDefault E.null )
                                                    , ( 9, row |> D.decodeValue (D.at [ "data", "bets", "0", "isRedemption" ] D.value) |> Result.withDefault E.null )
                                                    , ( 10, row |> D.decodeValue (D.at [ "data", "bets", "0", "isCancelled" ] D.value) |> Result.withDefault E.null )
                                                    , ( 11, row |> D.decodeValue (D.at [ "data", "bets", "0", "orderAmount" ] D.value) |> Result.withDefault E.null )
                                                    , ( 12, row |> D.decodeValue (D.at [ "data", "bets", "0", "loanAmount" ] D.value) |> Result.withDefault E.null )
                                                    , ( 13, row |> D.decodeValue (D.at [ "data", "bets", "0", "limitProb" ] D.value) |> Result.withDefault E.null )
                                                    , ( 14, row |> D.decodeValue (D.at [ "data", "bets", "0", "expiresAt" ] D.value) |> Result.withDefault E.null )
                                                    , ( 15, row |> D.decodeValue (D.at [ "data", "bets", "0", "id" ] D.value) |> Result.withDefault E.null )
                                                    , ( 16, row |> D.decodeValue (D.at [ "data", "bets", "0", "isApi" ] D.value) |> Result.withDefault E.null )
                                                    , ( 17, row |> D.decodeValue (D.at [ "data", "bets", "0", "silent" ] D.value) |> Result.withDefault E.null )
                                                    , ( 18, row |> D.decodeValue (D.at [ "data", "bets", "0", "visibility" ] D.value) |> Result.withDefault E.null )
                                                    , ( 19, row |> D.decodeValue (D.at [ "data", "bets", "0", "fills", "matchedBetId" ] D.value) |> Result.withDefault E.null )
                                                    , ( 20, row |> D.decodeValue (D.at [ "data", "bets", "0", "fills", "timestamp" ] D.value) |> Result.withDefault E.null )
                                                    , ( 21, row |> D.decodeValue (D.field "topic" D.value) |> Result.withDefault E.null )
                                                    , ( 22, row |> D.decodeValue (D.field "type" D.value) |> Result.withDefault E.null )
                                                    , ( 23, row |> D.decodeValue (D.at [ "data", "bets", "0", "fees", "creatorFee" ] D.value) |> Result.withDefault E.null )
                                                    , ( 24, row |> D.decodeValue (D.at [ "data", "bets", "0", "fees", "liquidityFee" ] D.value) |> Result.withDefault E.null )
                                                    , ( 25, row |> D.decodeValue (D.at [ "data", "bets", "0", "fees", "platformFee" ] D.value) |> Result.withDefault E.null )
                                                    ]
                                            )
                                }

                        "Manifold Market Latest" ->
                            Ok
                                { cols =
                                    Array.fromList
                                        [ Col 0 "creatorAvatarUrl" Image

                                        -- , Col 3 "question" Text
                                        , Col 1 "creatorName" Text

                                        -- , Col 2 "creatorUsername" Text
                                        , Col 14 "url" Link
                                        , Col 4 "probability" Text
                                        , Col 5 "isResolved" Text
                                        , Col 6 "volume" Number
                                        , Col 7 "volume24Hours" Number
                                        , Col 8 "uniqueBettorCount" Number
                                        , Col 9 "totalLiquidity" Number

                                        -- , Col 10 "createdTime" Timestamp
                                        -- , Col 11 "closeTime" Timestamp
                                        -- , Col 12 "lastBetTime" Timestamp
                                        -- , Col 13 "lastUpdatedTime" Timestamp
                                        , Col 15 "id" Text
                                        , Col 16 "creatorId" Text

                                        -- , Col 17 "slug" Text
                                        , Col 18 "mechanism" Text
                                        , Col 19 "outcomeType" Text
                                        , Col 20 "p" Text

                                        -- , Col 21 "pool" Text
                                        ]
                                , rows =
                                    query.rows
                                        |> Array.map
                                            (\row ->
                                                Dict.fromList
                                                    [ ( 0, row |> D.decodeValue (D.field "creatorAvatarUrl" D.value) |> Result.withDefault E.null )
                                                    , ( 1, row |> D.decodeValue (D.field "creatorName" D.value) |> Result.withDefault E.null )
                                                    , ( 2, row |> D.decodeValue (D.field "creatorUsername" D.value) |> Result.withDefault E.null )
                                                    , ( 3, row |> D.decodeValue (D.field "question" D.value) |> Result.withDefault E.null )
                                                    , ( 4, row |> D.decodeValue (D.field "probability" D.value) |> Result.withDefault E.null )
                                                    , ( 5, row |> D.decodeValue (D.field "isResolved" D.value) |> Result.withDefault E.null )
                                                    , ( 6, row |> D.decodeValue (D.field "volume" D.value) |> Result.withDefault E.null )
                                                    , ( 7, row |> D.decodeValue (D.field "volume24Hours" D.value) |> Result.withDefault E.null )
                                                    , ( 8, row |> D.decodeValue (D.field "uniqueBettorCount" D.value) |> Result.withDefault E.null )
                                                    , ( 9, row |> D.decodeValue (D.field "totalLiquidity" D.value) |> Result.withDefault E.null )
                                                    , ( 10, row |> D.decodeValue (D.field "createdTime" D.value) |> Result.withDefault E.null )
                                                    , ( 11, row |> D.decodeValue (D.field "closeTime" D.value) |> Result.withDefault E.null )
                                                    , ( 12, row |> D.decodeValue (D.field "lastBetTime" D.value) |> Result.withDefault E.null )
                                                    , ( 13, row |> D.decodeValue (D.field "lastUpdatedTime" D.value) |> Result.withDefault E.null )
                                                    , ( 14, row |> D.decodeValue (D.field "url" D.value) |> Result.withDefault E.null )
                                                    , ( 15, row |> D.decodeValue (D.field "id" D.value) |> Result.withDefault E.null )
                                                    , ( 16, row |> D.decodeValue (D.field "creatorId" D.value) |> Result.withDefault E.null )
                                                    , ( 17, row |> D.decodeValue (D.field "slug" D.value) |> Result.withDefault E.null )
                                                    , ( 18, row |> D.decodeValue (D.field "mechanism" D.value) |> Result.withDefault E.null )
                                                    , ( 19, row |> D.decodeValue (D.field "outcomeType" D.value) |> Result.withDefault E.null )
                                                    , ( 20, row |> D.decodeValue (D.field "p" D.value) |> Result.withDefault E.null )
                                                    , ( 21, row |> D.decodeValue (D.field "pool" D.value) |> Result.withDefault E.null )
                                                    ]
                                            )
                                }

                        "Bluesky Stats" ->
                            Ok
                                { cols =
                                    Array.fromList
                                        [ Col 0 "collection" Text
                                        , Col 1 "count" Number
                                        , Col 2 "creates" Number
                                        , Col 3 "deletes" Number
                                        , Col 4 "unique_users" Number
                                        , Col 5 "avg_per_minute" Number
                                        , Col 6 "has_text_content" Number
                                        , Col 7 "has_embeds" Number
                                        ]
                                , rows =
                                    let
                                        commits : List { collection : String, operation : String, did : String, hasText : Bool, hasEmbed : Bool, timeUs : Float }
                                        commits =
                                            query.rows
                                                |> Array.map
                                                    (\row ->
                                                        { collection = row |> D.decodeValue (D.at [ "commit", "collection" ] D.string) |> Result.withDefault ""
                                                        , operation = row |> D.decodeValue (D.at [ "commit", "operation" ] D.string) |> Result.withDefault ""
                                                        , did = row |> D.decodeValue (D.at [ "commit", "did" ] D.string) |> Result.withDefault ""
                                                        , hasText = row |> D.decodeValue (D.at [ "commit", "record", "text" ] D.string) |> Result.toMaybe |> Maybe.map (String.isEmpty >> not) |> Maybe.withDefault False
                                                        , hasEmbed = row |> D.decodeValue (D.at [ "commit", "record", "embed" ] D.value) |> Result.toMaybe |> Maybe.map (\_ -> True) |> Maybe.withDefault False
                                                        , timeUs = row |> D.decodeValue (D.field "time_us" D.float) |> Result.withDefault 0
                                                        }
                                                    )
                                                |> Array.toList

                                        groupedByCollection : List ( String, List { collection : String, operation : String, did : String, hasText : Bool, hasEmbed : Bool, timeUs : Float } )
                                        groupedByCollection =
                                            commits
                                                |> List.foldr
                                                    (\commit acc ->
                                                        case acc |> List.filter (Tuple.first >> (==) commit.collection) |> List.head of
                                                            Just ( collection, existing ) ->
                                                                acc
                                                                    |> List.filter (Tuple.first >> (/=) commit.collection)
                                                                    |> (::) ( collection, commit :: existing )

                                                            Nothing ->
                                                                ( commit.collection, [ commit ] ) :: acc
                                                    )
                                                    []

                                        timeRange : { min : Float, max : Float }
                                        timeRange =
                                            let
                                                times =
                                                    commits |> List.map .timeUs |> List.filter ((/=) 0)
                                            in
                                            { min = times |> List.minimum |> Maybe.withDefault 0
                                            , max = times |> List.maximum |> Maybe.withDefault 0
                                            }

                                        minutesSpan : Float
                                        minutesSpan =
                                            max 1 ((timeRange.max - timeRange.min) / 1000000 / 60)
                                    in
                                    groupedByCollection
                                        |> List.map
                                            (\( collection, items ) ->
                                                let
                                                    totalCount =
                                                        List.length items

                                                    createCount =
                                                        items |> List.filter (.operation >> (==) "create") |> List.length

                                                    deleteCount =
                                                        items |> List.filter (.operation >> (==) "delete") |> List.length

                                                    uniqueUsers =
                                                        items
                                                            |> List.map .did
                                                            |> List.foldr
                                                                (\did acc ->
                                                                    if List.member did acc then
                                                                        acc

                                                                    else
                                                                        did :: acc
                                                                )
                                                                []
                                                            |> List.length

                                                    avgPerMinute =
                                                        toFloat totalCount / minutesSpan

                                                    textContentCount =
                                                        items |> List.filter .hasText |> List.length

                                                    embedCount =
                                                        items |> List.filter .hasEmbed |> List.length
                                                in
                                                Dict.fromList
                                                    [ ( 0, E.string collection )
                                                    , ( 1, E.int totalCount )
                                                    , ( 2, E.int createCount )
                                                    , ( 3, E.int deleteCount )
                                                    , ( 4, E.int uniqueUsers )
                                                    , ( 5, E.float (round (avgPerMinute * 100) |> toFloat |> (\x -> x / 100)) )
                                                    , ( 6, E.int textContentCount )
                                                    , ( 7, E.int embedCount )
                                                    ]
                                            )
                                        |> Array.fromList
                                }

                        "Scrapscript Contributors" ->
                            Ok
                                { cols =
                                    Array.fromList
                                        [ Col 0 "author" Text
                                        , Col 1 "commit_count" Number
                                        , Col 4 "days_active" Number
                                        , Col 5 "avg_commits_per_day" Number
                                        , Col 6 "avg_message_length" Number
                                        , Col 7 "merge_commits" Number
                                        , Col 8 "fix_commits" Number
                                        , Col 2 "first_commit" Text
                                        , Col 3 "last_commit" Text
                                        ]
                                , rows =
                                    let
                                        commits : List Commit
                                        commits =
                                            query.rows
                                                |> Array.map
                                                    (D.decodeValue
                                                        (D.map5 Commit
                                                            (D.field "hash" D.string)
                                                            (D.field "author" D.string)
                                                            (D.field "email" D.string)
                                                            (D.field "date" D.string)
                                                            (D.field "message" D.string)
                                                        )
                                                    )
                                                |> Array.toList
                                                |> List.filterMap Result.toMaybe

                                        groupedByAuthor : List ( String, List { author : String, email : String, date : String, message : String, hash : String } )
                                        groupedByAuthor =
                                            commits
                                                |> List.foldr
                                                    (\commit acc ->
                                                        case acc |> List.filter (Tuple.first >> (==) commit.author) |> List.head of
                                                            Just ( author, existing ) ->
                                                                acc
                                                                    |> List.filter (Tuple.first >> (/=) commit.author)
                                                                    |> (::) ( author, commit :: existing )

                                                            Nothing ->
                                                                ( commit.author, [ commit ] ) :: acc
                                                    )
                                                    []

                                        parseDate : String -> Int
                                        parseDate dateStr =
                                            -- Simple date parsing - assumes ISO format, convert to days since epoch
                                            dateStr
                                                |> String.left 10
                                                |> String.split "-"
                                                |> List.map (String.toInt >> Maybe.withDefault 0)
                                                |> (\parts ->
                                                        case parts of
                                                            year :: month :: day :: _ ->
                                                                -- Rough approximation: days since year 2000
                                                                (year - 2000) * 365 + month * 30 + day

                                                            _ ->
                                                                0
                                                   )
                                    in
                                    groupedByAuthor
                                        |> List.map
                                            (\( author, authorCommits ) ->
                                                let
                                                    commitCount =
                                                        List.length authorCommits

                                                    dates =
                                                        authorCommits |> List.map (.date >> parseDate) |> List.sort

                                                    firstCommitDate =
                                                        dates |> List.head |> Maybe.withDefault 0

                                                    lastCommitDate =
                                                        dates |> List.reverse |> List.head |> Maybe.withDefault 0

                                                    daysActive =
                                                        max 1 (lastCommitDate - firstCommitDate + 1)

                                                    avgCommitsPerDay =
                                                        toFloat commitCount / toFloat daysActive

                                                    avgMessageLength =
                                                        let
                                                            totalLength =
                                                                authorCommits |> List.map (.message >> String.length) |> List.sum
                                                        in
                                                        if commitCount > 0 then
                                                            toFloat totalLength / toFloat commitCount

                                                        else
                                                            0

                                                    mergeCommits =
                                                        authorCommits |> List.filter (.message >> String.toLower >> String.contains "merge") |> List.length

                                                    fixCommits =
                                                        authorCommits |> List.filter (.message >> String.toLower >> (\msg -> String.contains "fix" msg || String.contains "bug" msg)) |> List.length

                                                    firstCommitStr =
                                                        authorCommits |> List.sortBy (.date >> parseDate) |> List.head |> Maybe.map .date |> Maybe.withDefault ""

                                                    lastCommitStr =
                                                        authorCommits |> List.sortBy (.date >> parseDate) |> List.reverse |> List.head |> Maybe.map .date |> Maybe.withDefault ""
                                                in
                                                Dict.fromList
                                                    [ ( 0, E.string author )
                                                    , ( 1, E.int commitCount )
                                                    , ( 2, E.string firstCommitStr )
                                                    , ( 3, E.string lastCommitStr )
                                                    , ( 4, E.int daysActive )
                                                    , ( 5, E.float (round (avgCommitsPerDay * 100) |> toFloat |> (\x -> x / 100)) )
                                                    , ( 6, E.float (round (avgMessageLength * 10) |> toFloat |> (\x -> x / 10)) )
                                                    , ( 7, E.int mergeCommits )
                                                    , ( 8, E.int fixCommits )
                                                    ]
                                            )
                                        |> Array.fromList
                                }

                        _ ->
                            Ok
                                { cols = Array.fromList [ Col 0 "Row" Json ]
                                , rows = query.rows |> Array.map (Dict.singleton 0)
                                }

                Ok (TableFeed (Git git)) ->
                    Ok
                        { cols =
                            Array.fromList
                                -- [ Col 0 "hash" Text
                                [ Col 1 "author" Text

                                -- , Col 2 "email" Text
                                , Col 3 "date" Text
                                , Col 4 "message" Text
                                ]
                        , rows =
                            git.rows
                                |> Array.map
                                    (\commit ->
                                        Dict.fromList
                                            [ ( 0, E.string commit.hash )
                                            , ( 1, E.string commit.author )
                                            , ( 2, E.string commit.email )
                                            , ( 3, E.string commit.date )
                                            , ( 4, E.string commit.message )
                                            ]
                                    )
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
                            -- TODO:
                            [ H.textarea [ A.onInput (always NoOp), S.minHeightRem 10 ]
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
