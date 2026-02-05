port module Main exposing (main)

---- IMPORTS ------------------------------------------------------------------

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events as Browser
import Browser.Navigation as Nav
import Clipboard
import Date exposing (Date)
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html as H exposing (Html, text)
import Html.Attributes as A
import Html.Events as A
import Html.Lazy as H
import Html.Style as S
import Http
import Json.Decode as D
import Json.Encode as E
import Navigation as Nav2 exposing (Index, Rect, SortOrder(..), TableBounds, xy, rect)
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


formatPercentage : Float -> String
formatPercentage value =
    -- Assumes value is a decimal (e.g., 0.25 = 25%)
    let
        percentage =
            value * 100 |> round2

        formatted =
            if percentage == toFloat (round percentage) then
                String.fromInt (round percentage)

            else
                String.fromFloat percentage
    in
    formatted ++ "%"



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


port signup : String -> Cmd msg


port login : { email : String, password : String } -> Cmd msg


port logout : () -> Cmd msg


port importCsv : { filename : String, content : String } -> Cmd msg


port authResult : (D.Value -> msg) -> Sub msg


port copyToClipboard : String -> Cmd msg


port pasteFromClipboard : (String -> msg) -> Sub msg


port requestCopy : (() -> msg) -> Sub msg


port queryEditorState : ({ cursorPos : Int, textBeforeCursor : String } -> msg) -> Sub msg


port insertAtCursor : String -> Cmd msg


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
    , auth : Auth
    , deleteConfirm : Maybe String
    , showSettings : Bool
    }


type alias Auth =
    { state : AuthState
    , email : String
    , password : String
    }


type AuthState
    = Anonymous
    | LoggingIn
    | LoggedIn { usrId : String }


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
    , stats : Result String (Array Stat)
    , sort : Maybe ( String, SortOrder )
    , filters : Dict String Filter
    , filterOpen : Maybe String
    , findReplace : Maybe FindReplace
    , queryAutocomplete : Maybe QueryAutocomplete
    , undoStack : List UndoEntry
    , redoStack : List UndoEntry
    }


type alias UndoEntry =
    { forward : List Patch -- Patches to redo
    , backward : List Patch -- Patches to undo (inverse)
    }


type alias QueryAutocomplete =
    { trigger : String -- The text that triggered autocomplete (e.g., "@tab")
    , suggestions : List String -- Matching sheet IDs
    , selectedIndex : Int -- Currently highlighted suggestion
    }


type alias FindReplace =
    { findText : String
    , replaceText : String
    , showReplace : Bool
    , matches : List Index
    , currentMatch : Int
    }


type Filter
    = TextContains String
    | TextEquals String
    | NumberGreaterThan Float
    | NumberLessThan Float
    | NumberBetween Float Float
    | BooleanIs Bool


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
    | Percentage
    | Date
    | Many Type
    | Link
    | SheetId
    | Json
    | Timestamp
    | Image
    | Delete
    | Create
    | Form (Dict String String)
    | Enum (List String)


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

        Percentage ->
            "pct"

        Date ->
            "date"

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

        Enum options ->
            "enum:" ++ String.join "," options



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
                , ( "pct", Percentage )
                , ( "percentage", Percentage )
                , ( "percent", Percentage )
                , ( "sheet_id", SheetId )
                , ( "link", Link )
                , ( "image", Image )
                , ( "form", Form Dict.empty )
                , ( "timestamp", Timestamp )
                , ( "datetime", Timestamp )
                , ( "date", Date )
                , ( "json", Json )
                , ( "text", Text )
                , ( "string", Text )
                , ( "create", Create )
                ]

        parseType : String -> Type
        parseType typeStr =
            if String.startsWith "enum:" typeStr then
                -- Parse "enum:option1,option2,option3"
                typeStr
                    |> String.dropLeft 5
                    |> String.split ","
                    |> List.map String.trim
                    |> List.filter (not << String.isEmpty)
                    |> Enum

            else
                Dict.get typeStr types |> Maybe.withDefault Unknown
    in
    D.oneOf
        [ D.map3 Col
            (D.field "key" string)
            (D.field "name" D.string)
            (D.field "type" (D.nullable D.string |> D.map (Maybe.map parseType >> Maybe.withDefault Unknown)))
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
                    , sort = Nothing
                    , filters = Dict.empty
                    , filterOpen = Nothing
                    , findReplace = Nothing
                    , queryAutocomplete = Nothing
                    , undoStack = []
                    , redoStack = []
                    }
                , auth =
                    { state = Anonymous
                    , email = ""
                    , password = ""
                    }
                , deleteConfirm = Nothing
                , showSettings = False
                }
    in
    ( model, changeId model.id )


route : Url -> Model -> Model
route url model =
    let
        showSettings =
            url.fragment == Just "settings"

        baseModel =
            url
                |> UrlP.parse
                    (UrlP.map
                        (\id search -> { model | id = id, search = Maybe.withDefault "" search })
                        (UrlP.top
                            </> UrlP.oneOf [ UrlP.string, UrlP.map "" UrlP.top ]
                            <?> UrlQ.string "q"
                        )
                    )
                |> Maybe.withDefault model
    in
    { baseModel | showSettings = showSettings }



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
    | DocDeleteConfirm Id
    | DocDeleteCancel
    | SettingsOpen
    | SettingsClose
    | SettingsNameChange String
    | SettingsTagsChange String
    | KeyDown KeyEvent
    | CellMouseClick
    | CellMouseDoubleClick String
    | CellMouseDown
    | CellMouseUp
    | CellHover Index
    | ColumnSort String
    | FilterToggle String
    | FilterSet String Filter
    | FilterClear String
    | FilterInput String String
    | FindOpen Bool
    | FindClose
    | FindTextChange String
    | ReplaceTextChange String
    | FindNext
    | FindPrev
    | ReplaceOne
    | ReplaceAll
    | Undo
    | Redo
    | InputChange Input String
    | ShopFetch (Result Http.Error Table)
    | CsvImportSelect
    | CsvImportFile File
    | CsvImportUpload String String
    | AuthMsg AuthMsg
    | AuthResult D.Value
    | ClipboardCopy
    | ClipboardPaste String
    | SelectAll
    | QueryEditorUpdate { cursorPos : Int, textBeforeCursor : String }
    | AutocompleteSelect String
    | AutocompleteNav Int
    | AutocompleteClose


type alias KeyEvent =
    { key : String
    , shift : Bool
    , ctrl : Bool
    , meta : Bool
    }


type AuthMsg
    = AuthEmailChange String
    | AuthPasswordChange String
    | AuthSubmit
    | AuthLogout


type DocMsg
    = SheetWrite Index
    | SheetRowPush Int
    | SheetColumnPush
    | SheetRowDelete (List Int)
    | SheetColumnDelete (List Int)
    | SheetClearCells (List Index)
    | CellCheck Index Bool


type Input
    = SheetSearch
    | CellWrite
    | QueryCode
    | AuthEmail
    | AuthPassword



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
        , authResult AuthResult
        , Browser.onKeyDown keyEventDecoder
        , pasteFromClipboard ClipboardPaste
        , requestCopy (always ClipboardCopy)
        , queryEditorState QueryEditorUpdate
        ]


keyEventDecoder : D.Decoder Msg
keyEventDecoder =
    D.map5
        (\key shift ctrl meta tagName ->
            -- Skip keyboard handling when focus is on input elements
            if List.member tagName [ "INPUT", "TEXTAREA", "SELECT" ] then
                NoOp

            else
                KeyDown { key = key, shift = shift, ctrl = ctrl, meta = meta }
        )
        (D.field "key" D.string)
        (D.field "shiftKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.field "metaKey" D.bool)
        (D.at [ "target", "tagName" ] D.string |> D.maybe |> D.map (Maybe.withDefault ""))



---- UPDATE -------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ sheet, auth } as model) =
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
                    , stats = data.data.doc |> D.decodeValue docDecoder |> Result.mapError D.errorToString |> Result.andThen computeStats
                    , sort = Nothing
                    , filters = Dict.empty
                    , filterOpen = Nothing
                    , findReplace = Nothing
                    , queryAutocomplete = Nothing
                    , undoStack = []
                    , redoStack = []
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
                (let parsedDoc = data.data.doc |> D.decodeValue docDecoder |> Result.mapError D.errorToString
                 in { model | sheet = { sheet | doc = parsedDoc, stats = Result.andThen computeStats parsedDoc } })
              -- TODO: Fetch table rows depending on type, e.g. portal:123
            , Cmd.none
            )

        DocNotify data ->
            -- Handle document notifications (collaboration events, peer updates, etc.)
            if data.id /= model.sheet.id then
                ( model, Cmd.none )

            else
                -- Decode the notification and update model accordingly
                let
                    notificationType =
                        data.data
                            |> D.decodeValue (D.field "type" D.string)
                            |> Result.withDefault ""
                in
                case notificationType of
                    "refresh" ->
                        -- Request a fresh copy of the document
                        ( model, changeId model.sheet.id )

                    "error" ->
                        let
                            errorMsg =
                                data.data
                                    |> D.decodeValue (D.field "message" D.string)
                                    |> Result.withDefault "Unknown error"
                        in
                        ( { model | error = errorMsg }, Cmd.none )

                    _ ->
                        -- Unknown notification type, ignore
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
            case sheet.doc of
                Ok Library ->
                    ( { model | sheet = { sheet | write = Nothing } }
                    , case edit of
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
                    )

                Ok (Tab table) ->
                    let
                        -- Helper to get old cell value as E.Value
                        getOldValue : Int -> String -> E.Value
                        getOldValue rowIdx key =
                            table.rows
                                |> Array.get (rowIdx - 1)
                                |> Maybe.andThen (Dict.get key)
                                |> Maybe.map (\v -> D.decodeValue D.value v |> Result.withDefault E.null)
                                |> Maybe.withDefault E.null

                        -- Compute forward and backward patches based on edit type
                        ( forwardPatches, backwardPatches ) =
                            case edit of
                                SheetWrite { x, y } ->
                                    case ( max 0 y, Array.get x table.cols ) of
                                        ( 0, Just col ) ->
                                            -- Editing column header (row 0)
                                            let
                                                forward =
                                                    sheet.write
                                                        |> Maybe.map
                                                            (\write ->
                                                                [ { action = "set"
                                                                  , path = [ E.int 0, E.string (String.fromInt x) ]
                                                                  , value =
                                                                        if y == -1 then
                                                                            E.object [ ( "name", E.string col.name ), ( "type", E.string write ), ( "key", E.string col.key ) ]

                                                                        else
                                                                            E.object [ ( "name", E.string write ), ( "type", E.string (typeName col.typ) ), ( "key", E.string col.key ) ]
                                                                  }
                                                                ]
                                                            )
                                                        |> Maybe.withDefault []

                                                backward =
                                                    [ { action = "set"
                                                      , path = [ E.int 0, E.string (String.fromInt x) ]
                                                      , value = E.object [ ( "name", E.string col.name ), ( "type", E.string (typeName col.typ) ), ( "key", E.string col.key ) ]
                                                      }
                                                    ]
                                            in
                                            ( forward, backward )

                                        ( rowY, Just col ) ->
                                            -- Editing data cell
                                            let
                                                oldValue =
                                                    getOldValue rowY col.key

                                                forward =
                                                    [ { action = "set"
                                                      , path = [ E.int rowY, E.string col.key ]
                                                      , value = sheet.write |> Maybe.map E.string |> Maybe.withDefault E.null
                                                      }
                                                    ]

                                                backward =
                                                    [ { action = "set"
                                                      , path = [ E.int rowY, E.string col.key ]
                                                      , value = oldValue
                                                      }
                                                    ]
                                            in
                                            ( forward, backward )

                                        _ ->
                                            ( [], [] )

                                SheetRowPush _ ->
                                    let
                                        rowCount =
                                            Array.length table.rows + 1

                                        forward =
                                            [ { action = "push"
                                              , path = []
                                              , value = E.list identity [ E.object [] ]
                                              }
                                            ]

                                        backward =
                                            [ { action = "splice"
                                              , path = []
                                              , value = E.list E.int [ rowCount, 1 ]
                                              }
                                            ]
                                    in
                                    ( forward, backward )

                                SheetColumnPush ->
                                    let
                                        colCount =
                                            Array.length table.cols

                                        forward =
                                            [ { action = "push"
                                              , path = [ E.int 0 ]
                                              , value = E.list identity [ E.object [ ( "name", E.string "" ), ( "type", E.string "text" ), ( "key", E.int colCount ) ] ]
                                              }
                                            ]

                                        backward =
                                            [ { action = "splice"
                                              , path = [ E.int 0 ]
                                              , value = E.list E.int [ colCount, 1 ]
                                              }
                                            ]
                                    in
                                    ( forward, backward )

                                SheetRowDelete indices ->
                                    -- Delete rows - no undo support for now (would need to store row data)
                                    let
                                        forward =
                                            indices
                                                |> List.sort
                                                |> List.reverse
                                                |> List.map
                                                    (\i ->
                                                        { action = "splice"
                                                        , path = []
                                                        , value = E.list E.int [ i, 1 ]
                                                        }
                                                    )
                                    in
                                    ( forward, [] )

                                SheetColumnDelete indices ->
                                    -- Delete columns - no undo support for now (would need to store column data)
                                    let
                                        colKeys =
                                            indices
                                                |> List.filterMap (\i -> Array.get i table.cols)
                                                |> List.map .key

                                        colPatches =
                                            indices
                                                |> List.sort
                                                |> List.reverse
                                                |> List.map
                                                    (\i ->
                                                        { action = "splice"
                                                        , path = [ E.int 0 ]
                                                        , value = E.list E.int [ i, 1 ]
                                                        }
                                                    )

                                        rowPatches =
                                            table.rows
                                                |> Array.toIndexedList
                                                |> List.concatMap
                                                    (\( rowIdx, _ ) ->
                                                        colKeys
                                                            |> List.map
                                                                (\key ->
                                                                    { action = "del"
                                                                    , path = [ E.int (rowIdx + 1), E.string key ]
                                                                    , value = E.null
                                                                    }
                                                                )
                                                    )
                                    in
                                    ( colPatches ++ rowPatches, [] )

                                SheetClearCells indices ->
                                    let
                                        patchPairs =
                                            indices
                                                |> List.filterMap
                                                    (\idx ->
                                                        Array.get idx.x table.cols
                                                            |> Maybe.map
                                                                (\col ->
                                                                    ( { action = "set"
                                                                      , path = [ E.int idx.y, E.string col.key ]
                                                                      , value = E.string ""
                                                                      }
                                                                    , { action = "set"
                                                                      , path = [ E.int idx.y, E.string col.key ]
                                                                      , value = getOldValue idx.y col.key
                                                                      }
                                                                    )
                                                                )
                                                    )

                                        forward =
                                            List.map Tuple.first patchPairs

                                        backward =
                                            List.map Tuple.second patchPairs
                                    in
                                    ( forward, backward )

                                CellCheck i c ->
                                    case Array.get i.x table.cols of
                                        Just col ->
                                            let
                                                oldValue =
                                                    getOldValue i.y col.key

                                                forward =
                                                    [ { action = "set"
                                                      , path = [ E.int i.y, E.string col.key ]
                                                      , value = E.bool c
                                                      }
                                                    ]

                                                backward =
                                                    [ { action = "set"
                                                      , path = [ E.int i.y, E.string col.key ]
                                                      , value = oldValue
                                                      }
                                                    ]
                                            in
                                            ( forward, backward )

                                        Nothing ->
                                            ( [], [] )

                        -- Update undo stack if we have patches to track
                        newUndoStack =
                            if List.isEmpty forwardPatches || List.isEmpty backwardPatches then
                                sheet.undoStack

                            else
                                { forward = forwardPatches, backward = backwardPatches } :: sheet.undoStack |> List.take 50

                        -- Clear redo stack on new changes (unless no undo tracking)
                        newRedoStack =
                            if List.isEmpty backwardPatches then
                                sheet.redoStack

                            else
                                []
                    in
                    if List.isEmpty forwardPatches then
                        ( { model | sheet = { sheet | write = Nothing } }, Cmd.none )

                    else
                        ( { model
                            | sheet =
                                { sheet
                                    | write = Nothing
                                    , undoStack = newUndoStack
                                    , redoStack = newRedoStack
                                }
                          }
                        , changeDoc { id = sheet.id, data = forwardPatches }
                        )

                _ ->
                    ( { model | sheet = { sheet | write = Nothing } }, Cmd.none )

        DocDelete id ->
            -- Show confirmation instead of immediately deleting
            ( { model | deleteConfirm = Just id }, Cmd.none )

        DocDeleteConfirm id ->
            -- Actually delete after confirmation
            ( { model | deleteConfirm = Nothing }, deleteDoc id )

        DocDeleteCancel ->
            ( { model | deleteConfirm = Nothing }, Cmd.none )

        SettingsOpen ->
            ( { model | showSettings = True }, Cmd.none )

        SettingsClose ->
            ( { model | showSettings = False }, Nav.replaceUrl model.nav ("/" ++ model.sheet.id) )

        SettingsNameChange newName ->
            ( model, updateLibrary (Idd sheet.id { name = Just newName, tags = Nothing }) )

        SettingsTagsChange newTags ->
            let
                tags =
                    newTags
                        |> String.split ","
                        |> List.map String.trim
                        |> List.filter (not << String.isEmpty)
            in
            ( model, updateLibrary (Idd sheet.id { name = Nothing, tags = Just tags }) )

        DocNew x ->
            ( model, newDoc x )

        DocNewTable ->
            ( model, newDoc <| E.object [ ( "type", E.string "table" ), ( "data", E.list identity [ E.list identity [ E.object [ ( "name", E.string "a" ), ( "type", E.string "text" ), ( "key", E.string "a" ) ] ] ] ) ] )

        DocNewQuery ->
            ( model, newDoc <| E.object [ ( "type", E.string "query" ), ( "data", E.list identity [ E.object [ ( "lang", E.string "sql" ), ( "code", E.string "select 1" ) ] ] ) ] )

        ShopFetch x ->
            ( { model | sheet = { sheet | table = Result.mapError (always "Something went wrong.") x } }, Cmd.none )

        CsvImportSelect ->
            ( model, Select.file [ "text/csv", ".csv" ] CsvImportFile )

        CsvImportFile file ->
            ( model
            , Task.perform (CsvImportUpload (File.name file)) (File.toString file)
            )

        CsvImportUpload filename content ->
            ( model, importCsv { filename = filename, content = content } )

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

        InputChange AuthEmail x ->
            ( { model | auth = { auth | email = x } }, Cmd.none )

        InputChange AuthPassword x ->
            ( { model | auth = { auth | password = x } }, Cmd.none )

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

        ColumnSort key ->
            let
                newSort =
                    case sheet.sort of
                        Just ( currentKey, Ascending ) ->
                            if currentKey == key then
                                Just ( key, Descending )

                            else
                                Just ( key, Ascending )

                        Just ( currentKey, Descending ) ->
                            if currentKey == key then
                                Nothing

                            else
                                Just ( key, Ascending )

                        Nothing ->
                            Just ( key, Ascending )
            in
            ( { model | sheet = { sheet | sort = newSort } }, Cmd.none )

        FilterToggle key ->
            let
                newFilterOpen =
                    if sheet.filterOpen == Just key then
                        Nothing

                    else
                        Just key
            in
            ( { model | sheet = { sheet | filterOpen = newFilterOpen } }, Cmd.none )

        FilterSet key filter ->
            ( { model
                | sheet =
                    { sheet
                        | filters = Dict.insert key filter sheet.filters
                        , filterOpen = Nothing
                    }
              }
            , Cmd.none
            )

        FilterClear key ->
            ( { model
                | sheet =
                    { sheet
                        | filters =
                            if key == "" then
                                Dict.empty

                            else
                                Dict.remove key sheet.filters
                        , filterOpen = Nothing
                    }
              }
            , Cmd.none
            )

        FilterInput key value ->
            -- Create a TextContains filter from the input value
            if String.isEmpty value then
                ( { model | sheet = { sheet | filters = Dict.remove key sheet.filters } }, Cmd.none )

            else
                ( { model | sheet = { sheet | filters = Dict.insert key (TextContains value) sheet.filters } }, Cmd.none )

        FindOpen showReplace ->
            ( { model
                | sheet =
                    { sheet
                        | findReplace =
                            Just
                                { findText = ""
                                , replaceText = ""
                                , showReplace = showReplace
                                , matches = []
                                , currentMatch = 0
                                }
                    }
              }
            , Task.attempt (always NoOp) (Dom.focus "find-input")
            )

        FindClose ->
            ( { model | sheet = { sheet | findReplace = Nothing } }, Cmd.none )

        FindTextChange findText ->
            case sheet.findReplace of
                Just fr ->
                    let
                        -- Find all matching cells
                        matches =
                            case sheet.doc of
                                Ok (Tab tbl) ->
                                    if String.isEmpty findText then
                                        []

                                    else
                                        tbl.rows
                                            |> Array.toIndexedList
                                            |> List.concatMap
                                                (\( rowIdx, row ) ->
                                                    tbl.cols
                                                        |> Array.toIndexedList
                                                        |> List.filterMap
                                                            (\( colIdx, col ) ->
                                                                let
                                                                    cellVal =
                                                                        Dict.get col.key row
                                                                            |> Maybe.andThen (D.decodeValue string >> Result.toMaybe)
                                                                            |> Maybe.withDefault ""
                                                                in
                                                                if String.contains (String.toLower findText) (String.toLower cellVal) then
                                                                    Just (xy colIdx (rowIdx + 1))

                                                                else
                                                                    Nothing
                                                            )
                                                )

                                _ ->
                                    []

                        newFr =
                            { fr
                                | findText = findText
                                , matches = matches
                                , currentMatch = 0
                            }

                        -- Select the first match if any
                        newSelect =
                            case List.head matches of
                                Just idx ->
                                    Rect idx idx

                                Nothing ->
                                    sheet.select
                    in
                    ( { model | sheet = { sheet | findReplace = Just newFr, select = newSelect } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ReplaceTextChange replaceText ->
            case sheet.findReplace of
                Just fr ->
                    ( { model | sheet = { sheet | findReplace = Just { fr | replaceText = replaceText } } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FindNext ->
            case sheet.findReplace of
                Just fr ->
                    if List.isEmpty fr.matches then
                        ( model, Cmd.none )

                    else
                        let
                            nextIdx =
                                modBy (List.length fr.matches) (fr.currentMatch + 1)

                            nextMatch =
                                fr.matches |> List.drop nextIdx |> List.head

                            newSelect =
                                case nextMatch of
                                    Just idx ->
                                        Rect idx idx

                                    Nothing ->
                                        sheet.select
                        in
                        ( { model | sheet = { sheet | findReplace = Just { fr | currentMatch = nextIdx }, select = newSelect } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FindPrev ->
            case sheet.findReplace of
                Just fr ->
                    if List.isEmpty fr.matches then
                        ( model, Cmd.none )

                    else
                        let
                            len =
                                List.length fr.matches

                            prevIdx =
                                modBy len (fr.currentMatch - 1 + len)

                            prevMatch =
                                fr.matches |> List.drop prevIdx |> List.head

                            newSelect =
                                case prevMatch of
                                    Just idx ->
                                        Rect idx idx

                                    Nothing ->
                                        sheet.select
                        in
                        ( { model | sheet = { sheet | findReplace = Just { fr | currentMatch = prevIdx }, select = newSelect } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ReplaceOne ->
            case ( sheet.findReplace, sheet.doc ) of
                ( Just fr, Ok (Tab tbl) ) ->
                    case fr.matches |> List.drop fr.currentMatch |> List.head of
                        Just matchIdx ->
                            case Array.get matchIdx.x tbl.cols of
                                Just col ->
                                    ( model
                                    , changeDoc
                                        { id = sheet.id
                                        , data =
                                            [ { action = "set"
                                              , path = [ E.int matchIdx.y, E.string col.key ]
                                              , value = E.string fr.replaceText
                                              }
                                            ]
                                        }
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReplaceAll ->
            case ( sheet.findReplace, sheet.doc ) of
                ( Just fr, Ok (Tab tbl) ) ->
                    let
                        patches =
                            fr.matches
                                |> List.filterMap
                                    (\matchIdx ->
                                        Array.get matchIdx.x tbl.cols
                                            |> Maybe.map
                                                (\col ->
                                                    { action = "set"
                                                    , path = [ E.int matchIdx.y, E.string col.key ]
                                                    , value = E.string fr.replaceText
                                                    }
                                                )
                                    )
                    in
                    if List.isEmpty patches then
                        ( model, Cmd.none )

                    else
                        ( model
                        , changeDoc { id = sheet.id, data = patches }
                        )

                _ ->
                    ( model, Cmd.none )

        Undo ->
            case sheet.undoStack of
                [] ->
                    ( model, Cmd.none )

                entry :: rest ->
                    ( { model
                        | sheet =
                            { sheet
                                | undoStack = rest
                                , redoStack = entry :: sheet.redoStack
                            }
                      }
                    , changeDoc { id = sheet.id, data = entry.backward }
                    )

        Redo ->
            case sheet.redoStack of
                [] ->
                    ( model, Cmd.none )

                entry :: rest ->
                    ( { model
                        | sheet =
                            { sheet
                                | redoStack = rest
                                , undoStack = entry :: sheet.undoStack
                            }
                      }
                    , changeDoc { id = sheet.id, data = entry.forward }
                    )

        KeyDown event ->
            -- Handle global shortcuts first (Ctrl+F, Ctrl+H, Escape for find/replace)
            if (event.ctrl || event.meta) && event.key == "f" then
                update (FindOpen False) model

            else if (event.ctrl || event.meta) && event.key == "h" then
                update (FindOpen True) model

            else if event.key == "Escape" && sheet.findReplace /= Nothing then
                update FindClose model

            else if event.key == "Enter" && sheet.findReplace /= Nothing then
                update FindNext model

            else if (event.ctrl || event.meta) && event.key == "z" && not event.shift then
                update Undo model

            else if (event.ctrl || event.meta) && event.key == "z" && event.shift then
                update Redo model

            else if (event.ctrl || event.meta) && event.key == "y" then
                update Redo model

            else
                let
                    isEditing =
                        sheet.write /= Nothing

                    isFindOpen =
                        sheet.findReplace /= Nothing

                    sel =
                        sheet.select.a

                    -- Get table bounds for navigation
                    bounds =
                        case sheet.doc of
                            Ok (Tab tbl) ->
                                { maxX = Array.length tbl.cols - 1
                                , maxY = Array.length tbl.rows
                                }

                            Ok Library ->
                                { maxX = Array.length libraryCols - 1
                                , maxY = Dict.size model.library
                                }

                            _ ->
                                { maxX = 0, maxY = 0 }

                    -- Move selection, clamping to bounds
                    move : Int -> Int -> ( Model, Cmd Msg )
                    move dx dy =
                        let
                            newX =
                                clamp 0 bounds.maxX (sel.x + dx)

                            newY =
                                clamp 1 bounds.maxY (sel.y + dy)

                            newSel =
                                xy newX newY
                        in
                        ( { model | sheet = { sheet | select = Rect newSel newSel } }
                        , Cmd.none
                        )

                    -- Start editing the current cell
                    startEdit =
                        case sheet.doc of
                            Ok (Tab tbl) ->
                                let
                                    col =
                                        Array.get sel.x tbl.cols

                                    row =
                                        Array.get (sel.y - 1) tbl.rows
                                in
                                case ( col, row ) of
                                    ( Just c, Just r ) ->
                                        let
                                            val =
                                                r
                                                    |> Dict.get c.key
                                                    |> Maybe.andThen (D.decodeValue string >> Result.toMaybe)
                                                    |> Maybe.withDefault ""
                                        in
                                        ( { model | sheet = { sheet | write = Just val } }
                                        , Task.attempt (always NoOp) (Dom.focus "new-cell")
                                        )

                                    _ ->
                                        ( model, Cmd.none )

                            Ok Library ->
                                -- Library editing handled differently
                                ( model, Cmd.none )

                            _ ->
                                ( model, Cmd.none )
                in
                if isEditing then
                    -- When editing a cell
                    case event.key of
                    "Enter" ->
                        -- Confirm edit and move down
                        let
                            newY =
                                clamp 1 bounds.maxY (sel.y + 1)

                            newSel =
                                xy sel.x newY
                        in
                        ( { model | sheet = { sheet | select = Rect newSel newSel } }
                        , Task.attempt (always NoOp) (Dom.blur "new-cell")
                        )

                    "Escape" ->
                        -- Cancel edit (discard changes)
                        ( { model | sheet = { sheet | write = Nothing } }
                        , Cmd.none
                        )

                    "Tab" ->
                        -- Confirm and move horizontally
                        let
                            dx =
                                iif event.shift -1 1

                            newX =
                                clamp 0 bounds.maxX (sel.x + dx)

                            newSel =
                                xy newX sel.y
                        in
                        ( { model | sheet = { sheet | select = Rect newSel newSel } }
                        , Task.attempt (always NoOp) (Dom.blur "new-cell")
                        )

                    _ ->
                        ( model, Cmd.none )

            else if sel.x < 0 || sel.y < 0 then
                -- No selection yet, ignore navigation
                ( model, Cmd.none )

            else
                -- When not editing, navigate with keys
                let
                    -- Expand selection instead of moving when shift is held
                    expand dx dy =
                        let
                            newSelect =
                                Nav2.expandSelection bounds dx dy sheet.select
                        in
                        ( { model | sheet = { sheet | select = newSelect } }, Cmd.none )

                    -- Get selected row indices for deletion
                    selectedRows =
                        let
                            norm =
                                Nav2.normalizeRect sheet.select
                        in
                        List.range norm.a.y norm.b.y

                    -- Get selected column indices for deletion
                    selectedCols =
                        let
                            norm =
                                Nav2.normalizeRect sheet.select
                        in
                        List.range norm.a.x norm.b.x

                    -- Get all selected cell indices for clearing
                    selectedCells =
                        Nav2.rectToIndices sheet.select
                in
                case event.key of
                    "ArrowUp" ->
                        if event.shift then
                            expand 0 -1

                        else
                            move 0 -1

                    "ArrowDown" ->
                        if event.shift then
                            expand 0 1

                        else
                            move 0 1

                    "ArrowLeft" ->
                        if event.shift then
                            expand -1 0

                        else
                            move -1 0

                    "ArrowRight" ->
                        if event.shift then
                            expand 1 0

                        else
                            move 1 0

                    "Tab" ->
                        move (iif event.shift -1 1) 0

                    "Enter" ->
                        -- Start editing current cell
                        startEdit

                    "Delete" ->
                        -- Clear selected cells (or delete row if Ctrl)
                        if event.ctrl || event.meta then
                            update (DocMsg (SheetRowDelete selectedRows)) model

                        else
                            update (DocMsg (SheetClearCells selectedCells)) model

                    "Backspace" ->
                        -- Clear selected cells
                        update (DocMsg (SheetClearCells selectedCells)) model

                    "a" ->
                        -- Ctrl+A to select all
                        if event.ctrl || event.meta then
                            update SelectAll model

                        else
                            -- Start editing with 'a'
                            case sheet.doc of
                                Ok (Tab tbl) ->
                                    case Array.get sel.x tbl.cols of
                                        Just _ ->
                                            ( { model | sheet = { sheet | write = Just "a" } }
                                            , Task.attempt (always NoOp) (Dom.focus "new-cell")
                                            )

                                        _ ->
                                            ( model, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                    "Home" ->
                        -- Jump to beginning of row, or top-left with Ctrl
                        if event.ctrl || event.meta then
                            let
                                newSel =
                                    xy 0 1
                            in
                            ( { model | sheet = { sheet | select = Rect newSel newSel } }, Cmd.none )

                        else
                            let
                                newSel =
                                    xy 0 sel.y
                            in
                            ( { model | sheet = { sheet | select = Rect newSel newSel } }, Cmd.none )

                    "End" ->
                        -- Jump to end of row, or bottom-right with Ctrl
                        if event.ctrl || event.meta then
                            let
                                newSel =
                                    xy bounds.maxX bounds.maxY
                            in
                            ( { model | sheet = { sheet | select = Rect newSel newSel } }, Cmd.none )

                        else
                            let
                                newSel =
                                    xy bounds.maxX sel.y
                            in
                            ( { model | sheet = { sheet | select = Rect newSel newSel } }, Cmd.none )

                    _ ->
                        -- If it's a single printable character, start editing with it
                        if String.length event.key == 1 && not event.ctrl && not event.meta then
                            case sheet.doc of
                                Ok (Tab tbl) ->
                                    let
                                        col =
                                            Array.get sel.x tbl.cols
                                    in
                                    case col of
                                        Just c ->
                                            -- Start editing with the typed character
                                            ( { model | sheet = { sheet | write = Just event.key } }
                                            , Task.attempt (always NoOp) (Dom.focus "new-cell")
                                            )

                                        _ ->
                                            ( model, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        else
                            ( model, Cmd.none )

        QueryEditorUpdate { cursorPos, textBeforeCursor } ->
            -- Handle special keyboard navigation signals
            case textBeforeCursor of
                "__NAV_DOWN__" ->
                    update (AutocompleteNav 1) model

                "__NAV_UP__" ->
                    update (AutocompleteNav -1) model

                "__SELECT__" ->
                    case sheet.queryAutocomplete of
                        Just ac ->
                            ac.suggestions
                                |> List.drop ac.selectedIndex
                                |> List.head
                                |> Maybe.map (\ref -> update (AutocompleteSelect ref) model)
                                |> Maybe.withDefault ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                "__CLOSE__" ->
                    update AutocompleteClose model

                _ ->
                    -- Normal cursor position update - check for autocomplete trigger
                    let
                        -- Find the last @ and get text after it
                        maybeAtIndex =
                            String.indices "@" textBeforeCursor
                                |> List.reverse
                                |> List.head

                        autocomplete =
                            case maybeAtIndex of
                                Just atIdx ->
                                    let
                                        trigger =
                                            String.dropLeft atIdx textBeforeCursor

                                        -- Filter matching sheet IDs
                                        searchTerm =
                                            String.dropLeft 1 trigger
                                                |> String.toLower

                                        matches =
                                            model.library
                                                |> Dict.keys
                                                |> List.filter
                                                    (\k ->
                                                        (String.startsWith "table:" k || String.startsWith "query:" k)
                                                            && String.contains searchTerm (String.toLower k)
                                                    )
                                                |> List.take 8
                                    in
                                    if String.contains " " trigger || String.contains "\n" trigger then
                                        -- Space or newline after @ cancels autocomplete
                                        Nothing

                                    else if List.isEmpty matches then
                                        Nothing

                                    else
                                        Just
                                            { trigger = trigger
                                            , suggestions = matches
                                            , selectedIndex = 0
                                            }

                                Nothing ->
                                    Nothing
                    in
                    ( { model | sheet = { sheet | queryAutocomplete = autocomplete } }, Cmd.none )

        AutocompleteSelect ref ->
            -- Insert the selected reference and close autocomplete
            case sheet.queryAutocomplete of
                Just ac ->
                    let
                        -- Calculate what to insert (reference minus what's already typed)
                        toInsert =
                            String.dropLeft (String.length ac.trigger) ref
                    in
                    ( { model | sheet = { sheet | queryAutocomplete = Nothing } }
                    , insertAtCursor toInsert
                    )

                Nothing ->
                    ( model, Cmd.none )

        AutocompleteNav delta ->
            case sheet.queryAutocomplete of
                Just ac ->
                    let
                        newIndex =
                            modBy (List.length ac.suggestions) (ac.selectedIndex + delta)
                    in
                    ( { model | sheet = { sheet | queryAutocomplete = Just { ac | selectedIndex = newIndex } } }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        AutocompleteClose ->
            ( { model | sheet = { sheet | queryAutocomplete = Nothing } }, Cmd.none )

        AuthMsg authMsg ->
            case authMsg of
                AuthEmailChange email ->
                    ( { model | auth = { auth | email = email } }, Cmd.none )

                AuthPasswordChange password ->
                    ( { model | auth = { auth | password = password } }, Cmd.none )

                AuthSubmit ->
                    if String.isEmpty auth.password then
                        -- Signup flow: send verification email
                        ( { model | auth = { auth | state = LoggingIn } }
                        , signup auth.email
                        )

                    else
                        -- Login flow
                        ( { model | auth = { auth | state = LoggingIn } }
                        , login { email = auth.email, password = auth.password }
                        )

                AuthLogout ->
                    ( { model | auth = { state = Anonymous, email = "", password = "" } }
                    , logout ()
                    )

        AuthResult data ->
            let
                decoded =
                    D.decodeValue
                        (D.oneOf
                            [ D.field "usr_id" D.string |> D.map (\usrId -> LoggedIn { usrId = usrId })
                            , D.field "error" D.string |> D.map (\_ -> Anonymous)
                            , D.succeed Anonymous
                            ]
                        )
                        data
            in
            case decoded of
                Ok newState ->
                    ( { model | auth = { auth | state = newState, password = "" } }, Cmd.none )

                Err _ ->
                    ( { model | auth = { auth | state = Anonymous } }, Cmd.none )

        ClipboardCopy ->
            -- Copy selected cells to clipboard as TSV
            case sheet.doc of
                Ok (Tab tbl) ->
                    let
                        sel =
                            Nav2.normalizeRect sheet.select

                        -- Extract selected cells as 2D list of strings
                        rows =
                            List.range sel.a.y sel.b.y
                                |> List.map
                                    (\y ->
                                        List.range sel.a.x sel.b.x
                                            |> List.map
                                                (\x ->
                                                    Array.get x tbl.cols
                                                        |> Maybe.andThen
                                                            (\col ->
                                                                Array.get (y - 1) tbl.rows
                                                                    |> Maybe.andThen (Dict.get col.key)
                                                                    |> Maybe.andThen (D.decodeValue string >> Result.toMaybe)
                                                            )
                                                        |> Maybe.withDefault ""
                                                )
                                    )

                        tsv =
                            Clipboard.serializeToTsv rows
                    in
                    ( model, copyToClipboard tsv )

                _ ->
                    ( model, Cmd.none )

        ClipboardPaste text ->
            -- Parse pasted data and insert into table, expanding bounds as needed
            case sheet.doc of
                Ok (Tab tbl) ->
                    let
                        sel =
                            sheet.select.a

                        -- Detect format and parse
                        data =
                            case Clipboard.detectFormat text of
                                Clipboard.Tsv ->
                                    Clipboard.parseTsv text

                                Clipboard.Csv ->
                                    Clipboard.parseCsv text

                                Clipboard.JsonArray ->
                                    Clipboard.parseJson text
                                        |> Result.withDefault [ [ text ] ]

                                Clipboard.PlainText ->
                                    [ [ text ] ]

                        -- Calculate required dimensions
                        pasteWidth =
                            data |> List.map List.length |> List.maximum |> Maybe.withDefault 0

                        pasteHeight =
                            List.length data

                        currentColCount =
                            Array.length tbl.cols

                        currentRowCount =
                            Array.length tbl.rows

                        -- How many new columns/rows needed?
                        neededCols =
                            max 0 (sel.x + pasteWidth - currentColCount)

                        neededRows =
                            max 0 (sel.y + pasteHeight - 1 - currentRowCount)

                        -- Generate patches to add new columns
                        newColPatches =
                            List.range 0 (neededCols - 1)
                                |> List.map
                                    (\i ->
                                        let
                                            newKey =
                                                String.fromInt (currentColCount + i)
                                        in
                                        { action = "push"
                                        , path = [ E.int 0 ]
                                        , value =
                                            E.list identity
                                                [ E.object
                                                    [ ( "name", E.string "" )
                                                    , ( "type", E.string "text" )
                                                    , ( "key", E.string newKey )
                                                    ]
                                                ]
                                        }
                                    )

                        -- Generate patches to add new rows
                        newRowPatches =
                            List.range 0 (neededRows - 1)
                                |> List.map
                                    (\_ ->
                                        { action = "push"
                                        , path = []
                                        , value = E.list identity [ E.object [] ]
                                        }
                                    )

                        -- Build column key lookup (existing + new)
                        colKey : Int -> String
                        colKey x =
                            if x < currentColCount then
                                Array.get x tbl.cols
                                    |> Maybe.map .key
                                    |> Maybe.withDefault (String.fromInt x)

                            else
                                String.fromInt x

                        -- Generate patches for each cell value
                        cellPatches =
                            data
                                |> List.indexedMap
                                    (\rowOffset row ->
                                        row
                                            |> List.indexedMap
                                                (\colOffset value ->
                                                    let
                                                        x =
                                                            sel.x + colOffset

                                                        y =
                                                            sel.y + rowOffset
                                                    in
                                                    { action = "set"
                                                    , path = [ E.int y, E.string (colKey x) ]
                                                    , value = E.string value
                                                    }
                                                )
                                    )
                                |> List.concat

                        -- Combine all patches: columns first, then rows, then values
                        allPatches =
                            newColPatches ++ newRowPatches ++ cellPatches
                    in
                    if List.isEmpty allPatches then
                        ( model, Cmd.none )

                    else
                        ( model, changeDoc { id = sheet.id, data = allPatches } )

                _ ->
                    ( model, Cmd.none )

        SelectAll ->
            case sheet.doc of
                Ok (Tab tbl) ->
                    let
                        bounds =
                            { maxX = Array.length tbl.cols - 1
                            , maxY = Array.length tbl.rows
                            }

                        newSelect =
                            Nav2.selectAll bounds
                    in
                    ( { model | sheet = { sheet | select = newSelect } }, Cmd.none )

                _ ->
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


viewSettings : Bool -> SheetInfo -> Html Msg
viewSettings show info =
    if not show then
        text ""

    else
        H.div
            [ S.positionFixed
            , S.top "0"
            , S.left "0"
            , S.right "0"
            , S.bottom "0"
            , S.backgroundColor "rgba(0,0,0,0.5)"
            , S.displayFlex
            , S.alignItemsCenter
            , S.justifyContentCenter
            , S.zIndex "1000"
            , A.onClick SettingsClose
            ]
            [ H.div
                [ S.backgroundColor "#fff"
                , S.border "1px solid #aaa"
                , S.borderRadius "4px"
                , S.padding "1rem"
                , S.minWidthRem 20
                , S.boxShadow "0 2px 8px rgba(0,0,0,0.15)"
                , A.stopPropagationOn "click" (D.succeed ( NoOp, True ))
                ]
                [ H.div [ S.displayFlex, S.justifyContentSpaceBetween, S.alignItemsCenter, S.marginBottom "1rem" ]
                    [ H.h3 [ S.margin "0" ] [ text "Sheet Settings" ]
                    , H.button
                        [ A.onClick SettingsClose
                        , S.border "none"
                        , S.background "transparent"
                        , S.cursorPointer
                        , S.fontSizeRem 1.2
                        ]
                        [ text "×" ]
                    ]
                , H.div [ S.marginBottom "1rem" ]
                    [ H.label [ S.display "block", S.marginBottom "0.25rem", S.fontWeight "600" ] [ text "Name" ]
                    , H.input
                        [ A.type_ "text"
                        , A.value info.name
                        , A.onInput SettingsNameChange
                        , A.placeholder "Sheet name"
                        , A.attribute "onfocus" "this.select()"
                        , S.width "100%"
                        , S.padding "0.5rem"
                        , S.border "1px solid #ccc"
                        , S.borderRadius "4px"
                        ]
                        []
                    ]
                , H.div [ S.marginBottom "1rem" ]
                    [ H.label [ S.display "block", S.marginBottom "0.25rem", S.fontWeight "600" ] [ text "Tags" ]
                    , H.input
                        [ A.type_ "text"
                        , A.value (String.join ", " info.tags)
                        , A.onInput SettingsTagsChange
                        , A.placeholder "tag1, tag2, tag3"
                        , S.width "100%"
                        , S.padding "0.5rem"
                        , S.border "1px solid #ccc"
                        , S.borderRadius "4px"
                        ]
                        []
                    , H.small [ S.color "#666" ] [ text "Separate tags with commas" ]
                    ]
                , H.button
                    [ A.onClick SettingsClose
                    , S.width "100%"
                    , S.padding "0.5rem 1rem"
                    , S.border "none"
                    , S.borderRadius "4px"
                    , S.background "#007bff"
                    , S.color "#fff"
                    , S.cursorPointer
                    ]
                    [ text "Done" ]
                ]
            ]


viewDeleteConfirm : Maybe String -> Html Msg
viewDeleteConfirm maybeId =
    case maybeId of
        Nothing ->
            text ""

        Just id ->
            H.div
                [ S.positionFixed
                , S.top "0"
                , S.left "0"
                , S.right "0"
                , S.bottom "0"
                , S.backgroundColor "rgba(0,0,0,0.5)"
                , S.displayFlex
                , S.alignItemsCenter
                , S.justifyContentCenter
                , S.zIndex "1000"
                ]
                [ H.div
                    [ S.backgroundColor "#fff"
                    , S.border "1px solid #aaa"
                    , S.borderRadius "4px"
                    , S.padding "1rem"
                    , S.maxWidthRem 20
                    , S.boxShadow "0 2px 8px rgba(0,0,0,0.15)"
                    ]
                    [ H.p [ S.marginBottom "1rem" ]
                        [ text "Are you sure you want to delete this sheet? This cannot be undone." ]
                    , H.div [ S.displayFlex, S.gapRem 0.5, S.justifyContentFlexEnd ]
                        [ H.button
                            [ A.onClick DocDeleteCancel
                            , S.padding "0.5rem 1rem"
                            , S.border "1px solid #ccc"
                            , S.borderRadius "4px"
                            , S.background "#f0f0f0"
                            , S.cursorPointer
                            ]
                            [ text "Cancel" ]
                        , H.button
                            [ A.onClick (DocDeleteConfirm id)
                            , S.padding "0.5rem 1rem"
                            , S.border "none"
                            , S.borderRadius "4px"
                            , S.background "#dc3545"
                            , S.color "#fff"
                            , S.cursorPointer
                            ]
                            [ text "Delete" ]
                        ]
                    ]
                ]


viewFindReplace : Maybe FindReplace -> Html Msg
viewFindReplace maybeFindReplace =
    case maybeFindReplace of
        Nothing ->
            text ""

        Just fr ->
            H.div
                [ S.positionFixed
                , S.topRem 3
                , S.rightRem 1
                , S.backgroundColor "#fff"
                , S.border "1px solid #aaa"
                , S.borderRadius "4px"
                , S.padding "0.5rem"
                , S.displayFlex
                , S.flexDirectionColumn
                , S.gapRem 0.5
                , S.zIndex "100"
                , S.boxShadow "0 2px 8px rgba(0,0,0,0.15)"
                ]
                [ H.div [ S.displayFlex, S.justifyContentSpaceBetween, S.alignItemsCenter ]
                    [ H.span [ S.fontWeight "600", S.fontSizeRem 0.875 ]
                        [ text (iif fr.showReplace "Find & Replace" "Find") ]
                    , H.button
                        [ A.onClick FindClose
                        , S.border "none"
                        , S.background "transparent"
                        , S.cursorPointer
                        , S.fontSizeRem 1
                        ]
                        [ text "×" ]
                    ]
                , H.div [ S.displayFlex, S.gapRem 0.25 ]
                    [ H.input
                        [ A.placeholder "Find..."
                        , A.value fr.findText
                        , A.onInput FindTextChange
                        , A.id "find-input"
                        , S.padding "0.25rem 0.5rem"
                        , S.border "1px solid #ccc"
                        , S.borderRadius "2px"
                        , S.widthRem 12
                        ]
                        []
                    , H.button
                        [ A.onClick FindPrev
                        , S.padding "0.25rem 0.5rem"
                        , S.border "1px solid #ccc"
                        , S.borderRadius "2px"
                        , S.cursorPointer
                        ]
                        [ text "◀" ]
                    , H.button
                        [ A.onClick FindNext
                        , S.padding "0.25rem 0.5rem"
                        , S.border "1px solid #ccc"
                        , S.borderRadius "2px"
                        , S.cursorPointer
                        ]
                        [ text "▶" ]
                    ]
                , H.div [ S.fontSizeRem 0.75, S.opacity "0.7" ]
                    [ text
                        (if List.isEmpty fr.matches then
                            iif (String.isEmpty fr.findText) "" "No matches"

                         else
                            String.fromInt (fr.currentMatch + 1)
                                ++ " of "
                                ++ String.fromInt (List.length fr.matches)
                        )
                    ]
                , if fr.showReplace then
                    H.div [ S.displayFlex, S.flexDirectionColumn, S.gapRem 0.25 ]
                        [ H.input
                            [ A.placeholder "Replace with..."
                            , A.value fr.replaceText
                            , A.onInput ReplaceTextChange
                            , S.padding "0.25rem 0.5rem"
                            , S.border "1px solid #ccc"
                            , S.borderRadius "2px"
                            , S.widthRem 12
                            ]
                            []
                        , H.div [ S.displayFlex, S.gapRem 0.25 ]
                            [ H.button
                                [ A.onClick ReplaceOne
                                , S.padding "0.25rem 0.5rem"
                                , S.border "1px solid #ccc"
                                , S.borderRadius "2px"
                                , S.cursorPointer
                                , S.fontSizeRem 0.75
                                ]
                                [ text "Replace" ]
                            , H.button
                                [ A.onClick ReplaceAll
                                , S.padding "0.25rem 0.5rem"
                                , S.border "1px solid #ccc"
                                , S.borderRadius "2px"
                                , S.cursorPointer
                                , S.fontSizeRem 0.75
                                ]
                                [ text "Replace All" ]
                            ]
                        ]

                  else
                    text ""
                ]


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


computeStats : Doc -> Result String (Array Stat)
computeStats doc =
    case doc of
        Tab tbl ->
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

        _ ->
            Err ""


resolveTable : Model -> Result String Table
resolveTable model =
    case ( model.sheet.doc, model.sheet.table ) of
        ( Ok (Tab tbl), _ ) ->
            Ok tbl

        ( Ok Library, _ ) ->
            Ok
                { cols = libraryCols
                , rows =
                    model.library
                        |> Dict.filter (\k v -> k /= "" && not v.scratch && List.any (String.contains model.search) (k :: v.name :: v.tags))
                        |> Dict.toList
                        |> List.map
                            (\( k, v ) ->
                                Dict.fromList
                                    [ ( "sheet_id", E.string k )
                                    , ( "type", E.string (Maybe.withDefault "" <| List.head <| String.split ":" k) )
                                    , ( "name", E.string (iif (String.isEmpty (String.trim v.name)) "(untitled)" v.name) )
                                    , ( "tags", E.list E.string v.tags )
                                    , ( "delete", iif v.system E.null (E.string k) )
                                    ]
                            )
                        |> Array.fromList
                }

        ( _, Ok tbl ) ->
            Ok tbl

        ( Err err1, Err err2 ) ->
            Err (err1 ++ " " ++ err2)

        ( _, Err err ) ->
            Err err


applyFilter : Filter -> String -> Row -> Bool
applyFilter filter key row =
    let
        val =
            Dict.get key row |> Maybe.andThen (D.decodeValue string >> Result.toMaybe) |> Maybe.withDefault ""

        numVal =
            String.toFloat val
    in
    case filter of
        TextContains substr ->
            String.contains (String.toLower substr) (String.toLower val)

        TextEquals exact ->
            String.toLower val == String.toLower exact

        NumberGreaterThan n ->
            Maybe.map (\v -> v > n) numVal |> Maybe.withDefault False

        NumberLessThan n ->
            Maybe.map (\v -> v < n) numVal |> Maybe.withDefault False

        NumberBetween lo hi ->
            Maybe.map (\v -> v >= lo && v <= hi) numVal |> Maybe.withDefault False

        BooleanIs b ->
            (String.toLower val == "true" || val == "1") == b


filterAndSort : Sheet -> Array Row -> Array Row
filterAndSort sheet rows =
    let
        passes row =
            Dict.foldl (\key filter acc -> acc && applyFilter filter key row) True sheet.filters

        filtered =
            Array.filter passes rows
    in
    case sheet.sort of
        Just ( sortKey, sortOrder ) ->
            let
                cmp r1 r2 =
                    let
                        v1 =
                            Dict.get sortKey r1 |> Maybe.andThen (D.decodeValue string >> Result.toMaybe) |> Maybe.withDefault ""

                        v2 =
                            Dict.get sortKey r2 |> Maybe.andThen (D.decodeValue string >> Result.toMaybe) |> Maybe.withDefault ""
                    in
                    Maybe.map2 compare (String.toFloat v1) (String.toFloat v2) |> Maybe.withDefault (compare v1 v2)

                sorted =
                    Array.toList filtered |> List.sortWith cmp
            in
            Array.fromList (iif (sortOrder == Descending) (List.reverse sorted) sorted)

        Nothing ->
            filtered


typeAlign : Type -> H.Attribute Msg
typeAlign typ =
    case typ of
        Create -> S.textAlignRight
        SheetId -> S.textAlignCenter
        Boolean -> S.textAlignCenter
        Usd -> S.textAlignRight
        Number -> S.textAlignRight
        Percentage -> S.textAlignRight
        Delete -> S.textAlignCenter
        Form _ -> S.textAlignCenter
        _ -> S.textAlignLeft


typeWidth : Type -> H.Attribute Msg
typeWidth typ =
    case typ of
        Create -> S.widthRem 10
        SheetId -> S.widthRem 3
        Boolean -> S.widthRem 2
        Number -> S.widthRem 5
        Usd -> S.widthRem 5
        Percentage -> S.widthRem 4
        Delete -> S.widthRem 4
        _ -> S.widthAuto


cellClasses : Sheet -> Int -> Int -> H.Attribute Msg
cellClasses sheet i n =
    let
        { a, b } =
            sheet.select

        between a_ b_ i_ =
            min a_ b_ <= i_ && i_ <= max a_ b_

        eq a_ b_ i_ =
            a_ == i_ && i_ == b_

        cellIdx =
            xy i n

        isMatch =
            case sheet.findReplace of
                Just fr -> List.member cellIdx fr.matches
                Nothing -> False

        isCurrentMatch =
            case sheet.findReplace of
                Just fr -> fr.matches |> List.drop fr.currentMatch |> List.head |> (==) (Just cellIdx)
                Nothing -> False
    in
    A.classList
        [ ( "selected", (sheet.select /= rect -1 -1 -1 -1) && (between a.x b.x i || eq a.x b.x -1) && (between a.y b.y n || eq a.y b.y -1) )
        , ( "r0", n == 0 )
        , ( "match-highlight", isMatch && not isCurrentMatch )
        , ( "match-current", isCurrentMatch )
        ]


cellDecoder : Type -> Int -> Int -> D.Decoder (Maybe (Html Msg))
cellDecoder typ i n =
    D.maybe
        (case typ of
            Unknown -> D.map text string
            SheetId -> D.string |> D.map (\id -> H.a [ A.href ("/" ++ id), S.overflowVisible, S.whiteSpaceNowrap, S.paddingRightRem 0.5 ] [ text "view" ])
            Link -> D.string |> D.map (\href -> H.a [ A.href href, A.target "_blank", A.rel "noopener noreferrer", S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.wordBreakKeepAll, S.hyphensNone ] [ text "link" ])
            Image -> D.string |> D.map (\src -> H.img [ A.src src ] [])
            Text -> D.map text string
            Boolean -> boolean |> D.map (\c -> H.input [ A.type_ "checkbox", A.checked c, A.onCheck (DocMsg << CellCheck { x = i, y = n }) ] [])
            Number -> D.oneOf [ D.map (text << String.fromFloat << round2) number, D.map text string ]
            Usd -> D.oneOf [ D.map (text << usd) number, D.map text string ]
            Percentage -> D.oneOf [ D.map (text << formatPercentage) number, D.map text string ]
            Date -> D.map text string
            Enum _ -> D.map text string
            Delete -> D.string |> D.map (\sheet_id -> H.button [ A.onClick (DocDelete sheet_id) ] [ text "delete" ])
            Create -> D.value |> D.map (\val -> H.button [ A.onClick (DocNew val) ] [ text "add to library" ])
            Form _ ->
                D.map3
                    (\method _ fields ->
                        H.form [ A.onSubmit NoOp, S.displayGrid, S.gridTemplateColumns "auto 1fr", S.paddingRem 1 ] <|
                            List.concatMap (\f -> [ H.label [] [ text f.label ], H.input [] [] ]) fields
                                ++ [ H.span [] [], H.button [ A.type_ "submit" ] [ text method ] ]
                    )
                    (D.field "method" D.string)
                    (D.field "action" D.string)
                    (D.field "fields" (D.list (D.map (\label -> { label = label }) (D.field "label" D.string))))
            _ -> D.map text string
        )


viewStatCell : Maybe Stat -> List (Html Msg)
viewStatCell maybeStat =
    let
        grid =
            H.div [ S.displayGrid, S.gridTemplateColumns "auto auto", S.gapRem 0, S.gridColumnGapRem 0.5, S.justifyContentFlexStart, S.opacity "0.5" ]

        kv k v =
            [ H.span [] [ text k ], H.span [] [ text v ] ]
    in
    case maybeStat of
        Just (Numeric stat) ->
            [ grid <|
                kv "min" (Maybe.withDefault "" (Maybe.map (String.fromFloat << round2) stat.min))
                    ++ kv "max" (Maybe.withDefault "" (Maybe.map (String.fromFloat << round2) stat.max))
                    ++ kv "mean" (iif (stat.count == 0) "" (String.fromInt (round (stat.sum / toFloat stat.count))))
                    ++ kv "count" (String.fromInt stat.count)
            ]

        Just (Descriptive stat) ->
            [ grid <|
                kv "min" (Maybe.withDefault "" (Maybe.map String.fromInt stat.min))
                    ++ kv "max" (String.fromInt stat.max)
                    ++ kv "mean" (iif (stat.count == 0) "" (String.fromInt (stat.sum // stat.count)))
                    ++ kv "count" (String.fromInt stat.count)
                    ++ kv "keywords" (String.join " " (Dict.keys (Dict.filter (\k v -> String.length k >= 4 && v >= 2) stat.keywords)))
            ]

        _ ->
            []


viewHeaderCell : Sheet -> Col -> List (Html Msg)
viewHeaderCell sheet col =
    case col.name of
        "" ->
            []

        _ ->
            let
                sortIndicator =
                    case sheet.sort of
                        Just ( sortKey, Ascending ) -> iif (sortKey == col.key) " ▲" ""
                        Just ( sortKey, Descending ) -> iif (sortKey == col.key) " ▼" ""
                        Nothing -> ""

                hasFilter =
                    Dict.member col.key sheet.filters

                isFilterOpen =
                    sheet.filterOpen == Just col.key

                currentFilterValue =
                    case Dict.get col.key sheet.filters of
                        Just (TextContains v) -> v
                        _ -> ""
            in
            [ H.div [ S.displayFlex, S.flexDirectionColumn, S.positionRelative ]
                [ H.div [ S.displayFlex, S.alignItemsCenter, S.gapRem 0.25 ]
                    [ H.span [ S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.fontWeight "600", S.cursorPointer, A.onClick (ColumnSort col.key) ]
                        [ text (col.name ++ sortIndicator) ]
                    , H.span [ S.cursorPointer, S.opacity (iif hasFilter "1" "0.3"), S.fontSizeRem 0.7, A.onClick (FilterToggle col.key), A.title "Filter" ]
                        [ text (iif hasFilter "⧩" "▽") ]
                    ]
                , if isFilterOpen then
                    H.div [ S.positionAbsolute, S.top "100%", S.left "0", S.backgroundColor "#fff", S.border "1px solid #ccc", S.borderRadius "4px", S.padding "0.5rem", S.zIndex "100", S.boxShadow "0 2px 8px rgba(0,0,0,0.15)", S.minWidth "150px" ]
                        [ H.input [ A.placeholder "contains...", A.value currentFilterValue, A.onInput (FilterInput col.key), S.width "100%", S.padding "0.25rem", S.border "1px solid #ddd", S.borderRadius "2px", S.fontSizeRem 0.8 ] []
                        , if hasFilter then
                            H.button [ A.onClick (FilterClear col.key), S.marginTop "0.25rem", S.padding "0.25rem 0.5rem", S.fontSizeRem 0.7, S.background "#f0f0f0", S.border "1px solid #ccc", S.borderRadius "2px", S.cursorPointer ] [ text "Clear" ]
                          else
                            text ""
                        ]
                  else
                    text ""
                ]
            ]


viewEditCell : Sheet -> Col -> List (Html Msg)
viewEditCell sheet col =
    case col.typ of
        Enum options ->
            [ H.select
                [ A.id "new-cell", A.value (Maybe.withDefault "" sheet.write), A.onInput (InputChange CellWrite), A.onBlur (DocMsg (SheetWrite sheet.select.a)), S.width "100%", S.height "100%" ]
                (H.option [ A.value "" ] [ text "-- select --" ]
                    :: List.map (\opt -> H.option [ A.value opt, A.selected (Just opt == sheet.write) ] [ text opt ]) options
                )
            ]

        _ ->
            [ H.input [ A.id "new-cell", A.value (Maybe.withDefault "" sheet.write), A.onInput (InputChange CellWrite), A.onBlur (DocMsg (SheetWrite sheet.select.a)), S.width "100%", S.height "100%", S.minWidthRem 8 ] [] ]


viewCell : Sheet -> Result String (Array Stat) -> Int -> Int -> Col -> Row -> Html Msg
viewCell sheet stats i n col row =
    H.td
        [ A.onClick CellMouseClick
        , A.onDoubleClick <|
            CellMouseDoubleClick <|
                case String.fromInt n of
                    "0" -> col.name
                    "-1" -> typeName col.typ
                    "-2" -> typeName col.typ
                    _ -> row |> Dict.get col.key |> Maybe.andThen (D.decodeValue string >> Result.toMaybe) |> Maybe.withDefault ""
        , A.onMouseDown CellMouseDown
        , A.onMouseUp CellMouseUp
        , A.onMouseEnter (CellHover (xy i n))
        , S.heightRem 1.25
        , S.lineHeight (iif (n == 0) "1.75" "")
        , cellClasses sheet i n
        , typeAlign col.typ
        , typeWidth col.typ
        ]
    <|
        if sheet.write /= Nothing && sheet.select == rect i n i n then
            viewEditCell sheet col

        else
            case String.fromInt n of
                "-2" ->
                    viewStatCell (Maybe.andThen (Array.get i) (Result.toMaybe stats))

                "-1" ->
                    [ H.p [ S.displayBlock, S.textOverflowEllipsis, S.overflowHidden, S.whiteSpaceNowrap, S.opacity "0.5", S.fontSizeSmall ] [ text (typeName col.typ) ] ]

                "0" ->
                    viewHeaderCell sheet col

                _ ->
                    [ row
                        |> Dict.get col.key
                        |> Maybe.withDefault (E.string "")
                        |> D.decodeValue (cellDecoder col.typ i n)
                        |> Result.map (Maybe.withDefault (text ""))
                        |> Result.mapError (D.errorToString >> text)
                        |> result
                    ]


viewTableRow : Sheet -> Doc -> Result String (Array Stat) -> Array Col -> Int -> Row -> Html Msg
viewTableRow sheet doc stats cols n row =
    H.tr
        [ case String.fromInt n of
            "-2" -> S.backgroundColor "#ececec"
            "-1" -> S.backgroundColor "#f6f6f6"
            "0" -> S.backgroundColor "#f6f6f6"
            _ -> S.backgroundColor "#fff"
        , case ( String.fromInt n, stats ) of
            ( "-2", Err _ ) -> S.displayNone
            _ -> S.displayTableRow
        ]
    <|
        List.indexedMap (\i col -> viewCell sheet stats i n col row) (Array.toList cols)
            ++ [ case doc of
                    Tab _ -> H.th [ A.onClick (DocMsg SheetColumnPush), S.textAlignLeft, S.widthRem 0.001, S.whiteSpaceNowrap, S.opacity "0.5" ] [ text (iif (n == 0) "→" "") ]
                    _ -> text ""
               ]


viewFilterBar : Sheet -> Int -> Int -> Html Msg
viewFilterBar sheet filteredCount totalCount =
    if Dict.isEmpty sheet.filters then
        text ""

    else
        H.div [ S.padding "0.25rem 0.5rem", S.backgroundColor "#fff8e0", S.borderBottom "1px solid #e0d8a0", S.fontSizeRem 0.75, S.displayFlex, S.justifyContentSpaceBetween, S.alignItemsCenter ]
            [ H.span [] [ text ("Showing " ++ String.fromInt filteredCount ++ " of " ++ String.fromInt totalCount ++ " rows") ]
            , H.button [ A.onClick (FilterClear ""), S.padding "0.125rem 0.5rem", S.fontSizeRem 0.7, S.background "#fff", S.border "1px solid #ccc", S.borderRadius "2px", S.cursorPointer ] [ text "Clear all filters" ]
            ]


viewTableFooter : Sheet -> Array Col -> Int -> Html Msg
viewTableFooter sheet cols rowCount =
    H.tfoot [] <|
        case sheet.doc of
            Ok Library ->
                List.map
                    (\( label, msg ) ->
                        H.tr [ A.onClick msg ] <|
                            H.td [ S.opacity "0.25" ] [ text label ]
                                :: List.map (\typ -> H.td [ S.opacity "0.25" ] [ text typ ]) [ "text", "list text", "" ]
                    )
                    [ ( "table:...", DocNewTable ), ( "query:...", DocNewQuery ) ]
                    ++ [ H.tr [] <|
                            H.td [ S.opacity "0.25" ]
                                [ H.label [ S.cursorPointer ]
                                    [ text "import csv..."
                                    , H.input [ A.type_ "file", A.accept ".csv,text/csv", A.on "change" (D.at [ "target", "files" ] (D.index 0 File.decoder) |> D.map CsvImportFile), S.display "none" ] []
                                    ]
                                ]
                                :: List.map (\typ -> H.td [ S.opacity "0.25" ] [ text typ ]) [ "text", "list text", "" ]
                       ]

            Ok (Tab _) ->
                [ H.tr [ A.onClick (DocMsg (SheetRowPush rowCount)) ] <|
                    List.indexedMap (\_ col -> H.td [ S.opacity "0.25" ] [ text (typeName col.typ) ]) (Array.toList cols)
                        ++ [ H.th [ S.widthRem 0.001, S.whiteSpaceNowrap, S.opacity "0.5" ] [ text "↴" ] ]
                ]

            _ ->
                []


viewError : String -> Html Msg
viewError error =
    case error of
        "" ->
            text ""

        _ ->
            H.div [ S.backgroundColor "#fee", S.border "1px solid #c88", S.borderRadius "4px", S.padding "0.75rem", S.margin "0.5rem", S.fontFamily "monospace", S.fontSizeRem 0.8, S.whiteSpacePre, S.overflowXAuto ]
                [ H.div [ S.displayFlex, S.justifyContentSpaceBetween, S.marginBottomRem 0.5 ]
                    [ H.strong [] [ text "Error" ]
                    , H.button [ A.onClick (DocError ""), S.border "none", S.background "transparent", S.cursorPointer, S.fontSizeRem 1 ] [ text "×" ]
                    ]
                , text error
                ]


viewAuthForm : Auth -> Html Msg
viewAuthForm auth =
    case auth.state of
        LoggedIn _ ->
            text ""

        _ ->
            H.form [ A.id "account", A.onSubmit (AuthMsg AuthSubmit), S.displayGrid, S.gapRem 0.5, S.maxWidth "100vw", S.width "100%", S.gridTemplateColumns "1fr 1fr auto", S.paddingRem 0.5, S.borderTop "1px solid #aaa", S.backgroundColor "#ccc", S.positionAbsolute, S.bottomPx 0, S.zIndex "10" ]
                [ H.input [ S.minWidthRem 2, A.placeholder "email", A.type_ "email", A.name "email", A.value auth.email, A.onInput (InputChange AuthEmail), A.disabled (auth.state == LoggingIn) ] []
                , H.input [ S.minWidthRem 2, A.placeholder "password", A.type_ "password", A.name "password", A.value auth.password, A.onInput (InputChange AuthPassword), A.disabled (auth.state == LoggingIn) ] []
                , H.button [ A.type_ "submit", S.background "#eee", A.disabled (auth.state == LoggingIn) ]
                    [ text (iif (auth.state == LoggingIn) "..." (iif (String.isEmpty auth.password) "signup" "login")) ]
                ]


viewToolbar : Model -> SheetInfo -> Html Msg
viewToolbar model info =
    let
        sheet =
            model.sheet
    in
    H.div [ S.displayFlex, S.flexDirectionRow, S.alignItemsCenter, S.whiteSpaceNowrap, S.gapRem 0.5, S.paddingRem 0.5, S.borderBottom "1px solid #aaa", S.background "#f0f0f0" ] <|
        List.concat
            [ [ H.a [ A.href "/", S.fontWeight "900", S.fontSizeRem 1.5, S.heightRem 1, S.lineHeight "0.55" ] [ text "⊞" ]
              , H.a [ A.href "/", S.fontWeight "900", A.id "title", S.marginLeftRem -0.25 ] [ text "scrapsheets" ]
              , text "/"
              ]
            , case model.auth.state of
                LoggedIn { usrId } -> [ H.span [ A.onClick (AuthMsg AuthLogout), S.cursorPointer ] [ text ("user:" ++ usrId) ], text "/" ]
                _ -> [ H.span [] [ text "anon" ], text "/" ]
            , iif (sheet.id == "")
                [ H.span [] [ text "library" ] ]
                [ H.a [ A.href "#settings" ] [ text (iif (String.trim info.name == "") "untitled" info.name) ] ]
            , case sheet.doc of
                Ok (Tab _) ->
                    [ H.a [ A.href ("https://api.sheets.scrap.land/export/" ++ sheet.id ++ ".csv"), A.download (sheet.id ++ ".csv"), S.marginLeftAuto, S.backgroundColor "#e8e8e8", S.padding "2px 8px", S.borderRadius "2px", S.fontSizeRem 0.75 ] [ text "export csv" ] ]

                _ ->
                    []
            ]


viewQueryEditor : Model -> Query_ -> Html Msg
viewQueryEditor model query =
    let
        sheet =
            model.sheet

        lineCount =
            max 1 (List.length (String.lines (String.trim query.code)))

        sheetRefs =
            model.library |> Dict.keys |> List.filter (\k -> String.startsWith "table:" k || String.startsWith "query:" k) |> List.take 5
    in
    H.div [ S.displayFlex, S.flexDirectionColumn, S.height "100%", S.width "100%", S.minWidth "25vw" ]
        [ H.div [ S.displayFlex, S.flexGrow "1", S.minHeightRem 8, S.overflowAuto, S.backgroundColor "#f8f8f8" ]
            [ H.div [ S.paddingRem 1, S.paddingRight "0.5rem", S.backgroundColor "#eee", S.borderRight "1px solid #ddd", S.fontFamily "monospace", S.fontSizeRem 0.75, S.lineHeightRem 1.5, S.color "#999", S.textAlign "right", S.userSelect "none", S.minWidthRem 2.5 ]
                (List.range 1 lineCount |> List.map (\n -> H.div [] [ text (String.fromInt n) ]))
            , H.div [ S.positionRelative, S.flexGrow "1", S.height "100%" ]
                [ H.textarea [ A.id "code", A.onInput (InputChange QueryCode), A.spellcheck False, S.height "100%", S.width "100%", S.whiteSpacePre, S.fontFamily "monospace", S.fontSizeRem 0.75, S.border "none", S.backgroundColor "transparent", S.paddingRem 1, S.paddingLeft "0.5rem", S.lineHeightRem 1.5, S.resize "none", S.outline "none" ]
                    [ text (String.trim query.code) ]
                , case sheet.queryAutocomplete of
                    Nothing ->
                        text ""

                    Just ac ->
                        H.div [ S.positionAbsolute, S.top "2rem", S.left "0.5rem", S.backgroundColor "#fff", S.border "1px solid #ccc", S.borderRadius "4px", S.boxShadow "0 2px 8px rgba(0,0,0,0.15)", S.zIndex "100", S.maxHeightRem 12, S.overflowYAuto, S.minWidthRem 15 ]
                            (ac.suggestions
                                |> List.indexedMap
                                    (\i ref ->
                                        H.div [ A.onClick (AutocompleteSelect ref), S.padding "0.5rem 0.75rem", S.cursorPointer, S.fontFamily "monospace", S.fontSizeRem 0.75, S.backgroundColor (iif (i == ac.selectedIndex) "#e8f4ff" "transparent"), S.borderBottom "1px solid #eee" ]
                                            [ H.span [ S.color "#888" ] [ text "@" ], text ref ]
                                    )
                            )
                ]
            ]
        , case model.error of
            "" -> text ""
            err ->
                H.div [ S.backgroundColor "#fee", S.borderTop "2px solid #c66", S.padding "0.75rem", S.fontFamily "monospace", S.fontSizeRem 0.75, S.whiteSpacePre, S.overflowXAuto, S.maxHeightRem 8, S.overflowYAuto ]
                    [ H.div [ S.displayFlex, S.justifyContentSpaceBetween, S.alignItemsStart ]
                        [ H.span [ S.color "#c00" ] [ text err ]
                        , H.button [ A.onClick (DocError ""), S.border "none", S.background "transparent", S.cursorPointer, S.color "#c00", S.fontSizeRem 1, S.marginLeft "0.5rem" ] [ text "×" ]
                        ]
                    ]
        , if List.isEmpty sheetRefs then
            text ""
          else
            H.div [ S.padding "0.5rem 0.75rem", S.backgroundColor "#f0f0f0", S.borderTop "1px solid #ddd", S.fontSizeRem 0.7, S.color "#666" ]
                [ H.span [ S.fontWeight "600" ] [ text "Sheet refs: " ]
                , text (String.join ", " (List.map (\s -> "@" ++ s) sheetRefs))
                , iif (Dict.size model.library > 5) (H.span [ S.color "#999" ] [ text " ..." ]) (text "")
                ]
        ]


view : Model -> Browser.Document Msg
view ({ sheet } as model) =
    let
        info =
            model.library |> Dict.get sheet.id |> Maybe.withDefault { name = "", tags = [], scratch = False, system = False, thumb = (), peers = Public }

        stats =
            sheet.stats

        table =
            resolveTable model
    in
    { title = "scrapsheets"
    , body =
        [ viewAuthForm model.auth
        , viewFindReplace sheet.findReplace
        , viewDeleteConfirm model.deleteConfirm
        , viewSettings model.showSettings info
        , H.div [ S.displayGrid, S.gapRem 0, S.userSelectNone, S.cursorPointer, A.style "-webkit-user-select" "none", S.maxWidth "100vw", S.maxHeight "100vh", S.height "100%", S.width "100%" ]
            [ H.main_ [ S.displayFlex, S.flexDirectionColumn, S.width "100%", S.overflowXAuto, S.gapRem 0 ]
                [ viewToolbar model info
                , H.div [ S.displayFlex, S.flexDirectionRow, S.justifyContentSpaceBetween, S.gapRem 0, S.borderBottom "1px solid #aaa", S.zIndex "2", S.marginBottomPx -1 ]
                    [ H.div [ S.displayFlex, S.width "100%", S.height "100%" ]
                        [ H.input [ A.value model.search, A.onInput (InputChange SheetSearch), A.placeholder "search", S.width "100%", S.border "none", S.backgroundColor "#fff", S.padding "0.25rem 0.5rem", S.fontSizeRem 0.875 ] [] ]
                    ]
                , H.div [ S.overflowAuto, S.height "100%", S.backgroundColor "#eee" ]
                    [ viewError model.error
                    , case table of
                        Err "" ->
                            H.div [ S.displayFlex ] [ H.span [ S.textAlignCenter, S.width "100%", S.paddingRem 2, S.opacity "0.5" ] [ text "loading" ] ]

                        Err err ->
                            H.p [] [ text err ]

                        Ok { cols, rows } ->
                            let
                                sortedRows =
                                    filterAndSort sheet rows

                                doc =
                                    sheet.doc |> Result.withDefault Library
                            in
                            H.div []
                                [ viewFilterBar sheet (Array.length sortedRows) (Array.length rows)
                                , H.table [ S.borderCollapseCollapse, S.width "100%", A.onMouseLeave (CellHover (xy -1 -1)) ]
                                    [ H.thead [] []
                                    , H.tbody [] <|
                                        Array.toList <|
                                            Array.indexedMap (\n_ row -> viewTableRow sheet doc stats cols (n_ - 2) row) <|
                                                Array.append (Array.initialize 3 (always Dict.empty)) sortedRows
                                    , viewTableFooter sheet cols (Array.length rows)
                                    ]
                                ]
                    ]
                ]
            , H.aside [ A.id "aside", S.displayFlex, S.flexDirectionColumn, S.height "100%", S.backgroundColor "#fff" ] <|
                case sheet.doc of
                    Ok (Query query) -> [ viewQueryEditor model query ]
                    _ -> []
            ]
        ]
    }
