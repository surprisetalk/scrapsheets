module Main exposing (main)

---- IMPORTS ------------------------------------------------------------------

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Date exposing (Date)
import Dict exposing (Dict)
import Html as H exposing (Html, code, text)
import Html.Attributes as A
import Html.Events as A
import Html.Lazy as H
import Html.Style as S exposing (textAlignLeft)
import Http exposing (stringResolver)
import Json.Decode as D
import Json.Encode as E
import Parser as P exposing ((|.), (|=), Parser)
import Regex exposing (Regex)
import Set exposing (Set)
import Task exposing (Task)
import Time exposing (Month(..), millisToPosix)
import Url exposing (Url)



---- HELPERS ------------------------------------------------------------------


flip : (a -> b -> c) -> (b -> a -> c)
flip f a b =
    f b a


result : Result a a -> a
result x =
    case x of
        Ok a ->
            a

        Err a ->
            a



---- MAIN ---------------------------------------------------------------------


main : Program () Model Msg
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
subs _ =
    Sub.none



---- MODEL --------------------------------------------------------------------


type alias Id =
    String


type alias Model =
    { nav : Nav.Key
    , library : Dict Id Book
    , sheet : Sheet
    }


type alias Sheet =
    { bookId : Id
    , sheetId : Id
    , search : String
    , newTag : Maybe String
    , select : Rect
    , hover : Index
    , write : Maybe String
    , cols : Array Col
    , rows : Result () (Array (Dict String D.Value))
    , source : Source
    }


type alias Book =
    { dir : String
    , peers : Dict Id ()
    , sheets : Dict Id { name : String, tags : Set String, thumb : Svg }
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


type
    Source
    -- TODO: Move some of these to the shop as free templates.
    -- TODO: email, settings, databases, git, github, stripe, logs, sheets, tests, code stats, social media keywords
    = Table
    | Json
    | Filesystem
    | Github {}
    | Stripe {}
    | Bluesky {}
    | Form {}
    | Email {}
    | Rss {}
    | Calendar {}
    | Http {}
    | Webhook {}
    | Code
        { lang : Language
        , query : String
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


type alias Col =
    { key : String, label : String, t : ColType }


type ColType
    = Virtual Formula
    | Concrete Type


type Formula
    = Exceed String


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


toType : ColType -> Type
toType column =
    case column of
        Virtual (Exceed _) ->
            Text

        Concrete t ->
            t



---- INIT ---------------------------------------------------------------------


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ nav =
    Tuple.pair
        -- TODO: Load from root document via flag.
        { nav = nav
        , library =
            Dict.empty
                |> Dict.insert "123"
                    { dir = "device"
                    , peers = Dict.empty
                    , sheets =
                        Dict.empty
                            |> Dict.insert "123" { name = "indexed-db", tags = Set.empty, thumb = () }
                            |> Dict.insert "234" { name = "filesystem", tags = Set.empty, thumb = () }
                    }
                |> Dict.insert "234"
                    { dir = "personal"
                    , peers = Dict.empty
                    , sheets =
                        Dict.empty
                            |> Dict.insert "123"
                                { name = "my-sheet-2"
                                , tags = Set.empty |> Set.insert "my-tag-3"
                                , thumb = ()
                                }
                    }
        , sheet =
            { bookId = "234"
            , sheetId = "123"
            , search = "TODO: search"
            , newTag = Nothing
            , hover = xy -1 -1
            , select = Rect (xy -1 -1) (xy -1 -1)
            , write = Nothing
            , cols =
                Array.fromList <|
                    List.map (\( k, v ) -> { key = k, label = k, t = v }) <|
                        [ ( "A", Concrete Text )
                        , ( "B", Concrete Number )
                        , ( "C", Virtual (Exceed "A++2*B") )
                        , ( "D", Virtual (Exceed "1.5*B") )
                        ]
            , rows =
                Ok <|
                    Array.fromList <|
                        List.map Dict.fromList <|
                            [ [ ( "A", E.string "hello" ), ( "B", E.string "world" ), ( "C", E.int 89 ) ]
                            , [ ( "A", E.int 48 ), ( "B", E.float 1.23 ), ( "D", E.string "62" ) ]
                            , [ ( "A", E.bool True ), ( "B", E.bool False ), ( "C", E.string "true" ) ]
                            , [ ( "B", E.string "4.56" ), ( "D", E.string "boo" ) ]
                            ]
            , source = Table
            }
        }
        Cmd.none



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
    | SheetNameEditing String
    | TagEditing (Maybe String)
    | SearchEditing String
    | ColumnPushing
    | ColumnEditing Int Col
    | RowPushing Int
    | Selecting (Maybe String) Rect
    | CellWriting String
    | CellSaving
    | CellHovering Index
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest



---- UPDATE -------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ sheet } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SheetNameEditing name ->
            -- TODO:
            ( model, Cmd.none )

        TagEditing newTag ->
            ( { model | sheet = { sheet | newTag = newTag } }, Cmd.none )

        SearchEditing search ->
            ( { model | sheet = { sheet | search = search } }, Cmd.none )

        ColumnPushing ->
            ( { model | sheet = { sheet | cols = sheet.cols |> Array.push { key = "", label = "", t = Concrete Text } } }, Cmd.none )

        ColumnEditing i col ->
            ( { model | sheet = { sheet | cols = sheet.cols |> Array.set i col } }, Cmd.none )

        RowPushing i ->
            ( { model | sheet = { sheet | rows = sheet.rows |> Result.map (\rows -> Array.slice 0 (i + 1) rows |> Array.push Dict.empty |> flip Array.append (Array.slice (i + 1) (Array.length rows) rows)) } }, Cmd.none )

        Selecting write s ->
            ( { model | sheet = { sheet | write = write, select = rect (min s.a.x s.b.x) (min s.a.y s.b.y) (max s.a.x s.b.x) (max s.a.y s.b.y) } }
            , Task.attempt (always NoOp) (Dom.focus "new-cell")
            )

        CellSaving ->
            let
                { x, y } =
                    sheet.select.a

                row : Dict String D.Value
                row =
                    sheet.cols
                        |> Array.get x
                        |> Maybe.map .key
                        |> Maybe.map
                            (\key ->
                                sheet.rows
                                    |> Result.withDefault Array.empty
                                    |> Array.get y
                                    |> Maybe.withDefault Dict.empty
                                    |> Dict.update key (always (Maybe.map E.string sheet.write))
                            )
                        |> Maybe.withDefault Dict.empty
            in
            ( { model | sheet = { sheet | write = Nothing, rows = sheet.rows |> Result.map (Array.set y row) } }, Cmd.none )

        CellWriting write ->
            ( { model | sheet = { sheet | write = Just write } }, Cmd.none )

        CellHovering hover ->
            ( { model | sheet = { sheet | hover = hover } }, Cmd.none )

        UrlChanged url ->
            -- TODO
            ( model, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            -- TODO: ?q=+any ?q=-any ?q==any
            ( model, Cmd.none )

        LinkClicked (Browser.External url) ->
            -- TODO
            ( model, Cmd.none )



---- VIEW ---------------------------------------------------------------------


view : Model -> Browser.Document Msg
view ({ sheet } as model) =
    let
        book : Book
        book =
            model.library |> Dict.get sheet.bookId |> Maybe.withDefault { dir = "", peers = Dict.empty, sheets = Dict.empty }

        { name, tags, thumb } =
            book.sheets |> Dict.get sheet.sheetId |> Maybe.withDefault { name = "", tags = Set.empty, thumb = () }

        nrows : Int
        nrows =
            Array.length (Result.withDefault Array.empty sheet.rows)
    in
    { title = "scrapsheets"
    , body =
        [ H.node "style" [] [ text "body * { box-sizing: border-box; gap: 1rem; }" ]
        , H.node "style" [] [ text "body { font-family: sans-serif; }" ]
        , H.node "style" [] [ text "td { border: 1px solid black; height: 1rem; }" ]
        , H.node "style" [] [ text "td:hover { background: #fafafa; }" ]
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
                        , H.input [ A.value name, A.onInput SheetNameEditing ] []
                        , H.div [ S.displayFlex, S.flexDirectionRow, S.gapRem 0.5 ] <|
                            List.concat
                                [ case sheet.newTag of
                                    Nothing ->
                                        [ H.button [ A.onClick (TagEditing (Just "")) ] [ text "#" ] ]

                                    Just value ->
                                        [ H.input [ A.value value, A.onInput (TagEditing << Just) ] [] ]
                                , List.map (\tag -> H.a [ A.href ("?q=+tag:" ++ tag) ] [ text ("#" ++ tag) ]) <| Set.toList tags
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
                , H.input [ A.value sheet.search, A.onInput SearchEditing, S.width "100%" ] []

                -- TODO: https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed
                , H.table [ S.borderCollapseCollapse, A.onMouseLeave (CellHovering (xy -1 -1)) ]
                    [ H.thead []
                        [ H.tr [] <|
                            List.concat
                                [ [ H.th
                                        [ A.onClick (Selecting Nothing (rect -1 0 -1 nrows))
                                        , A.onMouseEnter (CellHovering (xy -1 nrows))
                                        , S.verticalAlignBottom
                                        ]
                                        [ text "↕️" ]
                                  ]
                                , List.indexedMap
                                    (\i col ->
                                        H.th
                                            [ A.onClick (Selecting Nothing (rect i -1 i -1))
                                            , A.onMouseDown (Selecting Nothing (rect i -1 i -1))
                                            , A.onMouseUp (Selecting Nothing { a = sheet.select.a, b = xy i -1 })
                                            , A.onMouseEnter (CellHovering (xy i -1))
                                            , S.textAlignLeft
                                            , S.verticalAlignBottom
                                            ]
                                            [ H.div [ S.displayFlex, S.flexDirectionColumn, S.justifyContentFlexEnd, S.gapRem 0 ]
                                                [ H.div [ S.displayFlex, S.flexWrapWrap, S.gapRem 0.5, S.fontSizeSmall ] <|
                                                    case toType col.t of
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
                                                , H.input [ A.value col.label, A.onInput (\label -> ColumnEditing i { col | label = label }) ] []
                                                , H.input [ A.value col.key, A.onInput (\key -> ColumnEditing i { col | key = key }) ] []
                                                , case col.t of
                                                    Virtual (Exceed formula) ->
                                                        H.input [ A.value formula, A.onInput (\x -> ColumnEditing i { col | t = Virtual (Exceed x) }) ] []

                                                    Concrete t ->
                                                        let
                                                            types : List ( String, Type )
                                                            types =
                                                                [ ( "same", Text ), ( "text/from-float", Number ) ]
                                                        in
                                                        H.select [ A.onInput (\x -> ColumnEditing i { col | t = Concrete <| Maybe.withDefault Text <| Dict.get x <| Dict.fromList <| types }) ] <|
                                                            List.map (\( k, v ) -> H.option [ A.value k, A.selected (v == t) ] [ text k ]) <|
                                                                types
                                                ]
                                            ]
                                    )
                                    (Array.toList sheet.cols)
                                , [ H.th [ A.onClick ColumnPushing, S.verticalAlignBottom ] [ text "➡️" ] ]
                                ]
                        ]
                    , H.tbody [] <|
                        Array.toList <|
                            Array.indexedMap
                                (\n row ->
                                    H.tr [] <|
                                        List.concat
                                            [ [ H.th
                                                    [ A.onClick (Selecting Nothing (rect -1 n -1 n))
                                                    , A.onMouseDown (Selecting Nothing (rect -1 n -1 n))
                                                    , A.onMouseUp (Selecting Nothing { a = sheet.select.a, b = xy -1 n })
                                                    , A.onMouseEnter (CellHovering (xy -1 n))
                                                    ]
                                                    [ text "↔️" ]
                                              ]
                                            , List.indexedMap
                                                (\i col ->
                                                    -- TODO: Don't allow editing if Virtual column.
                                                    H.td
                                                        [ A.onClick (Selecting (row |> Dict.get col.key |> Maybe.andThen (D.decodeValue string >> Result.toMaybe) |> Maybe.withDefault "" |> Just) { a = xy i n, b = xy i n })
                                                        , A.onMouseDown (Selecting Nothing (rect i n i n))
                                                        , A.onMouseUp (Selecting Nothing { a = sheet.select.a, b = xy i n })
                                                        , A.onMouseEnter (CellHovering (xy i n))
                                                        , S.backgroundColor <|
                                                            if (sheet.select /= rect -1 -1 -1 -1) && ((sheet.select.a.x <= i && i <= sheet.select.b.x) || (sheet.select.a.x == -1 && -1 == sheet.select.b.x)) && ((sheet.select.a.y <= n && n <= sheet.select.b.y) || (sheet.select.a.y == -1 && -1 == sheet.select.b.y)) then
                                                                "#eee"

                                                            else
                                                                ""
                                                        ]
                                                        [ if sheet.write /= Nothing && sheet.select == rect i n i n then
                                                            H.input [ A.id "new-cell", A.value (Maybe.withDefault "" sheet.write), A.onInput CellWriting, A.onBlur CellSaving, S.width "100%" ] []

                                                          else
                                                            row |> Dict.get col.key |> Maybe.withDefault (E.string "") |> D.decodeValue string |> Result.withDefault "TODO: parse error" |> text
                                                        ]
                                                )
                                                (Array.toList sheet.cols)

                                            -- TODO: Drag this to reorder the row.
                                            , [ H.th [ A.onClick (RowPushing n), S.textAlignCenter ] [ text "↩️" ] ]
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
