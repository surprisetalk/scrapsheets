module Main exposing (main)

---- IMPORTS ------------------------------------------------------------------

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Date exposing (Date)
import Dict exposing (Dict)
import Html as H exposing (Html, code, text)
import Html.Attributes as A exposing (..)
import Html.Events as A exposing (..)
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


result : (x -> b) -> (a -> b) -> Result x a -> b
result fx fa res =
    case res of
        Ok a ->
            fa a

        Err x ->
            fx x


maybe : x -> (a -> x) -> Maybe a -> x
maybe x fa =
    Maybe.map fa >> Maybe.withDefault x



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
subscriptions model =
    Sub.none



---- MODEL --------------------------------------------------------------------


type alias Model =
    { nav : Nav.Key
    , open : Sheet Content
    , library : Dict String (Sheet ())
    , settings : { scrapbooks : Dict String () }
    , newTag : Maybe String
    , newCell : Maybe String
    , selected : Region
    , hover : Index
    , filter : String
    }


type alias Region =
    { a : Index, b : Index }


type alias Index =
    ( Int, Int )


type Sheet content
    = Sheet
        { id : String
        , name : String
        , tags : Set String
        , created : Time.Posix
        , updated : Time.Posix
        , thumb : Svg
        , content : content
        }


type alias Svg =
    -- TODO
    ()


type
    Content
    -- TODO: Move some of these to the shop as free templates.
    -- TODO: email, settings, databases, git, github, stripe, logs, sheets, tests, code stats, social media keywords
    = Sql
    | Prql
    | Fql
    | Gql
    | Scrap
    | Js
    | Python
    | R
    | Julia
    | Json D.Value
    | Canvas
    | Cells
        { columns : Dict Int ( String, Column )
        , cells : Array (Dict Int D.Value)
        }
    | Raw
    | File
    | Sheets
    | J
    | K
    | Apl
    | Chart
    | Mailbox
    | Form
    | Http (Result Http.Error ())
    | Db
    | Rss (Result Http.Error ())
    | Cal
    | Hook


type
    Column
    -- TODO: Virtual vs. Concrete?
    = Formula Formula
    | Data Type


type Formula
    = Exceed String


type
    Type
    -- = Text
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
    -- | Checkbox Bool
    -- | Input {}
    -- | Slider {}
    -- | Link {}
    -- | Number
    = Text
    | Number


toType : Column -> Type
toType column =
    case column of
        Formula (Exceed _) ->
            -- TODO: Infer this.
            Text

        Data t ->
            t



---- INIT ---------------------------------------------------------------------


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ nav =
    Tuple.pair
        { nav = nav
        , open =
            Sheet
                { id = ""
                , tags = Set.empty |> Set.insert "my-tag-2"
                , name = "my-sheet"
                , created = Time.millisToPosix 0
                , updated = Time.millisToPosix 0
                , thumb = ()
                , content =
                    Cells
                        { columns =
                            Dict.fromList <|
                                List.indexedMap Tuple.pair <|
                                    [ ( "A", Data Text )
                                    , ( "B", Data Number )
                                    , ( "C", Formula (Exceed "A++2*B") )
                                    , ( "D", Formula (Exceed "1.5*B") )
                                    ]
                        , cells =
                            Array.fromList <|
                                List.map Dict.fromList <|
                                    [ [ ( 0, E.string "hello" ), ( 1, E.string "world" ), ( 2, E.int 89 ) ]
                                    , [ ( 0, E.int 48 ), ( 1, E.float 1.23 ), ( 3, E.string "62" ) ]
                                    , [ ( 0, E.bool True ), ( 1, E.bool False ), ( 2, E.string "true" ) ]
                                    , [ ( 1, E.string "4.56" ), ( 3, E.string "boo" ) ]
                                    ]
                        }
                }
        , library = Dict.empty
        , settings = { scrapbooks = Dict.empty }
        , newTag = Nothing
        , newCell = Just "hello"
        , selected = { a = ( -1, -1 ), b = ( -1, -1 ) }
        , hover = ( -1, -1 )
        , filter = ""
        }
        Cmd.none



---- MESSAGES -----------------------------------------------------------------


type Msg
    = NoOp
    | SheetNameEditing String
    | TagEditing (Maybe String)
    | FilterEditing String
    | ColumnLabelEditing Int String
    | ColumnEditing Int Column
    | DefinitionEditing String
    | RegionSelecting (Maybe String) Region
    | CellEditing (Maybe String)
    | CellSaving
    | CellHovering Index
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest



---- PARSER -------------------------------------------------------------------


string : D.Decoder String
string =
    D.oneOf
        [ D.string
        , D.map String.fromInt D.int
        , D.map String.fromFloat D.float
        ]



---- UPDATE -------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SheetNameEditing name ->
            let
                (Sheet sheet) =
                    model.open
            in
            ( { model | open = Sheet { sheet | name = name } }, Cmd.none )

        TagEditing newTag ->
            ( { model | newTag = newTag }, Cmd.none )

        FilterEditing filter ->
            ( { model | filter = filter }, Cmd.none )

        DefinitionEditing _ ->
            -- TODO: Update the columns.
            ( model, Cmd.none )

        ColumnLabelEditing _ _ ->
            -- TODO: Update column labels.
            ( model, Cmd.none )

        ColumnEditing _ _ ->
            -- TODO: Update column definition.
            ( model, Cmd.none )

        RegionSelecting newCell region ->
            -- TODO: Always set .a to min() and .b to max() so that comparison is easy.
            ( { model | newCell = newCell, selected = region }, Task.attempt (always NoOp) (Dom.focus "new-cell") )

        CellSaving ->
            let
                (Sheet open) =
                    model.open

                ( x, y ) =
                    model.selected.a
            in
            ( { model
                | newCell = Nothing
                , open =
                    Sheet
                        { open
                            | content =
                                case open.content of
                                    Cells cells ->
                                        Cells { cells | cells = cells.cells |> Array.set y (cells.cells |> Array.get y |> Maybe.map (Dict.update x (always (Maybe.map E.string model.newCell))) |> Maybe.withDefault Dict.empty) }

                                    _ ->
                                        -- TODO: More options.
                                        open.content
                        }
              }
            , Cmd.none
            )

        CellEditing newCell ->
            ( { model | newCell = newCell }, Cmd.none )

        CellHovering index ->
            ( { model | hover = index }, Cmd.none )

        UrlChanged url ->
            -- TODO
            ( model, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            -- TODO
            ( model, Cmd.none )

        LinkClicked (Browser.External url) ->
            -- TODO
            ( model, Cmd.none )



---- VIEW ---------------------------------------------------------------------


view : Model -> Browser.Document Msg
view model =
    { title = "scrapsheets"
    , body =
        let
            (Sheet sheet) =
                model.open
        in
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
                        , H.input [ A.value sheet.name, A.onInput SheetNameEditing ] []
                        , H.div [ S.displayFlex, S.flexDirectionRow, S.gapRem 0.5 ] <|
                            List.concat
                                [ case model.newTag of
                                    Nothing ->
                                        [ H.button [ A.onClick (TagEditing (Just "")) ] [ text "#" ] ]

                                    Just value ->
                                        [ H.input [ A.value value, A.onInput (TagEditing << Just) ] [] ]
                                , List.map (\tag -> H.a [ A.href "?tag=TODO" ] [ text ("#" ++ tag) ]) <| Set.toList sheet.tags
                                ]
                        ]
                    , H.div [ S.displayFlex, S.flexDirectionRowReverse ]
                        [ H.a [ A.href "#history" ] [ text "history (1)" ]
                        , H.a [ A.href "#share" ] [ text "share" ]
                        , H.div [ S.displayFlex, S.flexDirectionRowReverse, S.gapRem 0.5 ]
                            [ H.a [ A.href "?following=" ] [ text "taylor" ]
                            , H.a [ A.href "?following=sarah" ] [ text "sarah" ]
                            ]
                        ]
                    ]

                -- TODO: All current filters should be rendered as text in the searchbar. This helps people (1) learn the language and (2) indicate that they're searching rather than editing.
                , H.input [ A.value model.filter, A.onInput FilterEditing, S.width "100%" ] []
                , H.lazy3 viewSheet model.selected model.newCell sheet.content
                ]
            , H.aside [ S.displayFlex, S.flexDirectionColumn, S.minWidthRem 15 ]
                -- TODO: This section automatically populates based on context. It's like an inspector that's shows you details on what you're currently doing.
                -- TODO: Prefer .selected and fallback to .hover.
                -- TODO: [ "definition", "scrappy", "share", "history", "problems", "related", "help" ]
                [ H.span [] [ text "definition" ]
                , case sheet.content of
                    Cells { columns } ->
                        H.textarea [ A.onInput DefinitionEditing, S.minHeightRem 10 ] [ text (Debug.toString columns) ]

                    _ ->
                        H.span [] [ text "TODO: definition" ]
                ]
            ]
        ]
    }


viewSheet : Region -> Maybe String -> Content -> Html Msg
viewSheet region newCell content =
    -- TODO: https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed
    case content of
        Cells { columns, cells } ->
            let
                ncols : Int
                ncols =
                    Maybe.withDefault -1 (List.maximum (Dict.keys columns))

                viewHeader : Int -> ( String, Column ) -> List (Html Msg)
                viewHeader i ( label, column ) =
                    [ H.div [ S.displayFlex, S.flexWrapWrap, S.gapRem 0.5 ] <|
                        case toType column of
                            Text ->
                                let
                                    stats : Dict String Int
                                    stats =
                                        cells
                                            |> Array.foldl
                                                (\a -> Dict.update (Dict.get i a |> Maybe.withDefault (E.string "") |> D.decodeValue string |> Result.withDefault "") (Maybe.withDefault 0 >> (+) 1 >> Just))
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
                                        cells |> Array.map (Dict.get i >> Maybe.andThen (D.decodeValue (D.oneOf [ D.float, D.map toFloat D.int, D.andThen (String.toFloat >> maybe (D.fail "") D.succeed) D.string ]) >> Result.toMaybe))

                                    nums : List Float
                                    nums =
                                        vals |> Array.toList |> List.filterMap identity
                                in
                                -- TODO: String.left is a bad hack! figure out how to do better rounding.
                                List.map (\( k, v ) -> H.span [] [ text (k ++ ": " ++ (String.left 5 <| Maybe.withDefault "_" <| Maybe.map String.fromFloat v)) ]) <|
                                    [ ( "many", vals |> Array.filter ((/=) Nothing) |> Array.length |> toFloat |> Just )
                                    , ( "min", List.minimum nums )
                                    , ( "mean", iif (List.length nums > 0) (Just (List.sum nums / toFloat (List.length nums))) Nothing )
                                    , ( "max", List.maximum nums )
                                    ]
                    , H.input [ A.value label, A.onInput (ColumnLabelEditing i) ] []
                    , case column of
                        Formula (Exceed formula) ->
                            H.input [ A.value formula, A.onInput (ColumnEditing i << Formula << Exceed) ] []

                        Data t ->
                            let
                                types : List ( String, Type )
                                types =
                                    [ ( "same", Text ), ( "text/from-float", Number ) ]
                            in
                            H.select [ A.onInput (ColumnEditing i << Data << Maybe.withDefault Text << flip Dict.get (Dict.fromList types)) ] <|
                                List.map (\( k, v ) -> H.option [ A.value k, A.selected (v == t) ] [ text k ]) <|
                                    types
                    ]

                viewRow : Int -> Dict Int D.Value -> Html Msg
                viewRow n row =
                    H.tr [] <|
                        (::)
                            (H.th
                                [ A.onClick (RegionSelecting Nothing { a = ( -1, n ), b = ( -1, n ) })
                                , A.onMouseDown (RegionSelecting Nothing { a = ( -1, n ), b = ( -1, n ) })
                                , A.onMouseUp (RegionSelecting Nothing { region | b = ( -1, n ) })
                                , A.onMouseEnter (CellHovering ( -1, n ))
                                ]
                                [ text (String.fromInt n) ]
                            )
                        <|
                            List.map
                                (\i ->
                                    -- TODO: Don't allow editing if Formula column.
                                    H.td
                                        [ A.onClick (RegionSelecting (row |> Dict.get i |> Maybe.andThen (D.decodeValue string >> Result.toMaybe) |> Maybe.withDefault "" |> Just) { a = ( i, n ), b = ( i, n ) })
                                        , A.onMouseDown (RegionSelecting Nothing { a = ( i, n ), b = ( i, n ) })
                                        , A.onMouseUp (RegionSelecting Nothing { region | b = ( i, n ) })
                                        , A.onMouseEnter (CellHovering ( i, n ))
                                        , S.backgroundColor <|
                                            if (region /= { a = ( -1, -1 ), b = ( -1, -1 ) }) && ((Tuple.first region.a <= i && i <= Tuple.first region.b) || (Tuple.first region.a == -1 && -1 == Tuple.first region.b)) && ((Tuple.second region.a <= n && n <= Tuple.second region.b) || (Tuple.second region.a == -1 && -1 == Tuple.second region.b)) then
                                                "#eee"

                                            else
                                                ""
                                        ]
                                    <|
                                        -- TODO: Needs to match selected region.
                                        if newCell /= Nothing && ( i, n ) == region.a && ( i, n ) == region.b then
                                            [ H.input [ A.id "new-cell", A.value (Maybe.withDefault "" newCell), A.onInput (CellEditing << Just), A.onBlur CellSaving, S.width "100%" ] []
                                            ]

                                        else
                                            maybe [] (ls << viewCell (Dict.get i row) << Tuple.second) <|
                                                Dict.get i columns
                                )
                            <|
                                List.range 0 ncols

                viewCell : Maybe D.Value -> Column -> Html Msg
                viewCell x col =
                    x
                        |> Maybe.withDefault (E.string "")
                        |> D.decodeValue (D.oneOf [ D.string, D.map String.fromInt D.int, D.map String.fromFloat D.float ])
                        |> result (always (text "TODO: parse error")) text
            in
            H.table [ S.borderCollapseCollapse, A.onMouseLeave (CellHovering ( -1, -1 )) ]
                [ H.thead []
                    [ H.tr [] <|
                        List.map
                            (\i ->
                                H.th
                                    [ A.onClick (RegionSelecting Nothing { a = ( i, -1 ), b = ( i, -1 ) })
                                    , A.onMouseDown (RegionSelecting Nothing { a = ( i, -1 ), b = ( i, -1 ) })
                                    , A.onMouseUp (RegionSelecting Nothing { region | b = ( i, -1 ) })
                                    , A.onMouseEnter (CellHovering ( i, -1 ))
                                    , S.textAlignLeft
                                    ]
                                <|
                                    ls <|
                                        H.div [ S.displayFlex, S.flexDirectionColumn ] <|
                                            maybe [] (viewHeader i) <|
                                                Dict.get i columns
                            )
                        <|
                            List.range -1 ncols
                    ]
                , H.tbody [] <| Array.toList <| Array.indexedMap viewRow cells
                ]

        Json data ->
            data
                |> D.decodeValue
                    (D.oneOf
                        [ D.fail "array of column arrays" -- TODO
                        , D.fail "array of row arrays" -- TODO
                        , D.fail "array of objects" -- TODO
                        , D.fail "object of arrays" -- TODO
                        ]
                    )
                |> result (D.errorToString >> text)
                    (List.map (\_ -> H.tr [] [ H.td [] [ text "TODO: json cell" ] ]) >> H.tbody [] >> ls >> H.table [ S.borderCollapseCollapse ])

        _ ->
            text "TODO"
