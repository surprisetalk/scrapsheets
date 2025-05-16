module Main exposing (main)

---- NOTES --------------------------------------------------------------------
--
-- - local first
-- - deno/hono/pg for syncing and "magic" ingestion link
-- - elm
-- - elm nativescript for native
--
-- - nav
--   - blank sheet
--   - my sheets (as scrapsheet): tags (cloud, local, org, friend)
--   - scrap store (as scrapsheet) for templates and formulas and starterkits and popular sheets
--   - settings (as scrapsheet)
--   - help
-- - sheet tools
--   - assistant
--   - view settings
--   - permissions
--   - history
--   - linting
--   - backlinks
--   - help
--
-- email, settings, databases, git, github, stripe, logs, sheets, tests, code stats, social media keywords
--
--
---- IMPORTS ------------------------------------------------------------------

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import Dict exposing (Dict)
import Html as H exposing (Html, code, text)
import Html.Attributes as A exposing (..)
import Html.Events as A exposing (..)
import Html.Lazy as H
import Html.Style as S
import Http exposing (stringResolver)
import Json.Decode as D
import Json.Encode as E
import Parser as P exposing ((|.), (|=), Parser)
import Pratt as P
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
    { open : Sheet Content
    , library : Dict String (Sheet ())
    , settings : { scrapbooks : Dict String () }
    }


type Sheet content
    = Sheet
        { id : String
        , tags : Set String
        , created : Time.Posix
        , updated : Time.Posix
        , thumb : Svg
        , content : content
        }


type alias Svg =
    -- TODO
    ()


type Content
    = Api Api
    | Sql
    | Prql
    | Fql
    | Scrap
    | Js
    | Json D.Value
    | Cells
        { columns : Dict Int ( String, Column )
        , cells : Array (Dict Int D.Value)
        }
    | Raw
    | File
    | Sheets


type Api
    = Mailbox
    | Form
    | Http (Result Http.Error ())
    | Gql
    | Db
    | Rss (Result Http.Error ())
    | Cal
    | Hook


type Column
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



---- INIT ---------------------------------------------------------------------


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    Tuple.pair
        { open =
            Sheet
                { id = ""
                , tags = Set.empty
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
                                    , [ ( 0, E.int 48 ), ( 1, E.float 1.23 ), ( 2, E.string "62" ) ]
                                    , [ ( 0, E.bool True ), ( 1, E.bool False ), ( 2, E.string "true" ) ]
                                    , [ ( 1, E.string "woof" ) ]
                                    ]
                        }
                }
        , library = Dict.empty
        , settings = { scrapbooks = Dict.empty }
        }
        Cmd.none



---- MESSAGES -----------------------------------------------------------------


type Msg
    = NoOp
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest



---- PARSER -------------------------------------------------------------------


decoder =
    "TODO"



---- UPDATE -------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
        , H.div [ S.displayFlex, S.flexDirectionRow, S.paddingRem 1 ] <|
            [ H.aside [ S.displayFlex, S.flexDirectionColumn ] <|
                List.map (\x -> H.a [ A.href x ] [ text x ])
                    [ "new", "books", "shop", "settings", "help" ]
            , H.main_ [ S.displayFlex, S.flexDirectionColumn, S.height "100%", S.width "100%" ]
                -- TODO: All current filters should be rendered as text in the searchbar. This helps people (1) learn the language and (2) indicate that they're searching rather than editing.
                [ H.input [ A.type_ "search", S.width "100%" ] []
                , H.lazy viewMain sheet.content
                ]
            , H.aside [ S.displayFlex ]
                -- TODO: Settings/tools.
                [ H.lazy viewSettings sheet.content
                ]
            ]
        ]
    }


viewMain : Content -> Html Msg
viewMain content =
    -- TODO: https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed
    case content of
        Cells { columns, cells } ->
            let
                ncols : Int
                ncols =
                    Maybe.withDefault -1 (List.maximum (Dict.keys columns))

                viewHeader : Int -> ( String, Column ) -> List (Html Msg)
                viewHeader i ( label, column ) =
                    [ H.h3 [] [ text label ]
                    , case column of
                        Formula (Exceed formula) ->
                            H.input [ A.value formula ] []

                        Data Text ->
                            H.p [] [ text "TODO: stats" ]

                        Data Number ->
                            H.p [] [ text "TODO: stats" ]
                    ]

                viewRow : Dict Int D.Value -> Html Msg
                viewRow row =
                    H.tr [] <| List.map (\i -> H.td [] <| maybe [] (ls << viewCell (Dict.get i row) << Tuple.second) <| Dict.get i columns) <| List.range 0 ncols

                viewCell : Maybe D.Value -> Column -> Html Msg
                viewCell x col =
                    case col of
                        -- TODO: Show an error if there is cell data overwrites a formula cell.
                        Formula (Exceed formula) ->
                            text <| String.join "" [ "TODO: =(", formula, ")" ]

                        Data _ ->
                            x
                                |> Maybe.withDefault (E.string "")
                                |> D.decodeValue (D.oneOf [ D.string, D.map String.fromInt D.int, D.map String.fromFloat D.float ])
                                |> result (always (text "TODO: parse error")) text
            in
            H.table [ S.borderCollapseCollapse ]
                [ H.thead [] [ H.tr [] <| List.map (\i -> H.th [ S.textAlignLeft ] <| maybe [] (viewHeader i) <| Dict.get i columns) <| List.range 0 ncols ]
                , H.tbody [] <| Array.toList <| Array.map viewRow cells
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
                |> result (Debug.toString >> text)
                    (List.map (\_ -> H.tr [] [ H.td [] [ text "TODO: json cell" ] ]) >> H.tbody [] >> ls >> H.table [ S.borderCollapseCollapse ])

        _ ->
            text "TODO"


viewSettings : Content -> Html Msg
viewSettings content =
    text ""
