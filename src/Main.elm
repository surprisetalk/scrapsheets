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
        { columns : Dict Int { label : String, type_ : Type }
        , cells : Dict Int (Dict Int D.Value)
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


type Type
    = Text
    | Bytes
    | Tag
    | List
    | Date
    | Color
    | Image
    | Subsheet
    | Shape2d
    | Shape3d
    | Vector
    | Rows (List Type)
    | Doc
    | Plot
    | Map
    | Checkbox Bool
    | Input {}
    | Slider {}
    | Link {}
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
                            Dict.fromList
                                [ ( 0, { label = "A", type_ = Text } )
                                , ( 1, { label = "B", type_ = Number } )
                                , ( 2, { label = "C", type_ = Checkbox False } )
                                ]
                        , cells =
                            Dict.fromList <|
                                List.map (Tuple.mapSecond Dict.fromList) <|
                                    [ ( 0, [ ( 1, E.string "hello" ), ( 2, E.string "world" ), ( 5, E.int 89 ) ] )
                                    , ( 1, [ ( 0, E.int 48 ), ( 2, E.float 1.23 ), ( 5, E.string "62" ) ] )
                                    , ( 2, [ ( 2, E.bool True ), ( 3, E.bool False ), ( 4, E.string "true" ) ] )
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
        , H.node "style" [] [ text "td { border: 1px solid black; height: 1rem; }" ]
        , H.div [ S.displayFlex, S.flexDirectionRow, S.paddingRem 1 ] <|
            [ H.aside [ S.displayFlex, S.flexDirectionColumn ] <|
                List.map (\x -> H.a [ A.href x ] [ text x ])
                    [ "new", "books", "shop", "settings", "help" ]
            , H.main_ [ S.displayFlex, S.flexDirectionColumn, S.height "100%", S.width "100%" ]
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
    result text (H.table [ S.borderCollapseCollapse ] << ls << H.tbody []) <|
        case content of
            Cells { columns, cells } ->
                let
                    ncols =
                        1 + Maybe.withDefault -1 (List.maximum (List.concatMap Dict.keys (Dict.values cells)))

                    nrows =
                        1 + Maybe.withDefault -1 (List.maximum (Dict.keys columns))
                in
                Ok <|
                    List.concat
                        [ ls <| H.tr [] <| Array.toList <| Array.initialize ncols (\x -> H.th [] [ text <| maybe "" .label <| Dict.get x columns ])
                        , Array.toList <|
                            Array.initialize ncols
                                (\y ->
                                    H.tr [] <|
                                        (Array.toList <|
                                            Array.initialize nrows
                                                (\x -> Maybe.withDefault (H.td [] []) <| Maybe.map2 viewCell (Maybe.map .type_ <| Dict.get x columns) <| Maybe.andThen (Dict.get y) <| Dict.get x <| cells)
                                        )
                                )
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
                    |> Result.mapError Debug.toString
                    |> Result.map (List.map (\_ -> H.tr [] [ H.td [] [ text "TODO" ] ]))

            _ ->
                Err "TODO"


viewCell : Type -> D.Value -> Html Msg
viewCell t x =
    H.td
        (case t of
            Number ->
                [ S.textAlignRight ]

            _ ->
                []
        )
    <|
        ls <|
            Result.withDefault (text "TODO: error") <|
                flip D.decodeValue x <|
                    case t of
                        Text ->
                            D.map text <|
                                D.oneOf
                                    [ D.string
                                    , D.map String.fromInt D.int
                                    , D.map String.fromFloat D.float
                                    ]

                        Number ->
                            D.map text <|
                                D.oneOf
                                    [ D.map String.fromInt D.int
                                    , D.map String.fromFloat D.float
                                    ]

                        Checkbox defaultChecked ->
                            D.map (\checked -> H.input [ A.type_ "checkbox", A.checked checked ] []) <|
                                D.oneOf
                                    [ D.bool
                                    , D.string
                                        |> D.andThen
                                            (\s ->
                                                case String.toLower s of
                                                    "true" ->
                                                        D.succeed True

                                                    "false" ->
                                                        D.succeed False

                                                    _ ->
                                                        D.fail "expected \"true\" or \"false\""
                                            )
                                    , D.succeed defaultChecked
                                    ]

                        _ ->
                            D.map text <|
                                D.succeed "TODO: parse type"


viewSettings : Content -> Html Msg
viewSettings content =
    text ""
