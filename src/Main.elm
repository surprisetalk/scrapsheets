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
                                    , ( 2, [ ( 0, E.bool True ), ( 3, E.bool False ), ( 4, E.string "true" ) ] )
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
        [ H.node "style" [] [ text "body * { box-sizing: border-box; }" ]
        , H.div [ S.displayFlex, S.flexDirectionColumn ] <|
            [ H.aside [ S.displayFlex ] <|
                List.map (\x -> H.a [ A.href x ] [ text x ])
                    [ "new", "books", "shop", "settings", "help" ]
            , H.main_ [ S.displayFlex ]
                [ H.lazy viewMain sheet.content
                , H.input [ A.type_ "search" ] []
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
    result (\_ -> text "TODO") (H.table []) <|
        case content of
            Cells { columns, cells } ->
                Ok <|
                    Array.toList <|
                        Array.initialize (Maybe.withDefault 0 (List.maximum (List.concatMap Dict.keys (Dict.values cells))))
                            (\y ->
                                H.tr [] <|
                                    (Array.toList <|
                                        Array.initialize (Maybe.withDefault 0 (List.maximum (Dict.keys columns)))
                                            (\x -> H.td [] <| Maybe.withDefault [] <| Maybe.map (ls << H.td [] << ls) <| Maybe.andThen (viewCell (Maybe.map .type_ <| Dict.get y columns)) <| Maybe.andThen (Dict.get y) <| Dict.get x <| cells)
                                    )
                            )

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


viewCell : Maybe Type -> D.Value -> Maybe (Html Msg)
viewCell t x =
    Just (text "TODO")


viewSettings : Content -> Html Msg
viewSettings content =
    text "TODO"
