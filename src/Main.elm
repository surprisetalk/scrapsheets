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
import Rows
import Set exposing (Set)
import Task exposing (Task)
import Time exposing (Month(..))
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


type alias Sheet content =
    { id : String
    , tags : Set String
    , created : Time.Posix
    , updated : Time.Posix
    , thumb : Url.Url
    , content : content
    , view : View
    }


type Content
    = Api Api
    | Sql
    | Prql
    | Fql
    | Scrap
    | Js
    | Json
    | Bytes
    | File


type Api
    = Mailbox
    | Form
    | Http
    | Gql
    | Db
    | Rss
    | Cal
    | Hook
    | Form


type View
    = Raw {}
    | Rows { columns : List Type }
    | Shelf {}
    | Gallery {}
    | Doc {}
    | Plot {}
    | Map {}
    | Canvas {}
    | Kv {}
    | Query {}
    | Page {}
    | App {}


type Type
    = Text
    | Bytes
    | Tag
    | List
    | Date
    | Color
    | Image
    | Sheet
    | Shape2d
    | Shape3d
    | Vector
    | Rows (List Type)
    | Doc
    | Plot
    | Map
    | Checkbox bool
    | Input {}
    | Slider {}
    | Link {}



---- INIT ---------------------------------------------------------------------


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    Tuple.pair
        {}
        Cmd.none



---- MESSAGES -----------------------------------------------------------------


type Msg
    = NoOp
    | RowsMsg Rows.Msg
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
        [ H.node "style" [] [ text "body * { box-sizing: border-box; }" ]
        , H.div [ S.displayFlex, S.flexDirectionColumn ] <|
            let
                { main_, settings } =
                    case model.open.view of
                        Rows rows ->
                            H.map Rows.Msg (Rows.view rows model.open.content)
            in
            [ H.aside [ S.displayFlex ] <|
                List.map (\x -> H.a [ A.href x ] [ text x ])
                    [ "new", "library", "store", "settings", "help" ]
            , H.main_ [ S.displayFlex ] <|
                main__
            , H.aside [ S.displayFlex ] <|
                List.concatMap
                    (\( title, section ) ->
                        H.h2 [] [ text title ]
                            :: List.concatMap
                                (\( subtitle, setting ) -> [ H.h3 [] [ text subtitle ], setting ])
                                section
                    )
                    settings
            ]
        ]
    }
