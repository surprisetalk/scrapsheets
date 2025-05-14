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
        , columns : List Type
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
    | Raw
    | File


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



---- INIT ---------------------------------------------------------------------


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    Tuple.pair
        {}
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
                    [ "new", "library", "store", "settings", "help" ]
            , H.main_ [ S.displayFlex ]
                [ H.lazy2 viewSheet sheet.columns sheet.content
                ]
            , H.aside [ S.displayFlex ]
                -- TODO: Settings/tools.
                []
            ]
        ]
    }


viewSheet : List Type -> Content -> Html Msg
viewSheet columns content =
    result (\_ -> text "TODO") (H.table [] << List.map (H.tr [])) <|
        case content of
            Json data ->
                data
                    |> D.decodeValue
                        (D.oneOf
                            [ D.fail "2d array" -- TODO
                            , D.fail "array of objects" -- TODO
                            , D.fail "object of arrays" -- TODO
                            ]
                        )
                    |> Result.mapError Debug.toString
                    |> Result.map (\_ -> [ [ text "TODO" ] ])

            _ ->
                Err "TODO"
