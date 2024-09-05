module Main exposing (main)

---- IMPORTS ------------------------------------------------------------------

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import Dict exposing (Dict)
import Html as H exposing (Html, text)
import Html.Attributes as A exposing (..)
import Html.Events as A exposing (..)
import Html.Lazy as H
import Html.Style as S
import Http
import Json.Decode as D
import Json.Encode as E
import Regex exposing (Regex)
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
subscriptions _ =
    Sub.none



---- MODEL --------------------------------------------------------------------


type alias SheetId =
    Int


type alias Input valid data =
    { valid : valid
    , default : data
    , data : data
    }


type Col
    = Numbers (Array Float)
    | Strings (Array String)
    | Images (Array String)
    | Links (Array String)
    | Datepickers (Array (Input () Date))
    | Checkboxes (Array (Input () Bool))
    | Sliders (Array (Input ( Float, Float ) Float))
    | Fields (Array (Input Regex String))
    | Chart (Array ( Float, Float ))


type alias Sheet =
    { transpose : Bool
    , rows : Int
    , cols : Array ( String, Col )
    }


type alias Model =
    -- The output of `code` completely determines the shape and content of `sheet`.
    { sheets : Dict SheetId { code : String, sheet : Result String Sheet }

    -- the row beneath the sheet opens up with its code and a pointer and tools
    , shelf : List (List SheetId) -- use empty sheets as row/col spacers
    }



---- INIT ---------------------------------------------------------------------


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    ( { sheets =
            Dict.empty
                |> Dict.insert 1
                    { code = "TODO"
                    , sheet =
                        Ok
                            { transpose = False
                            , rows = 4
                            , cols =
                                Array.fromList
                                    [ ( "Col", Numbers <| Array.fromList [ 1, 2, 3, 4 ] )
                                    , ( "Col", Numbers <| Array.fromList [ 5, 6, 7, 8 ] )
                                    ]
                            }
                    }
      , shelf =
            [ [ 1 ]
            ]
      }
    , Cmd.none
    )



---- MESSAGES -----------------------------------------------------------------


type Msg
    = SheetNew
    | CodeEdit SheetId String
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest



---- UPDATE -------------------------------------------------------------------


type alias Env =
    Dict SheetId (Maybe Sheet)



{-

   sheet/from-json []
     -- todo

   sheet/from-csv [ sheet/col/numbers, shet/col/text ] csv
   . csv : text = "col1,col2\n" ++ text/join "\n" [ "1,a" , "2,b" , "3,c" ]

   sheet
     [ sheet/col/numbers  "col1" [ 1, 2, 3 ]
     , sheet/col/text     "col2" [ "a", "b", "c" ]
     , sheet/col/checkbox "col3" [ true, false, false ]
     ]

   sheet/map2 add s1 s2

   sheet/filter-number "col1" (gt 1) s1

   sheet/select [ "col1" ] s1

   sheet/join s1 s2

   sheet/stack s1 s2

-}


exec : Env -> String -> Result String Sheet
exec env code =
    -- TODO: Create a simple parser.
    Err "TODO"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SheetNew ->
            let
                sheetId : Int
                sheetId =
                    model.sheets |> Dict.keys |> List.maximum |> Maybe.withDefault 0 |> (+) 1
            in
            ( { model
                | shelf = model.shelf ++ [ [ sheetId ] ]
                , sheets =
                    model.sheets
                        |> Dict.insert sheetId
                            { code = "TODO"
                            , sheet = Ok { transpose = False, rows = 0, cols = Array.empty }
                            }
              }
            , Cmd.none
            )

        CodeEdit id code ->
            let
                env : Env
                env =
                    model.sheets |> Dict.map (always (.sheet >> Result.toMaybe))
            in
            ( { model | sheets = model.sheets |> Dict.insert id { code = code, sheet = exec env code } }
            , Cmd.none
            )

        UrlChanged url ->
            ( model, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            ( model, Cmd.none )

        LinkClicked (Browser.External url) ->
            ( model, Cmd.none )



---- VIEW ---------------------------------------------------------------------


viewCell : Int -> Col -> Maybe (Html Msg)
viewCell i col =
    case col of
        Numbers xs ->
            xs |> Array.get i |> Maybe.map (String.fromFloat >> text)

        Strings xs ->
            xs |> Array.get i |> Maybe.map text

        Images xs ->
            xs |> Array.get i |> Maybe.map (\src -> H.img [ A.src src ] [])

        Links xs ->
            xs |> Array.get i |> Maybe.map (\href -> H.a [ A.href href ] [])

        Datepickers xs ->
            -- TODO
            Nothing

        Checkboxes xs ->
            -- TODO
            Nothing

        Sliders xs ->
            -- TODO
            Nothing

        Fields xs ->
            -- TODO
            Nothing

        Chart xs ->
            case i of
                0 ->
                    -- TODO: Put everything in row 0 with a rowspan of the whole table
                    Nothing

                _ ->
                    Nothing


viewSheet : Sheet -> Html Msg
viewSheet sheet =
    -- TODO: Show some call to action for empty sheets? Or make the region clickable?
    H.table []
        [ H.thead []
            [ H.tr [] <|
                Array.toList <|
                    Array.map (H.th [] << ls << text << Tuple.first) <|
                        sheet.cols
            ]
        , H.tbody [] <|
            Array.toList <|
                Array.initialize sheet.rows
                    (\i ->
                        H.tr [] <|
                            Array.toList <|
                                Array.initialize (Array.length sheet.cols)
                                    (H.td []
                                        << ls
                                        << Maybe.withDefault (text "")
                                        << Maybe.andThen (viewCell i)
                                        << Maybe.map Tuple.second
                                        << flip Array.get sheet.cols
                                    )
                    )
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "scrapsheets"
    , body =
        -- TODO: Sheets that exist but aren't currently on the shelf should sit minimized in the corner or something like buffers waiting to be placed back on the shelf.
        [ H.node "style" [] [ text "" ]
        , H.main_ []
            [ H.div [ S.displayFlex, S.flexDirectionColumn ] <|
                List.append [ H.button [ A.onClick SheetNew ] [ text "New sheet" ] ] <|
                    List.map (H.div [ S.displayFlex, S.flexDirectionRow ]) <|
                        List.map
                            (List.map
                                (\id ->
                                    Maybe.withDefault (H.div [] [ text "TODO: sheet not found" ]) <|
                                        Maybe.map
                                            (\{ code, sheet } ->
                                                H.div [ S.displayFlex, S.flexDirectionColumn ]
                                                    [ case sheet of
                                                        Ok x ->
                                                            viewSheet x

                                                        Err x ->
                                                            H.div [] [ text ("TODO: error: " ++ x) ]
                                                    , H.textarea [ A.onInput (CodeEdit id), A.value code ] [ text code ]
                                                    ]
                                            )
                                        <|
                                            Dict.get id model.sheets
                                )
                            )
                        <|
                            model.shelf
            , H.aside []
                []
            ]
        ]
    }
