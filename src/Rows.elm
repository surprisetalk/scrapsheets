module Rows exposing (..)

---- MODEL --------------------------------------------------------------------


type alias Model =
    {}



---- INIT ---------------------------------------------------------------------


init : Model
init =
    {}



---- MESSAGES -----------------------------------------------------------------


type Msg
    = NoOp



---- VIEW ---------------------------------------------------------------------


view : Model -> Data -> Scrapsheets.View (Either Data.Msg Msg)
view config data =
    let
        rows : Result String Todo
        rows =
            case data of
                Json data ->
                    data
                        |> D.decode
                            (D.oneOf
                                [ D.failure "2d array"
                                , D.failure "array of objects"
                                , D.failure "object of arrays"
                                ]
                            )

                Api (Http (Success (Json data))) ->
                    Err "TODO"

                Api (Rss (Success (Json data))) ->
                    Err "TODO"
    in
    text "TODO"
