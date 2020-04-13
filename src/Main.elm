module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)

import Lib.Conway exposing (..)



-- MAIN

main: Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { grid : Grid
  }


init : Model
init =
  { grid=emptyCell 10 }



-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update _ model = model



-- VIEW


view : Model -> Html Msg
view _ = Html.p [] [Html.text "Hello world"]
