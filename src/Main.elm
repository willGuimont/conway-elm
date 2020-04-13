module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Lib.Conway exposing (Cell, CellType(..), Grid, emptyGrid, step, toggleCell)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


gridSize : Int
gridSize =
    20


type alias Model =
    { grid : Grid
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = emptyGrid gridSize }, Cmd.none )



-- UPDATE


type Msg
    = ToggleCell Cell
    | StepGrid


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCell cell ->
            ( { model | grid = toggleCell model.grid cell }, Cmd.none )

        StepGrid ->
            ( { model | grid = step model.grid }, Cmd.none )



-- VIEW


cellToString : Cell -> String
cellToString c =
    case c.cellType of
        Full ->
            "■"

        Empty ->
            "□"


view : Model -> Html Msg
view model =
    let
        table =
            List.map (\x -> Html.tr [] (List.map (\y -> Html.td [Html.Events.onClick (ToggleCell y), Html.Attributes.style "cursor" "crosshair", Html.Attributes.style "font-size" "2em" ] [ Html.text (cellToString y) ]) x)) model.grid.cells

    in
    Html.div []
        [ Html.p [Html.Attributes.style "font-weight" "bold"] [Html.text "Conway Game of Life"]
        , Html.table [] table
        , Html.button [ Html.Events.onClick StepGrid ] [ Html.text "Step" ]
        ]
