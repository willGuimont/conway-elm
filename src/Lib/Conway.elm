module Lib.Conway exposing (Cell, CellType(..), Grid, Position, emptyGrid, step, toggleCell)

import List.Extra


type CellType
    = Full
    | Empty


type alias Position =
    ( Int, Int )


type alias Cell =
    { position : Position
    , cellType : CellType
    }


type alias Grid =
    { cells : List (List Cell)
    , size : Int
    }


toggleCellType : Cell -> Cell
toggleCellType c =
    let
        nt =
            case c.cellType of
                Empty ->
                    Full

                Full ->
                    Empty
    in
    { c | cellType = nt }


emptyGrid : Int -> Grid
emptyGrid n =
    let
        cells =
            List.map (\x -> List.map (\y -> { position = ( x, y ), cellType = Empty }) (List.range 0 (n - 1))) (List.range 0 (n - 1))
    in
    { size = n, cells = cells }


getAtIndex : Int -> List a -> Maybe a
getAtIndex i xs =
    xs
        |> List.drop i
        |> List.head


getAtPos : Grid -> Position -> Cell
getAtPos g ( x, y ) =
    g.cells
        |> getAtIndex x
        |> Maybe.andThen (getAtIndex y)
        |> Maybe.withDefault { position = ( x, y ), cellType = Empty }


setAtPos : Grid -> Position -> Cell -> Grid
setAtPos g ( x, y ) c =
    let
        newList =
            List.Extra.getAt x g.cells
                |> Maybe.andThen (\p -> Just (List.Extra.setAt y c p))
                |> Maybe.andThen (\p -> Just (List.Extra.setAt x p g.cells))
                |> Maybe.withDefault g.cells
    in
    { g | cells = newList }


toggleCell : Grid -> Cell -> Grid
toggleCell g c =
    setAtPos g c.position (toggleCellType (getAtPos g c.position))


genIndexes : Int -> List (List ( Int, Int ))
genIndexes n =
    List.map (\x -> List.map (\y -> ( x, y )) (List.range 0 n)) (List.range 0 n)


step : Grid -> Grid
step g =
    let
        indexes =
            genIndexes (g.size - 1)

        f i =
            stepCell (getAtPos g i) (getNumLivingNeighbor g i)

        newCells =
            List.map (List.map f) indexes
    in
    { g | cells = newCells }


stepCell : Cell -> Int -> Cell
stepCell c neighbors =
    let
        ct =
            case c.cellType of
                Full ->
                    if neighbors == 2 || neighbors == 3 then
                        Full

                    else
                        Empty

                Empty ->
                    if neighbors == 3 then
                        Full

                    else
                        Empty
    in
    { c | cellType = ct }


cellToInt : Cell -> Int
cellToInt c =
    case c.cellType of
        Full ->
            1

        Empty ->
            0


getNumLivingNeighbor : Grid -> Position -> Int
getNumLivingNeighbor g ( x, y ) =
    genIndexes 2
        |> List.concat
        |> List.map (\(a, b) -> (a - 1, b - 1))
        |> List.Extra.remove ( 0, 0 )
        |> List.map (toIndex g ( x, y ))
        |> List.map (getAtPos g)
        |> List.map cellToInt
        |> List.sum


toIndex : Grid -> Position -> ( Int, Int ) -> Position
toIndex grid ( x, y ) ( dx, dy ) =
    ( wrapIndex (x + dx) grid.size, wrapIndex (y + dy) grid.size )


wrapIndex : Int -> Int -> Int
wrapIndex index size =
    modBy size (modBy size index + size)
