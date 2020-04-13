module Lib.Conway exposing (Cell(..), Grid, step, emptyCell)

import List.Extra

type Cell = Full | Empty

type alias Grid = {
    cells: List (List Cell),
    size: Int
    }

type alias Position = (Int, Int)

emptyCell: Int -> Grid
emptyCell n = let cells = List.map (\_ -> List.map (\_ -> Empty) (List.range 0 n)) (List.range 0 n)
                in {size=n, cells=cells}


getAtIndex: Int ->  List a  -> Maybe a
getAtIndex i xs = xs
                |> List.drop i
                |> List.head

getAtPos: Grid -> Position -> Cell
getAtPos g (x, y) = g.cells
                    |> getAtIndex x
                    |> Maybe.andThen (getAtIndex y)
                    |> Maybe.withDefault Empty

genIndexes: Int -> List (List (Int, Int))
genIndexes n = List.map (\x -> List.map (\y -> (x, y)) (List.range 0 n)) (List.range 0 n) 

step : Grid -> Grid
step g = let 
            indexes = 
                genIndexes (g.size - 1)
            f i = stepCell (getAtPos g i) (getNumLivingNeighbor g i)
            newCells = 
                List.map (List.map f) indexes
            in 
            {g | cells = newCells}

stepCell: Cell -> Int -> Cell
stepCell c neighbors = case c of
    Full -> if neighbors == 2 || neighbors ==  3 then Full else Empty
    Empty -> if neighbors == 3 then Full else Empty


cellToInt: Cell -> Int
cellToInt x = case x of
                Full -> 1
                Empty -> 0

getNumLivingNeighbor: Grid -> Position -> Int
getNumLivingNeighbor  g (x, y) = genIndexes 2 
                                 |> List.concat 
                                 |> List.Extra.remove (x, y)
                                 |> List.map (toIndex g (x, y))
                                 |> List.map (getAtPos g)
                                 |> List.map cellToInt
                                 |> List.sum

toIndex: Grid -> Position -> (Int, Int) -> Position
toIndex grid (x, y) (dx, dy) = (wrapIndex (x+dx) grid.size, wrapIndex (y+dy) grid.size)

wrapIndex: Int -> Int -> Int
wrapIndex index size = modBy size (modBy size index + size)


