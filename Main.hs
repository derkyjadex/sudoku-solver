import Data.List (intersect)

type Value = Int
type Board = [Value]

initial :: Board
initial = [
          5,3,0, 0,7,0, 0,0,0,
          6,0,0, 1,9,5, 0,0,0,
          0,9,8, 0,0,0, 0,6,0,

          8,0,0, 0,6,0, 0,0,3,
          4,0,0, 8,0,3, 0,0,1,
          7,0,0, 0,2,0, 0,0,6,

          0,6,0, 0,0,0, 2,8,0,
          0,0,0, 4,1,9, 0,0,5,
          0,0,0, 0,8,0, 0,7,9
          ]

getValueStr :: Value -> String
getValueStr 0 = "."
getValueStr v = show v

getColValues :: Board -> Int -> [Value]
getColValues [] _ = []
getColValues board col =
        board !! col : getColValues (drop 9 board) col

getRowValues :: Board -> Int -> [Value]
getRowValues board row =
        take 9 $ drop (row * 9) board

getBlockValues :: Board -> Int -> [Value]
getBlockValues board block =
        let cells = [0, 1, 2, 9, 10, 11, 18, 19, 20]
            offset = case block of
                         0 -> 0
                         1 -> 3
                         2 -> 6
                         3 -> 27
                         4 -> 30
                         5 -> 33
                         6 -> 54
                         7 -> 57
                         8 -> 60
                         _ -> undefined
            offsetCells = map (+offset) cells
         in map (board !!) offsetCells

getBlockNum :: Int -> Int -> Int
getBlockNum col row =
        let x = col `quot` 3
            y = row `quot` 3
         in y * 3 + x

getFreeByCol :: Board -> Int -> [Value]
getFreeByCol board col =
        let colValues = getColValues board col
         in filter (`notElem` colValues) [1..9]


getFreeByRow :: Board -> Int -> [Value]
getFreeByRow board row =
        let rowValues = getRowValues board row
         in filter (`notElem` rowValues) [1..9]

getFreeByBlock :: Board -> Int -> [Value]
getFreeByBlock board block =
        let blockValues = getBlockValues board block
         in filter (`notElem` blockValues) [1..9]

getFree :: Board -> Int -> Int -> [Value]
getFree board col row =
        let colFree = getFreeByCol board col
            rowFree = getFreeByRow board row
            block = getBlockNum col row
            blockFree = getFreeByBlock board block
         in intersect colFree $ intersect rowFree blockFree

getColRow :: Int -> (Int, Int)
getColRow i = (i `mod` 9, i `quot` 9)

getSolutionSpace :: Board -> [(Int, [Value])]
getSolutionSpace board =
        let freeCells = map fst $ filter (\(_, x) -> x == 0) $ zip [0..] board
         in map solutions freeCells
      where solutions i =
              let (col, row) = getColRow i
               in (i, getFree board col row)

applyMove :: Board -> Int -> Value -> Board
applyMove board i value =
        take i board ++ [value] ++ drop (i + 1) board

applySolutions :: [(Int, [Value])] -> Board -> Board
applySolutions [] board = board
applySolutions ((i,[v]):ss) board = applySolutions ss $ applyMove board i v
applySolutions _ _ = undefined

applySimpleSolutions :: Board -> Board
applySimpleSolutions board =
        let solutions = getSolutionSpace board
            simple = filter isSimple solutions
         in applySolutions simple board
      where isSimple (_, [_]) = True
            isSimple _ = False

isSolved :: Board -> Bool
isSolved = notElem 0

printBoard :: Board -> IO ()
printBoard [] = return ()
printBoard (a:b:c:d:e:f:g:h:i:rest) = do
        putStr $ getValueStr a
        putStr $ getValueStr b
        putStr $ getValueStr c
        putStr " "
        putStr $ getValueStr d
        putStr $ getValueStr e
        putStr $ getValueStr f
        putStr " "
        putStr $ getValueStr g
        putStr $ getValueStr h
        putStr $ getValueStr i
        putStrLn ""
        printBoard rest
printBoard _ = undefined

solve :: Board -> IO Board
solve board = do
        printBoard board
        putStrLn "-----------"
        if isSolved board
            then return board
            else solve $ applySimpleSolutions board

main :: IO ()
main = do
        _ <- solve initial
        return ()
