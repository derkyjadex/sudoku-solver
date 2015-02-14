import Data.List (intersect)
import GHC.Exts (sortWith)

type Value = Int
type Board = [Value]
type Col = Int
type Row = Int
type Index = Int
type Block = Int

easy, hard :: Board
easy = [ 5,3,0, 0,7,0, 0,0,0
       , 6,0,0, 1,9,5, 0,0,0
       , 0,9,8, 0,0,0, 0,6,0

       , 8,0,0, 0,6,0, 0,0,3
       , 4,0,0, 8,0,3, 0,0,1
       , 7,0,0, 0,2,0, 0,0,6

       , 0,6,0, 0,0,0, 2,8,0
       , 0,0,0, 4,1,9, 0,0,5
       , 0,0,0, 0,8,0, 0,7,9
       ]
hard = [ 0,0,0, 0,0,3, 0,2,0
       , 2,0,0, 0,1,9, 0,0,0
       , 0,0,1, 0,0,8, 0,9,7

       , 6,0,0, 0,0,0, 0,7,0
       , 7,0,9, 6,0,1, 8,0,4
       , 0,3,0, 0,0,0, 0,0,6

       , 3,6,0, 8,0,0, 7,0,0
       , 0,0,0, 9,5,0, 0,0,3
       , 0,8,0, 1,0,0, 0,0,0
       ]

getColValues :: Board -> Col -> [Value]
getColValues [] _ = []
getColValues board col =
        board !! col : getColValues (drop 9 board) col

getRowValues :: Board -> Row -> [Value]
getRowValues board row =
        take 9 $ drop (row * 9) board

getBlockValues :: Board -> Block -> [Value]
getBlockValues board block =
        let cells = [0, 1, 2, 9, 10, 11, 18, 19, 20]
            offset = block `quot` 3 * 27 + (block `mod` 3 * 3)
            offsetCells = map (+offset) cells
         in map (board !!) offsetCells

getBlockNum :: Col -> Row -> Block
getBlockNum col row =
        let x = col `quot` 3
            y = row `quot` 3
         in y * 3 + x

getFreeByCol :: Board -> Col -> [Value]
getFreeByCol board col =
        let colValues = getColValues board col
         in filter (`notElem` colValues) [1..9]


getFreeByRow :: Board -> Row -> [Value]
getFreeByRow board row =
        let rowValues = getRowValues board row
         in filter (`notElem` rowValues) [1..9]

getFreeByBlock :: Board -> Block -> [Value]
getFreeByBlock board block =
        let blockValues = getBlockValues board block
         in filter (`notElem` blockValues) [1..9]

getFree :: Board -> Col -> Row -> [Value]
getFree board col row =
        let colFree = getFreeByCol board col
            rowFree = getFreeByRow board row
            block = getBlockNum col row
            blockFree = getFreeByBlock board block
         in intersect colFree $ intersect rowFree blockFree

getColRow :: Index -> (Col, Row)
getColRow i = (i `mod` 9, i `quot` 9)

getSolutionSpace :: Board -> [(Index, [Value])]
getSolutionSpace board =
        let freeCells = map fst $ filter ((==0) . snd) $ zip [0..] board
         in map solutions freeCells
      where solutions i =
              let (col, row) = getColRow i
               in (i, getFree board col row)

applyMove :: Board -> Index -> Value -> Board
applyMove board i value =
        take i board ++ [value] ++ drop (i + 1) board

moveList :: Board -> [(Index, [Value])]
moveList board =
        let moves = getSolutionSpace board
         in sortWith (length . snd) moves

solve' :: [(Index, [Value])] -> Board -> [Board]
solve' [] board = [board]
solve' ((_, []):_) _ = []
solve' ((i, [v]):_) board =
        let board' = applyMove board i v
            ss = moveList board'
         in solve' ss board'
solve' ((i, v:vs):_) board =
        solve' [(i, [v])] board ++ solve' [(i, vs)] board

solve :: Board -> [Board]
solve board = solve' (moveList board) board

boardStr :: Board -> String
boardStr [] = []
boardStr board =
        boardStr' 0 board
        where boardStr' _ [] = "|\n+---+---+---+"
              boardStr' i (v:vs) =
                decorationAt i ++ valueStr v ++ boardStr' (i + 1) vs
              valueStr 0 = " "
              valueStr v = show v

decorationAt :: Index -> String
decorationAt n
    | n == 0 = "+---+---+---+\n|"
    | n `mod` 27 == 0 = "|\n+---+---+---+\n|"
    | n `mod` 9 == 0 = "|\n|"
    | n `mod` 3 == 0 = "|"
    | otherwise = ""

printBoard :: Board -> IO ()
printBoard board =
        putStrLn $ boardStr board

doSolve :: Board -> IO ()
doSolve board = do
        putStrLn "Initial:"
        printBoard board
        putStrLn "Solutions:"
        mapM_ printBoard $ solve board
        putStrLn ""

main :: IO ()
main = do
        doSolve easy
        doSolve hard
