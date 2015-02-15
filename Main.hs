import Data.List (intersect)
import GHC.Exts (sortWith)

type Value = Int
type Board = [Value]
type Col = Int
type Row = Int
type Index = Int
type Block = Int

easy, hard, evil :: String
easy = "530070000600195000098000060800060003400803001700020006060000280000419005000080079"
hard = "000003020200019000001008097600000070709601804030000006360800700000950003080100000"
evil = "502000008000800030000407060700010600040206090006040001050701000080002000900000806"

colValues :: Col -> Board -> [Value]
colValues _ [] = []
colValues col board =
        board !! col : colValues col (drop 9 board)

rowValues :: Row -> Board -> [Value]
rowValues row board =
        take 9 $ drop (row * 9) board

blockValues :: Block -> Board -> [Value]
blockValues block board =
        let cells = [0, 1, 2, 9, 10, 11, 18, 19, 20]
            offset = block `quot` 3 * 27 + (block `mod` 3 * 3)
            offsetCells = map (+offset) cells
         in map (board !!) offsetCells

blockNum :: Col -> Row -> Block
blockNum col row =
        let x = col `quot` 3
            y = row `quot` 3
         in y * 3 + x

freeByCol :: Col -> Board -> [Value]
freeByCol col board =
        let vs = colValues col board
         in filter (`notElem` vs) [1..9]

freeByRow :: Row -> Board -> [Value]
freeByRow row board =
        let vs = rowValues row board
         in filter (`notElem` vs) [1..9]

freeByBlock :: Block -> Board -> [Value]
freeByBlock block board =
        let vs = blockValues block board
         in filter (`notElem` vs) [1..9]

freeByIndex :: Index -> Board -> [Value]
freeByIndex i board =
        let col = i `mod` 9
            row = i `quot` 9
            block = blockNum col row
            colFree = freeByCol col board
            rowFree = freeByRow row board
            blockFree = freeByBlock block board
         in intersect colFree $ intersect rowFree blockFree

moveList :: Board -> [(Index, [Value])]
moveList board =
        let freeIndexes = map fst $ filter ((==0) . snd) $ zip [0..] board
            movesAt i = (i, freeByIndex i board)
            moves = map movesAt freeIndexes
         in sortWith (length . snd) moves

applyMove :: Index -> Value -> Board -> Board
applyMove i value board =
        take i board ++ [value] ++ drop (i + 1) board

solveWith :: [(Index, [Value])] -> Board -> [Board]
solveWith [] board = [board]
solveWith ((_, []):_) _ = []
solveWith ((i, [v]):_) board =
        let board' = applyMove i v board
            moves = moveList board'
         in solveWith moves board'
solveWith ((i, v:vs):_) board =
        solveWith [(i, [v])] board ++ solveWith [(i, vs)] board

solve :: Board -> [Board]
solve board = solveWith (moveList board) board

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

readBoard :: String -> Board
readBoard = map (\c -> read [c])

doSolve :: String -> IO ()
doSolve s = do
        let board = readBoard s
        putStrLn "Initial:"
        printBoard board
        putStrLn "Solutions:"
        mapM_ printBoard $ solve board
        putStrLn ""

main :: IO ()
main = do
        doSolve easy
        doSolve hard
        doSolve evil
