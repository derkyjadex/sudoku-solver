import Data.List (intersect)

type Value = Int
type Board = [Value]
type Col = Int
type Row = Int
type Index = Int
type Block = Int

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

applySolutions :: [(Index, [Value])] -> Board -> Board
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

boardStr :: Board -> String
boardStr [] = []
boardStr board =
        boardStr' 0 board
        where boardStr' _ [] = "|\n+---+---+---+"
              boardStr' n (v:vs) =
                decorationAt n ++ (valueStr v) ++ boardStr' (n + 1) vs
              valueStr 0 = " "
              valueStr v = show v

decorationAt :: Index -> String
decorationAt n
    | n == 0 = "+---+---+---+\n|"
    | n `mod` 27 == 0 = "|\n+---+---+---+\n|"
    | n `mod` 9 == 0 = "|\n|"
    | n `mod` 3 == 0 = "|"
    | otherwise = ""

solve :: Board -> IO Board
solve board = do
        putStrLn $ boardStr board
        putStrLn ""
        if isSolved board
            then return board
            else solve $ applySimpleSolutions board

main :: IO ()
main = do
        _ <- solve initial
        return ()
