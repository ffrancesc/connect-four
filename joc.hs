module Main (main) where

import System.Random (randomIO)
import System.Console.ANSI
import System.IO (BufferMode(..), stdin, hReady, hSetBuffering, hSetEcho)
import Data.Ord (comparing)
import Data.List (groupBy, maximumBy, sortOn)
import Data.Map.Strict  (Map, fromList, (!), insertWith) 
import Control.Monad (forM_, when)

---------- TYPE DECLARATIONS -----------
newtype Strategy = Strategy { runStrategy :: Board -> Player -> IO Int }

data Player = Y | R deriving (Eq, Show)

data Board = Board 
  { boardSize :: (Int, Int)
  , boardColumns :: Map Int [Player] }

data Game = Game
  { gameBoard   :: Board
  , playerY :: Strategy
  , playerR :: Strategy 
  , turn    :: Player }

------------ Strategies -------------------
randomS :: Strategy
randomS = Strategy $ \board _ -> randomPick (getAvailableCols board)
    where randomPick xs = do 
            random <- randomIO :: IO Int
            let index = random `mod` length xs
            return $ xs!!index

greedyS :: Strategy
greedyS = Strategy $ 
  \board player -> 
    let rival = otherPlayer player
        greedyHeuristic col = case (getLongestLine board player col, getLongestLine board rival col) of
            (4, _) -> 5 -- player can connect 4 --> score 5
            (_, 4) -> 4 -- rival can connect 4  --> score 4 
            (l, _) -> l -- player can connect l --> score (3, 2, 1)
    in return . maximumBy (comparing greedyHeuristic) . getAvailableCols $ board

smartS :: Strategy
smartS = Strategy $ error "TODO"

humanS :: Strategy
humanS = Strategy $ 
  \board p ->
    let validCols = getAvailableCols board  
        readValidColumn = do
          col <- arrowSelector p (fst  (boardSize board))
          if col `elem` validCols 
            then return col
            else readValidColumn
    in readValidColumn

----------- smartS ---------------



----------- BOARD UTILS ---------------


getEmptyBoard :: Int -> Int -> Board
getEmptyBoard nCols nRows = Board {boardSize = (nCols, nRows), boardColumns = empty}
  where empty = fromList $ [1..nCols] `zip` repeat []

getAvailableCols :: Board -> [Int]
getAvailableCols Board {boardSize = (nCols, nRows), boardColumns = columns} = 
  filter available [1..nCols] 
    where available i = length (columns ! i) < nRows

getLongestLine :: Board -> Player -> Int -> Int
getLongestLine b player col =
  let (nCols, nRows) = boardSize b
      columns = boardColumns b
      row = length (columns ! col) + 1
      count = length . takeWhile isPlayer . map (getPlayerAt b)
        where isPlayer Nothing  = False
              isPlayer (Just p) = p == player

      l = [col-1, col-2..1]
      r = [col+1..nCols]
      u = [row+1..nRows]
      d = [row-1, row-2..1]
      e  = repeat row
      vert = length . takeWhile (== player) . reverse $ columns ! col
      horL = count $ l `zip` e
      horR = count $ r `zip` e
      diagLU = count $ l `zip` u 
      diagLD = count $ l `zip` d
      diagRU = count $ r `zip` u
      diagRD = count $ r `zip` d

  in 1 + maximum [ vert
                 , horL + horR
                 , diagLU + diagRD
                 , diagLD + diagRU ]

isWinningMove :: Board -> Player -> Int -> Bool
isWinningMove b p c = getLongestLine b p c == 4 

getPlayerAt :: Board -> (Int, Int) -> Maybe Player
getPlayerAt Board {boardSize = (nCols, _), boardColumns = columns} (col, row) = 
  if inside 
    then Just $ columns ! col !! (row-1)
    else Nothing
  where inside = col >= 1 && col <= nCols && row >= 1 && row <= length (columns ! col) 

playMove :: Board -> Player -> Int -> Board
playMove b p c = b { boardColumns = insertWith (flip (++)) c [p] (boardColumns b) }

otherPlayer :: Player -> Player
otherPlayer R = Y
otherPlayer Y = R

---------------- GAME IO ------------------
colorForPlayer :: Player -> Color
colorForPlayer R = Red
colorForPlayer Y = Yellow

main :: IO ()
main = do
  initGame
  winner <- setupGame >>= runGame 
  putStrLn $ case winner of 
               Nothing -> "Game Drawn!"
               Just Y  -> "Yellow wins!"
               Just R  -> "Red wins!"
  getKey
  main

initGame :: IO ()
initGame = clearScreen -- >> hideCursor

setupGame :: IO Game
setupGame = do
  return Game{gameBoard = getEmptyBoard 7 6, playerY = greedyS, playerR = humanS, turn = R} 

runGame :: Game -> IO (Maybe Player)
runGame game = do
  let board = gameBoard game
  drawBoard (0,0) board
  setCursorPosition 0 0
  if null (getAvailableCols board)
    then return Nothing -- No more moves available -> Game drawn
    else do
      let player = turn game
          strat = case player of 
            Y -> playerY game
            R -> playerR game
      move <- runStrategy strat board player
      if isWinningMove board player move
        then return (Just player)
        else runGame game { gameBoard = playMove board player move, turn = otherPlayer player }
 
drawBoard :: (Int, Int) -> Board -> IO ()
drawBoard (x, y) board@Board{boardSize = (nCols, nRows), boardColumns = columns} = do
  -- Draw the frame
  setCursorPosition (y+nRows+1) x
  putStr $ "└"  ++ replicate nCols '─' ++ "┘"
  forM_ [1..nRows] $ \dy -> do
    setCursorPosition (y+dy) x 
    putStr "│"
    cursorForward nCols
    putStr "│"
  -- Draw the board
  forM_ ( (,) <$> [1..nCols] <*> [1..nRows] ) $ \(c, r) ->  do
    setCursorPosition (y+nRows-r+1) (x+c)
    case  getPlayerAt board (c, r) of
      Nothing -> putStr "·"
      Just p  -> setSGR [SetColor Foreground Dull (colorForPlayer p)] >> putChar '0' >> setSGR []  
            

arrowSelector :: Player -> Int -> IO Int
arrowSelector p n = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  go $ n `quot` 2
    where go i = do
            clearLine
            cursorForward i 
            setSGR [SetColor Foreground Dull (colorForPlayer p)]
            putStr "0"
            setSGR [Reset]
            cursorBackward (i+1)
            key <- getKey
            if key /= "\ESC" 
              then case key of
                "\ESC[C" -> if i < n then go (i+1) else go n
                "\ESC[D" -> if 1 < i then go (i-1) else go 1
                "\n"     -> cursorBackward 1 >> return i
                _        -> go i
              else arrowSelector p n

getKey :: IO String
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)