{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Random (randomIO)
import qualified System.Console.ANSI as ANSI
import System.IO (BufferMode(..), stdin, stdout, hReady, hSetBuffering, hSetEcho, hFlush)
import Data.Ord (comparing)
import Data.List (groupBy, maximumBy, sortOn, intersperse)
import Data.Maybe (fromJust)
import Data.Map.Strict  (Map, fromList, (!), insertWith) 
import Control.Monad (forM_)

---------- TYPE DECLARATIONS -----------
newtype Strategy = Strategy { runStrategy :: Board -> Player -> IO Int }

data Player = Red | Yellow deriving (Eq, Show)

data Board = Board 
  { boardSize    :: (Int, Int)
  , boardColumns :: Map Int [Player] }

data Game = Game
  { gameBoard      :: Board
  , strategyRed    :: Strategy
  , strategyYellow :: Strategy 
  , currentTurn    :: Player }

data GameCommand = GCSetup | GCPlay | GCExit  

data Key = KeyLeft | KeyRight | KeyEnter deriving Show

------------ Constants -------------------

(x0, y0) = (0, 0)

charEmpty = '·'
charPiece = 'O'
charWin   = '0'

colorEmpty  = ANSI.xterm6LevelRGB 1 1 1
colorRed    = ANSI.xterm6LevelRGB 5 0 0
colorYellow = ANSI.xterm6LevelRGB 5 5 0

colorForPlayer = \case 
  Red    -> colorRed
  Yellow -> colorYellow

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
        greedyHeuristic col = case (scorePlayer player, scorePlayer rival) of
            (4, _) -> 5 -- player can connect 4 --> score 5
            (_, 4) -> 4 -- rival can connect 4  --> score 4 
            (l, _) -> l -- player can connect l --> score (3, 2, 1)
            where scorePlayer p = length (getLongestLine board p col)
    in return . maximumBy (comparing greedyHeuristic) . getAvailableCols $ board

smartS :: Strategy
smartS = Strategy $ error "TODO"

humanS :: Strategy
humanS = Strategy $ 
  \board p ->
    let validCols = getAvailableCols board  
        (nCols, _) = boardSize board
        readValidColumn = do
          col <- columnSelector p (1, nCols)
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

getLongestLine :: Board -> Player -> Int -> [(Int, Int)]
getLongestLine b player col =
  let (nCols, nRows) = boardSize b
      columns = boardColumns b
      row = length (columns ! col) + 1
      count = takeWhile (isPlayer . getPlayerAt b)
        where isPlayer Nothing  = False
              isPlayer (Just p) = p == player

      l = [col-1, col-2..1]
      r = [col+1..nCols]
      u = [row+1..nRows]
      d = [row-1, row-2..1]
      
      x = repeat row
      y = repeat col

      vert = (col, row) : count (y `zip` d)
      hor = reverse (count (l `zip` x)) ++ [(col, row)] ++ count (r `zip` x)
      diag1 = reverse (count  (l `zip` d)) ++ [(col, row)] ++ count (r `zip` u)
      diag2 = reverse (count  (l `zip` u)) ++ [(col, row)] ++ count (r `zip` d)

  in maximumBy (comparing length) [ vert, hor, diag1, diag2]

getPlayerAt :: Board -> (Int, Int) -> Maybe Player
getPlayerAt Board {boardSize = (nCols, _), boardColumns = columns} (col, row) = 
  if inside 
    then Just $ columns ! col !! (row-1)
    else Nothing
  where inside = col >= 1 && col <= nCols && row >= 1 && row <= length (columns ! col) 

playMove :: Board -> Player -> Int -> Board
playMove b p c = b { boardColumns = insertWith (flip (++)) c [p] (boardColumns b) }

otherPlayer :: Player -> Player
otherPlayer = \case
  Yellow -> Red
  Red -> Yellow

---------------- GAME IO ------------------
main :: IO ()
main = initGame >> setupGame >>= loopGame
  where loopGame game =
          runGame game >> endGame >>= \case 
               GCPlay  -> loopGame game
               GCSetup -> main
               GCExit  -> return ()
  
initGame :: IO ()
initGame = ANSI.clearScreen  >> ANSI.hideCursor

endGame :: IO GameCommand
endGame = do
  ANSI.setCursorPosition 0 0
  menuSelector [ (GCPlay  , "Play again")
               , (GCSetup , "Options")
               , (GCExit  , "Exit") ]

setupGame :: IO Game
setupGame = do
  return Game { gameBoard = getEmptyBoard 7 6
              , strategyRed    = humanS
              , strategyYellow = humanS
              , currentTurn = Red } 

runGame :: Game -> IO (Maybe Player)
runGame game = do
  let board = gameBoard game
  drawBoard board
  if null (getAvailableCols board)
    then return Nothing -- No more moves available -> Game drawn
    else do
      let player   = currentTurn game
          strategy = case player of 
            Yellow -> strategyYellow game
            Red    -> strategyRed game
      column <- runStrategy strategy board player
      let line = getLongestLine board player column
          board' = playMove board player column
      if length line >= 4
        then drawWinningLine board' line >> return (Just player)
        else runGame game { gameBoard = board', currentTurn = otherPlayer player }
 
drawBoard :: Board -> IO ()
drawBoard board@Board{boardSize = (nCols, nRows), boardColumns = columns} = do
  -- Draw the frame
  ANSI.setCursorPosition (y0 + nRows + 1) x0
  putStr $ "└"  ++ replicate nCols '─' ++ "┘"
  forM_ [1..nRows] $ \dy -> do
    ANSI.setCursorPosition (y0 + dy) x0
    putStr "│"
    ANSI.cursorForward nCols
    putStr "│"
  -- Draw the board
  forM_ ( (,) <$> [1..nCols] <*> [1..nRows] ) $ \(c, r) ->  do
    ANSI.setCursorPosition (y0 + nRows - r + 1) (x0 + c)
    let (color, char) = case getPlayerAt board (c, r) of
                          Nothing -> (colorEmpty, charEmpty)
                          Just p  -> (colorForPlayer p, charPiece)
    ANSI.setSGR [ANSI.SetPaletteColor ANSI.Foreground color]
    putChar char
    ANSI.setSGR [] 
  hFlush stdout 
  
drawWinningLine :: Board -> [(Int, Int)] -> IO ()
drawWinningLine board@Board{boardSize = (nCols, nRows), boardColumns = columns} line = do
  forM_ line $ \(c, r) ->  do
    ANSI.setCursorPosition (y0 + nRows - r + 1) (x0 + c)
    let color = colorForPlayer . fromJust . getPlayerAt board $ (c, r)
    ANSI.setSGR [ANSI.SetPaletteColor ANSI.Foreground color]
    putChar charWin
    ANSI.setSGR [] 
  hFlush stdout 


menuSelector :: [(a, String)] -> IO a
menuSelector menu = do
  i <- selectorController (0, length menu - 1) printF 0
  return $ fst (menu !! i)
    where printF i = do
            let as = intersperse (putStr "  |  ") . mapIth i (\a -> highlight >> a >> ANSI.setSGR [] ) . map (putStr . snd) $ menu
            ANSI.clearLine
            sequence_ as
            ANSI.cursorBackward (5*(sum (map (length . snd) menu ) -1))
            hFlush stdout
          highlight = ANSI.setSGR [ANSI.SetSwapForegroundBackground True]

columnSelector :: Player -> (Int, Int) -> IO Int
columnSelector player (l, r) = do
  ANSI.setCursorPosition x0 y0
  i <- selectorController (l, r) printF ((r+l) `quot` 2)
  ANSI.clearLine
  return i
    where printF i = do
            ANSI.clearLine
            ANSI.cursorForward i 
            ANSI.setSGR [ANSI.SetPaletteColor ANSI.Foreground (colorForPlayer player)]
            putChar charPiece
            ANSI.setSGR []
            ANSI.cursorBackward (i+1)
            hFlush stdout

selectorController :: (Int, Int) -> (Int -> IO ()) -> Int -> IO Int
selectorController (l, r) paintF = go
  where go i = paintF i >> getKey >>= \case
            Just KeyRight -> if i < r then go (i+1) else go i
            Just KeyLeft  -> if l < i then go (i-1) else go i
            Just KeyEnter -> ANSI.cursorBackward 1 >> return i
            Nothing       -> go i

      --getKey :: IO (Maybe Key)
        getKey = do
          hSetEcho stdout False
          hSetBuffering stdin NoBuffering
          getChar >>= \case
            '\ESC' -> parseArrowKey
            '\n'   -> return $ Just KeyEnter
            _  -> getKey
        
      --parseArrowKey :: IO (Maybe Key)
        parseArrowKey =
          getChar >>= \case 
            '\ESC' -> parseArrowKey
            '['    -> getChar >>= \case
                  'C' -> return $ Just KeyRight
                  'D' -> return $ Just KeyLeft
                  _   -> return Nothing
            _   -> return Nothing
            
mapIth :: Int -> (a -> a) -> [a] -> [a]
mapIth i f xs = 
  let (a, b:c) = splitAt i xs
  in a ++ f b : c