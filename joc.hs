{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Random (randomIO)
import qualified System.Console.ANSI as ANSI
import System.IO (BufferMode(..), stdin, stdout, hReady, hSetBuffering, hSetEcho, hFlush)
import Data.Ord (comparing)
import Data.List (groupBy, maximumBy, sortOn, intersperse)
import Data.Maybe (fromJust)
import Control.Monad (forM_)

---------- TYPE DECLARATIONS -----------
newtype Strategy = Strategy { runStrategy :: Board -> Player -> IO Int }

data Player = Red | Yellow deriving (Eq, Show)

data Board = Board
  { boardSize    :: (Int, Int)
  , boardColumns :: [[Player]] }

data Game = Game
  { gameBoard      :: Board
  , strategyRed    :: Strategy
  , strategyYellow :: Strategy
  , currentTurn    :: Player }

data GameCommand = GCSetup | GCPlay | GCExit

data Key = KeyLeft | KeyRight | KeyEnter

------------ GAME CONSTANTS -------------------
firstPlayer = Red

charEmpty = '·'
charPiece = 'O'
charWin   = '0'

colorEmpty  = ANSI.xterm6LevelRGB 1 1 1 -- Grey
colorRed    = ANSI.xterm6LevelRGB 5 0 0 -- Red
colorYellow = ANSI.xterm6LevelRGB 5 5 0 -- Yellow

defaultGame = Game
  { gameBoard      = emptyBoard (7, 6)
  , strategyRed    = humanS
  , strategyYellow = humanS
  , currentTurn    = firstPlayer }

colorForPlayer = \case
  Red    -> colorRed
  Yellow -> colorYellow

------------ Strategies -------------------
randomS :: Strategy
randomS = Strategy $ \board _ -> randomPick (getAvailableCols board)
    where randomPick xs = do
            random <- randomIO :: IO Int
            let i = random `mod` length xs
            return $ xs !! i

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
          col <- columnSelector p (0, nCols - 1)
          if col `elem` validCols
            then return col
            else readValidColumn
    in readValidColumn

----------- smartS ---------------



----------- BOARD UTILS ---------------
emptyBoard :: (Int, Int) -> Board
emptyBoard size@(nCols, _) = Board {boardSize = size, boardColumns = replicate nCols []}

getAvailableCols :: Board -> [Int]
getAvailableCols Board {boardSize = (nCols, nRows), boardColumns = columns} =
  filter available [0..nCols-1]
    where available i = length (columns !! i) < nRows

getLongestLine :: Board -> Player -> Int -> [(Int, Int)]
getLongestLine b player col =
  let (nCols, nRows) = boardSize b
      columns = boardColumns b
      row = length (columns !! col)
      count = takeWhile (isPlayer . getPlayerAt b)
        where isPlayer Nothing  = False
              isPlayer (Just p) = p == player

      l = [col-1, col-2..0]
      r = [col+1..nCols-1]
      u = [row+1..nRows-1]
      d = [row-1, row-2..0]

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
    then Just $ columns !! col !! row
    else Nothing
  where inside = col >= 0 && col < nCols && row >= 0 && row < length (columns !! col)

playMove :: Board -> Player -> Int -> Board
playMove b p c = b { boardColumns = mapIth c (++ [p]) (boardColumns b) }

otherPlayer :: Player -> Player
otherPlayer = \case
  Yellow -> Red
  Red -> Yellow

---------------- GAME IO ------------------
main :: IO ()
main = initGame >> setupGame >>= loopGame
  where loopGame g =
          runGame g >>= endGame >>= \case
               GCPlay  -> loopGame g
               GCSetup -> main
               GCExit  -> quitGame

initGame :: IO ()
initGame = ANSI.clearScreen >> ANSI.hideCursor

setupGame :: IO Game
setupGame = do
  ANSI.setCursorPosition 1 0
  putStr "Board size: "
  ANSI.setCursorPosition 3 2
  size <- menuSelector 
    [ ((6,5)  , "Mini (6x5)")
    , ((7,6)  , "Standard (7x6)")
    , ((9,7)  , "Large (9x7)") 
    , ((13,11), "Huge (13x11)") ]  

  redS <- promptStrategy (5, 0) Red
  yellowS  <- promptStrategy (9, 0) Yellow
  return Game
    { gameBoard      = emptyBoard size
    , strategyRed    = redS
    , strategyYellow = yellowS
    , currentTurn    = firstPlayer }

  where promptStrategy (y, x) p = do
          ANSI.setCursorPosition y x
          putStr "Player "
          ANSI.setSGR [ANSI.SetPaletteColor ANSI.Foreground (colorForPlayer p)]
          putStr (show p)
          ANSI.setSGR [ANSI.Reset]
          putStr " mode: " 
          ANSI.setCursorPosition (y+2) (x+2)
          menuSelector 
            [ (randomS, "Dummy")
            , (greedyS, "Mediocre")
            , (smartS , "Beast") 
            , (humanS , "Human")]

runGame :: Game -> IO (Game, Maybe Player)
runGame  game@Game{ gameBoard = board, currentTurn = player} = do
  ANSI.clearScreen
  drawBoard board
  if null (getAvailableCols board)
    then return (game, Nothing) -- No more moves available -> Game drawn
    else do
      let strategy = case player of
            Yellow -> strategyYellow game
            Red    -> strategyRed game
      column <- runStrategy strategy board player
      let line = getLongestLine board player column
          board' = playMove board player column
      if length line < 4
        then runGame game { gameBoard = board', currentTurn = otherPlayer player }
        else drawWinningLine board' line >> return (game, Just player)

endGame :: (Game, Maybe Player) -> IO GameCommand
endGame (game, winner) = do
  let height = (+3) . snd . boardSize. gameBoard $ game
  ANSI.setCursorPosition height 0
  case winner of 
    Nothing -> putStr "Game drawn.."
    Just p -> do
        putStr "Player "
        ANSI.setSGR [ANSI.SetPaletteColor ANSI.Foreground (colorForPlayer p)]
        putStr (show p)
        ANSI.setSGR [ANSI.Reset]
        putStr " won! "
        hFlush stdout
  ANSI.setCursorPosition (height+2) 2
  menuSelector [ (GCPlay , "Play again")
                , (GCSetup, "Options")
                , (GCExit , "Exit") ]

quitGame :: IO ()
quitGame = ANSI.showCursor

drawBoard :: Board -> IO ()
drawBoard board@Board{boardSize = (nCols, nRows), boardColumns = columns} = do
  -- Draw the frame
  ANSI.setCursorPosition (nRows + 1) 0
  putStr $ "└"  ++ replicate nCols '─' ++ "┘"
  forM_ [1..nRows] $ \dy -> do
    ANSI.setCursorPosition dy 0
    putStr "│"
    ANSI.cursorForward nCols
    putStr "│"
  -- Draw the board
  forM_ ( (,) <$> [0..nCols-1] <*> [0..nRows-1] ) $ \(c, r) ->  do
    ANSI.setCursorPosition (nRows - r) (c + 1)
    let (color, char) = case getPlayerAt board (c, r) of
                          Nothing -> (colorEmpty, charEmpty)
                          Just p  -> (colorForPlayer p, charPiece)
    ANSI.setSGR [ANSI.SetPaletteColor ANSI.Foreground color]
    putChar char
    ANSI.setSGR []
  hFlush stdout

drawWinningLine :: Board -> [(Int, Int)] -> IO ()
drawWinningLine board@Board{boardSize = (nCols, nRows), boardColumns = columns} line = do
  forM_ line $ \(c, r) -> do
    let color = colorForPlayer . fromJust . getPlayerAt board $ (c, r)
    ANSI.setCursorPosition (nRows - r) (c + 1)
    ANSI.setSGR [ANSI.SetPaletteColor ANSI.Foreground color]
    putChar charWin
    ANSI.setSGR [ANSI.Reset]
  hFlush stdout


menuSelector :: [(a, String)] -> IO a
menuSelector menu = do
  i <- selectorController (0, length menu - 1) printF 0
  return $ fst (menu !! i)
    where printF i = do
            ANSI.clearLine
            sequence_ . separate . mapIth i highlight . map (putStr . snd) $ menu
            ANSI.cursorBackward lengthMenu
            hFlush stdout
          highlight a = ANSI.setSGR [ANSI.SetSwapForegroundBackground True] >> a >> ANSI.setSGR [ANSI.Reset]
          separate = intersperse (putStr separator)
          separator = "  |  "
          lengthMenu = length (foldMap snd menu) + length separator * (length menu - 1)
columnSelector :: Player -> (Int, Int) -> IO Int
columnSelector player (l, r) = do
  ANSI.setCursorPosition 0 0
  i <- selectorController (l, r) printF ((l + r) `quot` 2)
  ANSI.clearLine
  return i
    where printF i = do
            ANSI.clearLine
            ANSI.cursorForward (i+1)
            ANSI.setSGR [ANSI.SetPaletteColor ANSI.Foreground (colorForPlayer player)]
            putChar charPiece
            ANSI.setSGR [ANSI.Reset]
            ANSI.cursorBackward (i+2)
            hFlush stdout

selectorController :: (Int, Int) -> (Int -> IO ()) -> Int -> IO Int
selectorController (l, r) paintF i0 = go i0
  where go i = paintF i >> getKey >>= \case
            Just KeyRight -> if i < r then go (i+1) else go i
            Just KeyLeft  -> if l < i then go (i-1) else go i
            Just KeyEnter -> return i
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