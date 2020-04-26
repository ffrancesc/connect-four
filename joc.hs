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
----------------------------------------

{- Wrapper for a strategy function, which computes a column to play for a
   given player on given board -}
newtype Strategy = Strategy { runStrategy :: Board -> Player -> IO Int }


data Player = Red | Yellow deriving (Eq, Show)

data Board = Board
  { boardSize    :: (Int, Int)
  , boardColumns :: [[Player]] }

data Game = Game
  { gameBoard      :: Board
  , redStrategy    :: Strategy
  , yellowStrategy :: Strategy
  , currentTurn    :: Player }

data GameCommand = GCSetup | GCPlay | GCExit

data Key = KeyLeft | KeyRight | KeyEnter

------------ GAME CONSTANTS -----------------
---------------------------------------------

firstPlayer = Red

charEmpty = '·' -- Empty cell 
charPiece = 'O' -- Piece cell
charWin   = 'Ø' -- Winner piece cell

colorEmpty  = ANSI.xterm6LevelRGB 1 1 1 -- Grey (for empty cells)
colorRed    = ANSI.xterm6LevelRGB 5 0 0 -- Red
colorYellow = ANSI.xterm6LevelRGB 5 5 0 -- Yellow


-------------- HELPER FUNCTIONS -------------
---------------------------------------------

-- | Gets the color for a player
colorForPlayer = \case
  Red    -> colorRed
  Yellow -> colorYellow

-- | Returns the other player
otherPlayer = \case
  Yellow -> Red
  Red -> Yellow

-- | Maps a function to only the ith element of a list.
mapIth :: Int -> (a -> a) -> [a] -> [a]
mapIth i f xs =
  let (a, b:c) = splitAt i xs
  in a ++ f b : c


------------ Strategies -------------------
-------------------------------------------

-- | Pick a random column from all the available ones
randomS :: Strategy
randomS = Strategy $ \board _ -> randomPick (getAvailableCols board)
  where randomPick xs = do
          random <- randomIO :: IO Int
          let i = random `mod` length xs
          return $ xs !! i
  
-- | Try to win. Otherwise, prevent the other player from winning. Play for the longest line.
greedyS :: Strategy
greedyS = Strategy $
  \board player ->
    let rival = otherPlayer player
        -- get a score from the lines the player/rival can make by playing on column col
        greedyHeuristic col = case (scorePlayer player, scorePlayer rival) of
            (4, _) -> 5 -- player can connect 4 --> score 5 (Highest)
            (_, 4) -> 4 -- rival can connect 4  --> score 4
            (l, _) -> l -- player can connect l --> score (3, 2, 1)
            where scorePlayer p = length (getLongestLine p col board)
    in return . maximumBy (comparing greedyHeuristic) . getAvailableCols $ board

smartS :: Strategy
smartS = Strategy $ error "TODO"

-- | Prompt the user to pick an available column.
humanS :: Strategy
humanS = Strategy $
  \board p ->
    let validCols = getAvailableCols board
        (nCols, _) = boardSize board
        readValidColumn = do
          col <- columnSelector p board
          if col `elem` validCols
            then return col
            else readValidColumn
    in readValidColumn

----------- smartS ---------------



----------- BOARD UTILS ---------------

-- |Returns an empty board of size (cols, rows)
emptyBoard :: (Int, Int) -> Board
emptyBoard size@(nCols, _) = Board {boardSize = size, boardColumns = replicate nCols []}

-- |Returns a list of the available (not fully filled) columns of a board
getAvailableCols :: Board -> [Int]
getAvailableCols Board {boardSize = (nCols, nRows), boardColumns = columns} =
  filter available [0..nCols-1]
    where available i = length (columns !! i) < nRows

{- |Given a player, a column and a board, returns the longest line of player stones
   caused by the player playing a piece on that column -}
getLongestLine :: Player -> Int -> Board -> [(Int, Int)]
getLongestLine player col board =
  let (nCols, nRows) = boardSize board
      columns = boardColumns board
      row = length (columns !! col)
      -- Given an horizontal and vertical range, runs through them while finding player pieces.
      walkDirs dx dy = takeWhile (isPlayer . getPlayerAt board) $ dx `zip` dy
        where isPlayer Nothing  = False
              isPlayer (Just p) = p == player
      lt  = [col-1, col-2..0] -- left direction
      rt  = [col+1..nCols-1]  -- right direction
      up  = [row+1..nRows-1]  -- up direction
      dn = [row-1, row-2..0]  -- down direction
      (cx, cy) = (repeat col, repeat row) -- constant direction

      ver = (col, row) : walkDirs cx dn -- Longest vertical line
      hor = reverse (walkDirs lt cy) ++ [(col, row)] ++ walkDirs rt cy -- Longest horizontal line
      dg1 = reverse (walkDirs lt dn) ++ [(col, row)] ++ walkDirs rt up -- Longest diagonal / line 
      dg2 = reverse (walkDirs lt up) ++ [(col, row)] ++ walkDirs rt dn -- Longest diagonal \ line

  in maximumBy (comparing length) [ver, hor, dg1, dg2]


-- |Given a board and a (col, row), returns the stone's player placed there (if any)
getPlayerAt :: Board -> (Int, Int) -> Maybe Player
getPlayerAt Board {boardSize = (nCols, _), boardColumns = columns} (col, row) =
  if inside
    then Just $ columns !! col !! row
    else Nothing
  where inside = col >= 0 && col < nCols && row >= 0 && row < length (columns !! col)


-- |Given a player and a column of a board, places a piece on that column
playMove :: Player -> Int -> Board -> Board
playMove player col board = board { boardColumns = mapIth col (++ [player]) (boardColumns board) }



---------------- GAME IO ------------------
-------------------------------------------
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
    , redStrategy    = redS
    , yellowStrategy = yellowS
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


{- |Given a Game, handles the alternating turns of that game until it ends.
   |Then, returns de state of the game as well as the winner (if any)  -}
runGame :: Game -> IO (Game, Maybe Player)
runGame  game@Game{ gameBoard = board, currentTurn = player} = do
  ANSI.clearScreen
  drawBoard board
  if null (getAvailableCols board)
    -- No more moves available, no one wins.
    then return (game, Nothing) 
    else do
      -- Get the strategy of the current turns player.
      let strategy = case player of  
            Yellow -> yellowStrategy game
            Red    -> redStrategy game
      -- Run the strategy to get a column to play
      column <- runStrategy strategy board player 
      let line = getLongestLine player column board
          -- Updated board by Player playing on column
          board' = playMove player column board
      if length line >= 4
        -- player has won.
        then drawWinningLine board' line >> return (game, Just player)
        else runGame game { gameBoard = board', currentTurn = otherPlayer player } 


{-- |Displays the winner of the game and prompts for what to do next. -}
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


{-- |Last thing to be called on main function -}
quitGame :: IO ()
quitGame = ANSI.showCursor


{-- |Draws a board at the topmost, leftmost corner of the console. -}
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


{-- |Redraws the line of a board that caused a player to win -}
drawWinningLine :: Board -> [(Int, Int)] -> IO ()
drawWinningLine board@Board{boardSize = (nCols, nRows), boardColumns = columns} line = do
  forM_ line $ \(c, r) -> do
    let color = colorForPlayer . fromJust . getPlayerAt board $ (c, r)
    ANSI.setCursorPosition (nRows - r) (c + 1)
    ANSI.setSGR [ANSI.SetPaletteColor ANSI.Foreground color]
    putChar charWin
    ANSI.setSGR [ANSI.Reset]
  hFlush stdout


{-- |Given a list of options and their descriptions displays an interactive menu
     for the user to pick an option by using the left/right arrows -}
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


{-- |Interactive picking of a column by using the left/right arrows -}
columnSelector :: Player -> Board -> IO Int
columnSelector player board = do
  let (nCols, _) = boardSize board
  ANSI.setCursorPosition 0 0
  i <- selectorController (0, nCols-1) printF (nCols `quot` 2)
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


{-- |Controller for interactive left/right arrow menus.
     Given a range (l, r), a display function for any given index within the range and a starting 
     index, decrements the index on a left arrow press, increments the index on a right arrow press
     and yields the result on an enter.
-}
selectorController :: (Int, Int) -> (Int -> IO ()) -> Int -> IO Int
selectorController (l, r) paintF = go 
  where go i = paintF i >> getKey >>= \case
            Just KeyRight -> if i < r then go (i+1) else go i
            Just KeyLeft  -> if l < i then go (i-1) else go i
            Just KeyEnter -> return i
            Nothing       -> go i
            
      --getKey :: IO (Maybe Key)
        -- Helper for handling a keypress from stdin.
        getKey = do
          hSetEcho stdout False
          hSetBuffering stdin NoBuffering
          getChar >>= \case
            '\ESC' -> parseArrowKey
            '\n'   -> return $ Just KeyEnter
            _  -> getKey
      --parseArrowKey :: IO (Maybe Key)
        -- Helper for parsing an ArrowKey from stdin.
        parseArrowKey =
          getChar >>= \case
            '\ESC' -> parseArrowKey
            '['    -> getChar >>= \case
                  'C' -> return $ Just KeyRight
                  'D' -> return $ Just KeyLeft
                  _   -> return Nothing
            _   -> return Nothing