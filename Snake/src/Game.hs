{-# LANGUAGE NamedFieldPuns #-}
module Game where

import System.IO.Unsafe
import Data.Time.Clock.POSIX
import System.Random
import qualified Board
import qualified Config
import Debug.Trace

import Graphics.Gloss.Game

data ItemState = ItemState { position  :: Board.Position
                           , speed     :: (Float, Float)
                           } deriving Show

data FoodItemState = FoodItemState { positionF        :: Board.Position
                                     , x              :: Int
                                     , foodPositionsX :: [Int]
                                     , foodPositionsY :: [Int]
                                   } deriving Show

data SnakeItemState = SnakeItemState { snakes :: [ItemState]
                                      , n     :: Int
                                     } deriving Show

data Mode = ModeSplash
          | ModeWon
          | ModeLost
          | ModeStart
          | ModeStill
          | ModeLeft
          | ModeRight
          | ModeUp
          | ModeDown
          deriving (Show, Eq)

data SnakeMode = ModeNormal
              | ModeSpecial
              deriving (Show, Eq)

data State = State { snakeState   :: SnakeItemState
                   , foodState    :: FoodItemState
                   , mode         :: Mode
                   , snakeMode    :: SnakeMode
                   , windowSize   :: (Int, Int)
                   , contentScale :: Float
                   , sec          :: Int
                   } deriving Show


initialState :: State
initialState = State { snakeState   = initialSnakeState
                     , foodState    = initialFoodState
                     , mode         = ModeSplash
                     , snakeMode    = ModeNormal
                     , windowSize   = Config.windowSize
                     , contentScale = 1
                     , sec          = 0
                     }


-- | Respond to key events.
handleEvent :: Event -> State -> State

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state  = if (mode state) == ModeRight then state else state { mode = ModeLeft }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state  = if (mode state) == ModeUp then state else state { mode = ModeDown }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state = if (mode state) == ModeLeft then state else state { mode = ModeRight }
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state    = if (mode state) == ModeDown then state else state { mode = ModeUp }
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state = if (mode state) == ModeLost || (mode state) == ModeSplash || (mode state) == ModeWon
                                                              then state { mode = ModeStart
                                                                          , snakeState = initialSnakeState
                                                                          , foodState  = initialFoodState
                                                                          }
                                                              else state

handleEvent (EventKey (Char '2') Down _ _) state = state { contentScale = 2.0 }
handleEvent (EventKey (Char '1') Down _ _) state = state { contentScale = 1.0 }
handleEvent (EventResize size) state = state { windowSize = size }

handleEvent _ state = state



collide :: Board.Position -> Board.Position -> Bool
collide item1 item2 = let (x1, y1) = item1
                          (x2, y2) = item2
                          distance = sqrt ( (x1 - x2) ** 2 + (y1 - y2) ** 2 )
                      in  distance < 0.5


itemsCollide :: SnakeItemState -> FoodItemState -> Bool
itemsCollide snakeState foodState = let snakePosition = position $ (snakes snakeState) !! 0
                                        foodPosition  = positionF $ foodState
                                    in collide snakePosition foodPosition


snakesCollide :: SnakeItemState -> Bool
snakesCollide snakeState = let headPosition  = position $ (snakes snakeState) !! 0
                               tailPositions = map position $ drop 4 $ snakes snakeState
                               res           = map (collide headPosition) tailPositions
                           in or res


update :: Float      -- ^ Broj sekundi od prethodnog azuriranja
       -> Game.State  -- ^ Predhodno stanje
       -> Game.State  -- ^ Novo stanje
update seconds oldState =
  if (mode oldState) == ModeWon then oldState else
       let newState = snakeUpdate seconds oldState
           n_snakes = n $ snakeState $ newState
           snakeSec = sec $ oldState
           isModeSpecial = (snakeMode oldState) == ModeSpecial
           newSnakeMode = if isModeSpecial && snakeSec < 100 then ModeSpecial else ModeNormal
           newSnakeSec  = if newSnakeMode == ModeSpecial then (snakeSec + 1) else 0
       in if n_snakes > 4 && snakesCollide (snakeState newState)
              then newState {
                        mode = ModeLost
                    }
          else if itemsCollide (snakeState newState) (foodState newState)
              then newState { Game.foodState  = foodUpdate oldState
                              , Game.snakeState = growSnake (snakeState newState)
                              , Game.snakeMode =  if (mod (x (foodState oldState)) 6) == 0 then ModeSpecial else newSnakeMode
                              , Game.mode = if (n (snakeState oldState)) > 97 then ModeWon else (mode oldState)
                              , Game.sec = newSnakeSec
                            }
          else newState {
              Game.sec = newSnakeSec
            , Game.snakeMode = newSnakeMode
          }



isItemPositionValid position =
        let check cnr = Board.Hall == (Board.field $ Board.movedBy cnr position)
            offset    = 0.49 -- eps
        in  all check [ (offset, 0), (-offset, 0), (0, offset), (0, -offset) ]


data UpdateFunctions = UpdateFunctions { successMove  :: ItemState -> Board.Position
                                       , successSpeed :: ItemState -> (Float, Float)
                                       , failureMove  :: ItemState -> Board.Position
                                       , failureSpeed :: ItemState -> (Float, Float)
                                       }

generalItemMove :: --(Game.State -> ItemState)
                UpdateFunctions
                -> Float
                -> Game.State
                -> Game.State
generalItemMove UpdateFunctions { successMove, successSpeed, failureMove, failureSpeed } seconds oldWorld =
                let oldSnakeState   = snakeState oldWorld
                    oldSnake        = snakes oldSnakeState
                    oldItem         = oldSnake !! 0
                    oldPosition     = position oldItem
                    oldSpeed        = speed oldItem
                    desiredPosition = successMove oldItem
                    (xPos, yPos)    =  oldPosition
                in if (snakeMode oldWorld) == ModeSpecial && not (isItemPositionValid desiredPosition)
                  then
                    oldWorld {
                    snakeState = SnakeItemState {
                          snakes = [ItemState { position =  case mode oldWorld of
                                                              ModeLeft  -> (fromIntegral(Config.boardWidth - 2) :: Float, yPos)
                                                              ModeRight -> (1.0, yPos)
                                                              ModeUp    -> (xPos, 1.0)
                                                              ModeDown  -> (xPos, fromIntegral(Config.boardHeight - 2) :: Float)
                                                              _        -> desiredPosition,
                                                speed    = successSpeed oldItem}]
                                    ++ (init oldSnake),
                          n = n oldSnakeState
                    }
                  }
                  else if isItemPositionValid desiredPosition
                    then oldWorld {
                            snakeState = SnakeItemState {
                                  snakes = [ItemState { position = desiredPosition,
                                                        speed    = successSpeed oldItem}]
                                            ++ (init oldSnake),
                                  n = n oldSnakeState
                            }
                        }
                    else oldWorld {
                            mode = ModeLost
                        }



randomList :: Int -> Int -> [Int]
randomList x n = take n $ randomRs (1, x) (mkStdGen (round (unsafePerformIO getPOSIXTime) :: Int))



-- Food

initialFoodState = let xs = randomList (Config.boardWidth -2) 100
                       ys = randomList (Config.boardHeight -2) 100
                   in FoodItemState { positionF = (fromIntegral (xs !! 0) :: Float, fromIntegral (ys !! 0) :: Float)
                                      , x = 1
                                      , foodPositionsX = xs
                                      , foodPositionsY = ys
                                    }

foodUpdate :: Game.State -> FoodItemState
foodUpdate oldWorld =
            let xs = foodPositionsX $ foodState $ oldWorld
                ys = foodPositionsY $ foodState $ oldWorld
                xn = ((x . foodState) oldWorld) + 1
            in  FoodItemState {
                      positionF = (fromIntegral (xs !! ((x . foodState) oldWorld)) :: Float, fromIntegral (ys !! ((x . foodState) oldWorld)) :: Float)
                      , x = xn
                      , foodPositionsX = xs
                      , foodPositionsY = ys
                    }


-- Snake

initialSnake = ItemState { position = Board.initialSnakePosition
                          , speed    = (0, -0.3)
                         }

initialSnakeState = SnakeItemState { snakes = [initialSnake]
                                    , n = 0
                                   }


growSnake :: SnakeItemState -> SnakeItemState
growSnake oldSnakeState =
      let newItem = ItemState { position = position $ snakes oldSnakeState !! (n oldSnakeState),
                                speed    = (0, -0.3)
                              }

      in SnakeItemState { snakes = (snakes oldSnakeState) ++ [newItem],
                          n = 1 + (n oldSnakeState)}



snakeUpdate :: Float -> Game.State -> Game.State
snakeUpdate seconds oldWorld =
               let newSpeed = case mode oldWorld of
                                   ModeLeft  -> (-0.3, 0)
                                   ModeRight -> (0.3, 0)
                                   ModeUp    -> (0, 0.3)
                                   ModeDown  -> (0, -0.3)
                                   _        -> (0, 0)
                   functions = UpdateFunctions { successMove  = \ item -> speed item + position item
                                               , successSpeed = \ item -> newSpeed
                                               , failureMove  = \ item -> position item
                                               , failureSpeed = \ item -> newSpeed
                                               }


                  in generalItemMove functions seconds oldWorld
