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

data FoodItemState = FoodItemState { positionF  :: Board.Position,
                                     x :: Int
                                   } deriving Show

data SnakeItemState = SnakeItemState { snakes :: [ItemState],
                                      n :: Int
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

data State = State { snakeState   :: SnakeItemState
                   , foodState  :: FoodItemState
                   , mode         :: Mode
                   , windowSize   :: (Int, Int)
                   , contentScale :: Float
                   } deriving Show


initialState :: State
initialState = State { snakeState   = initialSnakeState
                     , foodState  = initialFoodState
                     , mode         = ModeSplash
                     , windowSize   = Config.windowSize
                     , contentScale = 1
                     }


-- | Respond to key events.
handleEvent :: Event -> State -> State

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state  = state { mode = ModeLeft }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state  = state { mode = ModeDown }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state = state { mode = ModeRight }
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state    = state { mode = ModeUp }
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state = state { mode = ModeStart }

handleEvent (EventKey (Char '2') Down _ _) state = state { contentScale = 2.0 }
handleEvent (EventKey (Char '1') Down _ _) state = state { contentScale = 1.0 }

handleEvent (EventResize size) state = state { windowSize = size }

handleEvent _ state = state

itemsCollide :: Game.State -> SnakeItemState -> FoodItemState -> Bool
itemsCollide state item1 item2 = let (x1, y1) = position $ (snakes item1) !! 0
                                     (x2, y2) = positionF $ item2
                                     distance = sqrt ( (x1 - x2) ** 2 + (y1 - y2) ** 2 )
                                 in  distance < 0.5


update :: Float      -- ^ Broj sekundi od prethodnog azuriranja
       -> Game.State  -- ^ Predhodno stanje
       -> Game.State  -- ^ Novo stanje
update seconds oldState =
       let newState = snakeUpdate seconds oldState
       in if itemsCollide newState (snakeState newState) (foodState newState)
            then newState { Game.foodState = foodUpdate oldState,
                            Game.snakeState = growSnake (snakeState newState)
                          }
            else newState



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
                in if isItemPositionValid desiredPosition
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
                   in FoodItemState { positionF = (fromIntegral (xs !! 0) :: Float, fromIntegral (ys !! 0) :: Float) ,
                                      x = 1
                                    }

foodUpdate :: Game.State -> FoodItemState
foodUpdate oldWorld =
            let xs = randomList (Config.boardWidth -2) 100
                ys = randomList (Config.boardHeight -2) 100
            in FoodItemState {
                      positionF = (fromIntegral (xs !! ((x . foodState) oldWorld)) :: Float, fromIntegral (ys !! ((x . foodState) oldWorld)) :: Float)
                    , x = ((x . foodState) oldWorld) + 1
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
