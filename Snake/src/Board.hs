module Board ( Field (Hall, Wall)
             , Position
             , picture
             , initialSnakePosition
             , movedBy
             , field
             ) where

import qualified Config
import qualified Pictures

import Graphics.Gloss

import Data.Maybe
import Data.List

data Field = Hall | Wall deriving (Show, Eq)
type Position = (Float, Float)

-- Display functions for the board

fieldForChar :: Char -> Field
fieldForChar '#' = Wall
fieldForChar  _  = Hall

boardFields = [ map fieldForChar row
                | row <- Config.boardData ]

fieldPicture pic Hall  = Pictures.hall
fieldPicture pic Wall = pic

rowPicture ::  Picture -> [Field] -> Picture
rowPicture pic fields =
    let combine = \ current previous -> pictures [
                translate Config.blockSize 0 previous,
                current ]
    in  foldr1 combine $ map (fieldPicture pic) fields

picture :: Picture -> Picture
picture pic = let combine = \ current previous -> pictures [ translate 0 Config.blockSize previous, current ]
          in  foldl1 (flip combine) $ map (rowPicture pic) boardFields

-- Positioning functions

initialPosition :: Char -> Position
initialPosition c = let rowIndex = fromMaybe 0 $ findIndex (elem c) Config.boardData
                        row      = Config.boardData !! rowIndex
                        colIndex = fromMaybe 0 $ elemIndex c row
                    in (fromIntegral colIndex, fromIntegral $ Config.boardHeight - rowIndex)


initialSnakePosition  = initialPosition 'S'


movedBy :: (Float, Float) -> Position -> Position
movedBy (dx, dy) (x, y) = (x + dx, y + dy)

-- Testing functions

field :: Position -> Field
field (x, y) = fieldForChar $ Config.boardData !! (Config.boardHeight - round y - 1) !! (round x)
