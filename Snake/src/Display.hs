module Display ( showAt
               , splash
               , background
               , won
               , lost
               , board
               , snake
               , food
               ) where

import qualified Board
import qualified Game
import Config
import qualified Pictures as P

import Graphics.Gloss
import Graphics.Gloss.Game

showAt :: Picture -> Board.Position -> Picture
showAt picture (x, y) = translate (blockSize * x + boardOffsetHorizontal) (blockSize * y + boardOffsetVertical) picture

board :: Game.State -> Picture
board state=
  let pic = if (Game.snakeMode state) == Game.ModeSpecial then P.emptyWall else P.wall
  in  showAt (Board.picture pic) (0, 0) 

fullImage :: Picture -> (Int, Int) -> Picture
fullImage picture windowSize =
           let (_, (picWidth, picHeight)) = boundingBox picture
               (winWidth, winHeight)      = (fromIntegral $ fst windowSize, fromIntegral $ snd windowSize)
               horizontalScale            = winWidth / picWidth
               verticalScale              = winHeight / picHeight
               scaleFactor                = max horizontalScale verticalScale
           in scale scaleFactor scaleFactor $ picture

background windowSize = fullImage P.background windowSize
splash windowSize     = fullImage P.splash windowSize
won windowSize        = fullImage P.won windowSize
lost windowSize       = fullImage P.lost windowSize

food state = showAt (P.food) $ Game.positionF state
snake state = map (showAt (P.snake)) $ map Game.position $ Game.snakes $ Game.snakeState state
