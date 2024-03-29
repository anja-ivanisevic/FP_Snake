module Pictures where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Game
import Graphics.Gloss

wall      = png "data/wall.png"
emptyWall = png "data/lcon-001.png"
hall      = png "data/hall.png"

background = png "data/labyrinth-background.png"
splash     = png "data/snake_start.png"
lost       = png "data/labyrinth-lost.png"
won        = png "data/labyrinth-won.png"

itemPicture what = pictures [ color white $ rectangleWire 32 32, png ("data/" ++ what ++ "-001.png") ]

snake  = itemPicture "king"
food   = itemPicture "baby"
