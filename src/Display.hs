{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
module Display
( display
,  displaySurface
) where

import Prelude hiding ((.), id, null, filter, until)
import Control.Monad (void)
import Control.Wire hiding (empty)
import Control.Wire.Unsafe.Event as Event
import FRP.Netwire hiding (empty)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Data.List as List
import qualified Graphics.UI.SDL as SDL

import DataStructures

display :: SDL.Surface -> Frame -> IO ()
display screen game_state = do
    let (x,y) = player_pos game_state
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>= SDL.fillRect screen Nothing
    displaySurface screen (SDL.Color 0 50 200) (fromIntegral side,fromIntegral side) (player_pos game_state)
    mapM_
        (\(Bullet pos _ ) -> displaySurface screen (SDL.Color 255 0 0) (4,4) pos)
        (bullets game_state)
    mapM_
        (\(Ennemy pos dim _) -> displaySurface screen (SDL.Color 0 255 0) dim pos)
        (ennemies game_state)
    SDL.flip screen
    SDL.delay (1000 `div` 60)



displaySurface :: SDL.Surface -> SDL.Color -> Position -> Position -> IO (Bool)
displaySurface screen (SDL.Color red green blue) (w,h) (x,y) = do
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen red green blue >>=
        SDL.fillRect screen (Just $ SDL.Rect (round x) (round y) (round w) (round h))

