{-# LANGUAGE StandaloneDeriving #-}
module GestionEvents
( space
, parseEvents
, keyDown
, firing
, rebooting
) where

import Prelude hiding ((.), id, null, filter, until)
import Control.Wire hiding (empty)
import Control.Wire.Unsafe.Event as Event
import FRP.Netwire hiding (empty)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Data.List as List
import qualified Graphics.UI.SDL as SDL

import DataStructures

----------------------------------------SDL Events--------------------------------------------



space :: SDL.Keysym
space = SDL.Keysym SDL.SDLK_s [SDL.KeyModNone] ' '


--Code from ocharles
parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
    event <- SDL.pollEvent
    case event of
        SDL.NoEvent -> return keysDown
        SDL.KeyDown k -> parseEvents (insert k keysDown)
        SDL.KeyUp k -> parseEvents (delete k keysDown)
        _ -> parseEvents keysDown

--Code from ocharles
keyDown :: SDL.SDLKey -> Set SDL.Keysym -> Bool
keyDown k = not . null . filter ((== k) . SDL.symKey)


------------------------------------Netwire 5 Events------------------------------------------



instance (Eq a) => Eq (Event a) where
    (Event a) == (Event b) = a == b
    (Event a) == Event.NoEvent = False
    Event.NoEvent == (Event a) = False
    Event.NoEvent == Event.NoEvent = True


firing :: Event SDL.Keysym
firing = Event (space)

rebooting :: Event String
rebooting = Event ("reboot!")

deriving instance Ord SDL.Keysym

