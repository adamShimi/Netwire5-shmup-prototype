module DataStructures
( Position
, Frame(..)
, Bullet(..)
, Ennemy(..)
, window_height
, window_width
, side
, start_x
, start_y
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


window_height :: Int
window_height = 800

window_width :: Int
window_width = 600

side :: Int
side = 50


start_x :: Double
start_x = fromIntegral $ window_width `div` 2

start_y :: Double
start_y = fromIntegral $ window_height - 1 - side


type Position = (Double,Double)

data Frame = Frame { player_pos :: Position
                   , bullets :: [Bullet]
                   , ennemies :: [Ennemy]
                   , pressed_keys :: Set SDL.Keysym
                   , game_wire :: Wire (Timed NominalDiffTime ()) () IO Frame (Position, [Bullet], [Ennemy], Set SDL.Keysym, Event(String))
                   }

data Bullet = Bullet { bullet_pos :: Position
                     , transition :: Wire (Timed NominalDiffTime ()) () IO () Position
                     }

instance Eq Bullet where
    Bullet a _ == Bullet b _ = a == b


data Ennemy = Ennemy { ennemy_pos :: Position
                     , ennemy_dim :: Position
                     , bounce :: Wire (Timed NominalDiffTime ()) () IO (Position,(Wire (Timed NominalDiffTime ()) () IO () Position)) (Wire (Timed NominalDiffTime ()) () IO () Position)
                     }

instance Eq Ennemy where
    Ennemy a _ _ == Ennemy b _ _ = a == b
