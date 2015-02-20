{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
import Prelude hiding ((.), id, null, filter, until)
import Control.Monad (void)
import Control.Wire hiding (empty)
import Control.Wire.Unsafe.Event as Event
import FRP.Netwire hiding (empty)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Data.List as List
import qualified Graphics.UI.SDL as SDL

import Parser
import DataStructures
import GestionEvents
import Wires
import Display


main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode window_width window_height 32 [SDL.SWSurface]
    etat_init <- parser
    game_loop screen etat_init clockSession_


game_loop :: SDL.Surface -> Frame -> Session IO (Timed NominalDiffTime ()) -> IO()
game_loop screen game_state session= do
    new_pressed_keys <- parseEvents (pressed_keys game_state)
    if (keyDown SDL.SDLK_ESCAPE new_pressed_keys)
    then
        SDL.quit
    else do
        (ds, s') <- stepSession session
        let game_state_with_new_keys = Frame (player_pos game_state) (bullets game_state) (ennemies game_state) new_pressed_keys (game_wire game_state)
        (resultat, game_wire') <- stepWire (game_wire game_state) ds (Right game_state_with_new_keys)
        let (new_player_pos, new_bullets, new_ennemies, _) = either (const (player_pos game_state, bullets game_state, ennemies game_state, pressed_keys game_state)) id resultat
        let new_game_state = Frame new_player_pos new_bullets new_ennemies new_pressed_keys game_wire'
        display screen new_game_state
        game_loop screen new_game_state s'
