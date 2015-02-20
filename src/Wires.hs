{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
module Wires
( game
, bouncing
, angle_bouncing
, boss_bouncing
, fire
, bullets_mov
, player_mov
) where

import Prelude hiding ((.), id, null, filter, until)
import Control.Wire hiding (empty)
import Control.Wire.Unsafe.Event as Event
import FRP.Netwire hiding (empty)
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Data.List as List
import qualified Graphics.UI.SDL as SDL


import DataStructures
import GestionEvents



------------------------------------------Main Wire-------------------------------------------



game :: 
    Wire 
     (Timed NominalDiffTime ()) 
     () 
     IO 
     Frame 
     (Position, [Bullet], [Ennemy], Set SDL.Keysym)
game = proc game_state -> do
    new_player_pos <- player_mov -< (pressed_keys game_state, player_pos game_state)
    new_bullet <- fire -< (pressed_keys game_state,player_pos game_state)
    (reflected_bullets,new_ennemies) <- collide -< (new_bullet ++ (bullets game_state), ennemies game_state)
    new_bullets <- bullets_mov -< reflected_bullets
    returnA -< (new_player_pos, new_bullets, List.union new_ennemies (ennemies game_state), pressed_keys game_state)


fire :: 
    Wire 
     (Timed NominalDiffTime ()) 
     () 
     IO 
     ((Set SDL.Keysym), (Double, Double)) 
     [Bullet]
fire =
    let is_firing = proc (keyPressed, (x,y)) -> do
            is_shooting -< keyPressed
            returnA -< 
                [
                Bullet 
                (x + (fromIntegral side)/2 - 2,y) 
                ((integral (x + (fromIntegral side)/2 - 2) . 0.0)&&&(integral y . (-700.0)))
                ]
    in
    is_firing <|> pure []


is_shooting :: 
    Wire 
     (Timed NominalDiffTime ()) 
     () 
     IO 
     (Set SDL.Keysym) 
     (Event (SDL.Keysym))
is_shooting =
    when (keyDown SDL.SDLK_s) >>> ((when (/= Event.NoEvent) . once . (WConst (Right firing)))--> after 0.1 >>> is_shooting)




---------------------------------------Collision gestion--------------------------------------



collide :: 
    Wire 
     (Timed NominalDiffTime ()) 
     () 
     IO 
     ([Bullet],[Ennemy]) 
     ([Bullet],[Ennemy])
collide = WGen $ \ds (Right (!bul,!en)) -> do
    let (colliding, not_colliding) = List.partition (\(Bullet (x,y) _ , Ennemy (s,q) (w,h) _) -> (x+4 >= s)&&(x <= (s+w))&&(y+4 >= q)&&(y <= (q+h))) [(a,b) | a <- bul, b <- en]
    after_collision_either <- 
        mapM 
            (\(b,e) -> stepWire (bounce e) ds (Right (bullet_pos b,transition b))) 
            colliding
    return $ 
        (
        Right 
            (
                List.union (map (fst) $ after_collision colliding after_collision_either) (List.nub $ map (fst) not_colliding)
            ,
                (map (snd) $ after_collision colliding after_collision_either)
            )
        ,
        collide
        )



after_collision :: 
    [(Bullet,Ennemy)] -> 
    [( 
     Either () (Wire (Timed NominalDiffTime ()) () IO () Position)
     , 
     Wire 
      (Timed NominalDiffTime ()) 
      () 
      IO 
      (Position,(Wire (Timed NominalDiffTime ()) () IO () Position)) 
      (Wire (Timed NominalDiffTime ()) () IO () Position)
    )] -> 
    [(Bullet,Ennemy)]
after_collision = 
    zipWith 
        (\(b,e) (result_wire, stepped_wire) -> (Bullet (bullet_pos b) (either (const (WConst (Right (0.0,0.0)))) id result_wire), Ennemy (ennemy_pos e) (ennemy_dim e) stepped_wire))




--------------------------------------Different rebounds--------------------------------------



-- Classic straight rebound
bouncing :: 
    Wire 
     (Timed NominalDiffTime ()) 
     () 
     IO 
     (Position,(Wire (Timed NominalDiffTime ()) () IO () Position)) 
     (Wire (Timed NominalDiffTime ()) () IO () Position)
bouncing = 
    WGen $ 
        \ds (Right ((x,y),!w)) -> do
        return $ 
             (
             Right 
                 (
                 (integral x . 0.0)
                 &&&
                 (integral ((y - 700.0*(realToFrac(dtime ds)))+31.0) . 700.0)
                 )
             ,
             bouncing
             )


-- Rebound with an angle, given as a parameter
angle_bouncing :: 
    Double -> 
    Wire 
     (Timed NominalDiffTime ()) 
     () 
     IO 
     (Position,(Wire (Timed NominalDiffTime ()) () IO () Position)) 
     (Wire (Timed NominalDiffTime ()) () IO () Position)
angle_bouncing angle = WGen $ \ds (Right ((x,y),!w)) -> do
    return $ 
        (
        Right 
            (
            (integral x . ((pure $ cos (angle))*700.0))
            &&&
            (integral ((y - 700.0*(realToFrac(dtime ds)))+31.0) . ((pure $ sin (angle))*700.0))
            )
        ,
        angle_bouncing angle
        )



--Funny cyclical rebound, like a boss
boss_bouncing :: 
    Double -> 
    Double -> 
    Wire 
     (Timed NominalDiffTime ()) 
     () 
     IO 
     (Position,(Wire (Timed NominalDiffTime ()) () IO () Position)) 
     (Wire (Timed NominalDiffTime ()) () IO () Position)
boss_bouncing angle sens = WGen $ \ds (Right ((x,y),!w)) -> do
    if (angle + sens*0.1 >= pi) || (angle + sens*0.1 <= 0.0)
    then
        return $ 
            (
            Right 
                (
                (integral x . ((pure $ cos (angle))*700.0))
                &&&
                (integral ((y - 700.0*(realToFrac(dtime ds)))+31.0) . ((pure $ sin (angle))*700.0))
                )
            ,
            (boss_bouncing angle ((-1)*sens))
            )
    else
        return $ 
            (
            Right 
                (
                (integral x . ((pure $ cos (angle))*700.0))
                &&&
                (integral ((y - 700.0*(realToFrac(dtime ds)))+31.0) . ((pure $ sin (angle))*700.0))
                )
            ,
            (boss_bouncing (angle + sens*0.1) sens)
            )



--------------------------------------Bullets movement----------------------------------------



-- Wire stepping all the bullets
bullets_mov :: 
    Wire 
     (Timed NominalDiffTime ()) 
     () 
     IO 
     [Bullet] 
     [Bullet]
bullets_mov = proc pos_boulettes -> do
    new_pos_bullets <- step_many_wires -< pos_boulettes 
    returnA -< 
        (
        List.filter 
            (\(Bullet (x,y) _) -> (y > 0.0)&&(x > 0)&&(x < (fromIntegral window_width))&&(y < (fromIntegral window_height)))
            new_pos_bullets
        )



-- Multiple stepping for the bullets
step_many_wires :: 
    Wire 
     (Timed NominalDiffTime ()) 
     () 
     IO 
     [Bullet] 
     [Bullet]
step_many_wires = WGen $ \ds (Right w) -> do
    after <- mapM (\x -> stepWire (transition x) ds (Right ())) w
    return $ 
        (
        Right [Bullet ((either (const (0.0,0.0)) id) x) y | (x,y) <- after]
        ,
        step_many_wires
        )



---------------------------------------Player movement----------------------------------------



-- Wire corresponding to the overarching player movement, with integration of the speed in the two coordinates

player_mov ::
     Wire 
      (Timed NominalDiffTime ()) 
      () 
      IO 
      (Set SDL.Keysym,Position)
      Position
player_mov = (integral start_x . speed_x) &&& (integral start_y . speed_y)



-- Wire corresponding to the speed in the x coordinate

speed_x :: 
    Wire 
     (Timed NominalDiffTime ())
     () 
     IO 
     (Set SDL.Keysym,Position) 
     Double
speed_x = proc (keyPressed, (x, y)) -> do
    diffX <- 
        (pure (-100) . when (keyDown SDL.SDLK_LEFT) <|> pure 0) 
        &&& 
        (pure 100 . when (keyDown SDL.SDLK_RIGHT) <|> pure 0) 
        -< keyPressed
    finalX <- 
        pure (0) . when ((>) 0.0) 
        <|> pure (0) . when ((<) (fromIntegral (window_width - side))) 
        <|> pure 1 
        -< x + 2*(fst diffX + snd diffX)/100
    returnA -< finalX*(fst diffX + snd diffX)



-- Wire corresponding to the speed in the y coordinate

speed_y :: 
    Wire 
     (Timed NominalDiffTime ()) 
     () 
     IO 
     (Set SDL.Keysym,Position) 
     Double
speed_y = proc (keyPressed, (x, y)) -> do
    diffY <- 
        (pure (-100) . when (keyDown SDL.SDLK_UP) <|> pure 0) 
        &&& 
        (pure 100 . when (keyDown SDL.SDLK_DOWN) <|> pure 0) 
        -< keyPressed
    finalY <- 
        pure (0) . when ((>) 0.0) 
        <|> pure (0) . when ((<) (fromIntegral (window_height - side))) 
        <|> pure 1 
        -< y + 2*(fst diffY + snd diffY)/100
    returnA -< finalY*(fst diffY + snd diffY)

