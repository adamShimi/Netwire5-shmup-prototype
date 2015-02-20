module Parser
( parser 
) where

import Data.Set (empty)

import DataStructures
import Wires

parser :: IO(Frame)
parser = do
    content <- readFile "configMinimumViableProduct"
    let usefullContent = ((filter (\x -> take 2 x /= "--")) . lines) content
    let parsedEnnemies = parsingEnnemies usefullContent
    return $ Frame (start_x,start_y) [] parsedEnnemies empty game



parsingEnnemies :: [String] -> [Ennemy]
parsingEnnemies [] = []	
parsingEnnemies configLines@(x:xs) =
    (Ennemy 
        pos 
        dim
        (case (list_of_words !! 2) of
            "bouncing" -> bouncing
            "angle_bouncing" ->
                let angle = read (list_of_words !! 3) in
                angle_bouncing angle
            "boss_bouncing" ->
                let angle = read (list_of_words !! 3) in
                let sens = read (list_of_words !! 4) in
                boss_bouncing angle sens
        )
     )
     :parsingEnnemies xs
    where list_of_words  = ((drop 1) . words) x
          pos = read (list_of_words !! 0)
          dim = read (list_of_words !! 1)
    
