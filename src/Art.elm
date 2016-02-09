module Art where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Constants exposing (..)
import Rectangle exposing (..)
import Color exposing (..)
import Time exposing (..)
import TypeDefs exposing (..)
import Text 

monsterWidth = 30

monsterHeight : MonsterSize -> Float
monsterHeight height = case height of 
    Medium -> 30
    Small -> 15
    Dead -> 0


textColor = rgb 255 255 255
playerColor = rgb 210 210 210
obstacleColor = playerColor
backgroundColor = rgb 0 0 0

monsterColor size = 
    case size of 
        Dead -> rgb 200 0 0
        _ -> rgb 255 0 0

drawBackground : Form
drawBackground = filled backgroundColor (rect windowWidth windowHeight)

drawPlayer : Form
drawPlayer = move (playerWidth / 2,playerHeight / 2) (filled playerColor (rect playerWidth playerHeight))

drawMonster : Monster -> Form
drawMonster {size} = move (monsterWidth / 2, (monsterHeight size) / 2) (outlined (solid (monsterColor size)) (rect monsterWidth (monsterHeight size)))

drawObstacle : Rectangle -> Form
drawObstacle {minX , minY, maxX, maxY} =
    let
        solidBlack = solid obstacleColor
    in traced ({solidBlack| join = Sharp 0}) (path [(minX,minY), (maxX,minY), (maxX, maxY), (minX, maxY), (minX,minY)])
    
drawGameOver : State -> List Form
drawGameOver state =
    case state of
        GameOver -> [text (Text.color textColor (Text.fromString "GAME OVER!"))]
        Splash ->  [move (0, 150) (text (Text.color textColor (Text.fromString "Move with arrows.")))
                   ,move (0, 120) (text (Text.color textColor (Text.fromString "Objective: Stop all the red squares! Jump on them to stop them.")))
                   ,move (0, 90) (text (Text.color textColor (Text.fromString "Start the game by pressing any arrow key.")))]
        _ -> []