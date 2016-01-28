module Art where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Constants exposing (..)
import Rectangle exposing (..)
import Color exposing (..)

--todo move
import Vector exposing (..)
type alias Position = Vector 
type alias Entity a = {a | position: Position, velocity: Vector}
type alias Player = Entity {}
type alias Obstacle = Rectangle
type alias Monster = Entity {size: MonsterSize, direction: MonsterDirection} 
type alias Monsters = List Monster
type alias Model = {player:Player, monsters: Monsters}
type MonsterSize = Medium | Small | Dead
type MonsterDirection = L | R

monsterWidth = 30

monsterHeight : MonsterSize -> Float
monsterHeight height = case height of 
    Medium -> 30
    Small -> 15
    Dead -> 0


playerColor = rgb 255 255 255
obstacleColor = playerColor
backgroundColor = rgb 0 0 0

monsterColor size = 
    case size of 
        Dead -> rgb 200 0 0
        _ -> rgb 255 0 0

drawBackground : Form
drawBackground = filled backgroundColor (rect windowWidth windowHeight)

drawPlayer : Form
drawPlayer = move (playerWidth / 2,playerHeight / 2) (outlined (solid playerColor) (rect playerWidth playerHeight))

drawMonster : Monster -> Form
drawMonster {size} = move (monsterWidth / 2, (monsterHeight size) / 2) (outlined (solid (monsterColor size)) (rect monsterWidth (monsterHeight size)))

drawObstacle : Rectangle -> Form
drawObstacle {minX , minY, maxX, maxY} =
    let
        solidBlack = solid obstacleColor
    in traced ({solidBlack| join = Sharp 0}) (path [(minX,minY), (maxX,minY), (maxX, maxY), (minX, maxY), (minX,minY)])