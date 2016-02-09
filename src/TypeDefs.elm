module TypeDefs where

import Rectangle exposing (..)

import Vector exposing (..)
type alias Position = Vector 
type alias Entity a = {a | position: Position, velocity: Vector}
type alias Player = Entity {lastTickNotJumping : Int}
type alias Obstacle = Rectangle
type alias Monster = Entity {size: MonsterSize, direction: MonsterDirection} 
type alias Monsters = List Monster
type State = Splash | InGame | GameOver
type alias Model = {player:Player, monsters: Monsters, tick: Int, state: State}
type MonsterSize = Medium | Small | Dead
type MonsterDirection = L | R