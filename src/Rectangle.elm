module Rectangle where

import Constants exposing (..)
import Vector exposing (..)

type alias Rectangle = {minX:Float , minY: Float, maxX:Float, maxY:Float}

type Bump = Left | Right | Up | Down

createTile: Float -> Float -> Float -> Float -> Rectangle
createTile minX minY maxX maxY = createRectangle (minX * tileWidth) (minY * tileHeight) (maxX * tileWidth) (maxY * tileHeight)

createRectangle : Float -> Float -> Float -> Float -> Rectangle
createRectangle minX minY maxX maxY = {minX=minX, minY=minY, maxX=maxX, maxY=maxY}

intersect : Rectangle -> Rectangle -> Bool
intersect {minX , minY, maxX, maxY} other = 
    not (maxX <= other.minX || minX >= other.maxX || maxY <= other.minY || minY >= other.maxY)

bump: (Rectangle,Vector) -> Rectangle -> Maybe Bump
bump ({minX, minY, maxX, maxY}, velocity) other = 
    let     
        deltaX = velocity.x
        deltaY = velocity.y
        movedRectangle = createRectangle (minX + deltaX) (minY + deltaY) (maxX + deltaX) (maxY + deltaY)
        x = Debug.watch "minX, minY, maxX, maxY" (minX, minY, maxX, maxY)
        y = Debug.watch "other: minX, minY, maxX, maxY" (other.minX, other.minY, other.maxX, other.maxY)
    in
        if not (intersect movedRectangle other) then
            Nothing
        else if maxX <= other.minX && maxX + deltaX >= other.minX then Just Right
        else if maxY <= other.minY && maxY + deltaY >= other.minY then Just Up
        else if minX >= other.maxX && minX + deltaX <= other.maxX then Just Left
        else if minY >= other.maxY && minY + deltaY <= other.maxY then Just Down
        else Nothing
