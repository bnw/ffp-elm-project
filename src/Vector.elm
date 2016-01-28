module Vector where

type alias IntVector = {x:Int, y:Int}
type alias Vector = {x:Float, y:Float}

toVector : IntVector -> Vector
toVector {x,y} = {x=toFloat x, y=toFloat y}

addVectors : Vector -> Vector -> Vector
addVectors pos delta = {x=pos.x+delta.x, y=pos.y+delta.y}

addVectorIntVector : Vector -> IntVector -> Vector
addVectorIntVector pos delta = {x=pos.x + (toFloat delta.x), y=pos.y + (toFloat delta.y)}

