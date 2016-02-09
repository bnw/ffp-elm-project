import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Keyboard
import Array
import Rectangle exposing (..)
import Vector exposing (..)
import Constants exposing (..)
import Art exposing (..)

type alias MasterSignalType = (Time, IntVector)

earth : List Rectangle
earth = [createTile -12 -10 12 -9
        ,createTile -13 -10 -12 10
        ,createTile 12 -10 13 10
        
        ,createTile 4 -6 12 -5
        ,createTile 4 -5 5 -3
        ,createTile 0 -7 2 -6
        ,createTile -8 -7 -2 -4
        ,createTile -8 -4 -7 -3
        ,createTile -4 -1 -3 0
        ,createTile -12 2 -5 3
        ,createTile -7 5 -5 6
        ,createTile 3 5 4 6
        ,createTile 7 4 11 5
        ,createTile 6 1 12 2]

additionalMonsterBlockages : List Rectangle
additionalMonsterBlockages = [ createTile -2 -4 -1 -3
                              ,createTile -5 3 -4 4
                              ,createTile 5 2 6 3
                              ,createTile 6 5 7 6
                              ,createTile 11 5 12 6]

monsterRectangle : Monster -> Rectangle
monsterRectangle {size,position} = createRectangle position.x position.y (position.x + monsterWidth) (position.y + (monsterHeight size))

playerRectangle : Player -> Rectangle
playerRectangle {position} = createRectangle position.x position.y (position.x+playerWidth) (position.y+playerHeight)

changeVelocity : Entity a -> Vector -> Entity a
changeVelocity old delta = {old | velocity = old.velocity `addVectors` delta} 

applyVelocity : Entity a -> Entity a
applyVelocity old = {old| position = (old.position `addVectors` old.velocity)} 

moveForm : Entity a -> Form -> Form
moveForm {position} form = move (position.x, position.y) form

masterSignal : Signal MasterSignalType
masterSignal = Time.timestamp (Signal.sampleOn (fps 50) Keyboard.arrows)

view : Model -> Element
view {player, monsters} = 
    let
       forms = (drawBackground :: (moveForm player drawPlayer) :: (List.map drawObstacle earth)) 
                ++ (List.map (\monster -> moveForm monster (drawMonster monster)) monsters)
    in collage windowWidth windowHeight forms

createPlayer : Float -> Float -> Float -> Float -> Player
createPlayer x y deltaX deltaY = {position = {x = x, y=y}, velocity = { x= deltaX, y= deltaY}, lastTickNotJumping = 0}

createMonster : Float -> Float -> Float -> Float -> Monster
createMonster x y deltaX deltaY = 
    {position = {x = (x * 40), y = ((y+2) * 40)}, 
     velocity = {x = deltaX, y = deltaY}, 
     size = Medium,
     direction = R}

initialModel : Model
initialModel = {
    tick = 0,
    player = (createPlayer 0 0 0 0 ),
    monsters = [createMonster -4 -4 0 3
               ,createMonster 4 -8 0 0
               ,createMonster 8 -5 0 0
               ,createMonster -8 2 0 0
               ,createMonster 8 0 0 0
               ,createMonster 8 3 0 0]
    }

executePlayerBump : Int -> (Bump, Rectangle) -> Player -> Player
executePlayerBump currentTick (bump,rectangle) player = 
    let 
        newPlayer = if bump == Down 
                    then {player |lastTickNotJumping = currentTick}
                    else player
    in executeBump playerHeight playerWidth (bump,rectangle) newPlayer

executeBump : Float -> Float -> (Bump, Rectangle) -> Entity a -> Entity a
executeBump entityHeight entityWidth (bump, rectangle) entity  = 
    let 
        position = entity.position
        velocity = entity.velocity
    in
        case bump of
            Right   -> {entity| position = {position| x = rectangle.minX - entityWidth}, velocity = {velocity | x=0}}
            Left    -> {entity| position = {position| x = rectangle.maxX},                velocity = {velocity | x=0}}
            Up      -> {entity| position = {position| y = rectangle.minY - entityHeight}, velocity = {velocity | y=0}}
            Down    -> {entity| position = {position| y = rectangle.maxY               }, velocity = {velocity | y=0}}

applyFriction : Player -> Player
applyFriction old = 
    let
        x = old.velocity.x
        coefficient = if (Debug.watch "old.velocity.y " old.velocity.y) /= 0 then 0.1 else 0.48
        deltaX = if x>=0 then max -x -coefficient else min -x coefficient
    in changeVelocity old {y=0, x=deltaX}

applyToPlayer : (Player -> Player) -> Model -> Model
applyToPlayer f ({player} as model) = {model | player = f player}

applyGravity : Entity a -> Entity a
applyGravity old = changeVelocity old {y=-0.63, x=0}

applyUserInput : IntVector -> Int -> Player -> Player
applyUserInput keyboard tick ({lastTickNotJumping, velocity} as player)  = 
    let 
        keyboardVector = toVector keyboard
        x = keyboardVector.x * 1.3
        y = if keyboardVector.y <= 0 then keyboardVector.y
            else max 0 ((6 - 20 * ((toFloat tick) - (toFloat lastTickNotJumping)) / 30)^0.5)
    in changeVelocity player {x = x, y = y}

applyMaximumVelocity : Entity a -> Entity a
applyMaximumVelocity old = 
    let 
        x = if old.velocity.x >= 0 then min old.velocity.x 6 else max old.velocity.x -6
        y = if old.velocity.y >= 0 then min old.velocity.y 100 else max old.velocity.y -10
    in {old | velocity = {x=x, y=y}}

getBumps : List Rectangle -> (Rectangle, Vector) -> List (Bump, Rectangle)
getBumps additionalObstacles movingObject = List.foldl (\rectangle -> \previousBumps -> 
            case bump movingObject rectangle of
                Nothing -> previousBumps
                Just b -> ((b, rectangle) :: previousBumps)
            ) [] (earth ++ additionalObstacles)

applyToMonsters : (Monster -> Monster) -> Model -> Model
applyToMonsters f ({monsters} as model) = {model | monsters = List.map f monsters}  

applyMonsterBumps : Monster -> Monster
applyMonsterBumps monster = 
    let
        bumps = getBumps additionalMonsterBlockages ((monsterRectangle monster), monster.velocity)
    in List.foldl (executeBump (monsterHeight monster.size) monsterWidth) monster bumps

applyMonsterMinimumVelocity : Monster -> Monster
applyMonsterMinimumVelocity monster = 
    let 
        velocity = monster.velocity
        d1=Debug.watch "monster.velocity " (velocity.x, velocity.y)
        speed = monsterSpeed monster.size
    in
        if velocity.x == 0 then
            case monster.direction of 
                L -> {monster | direction = R, velocity = {velocity| x = speed}}
                R -> {monster | direction = L, velocity = {velocity| x = -speed}}
        else monster

applyMonsterHit : Player -> Monster ->  Monster
applyMonsterHit player monster = 
        if playerHitsMonster player monster then 
            {monster | velocity = {x=0,y=0}, size = decreaseMonsterSize monster.size}
        else 
            monster

playerHitsMonster : Player -> Monster -> Bool
playerHitsMonster player monster = 
    let 
        mRect = monsterRectangle monster
        pRect = playerRectangle player
    in pRect.minY == mRect.maxY && pRect.maxX >= mRect.minX && pRect.minX <= mRect.maxX && not (monster.size == Dead)

playerHitsMonsters : List Monster -> Player -> Bool
playerHitsMonsters monsters player = List.foldl (||) False (List.map (playerHitsMonster player) monsters)

decreaseMonsterSize : MonsterSize -> MonsterSize
decreaseMonsterSize size = case size of 
    Medium -> Small
    Small -> Dead
    Dead -> Dead

monsterSpeed : MonsterSize -> Float
monsterSpeed size = case size of 
    Medium -> 4.5
    Small -> 5.5
    Dead -> 0

playerObstacles : Model -> List Rectangle
playerObstacles {monsters} = earth ++ (List.map monsterRectangle (List.filter (\monster -> monster.size /= Dead) monsters))

applyBumps: List Rectangle -> (Entity a -> Rectangle) -> ((Bump, Rectangle) -> Entity a -> Entity a) -> Entity a -> Entity a
applyBumps rectangles getEntityRectangle executeBump ({velocity} as entity) = 
    let
        bumps = getBumps rectangles (getEntityRectangle entity, velocity)
    in List.foldl executeBump entity bumps

applyPlayerHitsMonster : Model -> Model
applyPlayerHitsMonster ({monsters, player} as model) = 
    let 
        newMonsters = List.map (applyMonsterHit player) monsters
        newPlayer = 
            if playerHitsMonsters monsters player then
                {player | velocity = {x=player.velocity.x, y=3}}
            else player
    in {model | player = newPlayer, monsters = newMonsters}

updateModel : MasterSignalType -> Model -> Model
updateModel (time, keyboard) oldModel = 
    let 
        currentTick = oldModel.tick + 1
        mutators = [
                    applyToMonsters applyGravity
                   ,applyToMonsters applyMonsterBumps
                   ,applyToMonsters applyMonsterMinimumVelocity
                   ,applyToMonsters applyVelocity
                   ,applyToPlayer applyFriction
                   ,applyToPlayer applyGravity
                   ,applyToPlayer (applyUserInput keyboard currentTick)
                   ,applyToPlayer applyMaximumVelocity
                   ,(\model -> applyToPlayer (applyBumps (playerObstacles model) playerRectangle (executePlayerBump currentTick)) model)
                   ,applyToPlayer applyVelocity
                   ,applyPlayerHitsMonster
                   ]
        
        newModel : Model
        newModel = List.foldl ( \mutator -> \model -> mutator model) oldModel mutators
                
        
        --debug
        player = newModel.player
        d1=Debug.watch "player.velocity " (player.velocity.x, player.velocity.y)
        d2=Debug.watch "player.position " (player.position.x, player.position.y)
        d5=Debug.watch "player.lastTickNotJumping " (player.lastTickNotJumping)
        d3=Debug.watch "intersect" (List.map (\r->intersect r (playerRectangle player)) earth)
    in
        {newModel | tick = currentTick}

main : Signal Element
main = Signal.map view (Signal.foldp updateModel initialModel masterSignal)

