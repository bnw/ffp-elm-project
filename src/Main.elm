import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Keyboard
import Array
import Rectangle exposing (..)
import Vector exposing (..)
import Constants exposing (..)
import Art exposing (..)

type alias ModelMutator = Model -> Model

earth : List Rectangle
earth = [createTile -12 -10 12 -9
        ,createTile -13 -10 -12 10
        ,createTile 12 -10 13 10
        ,createTile -8 -8 -4 -7
        ,createTile -1 -5 12 -4
        ,createTile -1 -4 0 -3]

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

masterSignal : Signal IntVector
masterSignal = Signal.sampleOn (fps 50) Keyboard.arrows

view : Model -> Element
view {player, monsters} = 
    let
       forms = (drawBackground :: (moveForm player drawPlayer) :: (List.map drawObstacle earth)) 
                ++ (List.map (\monster -> moveForm monster (drawMonster monster)) monsters)
    in collage windowWidth windowHeight forms

createPlayer : Float -> Float -> Float -> Float -> Player
createPlayer x y deltaX deltaY = {position = {x = x, y=y}, velocity = { x= deltaX, y= deltaY}}

createMonster : Float -> Float -> Float -> Float -> Monster
createMonster x y deltaX deltaY = 
    {position = {x = x, y=y}, 
     velocity = { x= deltaX, y= deltaY}, 
     size = Medium,
     direction = R}

initialModel : Model
initialModel = {
    player = (createPlayer 0 0 0 0 ),
    monsters = [createMonster 50 50 0 3
               ,createMonster 150 50 0 3]
    }

executePlayerBump : (Bump, Rectangle) -> Entity a -> Entity a
executePlayerBump = executeBump playerHeight playerWidth

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
        deltaX = if x>=0 then max -x -0.5 else min -x 0.5
    in changeVelocity old {y=0, x=deltaX}

applyToPlayer : (Player -> Player) -> Model -> Model
applyToPlayer f ({player} as model) = {model | player = f player}

applyGravity : Entity a -> Entity a
applyGravity old = changeVelocity old {y=-0.5, x=0}

applyUserInput : IntVector -> Entity a -> Entity a
applyUserInput keyboard entity  = changeVelocity entity (toVector keyboard)

applyMaximumVelocity : Entity a -> Entity a
applyMaximumVelocity old = 
    let 
        x = if old.velocity.x >= 0 then min old.velocity.x 6 else max old.velocity.x -6
        y = if old.velocity.y >= 0 then min old.velocity.y 4.5 else max old.velocity.y -8
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
        bumps = getBumps [] ((monsterRectangle monster), monster.velocity)
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
playerObstacles {monsters} = earth ++ (List.map monsterRectangle monsters)

monsterObstacles : Model -> List Rectangle
monsterObstacles model = earth

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
                {player | velocity = {x=player.velocity.x, y=6}}
            else player
    in {model | player = newPlayer, monsters = newMonsters}

updateModel : IntVector -> Model -> Model
updateModel keyboard oldModel = 
    let 
    
        mutators = [
                    applyToMonsters applyGravity
                   ,applyToMonsters applyMonsterBumps
                   ,applyToMonsters applyMonsterMinimumVelocity
                   ,applyToMonsters applyVelocity
                   ,applyToPlayer applyGravity
                   ,applyToPlayer applyFriction
                   ,applyToPlayer (applyUserInput keyboard)
                   ,applyToPlayer applyMaximumVelocity
                   ,(\model -> applyToPlayer (applyBumps (playerObstacles model) playerRectangle executePlayerBump) model)
                   ,applyToPlayer applyVelocity
                   ,applyPlayerHitsMonster
                   ]
        
        newModel : Model
        newModel = List.foldl ( \mutator -> \model -> mutator model) oldModel mutators
                
        
        --debug
        player = newModel.player
        d1=Debug.watch "player.velocity " (player.velocity.x, player.velocity.y)
        d2=Debug.watch "player.position " (player.position.x, player.position.y)
        d3=Debug.watch "intersect" (List.map (\r->intersect r (playerRectangle player)) earth)
    in
        newModel

main : Signal Element
main = Signal.map view (Signal.foldp updateModel initialModel masterSignal)

