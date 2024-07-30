import Control.Monad.State
import Data.Lens.Common
-- https://gist.github.com/qsorix/1861138 hanoi-4.hs
type Disc = Int
type Tower = [Disc]
data Towers = Towers
            { towerA_ :: Tower
            , towerB_ :: Tower
            , towerC_ :: Tower
            } deriving (Show)

data TowerName = A | B | C
                 deriving (Show)

type Move = (TowerName, TowerName)

type TowersState = State Towers

type TowerLens = Lens Towers Tower
data Pointer = Pointer TowerName TowerLens

ptrA = Pointer A (lens towerA_ (\a s -> s { towerA_ = a}))
ptrB = Pointer B (lens towerB_ (\a s -> s { towerB_ = a}))
ptrC = Pointer C (lens towerC_ (\a s -> s { towerC_ = a}))

pop :: TowerLens -> TowersState Disc
pop lense = do
    (x:xs) <- gets (getL lense)
    modify $ lense `setL` xs
    return x

push :: Disc -> TowerLens -> TowersState ()
push d lense = modify $ lense `modL` (\xs -> d:xs)

moveOne :: Pointer -> Pointer -> TowersState [Move]
moveOne (Pointer nsrc src) (Pointer ndst dst) =
    pop src >>=
    (flip push) dst >>
    return [(nsrc, ndst)]

moveMany :: Int -> Pointer -> Pointer -> Pointer -> TowersState [Move]
moveMany 1 src dst aux = moveOne src dst
moveMany n src dst aux = do
    m1 <- moveMany (n-1) src aux dst
    m2 <- moveMany    1  src dst aux
    m3 <- moveMany (n-1) aux dst src
    return (m1++m2++m3)

solve :: Towers -> ([Move], Towers)
solve towers =
    let count = length $ towerA_ towers
    in runState (moveMany count ptrA ptrB ptrC) towers

towers = Towers [1, 2, 3] [] []

main = putStrLn $ show $ solve towers