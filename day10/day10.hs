module Main where

import Control.Monad.State

import qualified Data.Text as S
import qualified Data.Text.IO as S

data Op = Noop | Addx Int


inputFile = "input.txt"

main = do
    input <- S.split (=='\n') <$> S.readFile inputFile
    let ops = fixOps $ map toOp input

    let (values, _) = runState (traverse runOp ops) 1
    let strengths = map (\v -> v * (values !! (v - 2))) [20, 60, 100, 140, 180, 220]

    print $ sum strengths
    
    pure ()


toOp :: S.Text -> Op
toOp op =
    if op == S.pack "noop" then
        Noop
    else
        Addx . read . S.unpack $ S.split (==' ') op !! 1

fixOps :: [Op] -> [Op]
fixOps [] = []
fixOps (Noop:r) = Noop:fixOps r
fixOps (Addx v:r) = Addx v:Noop:fixOps r

runOp :: Op -> State Int Int
runOp Noop = noop
runOp (Addx v) = addx v

noop :: State Int Int
noop = get

addx :: Int -> State Int Int
addx v = noop >> do
    prev <- get
    put (prev + v)
    return prev
