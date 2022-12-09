{-# LANGUAGE BlockArguments #-}

module Main where

import Text.ParserCombinators.Parsec

import System.IO


type Calories = Int

--

fileParser :: GenParser Char st [[Calories]]
fileParser = do
    result <- elfParser `sepBy` newline
    eof
    return result

elfParser :: GenParser Char st [Calories]
elfParser = many1 lineParser
    where lineParser = read <$> (many1 alphaNum <* newline)

--

main = withFile "input.txt" ReadMode \file -> do
    contents <- hGetContents file

    case runParser fileParser () "day1" contents of
        Right elves -> do
            let elves' = map sum elves
            let (a, b, c) = maximum3 elves'
            print $ a + b + c
        Left err -> print err

    return ()


maximum3 nums =
    maximum3' nums (-1, -1, -1)

    where maximum3' [] (a, b, c) = (a, b, c)
          maximum3' (x:xs) (a, b, c)
            | x > a     = maximum3' xs (x, a, b)
            | x > b     = maximum3' xs (a, x, b)
            | x > c     = maximum3' xs (a, b, x)
            | otherwise = maximum3' xs (a, b, c)