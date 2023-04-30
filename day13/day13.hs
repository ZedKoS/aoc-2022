module Main where

import Data.Function
import Data.List
import Text.ParserCombinators.Parsec


data Packet
    = PInt Int
    | PList [Packet]
    deriving (Show)

instance Eq Packet where
    PInt a == PInt b = a == b
    PList a == PList b =
        all (uncurry (==)) (zip a b) && length a == length b

    PInt x == PList a = PList [PInt x] == PList a
    PList a == PInt x = PList a == PList [PInt x]

instance Ord Packet where
    PInt a <= PInt b = a <= b

    PList (a:as) <= PList [] = False
    PList [] <= PList _ = True
    PList (a:as) <= PList (b:bs) = case compare a b of
        EQ -> as <= bs
        LT -> True
        GT -> False

    PInt x <= PList a = PList [PInt x] <= PList a
    PList a <= PInt x = PList a <= PList [PInt x]


inputFile = "input.txt"

divider2 = PList [PList [PInt 2]]
divider6 = PList [PList [PInt 6]]

main = do
    packetLines <- filter (not . null) . lines <$> readFile inputFile

    -- part 1
    let ordered = filter (\(_, (a, b)) -> a <= b) (enumerate . packetPairs $ packetLines)
    let indexSum = foldl' (\acc ipair -> acc + fst ipair) 0 ordered
    putStrLn $ "Index sum = " ++ show indexSum

    -- part 2
    let packets = sort $ [divider2, divider6] ++ map parsePacket packetLines
    let div2Index = elemIndex divider2 packets
    let div6Index = elemIndex divider6 packets

    let decoderKey = do
        a <- div2Index
        b <- div6Index
        return $ (a + 1) * (b + 1)
    
    case decoderKey of
        Just key -> putStrLn $ "Decoder key = " ++ show key
        Nothing -> putStrLn "decoder key error"


packetPairs :: [String] -> [(Packet, Packet)]
packetPairs [] = []
packetPairs (a:b:xs) = (parsePacket a, parsePacket b):packetPairs xs
packetPairs _ = error "Unpaired packet :("

parsePacket :: String -> Packet
parsePacket s = case runParser parsePacket' () "" s of
    Right result -> result
    Left err -> error "parse error"

parsePacket' = PList <$> parseList
parseList = between (char '[') (char ']') (sepBy parseElem (char ','))
parseElem = (PList <$> parseList) <|> (PInt <$> parseNum)
parseNum = read <$> many1 digit


splitOn :: (Eq t) => (t -> Bool) -> [t] -> [[t]]
splitOn p s = case break p s of
    ([], b) -> [b]
    (a, b)  -> a:splitOn p b

enumerate :: [t] -> [(Int, t)]
enumerate = enumerate' 1
    where enumerate' acc [] = []
          enumerate' acc (x:xs) = (acc, x):enumerate' (acc + 1) xs