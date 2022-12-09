module Main where

import System.IO
import qualified Data.List as List
-- import qualified Data.Map as Map
import Control.Monad.State
import Data.Function (on)

data Command = Cd Path | Ls
    deriving (Show)

data Path
    = PathRoot
    | RelativePath String
    | CurrentDir -- '.'
    | ParentDir  -- '..'
    deriving (Show)

data Output = CdOutput | LsOutput [LsEntry]
    deriving (Show)

data LsEntry
    = LsDir String
    | LsFile String Int
    deriving (Show)

data FileTree
    = FileNode String Int
    | Folder String [FileTree]


main = do
    contents <- lines <$> readFile "input.txt"

    let (Just commands) = parseCommands contents
    let (filesystem, _) = execState (executeCommands commands) (Folder "/" [], [])

    let part1 = recordDirSizeSum filesystem
    putStrLn $ "Part 1: " ++ show part1

    let freeSpace = 70000000 - sizeOfFileTree filesystem
    putStrLn $ "Free space: " ++ show freeSpace

    let part2 = minimum $ flagDelete filesystem (30000000 - freeSpace)
    putStrLn $ "Part 2: " ++ show part2

    -- let view = showFs filesystem 0
    -- putStrLn view

    pure ()


-- parsing

parseCommands :: [String] -> Maybe [(Command, Output)]
parseCommands [] = Just []
parseCommands (cmd@('$':_):rest) = do
    let (output, rest') = break ((== '$') . head) rest
    result <- parseCommandOutput (cmd:output)
    restResult <- parseCommands rest'
    return (result:restResult)
parseCommands _ = Nothing
        

parseCommandOutput :: [String] -> Maybe (Command, Output)
parseCommandOutput [] = Nothing
parseCommandOutput (cmd:rest) = do
    cmd <- parseCommand cmd
    case cmd of
        Cd path -> return (Cd path, CdOutput)
        Ls      -> do
            let entries = parseLsEntries rest
            return (Ls, LsOutput entries)

parseLsEntries :: [String] -> [LsEntry]
parseLsEntries = map parseEntry
    where parseEntry :: String -> LsEntry
          parseEntry entry =
            let w = words entry in
                case head w of
                    "dir" -> LsDir (w!!1)
                    num   ->
                        let size = read num
                            name = w!!1
                        in LsFile name size

parseCommand :: String -> Maybe Command
parseCommand ('$':' ':"ls") = Just Ls
parseCommand ('$':' ':cmd) =
    let (cd, rest) = break (==' ') cmd in
        if cd == "cd" then do
            pathRaw <- if null rest then Nothing else Just (tail rest)
            let path = parsePath pathRaw
            return $ Cd path
        else Nothing
parseCommand _ = Nothing

parsePath :: String -> Path
parsePath "/"  = PathRoot
parsePath "."  = CurrentDir
parsePath ".." = ParentDir
parsePath path = RelativePath path

splitOn :: (t -> Bool) -> [t] -> [[t]]
splitOn p s =
    case dropWhile p s of
        [] -> []
        s' -> w : splitOn p s''
            where (w, s'') = break p s'


-- execution

type Dir = [String]

executeCommands :: [(Command, Output)] -> State (FileTree, Dir) ()
executeCommands [] = pure ()
executeCommands ((cmd, out):rest) = do
    case cmd of
        Ls -> do
            let (LsOutput output) = out
            mapM_ (addToCwd . lsEntryToTree) output
        Cd path -> setCwd path
    executeCommands rest


getCwd :: State (FileTree, Dir) Dir
getCwd = gets snd

setCwd :: Path -> State (FileTree, Dir) ()
setCwd CurrentDir = pure ()
setCwd ParentDir = do
    (tree, cwd) <- get
    put (tree, init cwd)
setCwd PathRoot = do
    (tree, _) <- get
    put (tree, [])
setCwd (RelativePath path) = do
    (tree, cwd) <- get
    put (tree, cwd ++ [path])


nodeName :: FileTree -> String
nodeName (Folder folderName _) = folderName
nodeName (FileNode fileName _) = fileName

inFolder :: String -> [FileTree] -> Bool
inFolder name folder = name `elem` map nodeName folder

lsEntryToTree:: LsEntry -> FileTree
lsEntryToTree (LsDir name) = Folder name []
lsEntryToTree (LsFile name size) = FileNode name size

addToCwd :: FileTree -> State (FileTree, Dir) ()
addToCwd subtree = do
    (tree, cwd) <- get
    put (addToTree (subtree, cwd) tree, cwd)

addToTree :: (FileTree, [String]) -> FileTree -> FileTree
addToTree _ (FileNode _ _) = error "cannot add subtree to file"
addToTree (subtree, []) tree@(Folder folderName children) =
    if nodeName subtree `inFolder` children then
        tree
    else
        Folder folderName (subtree:children)
addToTree (subtree, p:ps) (Folder folderName children) =
    if p `inFolder` children then
        Folder folderName (map (\c ->
            if p == nodeName c then
                addToTree (subtree, ps) c
            else
                c
        ) children)
    else
        let subfolder = addToTree (subtree, ps) (Folder p []) in
        Folder folderName (subfolder:children)


sizeOfFileTree :: FileTree -> Int
sizeOfFileTree (FileNode _ size) = size
sizeOfFileTree (Folder _ children) = sum $ map sizeOfFileTree children

recordDirSizeSum :: FileTree -> Int
recordDirSizeSum (FileNode _ _) = 0
recordDirSizeSum tree@(Folder _ children) =
    let childrenDirSize = foldr (\c acc -> acc + recordDirSizeSum c) 0 children
        folderSize = sizeOfFileTree tree in
    
    if folderSize < 100000 then
        childrenDirSize + folderSize
    else
        childrenDirSize


flagDelete :: FileTree -> Int -> [Int]
flagDelete file@(FileNode _ _) _ = []
flagDelete tree@(Folder _ children) needed =
    let size = sizeOfFileTree tree in
        if size >= needed then
            size:concatMap (`flagDelete` needed) children 
        else
            []

showFs :: FileTree -> Int -> String
showFs tree depth = unlines $ showFs' tree 1 depth
    where showFs' tree depth maxdepth =
            -- inteded behavior: if maxdepth is set to, i.e., 0
            -- the filesystem is printed fully
            if depth == maxdepth then
                []
            else case tree of
                FileNode name size -> [name ++ show size]
                Folder name children ->
                    let subtrees = concatMap (\t -> showFs' t (depth + 1) maxdepth) children
                        shifted = map ("\t"++) subtrees in
                    (name ++ " (dir) " ++ show (sizeOfFileTree tree)) : shifted

