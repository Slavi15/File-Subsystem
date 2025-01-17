{-# LANGUAGE LambdaCase #-}
module Navigation where

import Core.FileSystem ( FileSystem(..) )

import Data.List (find)

import Parser (getNextDirectory)
import Output (printDirectory, printEntity)
import Utility (getName, isAbsolutePath)

pwd :: [FileSystem] -> String
pwd fs = reverse $ printDirectory fs

cd :: String -> [FileSystem] -> Maybe [FileSystem]
cd input fs = if isAbsolutePath input
    then goToRoot fs >>= \root -> cd' input root
    else cd' ("/" ++ input) fs

    where
        cd' :: String -> [FileSystem] -> Maybe [FileSystem]
        cd' input fs = case getNextDirectory input of
            Just ("", "") -> Just fs
            Just (rest, "..") -> goToParentDirectory fs >>= \parent -> cd' rest parent
            Just (rest, curr) -> goToSubDirectory curr fs >>= \sub -> cd' rest (sub : fs)
            Nothing -> Nothing

ls :: String -> [FileSystem] -> String
ls input fs = case cd input fs of
    Just ((MkDirectory _ contents) : _) -> concatMap printEntity contents
    Nothing -> "Invalid directory!\n"

goToRoot :: [FileSystem] -> Maybe [FileSystem]
goToRoot fs = case goToParentDirectory fs of
    (Just newFs) -> goToRoot newFs
    Nothing -> Just fs

goToParentDirectory :: [FileSystem] -> Maybe [FileSystem]
goToParentDirectory (current : parent@(MkDirectory name contents) : rest) =
    Just (MkDirectory name (replaceChild current contents) : rest)
goToParentDirectory _ = Nothing

replaceChild :: FileSystem -> [FileSystem] -> [FileSystem]
replaceChild updatedChild = map step
    where
        step :: FileSystem -> FileSystem
        step entry
            | getName entry == getName updatedChild = updatedChild
            | otherwise = entry

goToSubDirectory :: String -> [FileSystem] -> Maybe FileSystem
goToSubDirectory name (MkDirectory _ contents : _) =
    find (\case
        MkDirectory directoryName _ -> name == directoryName
        _ -> False) contents
goToSubDirectory _ _ = Nothing