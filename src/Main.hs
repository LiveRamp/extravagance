{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import           Accessors
import           Data.Either
import           Data.Foldable
import qualified Data.Map.Strict       as M
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Debug.Trace
import           Extravagance
import           Language.Java.Lexer
import           Language.Java.Parser
import           Language.Java.Pretty
import           Language.Java.Syntax
import           Replacer
import           System.Directory.Tree
import           System.Environment
import           Util

selectFiles :: DirTree a -> Bool
selectFiles File{} = True
selectFiles d      = False

parseCompilationUnit :: String -> Maybe CompilationUnit
parseCompilationUnit s = either (const Nothing) Just (parser compilationUnit s)

contentsExist :: DirTree (Maybe a) -> Bool
contentsExist f@File{} = isJust $ file f
contentsExist d        = True

foldMaybes :: DirTree (Maybe a) -> DirTree a
foldMaybes tree = fromJust <$>  filterDir contentsExist tree

getCompilationUnitsFromTree :: AnchoredDirTree String -> [CompilationUnit]
getCompilationUnitsFromTree tree = catMaybes $ toList $ fmap parseCompilationUnit (dirTree tree)

selectTargetedFiles :: PatchSet -> DirTree a -> Bool
selectTargetedFiles (PatchSet map) (File name _) = M.member (stripSuffix ".java" name) map
selectTargetedFiles _ _                          = True

merge :: (a -> b -> c) -> DirTree a -> DirTree b -> DirTree c
merge fn a = applicative (fn <$> a)

applicative :: DirTree (a -> b) -> DirTree a -> DirTree b
applicative (File _ fn) (File name contents) = File name (fn contents)
applicative (Dir _ fnContents) (Dir name contents) = Dir name (zipWith applicative fnContents contents)
applicative _ _ = Failed "" undefined

main = do
    [patchPath, srcPath] <- getArgs
    patchFiles <- readDirectoryWith (fmap T.unpack . TIO.readFile) patchPath
    let patchCompilationUnits = getCompilationUnitsFromTree patchFiles
    let patchSet = mconcat $ map generatePatchSet patchCompilationUnits
    srcFiles <- readDirectoryWith (fmap T.unpack . TIO.readFile) srcPath
    let selectSourceFiles = filterDir (selectTargetedFiles patchSet) (dirTree srcFiles)
    let srcCompilationUnits = foldMaybes $ parseCompilationUnit <$> selectSourceFiles
    let patchFunction = applyPatchSet patchSet
    let patchedSrcUnits = fmap (prettyPrint . patchFunction) srcCompilationUnits
    let repairedPatchedSrcUnits = merge replaceMatchingStrings selectSourceFiles patchedSrcUnits
    writeDirectoryWith (\f s -> TIO.writeFile f (T.pack s)) (anchor srcFiles :/ repairedPatchedSrcUnits)
    return ()
