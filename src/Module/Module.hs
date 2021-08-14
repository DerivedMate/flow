module Module.Module where

import           Control.Monad
import           Data.Foldable                  ( foldlM )
import           Data.Function           hiding ( (&) )
import           Data.List
import           Data.Maybe
import           Helper.String
import           Module.Graph
import           System.Directory
import           System.IO
import           Text.Pretty.Simple

type Segment = String
type Source = String

segmentsOfPath :: FilePath -> [Segment]
segmentsOfPath = filter (/= "/") . groupBy (\a b -> '/' `notElem` [a, b])

isAbsolute :: FilePath -> Bool
isAbsolute ('/' : _) = True
isAbsolute _         = False

resolveModulePath :: FilePath -> FilePath -> FilePath
resolveModulePath rootPath relativePath = morph
  (reverse . segmentsOfPath $ rootPath)
  (segmentsOfPath relativePath)
 where
  morph :: [Segment] -> [Segment] -> FilePath
  morph toor path | (".." : path') <- path = morph (tail toor) path'
                  | ("." : path') <- path  = morph toor path'
                  | (s : path') <- path    = morph (s : toor) path'
                  | otherwise = '/' : intercalate "/" (reverse toor)

{- contextOfModule (
  %% /<sth>/src/main.hf %%
  %% ... %%

  @import(`./a.hf`)
  @import(`./lib/b.hf`)

  %% ... %%
  )
  => Context 
        ()
        "/<sth>/src/main.hf" 
        [ "/<sth>/src/a.hf"
        , "/<sth>/src/lib/b.hf"
        ]
-}
contextOfModule :: FilePath -> Source -> Context String
contextOfModule path src = Context (unlines rest)
                                   path
                                   (resolveModulePath root <$> importPaths)
 where
  root               = ('/' :) . intercalate "/" . init . segmentsOfPath $ path
  importPrefix       = "@import(`"
  isImport           = isPrefixOf importPrefix
  rows               = lines src
  (importRows, rest) = partition (isImport . trimLeft) rows
  importPaths        = nub $ mapMaybe
    (fmap (takeWhile (/= '`')) . stripPrefix importPrefix . trimLeft)
    importRows

graphOfRoot :: FilePath -> IO (Graph Source)
graphOfRoot root = foldr (&) Empty <$> (runner [] [] . (: []) =<< aux root)
 where
  runner
    :: [Context Source] -> [FilePath] -> [Context Source] -> IO [Context Source]
  runner u visited cs | null filesToVisit = pure u'
                      | otherwise = mapM aux filesToVisit >>= runner u' visited'
   where
    filesToVisit = [ p | p <- nub (concatMap ctxOut cs), p `notElem` visited' ]
    visited'     = visited <> map ctxLabel cs
    u'           = cs <> u

  aux :: FilePath -> IO (Context Source)
  aux path = do
    file <- openFile path ReadMode
    src  <- hGetContents file
    pure $ contextOfModule path src

gatherFile :: FilePath -> IO (Either String Source)
gatherFile path = do
  path' <- makeAbsolute path
  g     <- graphOfRoot path'
  if gIsCyclic g
    then pure . Left $ "Error: cyclic dependency tree. Root path: " <> path
    else pure . Right . concatMap snd . gDepthNodesFrom path' $ g



