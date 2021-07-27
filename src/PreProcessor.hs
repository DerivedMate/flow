module PreProcessor where
 
import Data.List
import System.Directory
import System.IO

type Source = String
type Path   = String

removeComments :: Source -> Source
removeComments s = foldl1 (<>) 
                 $ foldl aux groupedSrc (pair [] bracePositions [])
  where
    -- Pairs to adjacent elements into tuples
    -- Returns a list of such pairs
    -- ! Ignores odd elements
    pair (a:b:_) as rs = pair [] as ((b, a):rs)
    pair p (a:as) rs   = pair (a:p) as rs
    pair p [] rs       = rs

    -- Group src to easier match "%%" and only "%%"
    groupedSrc         = group s
    bracePositions     = elemIndices "%%" groupedSrc

    aux s (i, j) = s0 <> s1
      where
        (s0, s')  = splitAt i s
        (s'', s1) = splitAt (j - i) (drop 1 s')

{-
    I've only now realized what a task
    I have undertaken. Let's leave it
    for later.
-}
inlineFiles :: Source -> IO Source
inlineFiles s = undefined 
    where
        resolveFilePath :: Path -> IO Path
        resolveFilePath = undefined 

        inlineFile :: Source -> IO Source
        inlineFile = undefined 

preProcess :: Source -> IO Source
preProcess = pure . removeComments