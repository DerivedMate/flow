{-# LANGUAGE TupleSections #-}

module Eval where
import           Data.List
import           Data.Maybe
import           Debug.Pretty.Simple
import           Debug.Trace
import           Eval.RT
import           Eval.Step
import           Syntax


runFlow :: ParseResult -> IO (Either String ())
runFlow (ParseResult Nothing Nothing src) =
  pure . Left $ "Compilation error at 1:1 :\n" <> (unlines . take 1 . lines) src
runFlow (ParseResult _ (Just r) src) | (not . null) r =
  pure
    .  Left
    $  "Compilation error at "
    <> show column
    <> ":"
    <> show line
    <> " :\n"
    <> loc
 where
  Just preR    = reverse <$> stripPrefix (reverse r) (reverse src)
  line         = length . lines $ preR
  lastTwoLines = unlines . reverse . take 2 . reverse . lines $ preR
  firstLine    = unlines . take 1 . lines $ r
  loc          = lastTwoLines <> "[!!]" <> firstLine
  column | (l : _) <- lineLengths = l
         | otherwise              = 0
    where lineLengths = length <$> lines preR

runFlow (ParseResult (Just ast) _ src) =
  step ast (State RTNil []) >>= aux >> pure (Right ())
 where
  aux [] = pure ()
  aux ds = sequence (iter <$> ds) >>= aux . concat
  iter (Datum e s) = step e s
  traceVars :: [String] -> Datum -> Datum
  traceVars ns d@(Datum e s) = traceShow vs d
    where vs = [ (n, getVar n s) | n <- ns ]
  traceLast :: Datum -> Datum
  traceLast d@(Datum e s) = traceShow (stLast s) d
  help ds = trace
    ("\n" <> intercalate " <<<<<<<< END >>>>>>>> \n\n" (fmap show ds) <> "\n\n")
    ds

runFlow _ = undefined
