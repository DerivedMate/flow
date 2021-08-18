module Main where
import           Data.Bifunctor
import           Data.List
import           Eval
import           Eval.RT
import           Reduction.Nullable             ( rNullableExp )
import           Reduction.Reducer              ( optimize )
import           Reduction.Static               ( rStaticExp )
import           Syntax
import           System.Directory.Internal.Prelude
                                                ( fromMaybe )
import           System.Environment
import           System.IO
import           Text.Printf                    ( printf )
import Lexer

interactiveShell :: IO ()
interactiveShell =
  putStrLn 
      ("Welcome to Flow!\n" <> intercalate
        "\n"
        ((\p -> "\t" <> fst p <> " - " <> snd p) <$> options)
      )
    >> loop
 where
  options =
    [ (":q"          , "quit shell")
    , (":ast <code>" , "print ast")
    , (":file <path>", "exec file")
    ]
  loop = putStr "flow> " >> hFlush stdout >> getLine >>= interpret
  interpret input
    | Just _ <- stripPrefix ":q" input
    = return ()
    | Just rest <- stripPrefix ":ast" input
    = flPrintAST (flParseString rest) >> loop
    | Just path <- stripPrefix ":file" input
    = runFile 0 path >> loop
    | otherwise
    = runFlow (flParseString input) >> loop

runFile :: Int -> FilePath -> IO ()
runFile optLvl path = do
  r0 <- flParseFile path
  case r0 of
    Left msg -> printf msg
    Right pr -> runFlow $ second (fmap optSrc) pr 
  where
    optSrc 
      | optLvl == 1
      = optimize rStaticExp
      | optLvl == 2
      = optimize rNullableExp . optimize rStaticExp
      | otherwise 
      = id
{-
do
   undefined
 where
  srcParse = flParseFile path
  optSrc
    | optLvl == 1
    = second (fmap (bimap id (optimize rStaticExp))) <$> srcParse 
    | optLvl == 2
    = second (fmap (bimap id (optimize rNullableExp . optimize rStaticExp)))
      <$> srcParse
    | otherwise
    = srcParse
-}

main :: IO ()
main = do
  isDev  <- (== Just "dev") <$> lookupEnv "FENV"
  optLvl <- maybe 0 read <$> lookupEnv "FOPT"
  if isDev
    then runFile optLvl "test/pg.hf"
    else do
      args <- getArgs
      case args of
        []             -> interactiveShell
        (filePath : _) -> runFile optLvl filePath
