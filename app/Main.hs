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
    = printAST (parseString rest) >> loop
    | Just path <- stripPrefix ":file" input
    = parseFile path >>= runFlow >> loop
    | otherwise
    = runFlow (parseString input) >> loop

runFile :: Int -> FilePath -> IO ()
runFile optLvl path = optSrc >>= runFlow >>= handleError
 where
  srcParse = parseFile path
  optSrc
    | optLvl == 1
    = (\r -> r { prExp = optimize rStaticExp <$> prExp r }) <$> srcParse
    | optLvl == 2
    = (\r ->
        r { prExp = optimize rNullableExp . optimize rStaticExp <$> prExp r }
      )
      <$> srcParse
    | otherwise
    = srcParse

handleError :: Either String () -> IO ()
handleError (Left msg) = printf msg
handleError _          = pure ()

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
