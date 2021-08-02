module Main where
import           Data.List
import           Eval
import           Eval.RT
import           Syntax
import           System.Directory.Internal.Prelude
                                                ( fromMaybe )
import           System.Environment
import           System.IO

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

main :: IO ()
main = do
  isDev <- (== Just "dev") <$> lookupEnv "production"
  if isDev
    then runFlow $ parseString "{~x:= 1} {~y:= 2} {+ x y} => { <~ Int }"
    else do
      args <- getArgs
      case args of
        []             -> interactiveShell
        (filePath : _) -> parseFile filePath >>= runFlow
