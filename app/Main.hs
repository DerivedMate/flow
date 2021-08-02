module Main where
import           Data.List
import           Eval
import           Eval.RT
import           Syntax
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

-- main :: IO ()
main = -- runFlow $ parseString "{(0; [1,2,3])} => fold {+ &0 &1} => { <~ Any }" 
  do
  args <- getArgs
  case args of
    []             -> interactiveShell
    (filePath : _) -> parseFile filePath >>= runFlow


testMain :: IO ()
testMain = do
  pure (rtParse (TList TInt) "[1, 2] 3") *> pure ()
