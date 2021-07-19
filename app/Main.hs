module Main where
import           Data.List
import           Eval
import           Syntax
import           System.Environment
import           System.IO

interactiveShell :: IO ()
interactiveShell = do
    putStrLn "Welcome to Flow!\n\t:q to close\n\t:ast <code> to print ast"
    loop
    where
        loop = do
            putStr "flow> "
            hFlush stdout
            getLine >>= interpret
        interpret input
            | Just _ <- stripPrefix ":q" input
            = return ()
            | Just rest <- stripPrefix ":ast" input
            = printAST (parseString rest) >> loop
            | otherwise
            = runFlow (parseString input) >> loop

main :: IO ()
main = do
    args <- getArgs
    case args of
        []             -> interactiveShell
        (filePath : _) -> parseFile filePath >>= runFlow

