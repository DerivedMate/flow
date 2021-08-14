module Helper.String where

trimLeft :: String -> String
trimLeft = dropWhile (`elem` " \t\n\r")
