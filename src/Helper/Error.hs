module Helper.Error where

data CompilationError = CompilationError
  { cErrLine   :: Int
  , cErrColumn :: Int
  , cErrText   :: String
  }
  deriving (Eq, Show)
