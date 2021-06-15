module Backend.Interface where
  
class Backend where
  gen :: Exp -> String