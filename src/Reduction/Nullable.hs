module Reduction.Nullable where
import           Data.Function
import           Eval.RT
import           Reduction.Reducer
import           Syntax

{-
  Given an expression in context (State),
  determine if it's nullable (=not executed/-able or ignored).
  If so, return Nil (+ optional info for the parent);
  otherwise, return the (potentially modified) node.


  forall A in M. A nullable -> M nullable 
-}


{- Observations:
  1. Compile time falsy conditionals:
    { ~f: a =
      False | - a 1
      > a 3 | a
    }

    is equivalent to:
    { ~f: a =
      > a 3 | a
    }

    since `False` is never met.
    Execution:
      1.  Determine if condition is always falsy (nullable + False)
      2a. If it is, (with `Cond c e rest`) return `nullable rest`
      2b. Otherwise, return `Cond c' ( nullable e ) ( nullable rest )` 

  2. Compile time truthy conditionals:
    { ~f: a = 
      True | a
           | 1
    }

    is equivalent to:
    { ~f: a = a } 

    All cases after the nullable one are
    removed.
    Execution:
      1.  Determine if condition is always truthy (nullable + True)
      2a. If it is, return `Cond c' ( nullable e ) FNil`
      2b. Otherwise, return `Cond c' ( nullable e ) ( reduce next_fe )`  
-}





