module Module.Graph where

import           Data.Function                  ( on )
import           Data.List
import           Data.Maybe
import           Debug.Trace

type NodeLabel = String
data Context v = Context
  { ctxVal   :: v
  , ctxLabel :: NodeLabel
  , ctxOut   :: [NodeLabel]
  }
  deriving (Show, Eq)

data Graph v
  = Empty
  | Graph (Context v) (Graph v)
  deriving (Show, Eq)

infixr 0 &
(&) :: Context v -> Graph v -> Graph v
(&) = Graph

gUFold :: (Context a -> c -> c) -> c -> Graph a -> c
gUFold f u Empty        = u
gUFold f u (Graph c g') = gUFold f (f c u) g'

gDepthFoldL :: (NodeLabel -> a -> c -> c) -> c -> Graph a -> c
gDepthFoldL f u Empty        = u
gDepthFoldL f u (Graph c g') = foldr (flip aux) (callF f c u) (ctxOut c)
 where
  callF f c u = f (ctxLabel c) (ctxVal c) u
  aux u l = case gMatch g' l of
    (Nothing, g'') -> u
    (Just c', g'') -> gDepthFoldL f u (Graph c' g'')

gMap :: (Context a -> Context b) -> Graph a -> Graph b
gMap f Empty        = Empty
gMap f (Graph c g') = Graph (f c) (gMap f g')

gVMap :: (a -> b) -> Graph a -> Graph b
gVMap f = gMap (\c -> c { ctxVal = f . ctxVal $ c })

gMatch :: Graph v -> NodeLabel -> (Maybe (Context v), Graph v)
gMatch g l = gUFold aux (Nothing, Empty) g
 where
  aux c (u, g') | l == l'   = (Just c, g')
                | otherwise = (u, Graph c g')
    where l' = ctxLabel c

gNodes :: Graph v -> [(NodeLabel, v)]
gNodes = gUFold aux [] where aux (Context v l _) u = (l, v) : u

gDepthNodes :: Eq v => Graph v -> [(NodeLabel, v)]
gDepthNodes = nubBy ((==) `on` fst) . gDepthFoldL (\l v u -> (l, v) : u) []

gDepthNodesFrom :: Eq v => NodeLabel -> Graph v -> [(NodeLabel, v)]
gDepthNodesFrom l g = case gMatch g l of
  (Nothing, _ ) -> gDepthNodes g
  (Just c , g') -> gDepthNodes (Graph c g')

gIsCyclic :: Show v => Graph v -> Bool
gIsCyclic = aux []
 where
  aux :: Show v => [NodeLabel] -> Graph v -> Bool
  aux _ Empty = False
  aux ls (Graph c g') | any (`elem` ls) (ctxOut c) = True
                      | otherwise = any (f g' (l : ls)) (ctxOut c)
   where
    l = ctxLabel c
    f g' ls l = case gMatch g' l of
      (Just c', g'') -> aux ls (Graph c' g'')
      _              -> False

