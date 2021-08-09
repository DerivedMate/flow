module Helper.AssocMap where

type AssocMap k v = [(k, v)]
assocExists :: Eq k => AssocMap k v -> k -> Bool
assocExists m k = any ((== k) . fst) m