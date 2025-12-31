{-# LANGUAGE Safe #-}

module PriorityMap
  ( PriorityMap,
    fromListContents,
    insert,
    listContents,
    lookupFirst,
    lookupKey,
    remove,
    splitAfterFirst,
    empty,
  )
where

import Data.Kind (Type)
import Prelude

type PriorityMap :: Type -> Type -> Type -> Type
newtype PriorityMap p k v = PM [(p, k, v)]

instance (Show p, Show k, Show v) => Show (PriorityMap p k v) where
  show (PM pm) = "fromListContents " <> show pm

empty :: PriorityMap p k v
empty = PM []

dropElt :: (Eq k) => PriorityMap p k v -> k -> PriorityMap p k v
dropElt (PM ls) k = PM $ filter (\(_, l, _) -> k /= l) ls

insert :: (Ord p, Eq k) => PriorityMap p k v -> p -> k -> v -> PriorityMap p k v
insert (PM []) p k v = PM [(p, k, v)]
insert prev@(PM (hd@(q, c, _) : prevTail)) p k v
  | p < q = let PM tl' = dropElt prev k in PM $ (p, k, v) : tl'
  | c == k = prev
  | otherwise = let PM tl' = insert (PM prevTail) p k v in PM $ hd : tl'

instance Functor (PriorityMap p k) where
  fmap f (PM pm) = PM $ fmap (\(p, k, v) -> (p, k, f v)) pm

instance Foldable (PriorityMap p k) where
  foldr :: (a -> b -> b) -> b -> PriorityMap p k a -> b
  foldr f b (PM pm) = foldr (\(_, _, v) c -> f v c) b pm

listContents :: PriorityMap p k v -> [(p, k, v)]
listContents (PM pm) = pm

fromListContents :: (Ord p, Eq k) => [(p, k, v)] -> PriorityMap p k v
fromListContents = foldr (\(p, k, v) pm -> insert pm p k v) empty

lookupKey :: (Eq k) => PriorityMap p k v -> k -> Maybe (p, v)
lookupKey (PM pm) = flip lookup (map (\(p, c, v) -> (c, (p, v))) pm)

remove :: (Eq k) => PriorityMap p k v -> k -> Maybe ((p, v), PriorityMap p k v)
remove (PM ls) needle = fmap PM <$> go ls id
  where
    -- go :: (Eq k) => [(p, k, v)] -> ([(p, k, v)] -> [(p, k, v)]) -> Maybe ((p, v), [(p, k, v)])
    go [] _ = Nothing
    go (pkv@(p, k, v) : tl) dlist
      | k == needle = Just ((p, v), dlist tl)
      | otherwise = go tl $ dlist . (pkv :)

splitAfterFirst :: PriorityMap p k v -> Maybe ((p, k, v), PriorityMap p k v)
splitAfterFirst (PM []) = Nothing
splitAfterFirst (PM ((p, k, v) : tl)) = Just ((p, k, v), PM tl)

lookupFirst :: PriorityMap p k v -> Maybe (p, k, v)
lookupFirst = fmap fst . splitAfterFirst
