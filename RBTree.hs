-- Implementation of Red Black Tree
-- Okasaki's Red-Black Trees in a Functional Setting
-- Invariant: Red node has no red parent
-- Invariant: Number of black nodes and leaves are equal for all paths to leaves
-- Note: This implementation supports min/max lookups but no deletions.
{-# LANGUAGE RecordWildCards #-}

module RBTree (
    isEmpty, member, size, minOf, maxOf,
    insert, fromList, toList,
    Tree
) where

data Color = Red | Black
data Tree a = Nil
            | Branch { color :: Color 
                     , left  :: Tree a
                     , value :: a 
                     , right :: Tree a
                     }

isEmpty :: Ord a => Tree a -> Bool
isEmpty Nil = True
isEmpty _   = False

member :: Ord a => a -> Tree a -> Bool
member _ Nil = False
member x Branch{..} = case compare x value of
    LT -> member x left
    EQ -> True
    GT -> member x right

leaf :: Ord a => a -> Tree a
leaf x = Branch Red Nil x Nil

insert :: Ord a => a -> Tree a -> Tree a
insert x s = (ins s) { color = Black }
    where ins Nil = leaf x
          ins b@Branch{..} = case compare x value of
            LT -> balance b { left = ins left } 
            EQ -> b 
            GT -> balance b {right = ins right}

size :: Ord a => Tree a -> Int
size Nil = 0
size Branch{..} = 1 + size left + size right

minOf, maxOf :: Ord a => Tree a -> Maybe a
minOf Nil = Nothing
minOf (Branch _ Nil x _) = Just x
minOf Branch{..} = minOf left
maxOf Nil = Nothing
maxOf (Branch _ _ x Nil) = Just x
maxOf Branch{..} = maxOf right

fromList :: Ord a => [a] -> Tree a
fromList = foldl (flip insert) Nil
toList :: Ord a => Tree a -> [a]
toList Nil = []
toList Branch{..} = toList left ++ [value] ++ toList right

-- Refer to P.3
balance :: Tree a -> Tree a
balance tree = case tree of
    Branch Black (Branch Red (Branch Red a x b) y c) z d -> mkT (a,b,c,d,x,y,z)
    Branch Black (Branch Red a x (Branch Red b y c)) z d -> mkT (a,b,c,d,x,y,z)
    Branch Black a x (Branch Red (Branch Red b y c) z d) -> mkT (a,b,c,d,x,y,z)
    Branch Black a x (Branch Red b y (Branch Red c z d)) -> mkT (a,b,c,d,x,y,z)
    _ -> tree
    where mkT (a,b,c,d,x,y,z) = Branch Red (Branch Black a x b) y (Branch Black c z d)
