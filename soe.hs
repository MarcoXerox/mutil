import qualified Data.IntSet as S
primesUpTo :: Int -> [Int]
primesUpTo n = 2 : 3 : 5 : 7 : put S.empty (takeWhile (<=n) (spin wheel 11))
    where
    put :: S.IntSet -> [Int] -> [Int]
    put _ [] = []
    put comps (x:xs) =
        if S.member x comps
        then put comps xs
        else x : put (S.union comps multiples') xs
            where multiples' = S.fromDistinctAscList [x*x, x*(x+2) .. n]
    spin (x:xs) n = n : spin xs (n + x)
    wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel
