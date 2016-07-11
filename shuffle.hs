import System.Random
-- excerpt from ChargeHuge
shuffle :: RandomGen g => g -> [a] -> [a]
shuffle _ [] = []
shuffle g xs = x : shuffle g' (front ++ end)
    where (front, (x:end)) = splitAt k xs
          (k, g') = randomR (0, length xs - 1) g
