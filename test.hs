factorial n = if n == 0 then 1 else n * factorial (n - 1)
composeList :: [(a -> a)] -> a -> a
composeList = composeL . reverse
composeL :: [(a -> a)] -> a -> a
composeL [] n = n
composeL l n  = composeL (tail l) ((head l) n)
reverse' :: [a] -> [a]
reverse' = helper []
  where helper result []     = result
        helper result (x:xs) = helper (x:result) xs
length' :: [a] -> Integer
length' = foldr (\a b -> b + 1) 0
foldl' _ acc []     = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs
