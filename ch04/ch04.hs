-- foldl :: (a -> b -> a) -> a -> [b] -> a

-- foldl func zero (x:xs) = foldl func (func zero x) xs
-- foldl _ zero [] = zero

foldlSum xs = foldl step 0 xs
    where step a b = a + b

niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs

-- foldr :: (a->b->b)->b->[a]->b

-- foldr func zero (x:xs) = func x (foldr func zero xs)
-- foldr _ zero [] = zero

niceSumfoldr :: [Int] -> Int
niceSumfoldr xs = foldr (+) 0 xs

--filter :: (a->Bool)->[a]->[a]

--filter _ [] = []
--filter func (x:xs) 
--        | func x    = x:(filter func xs)
--        | otherwise = filter func xs

myFilter p xs = foldr func [] xs
    where func item xs  | p item    = item : xs
                        | otherwise = xs

myFordMap func xs = foldr step [] xs
    where step item xs  = (func item):xs 