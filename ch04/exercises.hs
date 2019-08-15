safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]

safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) =safeLast xs

safeInit []  = Nothing
safeInit (x:[])  = Just []
safeInit (x:xs) =   let otherInit = safeInit xs
                    in Just  (x : case otherInit of
                                    Just a  -> a
                                    Nothing -> [])

splitWith :: (a->Bool)->[a] ->[[a]]

splitWith func xs 
                    | null xs   = []
                    | otherwise =   let (pre,lst) = span func xs
                                    in [pre]++(splitWith func lst)

myfirstword :: String -> String

myfirstword str = (unwords  (map head (map words (lines str))))

zhuanzhi :: String -> String

zhuanzhi str 
                | null str  = []
                | or (map null (lines str)) = []        
                | otherwise = (map head (lines str)) ++ ('\n':[]) ++
                                    (zhuanzhi (unlines (map tail (lines str)))) 