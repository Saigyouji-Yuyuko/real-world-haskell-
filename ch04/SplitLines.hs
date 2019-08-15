splitLines :: String -> [String]

splitLines [] = []
splitLines str =
    let (pre,suf) = break isLineTerminator str
    in  pre : case suf of
                ('\r':'\n':rest)-> splitLines rest
                ('\r':rest)     -> splitLines rest
                ('\n':rest)     -> splitLines rest
                _               -> []

isLineTerminator c = c == '\r' || c == '\n'