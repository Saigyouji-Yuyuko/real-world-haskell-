myDrop n xs =   if n<= 0 || null xs
                then xs
                else myDrop (n-1) (tail xs)
              
lastButOne xs = if length xs ==2
                then head xs
                else    if length xs > 2
                        then lastButOne (tail xs)
                        else  head (tail xs)  -- may be throw a error