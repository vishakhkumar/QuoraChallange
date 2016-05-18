main = do
    putStrLn "Enter your input"
    line1 <- getLine
    line2 <- getLine
    line_1 <- (splitOn (==' ') (line1))
{-   line_2 <- (splitOn (==' ') line2)
    putStrLn "Sup"
-}
    putStrLn (""++line1)
    putStrLn (""++line2)
--splitInput :: Char -> Char -> [Char]
--splitInput a b | (b!!0) /= ' ' = (splitInput (a++[head b]) (tail b))
--               | (b!!0) == ' ' = [a]:(splitInput "" (tail b))
--               | length b == 0 = []
compressedFunc a b | length a >= b = ((sum (map (\x->(x*(x+1)/2)) (collapse (map (\y->(signum y)*(-1) + (1 + (-1)**(abs (signum y)))/2) (zipWith (-) (tail (take b a)) (init (take b a))))))) - (sum (map (\x->(x*(x+1)/2)) (collapse (map (\y->(signum y)*(1) + (1+(-1)**(abs (signum y)))/2) (zipWith (-) (tail (take b a)) (init (take b a)))))))):(compressedFunc (tail a) b)
                   | otherwise     = []
collapse a
 | signum (head a) == (-1)           = (collapse (tail a))  -- disregard negative values.
 | (signum (a!!1) == (-1))           = (a!!0):(collapse (tail (tail a))) -- meet a negative interval. Must exterminate!
 | otherwise                         = collapse ((a!!0 + a!!1):(tail (tail a)))

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
 | f x = splitOn f xs
 | otherwise = let (h,t) = break f l in h:(splitOn f t)

 {-
******************************************************************
Just for reference. Since the problem is a trivial one,
the compressedFunc seemed appropiate.
Besides, it does look cool that Haskell can solve it in 6 lines :D
******************************************************************
--Example parameters
upvote = [3,4,5,6,7]
windowSize = 2

realFunc a b | length a >= b = (helperFunc (take b a)):(realFunc (tail a) b)
            | otherwise     = []
helperFunc a = (nonDecVal a) - (nonIncVal a)
nonDecVal a = noOfSubRanges (collapse (map (nonDec) (diff a)))
nonIncVal a = noOfSubRanges (collapse (map (nonInc) (diff a)))
diff a = (zipWith (-) (tail a) (init a))
noOfSubRanges a = (sum (map ( \x->(x*(x+1)/2) ) a))
collapse [] = []
collapse (x:[]) = [x]
collapse a
 | signum (head a) == (-1)           = (collapse (tail a))  -- disregard negative values.
 | (signum (a!!1) == (-1))           = (a!!0):(collapse (tail (tail a))) -- meet a negative interval. Must exterminate!
 | otherwise                         = collapse ((a!!0 + a!!1):(tail (tail a)))
nonInc a
 | a < 0     = (1)
 | a > 0     = (-1)
 | a == 0    = (1) -- to make the 0 collapse your way :)
nonDec a
 | a < 0     = (-1)
 | a > 0     = (1)
 | a == 0    = (1) -- to make the 0 collapse your way :)
-}
