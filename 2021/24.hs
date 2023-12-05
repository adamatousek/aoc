check :: Int -> Int
check = foldl foo 0 . flip zip params . pad . digits
    where digits 0 = []
          digits n = digits ( n `div` 10 ) ++ [ n `mod` 10 ]

          pad xs = replicate (14 - length xs) 0 ++ xs

          foo :: Int -> (Int, (Int, Int, Int)) -> Int
          foo z (w, (a, b, c)) = let z' = (z `div` a) * if x == w then 1 else 26
                                     x  = (z `mod` 26) + b
                                 in z' + if x == w then 0 else w + c

          params :: [ (Int, Int, Int) ]
          params =
                [ ( 1 ,  15 ,  15 )
                , ( 1 ,  15 ,  10 )
                , ( 1 ,  12 ,  2 )
                , ( 1 ,  13 ,  16 )
                , ( 26,  -12,  12 )
                , ( 1 ,  10 ,  11 )
                , ( 26,  -9 ,  5 )
                , ( 1 ,  14 ,  16 )
                , ( 1 ,  13 ,  6 )
                , ( 26,  -14,  15 )
                , ( 26,  -11,  3 )
                , ( 26,  -2 ,  12 )
                , ( 26,  -16,  10 )
                , ( 26,  -14,  13 )
                ]















