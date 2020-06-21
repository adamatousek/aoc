items =
    [ "asterisk"
    , "ornament"
    , "cake"
    , "space heater"
    , "festive hat"
    , "semiconductor"
    , "food ration"
    ]

bit = [False, True]
bitvecs = sequence . replicate 7 $ bit

foo = filter f [ [ x | (True, x) <- zip bv items ] | bv <- bitvecs ] where
    f x = and $ map ($ x) ([b,c,d,e] ++ a)
    a = map (\x -> nezaraz ["asterisk", "ornament", x]) ["festive hat"]
    b = nezaraz ["ornament", "cake", "space heater", "festive hat"]
    c x = any (`elem` x) ["ornament", "asterisk"]
    d = elem "festive hat"
    e = elem "ornament"
    nezaraz ys x = any (`notElem` x) ys

main = mapM_ (\x ->
                mapM_ (\y -> putStrLn $ "take " ++ y) x
                >> putStrLn "west"
                >> mapM_ (\y -> putStrLn $ "drop " ++ y) x
                >> putStrLn "----------------------------------"
             ) foo
