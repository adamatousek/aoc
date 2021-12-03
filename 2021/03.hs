{-# LANGUAGE TypeApplications #-}
import Data.Bool
import Control.Arrow

main :: IO ()
main = getContents >>= (print . solve . lines)

solve = map (map (toEnum @Bool . read . pure))
        >>> part1 . maj &&& part2
        >>> both (uncurry (*) . both toNum)

part1 :: [Bool] -> ([Bool], [Bool])
part1 = id &&& map not

part2 :: [[Bool]] -> ([Bool], [Bool])
part2 xs = go (zip xs xs) `both` (id, not)
 where go :: [([Bool], [Bool])] -> (Bool -> Bool) -> [Bool]
       go xxs crit = let b = (crit . head . maj . map fst) xxs
                     in case filter ((== b) . head . fst) xxs of
                        []       -> error "time for dipr"
                        [(_, y)] -> y
                        yys      -> flip go crit . map (first tail) $ yys

maj :: [[Bool]] -> [Bool]
maj = go . foldr1 (zipWith (+)) . map ((1 :) . map fromEnum)
    where go (total:xs) = map ((>= total) . (* 2)) $ xs

toNum :: [Bool] -> Int
toNum = sum . zipWith (bool 0) (map (2^) [0..]) . reverse

both f = f *** f
