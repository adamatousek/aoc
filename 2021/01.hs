import Control.Arrow

main :: IO ()
main = getContents >>= (print . solveBoth . map read . lines)

solveBoth :: [Int] -> (Int, Int)
solveBoth = solve1 &&& solve2

solve1 :: [Int] -> Int
solve1 xs = length . filter id . zipWith (<) xs . tail $ xs

solve2 = solve1 . map sum . windows
    where windows (x:s@(y:z:_)) = [x,y,z] : windows s
          windows _             = []
