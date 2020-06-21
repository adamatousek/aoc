import Control.Monad
import qualified Data.Set as S

main :: IO ()
main = do
        l <- getList
        print . findDup $ scanl (+) 0 l

getList :: IO [Int]
getList = go [] where
    go acc = do
        l <- getLine
        if null l
            then pure . cycle . reverse $ acc
            else go $ readSigned l : acc
    readSigned ('+':ds) = read ds
    readSigned ds = read ds

findDup :: Ord a => [a] -> a
findDup = go S.empty where
    go deja (x:xs)
      | x `S.member` deja = x
      | otherwise         = go (x `S.insert` deja) xs
