import qualified Data.Map as M

main :: IO ()
main = getList >>= print . commonChars . head. similars

getList :: IO [String]
getList = go [] where
    go acc = do
        l <- getLine
        if null l
            then pure acc
            else go $ l : acc


checksum :: [String] -> Int
checksum = uncurry (*) . foldr (\(x, y) (x2, y2) -> (fromEnum x + x2, fromEnum y + y2)) (0, 0) . map (countChars M.empty)
    where countChars m [] = let me = M.elems m in (2 `elem` me, 3 `elem` me)
          countChars m (c:s) = M.alter (Just . maybe 1 (+1)) c m `countChars` s

similars :: [String] -> [(String, String)]
similars xs = [ (s1, s2) | s1 <- xs, s2 <- xs, differInOne s1 s2 ]
    where differInOne s1 = (== 1) . length . filter not . zipWith (==) s1

commonChars :: (String, String) -> String
commonChars = fst . unzip . filter (uncurry (==)) . uncurry zip
