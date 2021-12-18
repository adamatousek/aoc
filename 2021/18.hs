import System.Environment ( getArgs )
import Control.Arrow
import Text.Parsec hiding ( (<|>) )
import Text.Parsec.String
import Control.Applicative hiding ( many )
import Control.Monad ( guard )
import Data.List ( foldl1' )

main :: IO ()
main = getArgs >>= parseFromFile (many pSnum) . head
               >>= print . (solve1 &&& solve2) . either (error . show) id
    where solve1 = (id &&& magnitude) . foldl1' snadd
          solve2 l = maximum [ magnitude (x `snadd` y) | x <- l, y <- l, x /= y ]

data Snum = N Int
          | P Snum Snum
          deriving ( Eq )

pSnum :: Parser Snum
pSnum = ( pSnumN <|> pSnumP ) <* spaces
    where pSnumN = N . read <$> many1 digit
          pSnumP = char '[' *> (liftA2 P (pSnum <* char ',') pSnum) <* char ']'

snadd :: Snum -> Snum -> Snum
snadd l r = snreduce (P l r)

snreduce :: Snum -> Snum
snreduce x = maybe x snreduce ( explode x <|> split x )
    where
        explode = fmap fst . go 4
        go 0 (P (N x) (N y)) = Just (N 0, (x, y))
        go 0 (P _ _) = error "weird pair to explode"
        go _ (N _) = Nothing
        go n (P l r) = tryLeft <|> tryRight
            where
                n' = n - 1
                tryLeft = do (l', (pl, pr)) <- go n' l
                             Just (P l' (pushLeft pr r), (pl, 0))
                tryRight = do (r', (pl, pr)) <- go n' r
                              Just (P (pushRight pl l) r', (0, pr))
                pushLeft 0 x = x
                pushLeft n (N m) = N (n + m)
                pushLeft n (P l r) = P (pushLeft n l) r
                pushRight 0 x = x
                pushRight n (N m) = N (n + m)
                pushRight n (P l r) = P l (pushRight n r)

        split (N n) = if n >= 10
                      then let l = n `div` 2
                            in Just (P (N l) (N (n - l)))
                      else Nothing
        split (P l r) = ( flip P r <$> split l ) <|> ( P l <$> split r )

magnitude :: Snum -> Int
magnitude (N x) = x
magnitude (P l r) = 3 * magnitude l + 2 * magnitude r

instance Show Snum where
    show (N x) = show x
    show (P x y) = concat [ "[", show x, ",", show y, "]" ]
