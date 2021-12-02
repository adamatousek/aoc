{-# LANGUAGE TemplateHaskell #-}
import Text.Parsec
import Control.Arrow
import Control.Lens
import Data.Functor

data Submarine = Submarine { _horisontal :: Integer
                           , _depth1     :: Integer
                           , _aim        :: Integer
                           , _depth2     :: Integer
                           }
               deriving (Eq, Show)

makeLenses ''Submarine

type Parser a = Parsec String Submarine a

main :: IO ()
main = getContents >>= print . fmt . either (error . show) id . runParser task (Submarine 0 0 0 0) ""
    where fmt (Submarine h d1 _ d2) = (h * d1, h * d2)

sym :: String -> Parser String
sym x = string x <* spaces

int :: Parser Integer
int = read <$> many1 digit

task :: Parser Submarine
task = many step *> getState
    where step = do dir <- choice . map sym . words $ "forward up down"
                    n <- int <* spaces
                    st <- getState
                    modifyState $ case dir of
                          'f':_ -> let a = st ^. aim
                                   in horisontal +~ n >>> depth2 +~ (n * a)
                          'u':_ -> depth1 -~ n >>> aim -~ n
                          'd':_ -> depth1 +~ n >>> aim +~ n
