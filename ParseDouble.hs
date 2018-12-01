module ParseDouble where


import Control.Monad

import Text.Parsec
import Control.Applicative hiding ((<|>))

num = many1 digit

positivo = char '+' *> num

negativo = (:) <$> char '-' <*> num

inteiro = positivo <|> negativo <|> num

double = fmap a $ (++) <$> inteiro <*> decimal
    where a      = read :: String -> Double
          decimal = option "" $ (:) <$> char '.' <*> num

main = forever $ do 
                    input <- getLine
                    parseTest double input