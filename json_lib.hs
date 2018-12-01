module Json_lib where

import ParseDouble

import Data.Typeable
import Control.Applicative
import Control.Monad
import Text.Read (readMaybe)
import Text.ParserCombinators.Parsec hiding ((<|>),many)


{-Json-}
{-Conjunto de valores possiveis para o Json-}
data Jvalue  = Js String | Jb Bool  
  | Ja [Jvalue] | Jn |Jo Json | Jnum Double  deriving (Show, Eq, Ord)
{-Par chave e valor Para atributos Json-}
data Jatribute = Jatribute (String , Jvalue) deriving (Show, Eq, Ord)
{-Objeto Json , composto de um array de atributos-}
data Json = Json [Jatribute] deriving (Show, Eq, Ord)

getJatribute :: Jatribute -> (String,Jvalue)
getJatribute  (Jatribute x) = x

getJson :: Json -> [Jatribute]
getJson  (Json a) = a

getString :: Jvalue -> String
getString (Js str) = str

getBool :: Jvalue -> Bool
getBool (Jb bool) = bool

getDouble :: Jvalue -> Double
getDouble (Jnum double) = double

getInteger :: Jvalue -> Integer
getInteger (Jnum integer) = truncate integer

getArray :: Jvalue -> [Jvalue]
getArray (Ja elem) = elem

{-fim do Json-}



{-Faz parse de bool -}
matchFalse :: Parser String {- valor falso retorna Right false , c.c erro-}
matchFalse = string "false"
matchTrue :: Parser String
matchTrue = string "true" {-Mesma coisa do anterior-}

alwaysFalse :: Parser Bool {- retorna sempre um parsed bool do false-}
alwaysFalse = pure False
alwaysTrue :: Parser Bool {-mesmo que o anterior-}
alwaysTrue = pure True

boolFalse :: Parser Bool
boolFalse =  matchFalse *> alwaysFalse {-se match false não lançar erro retorna um parser bool-}
boolTrue :: Parser Bool
boolTrue = matchTrue *> alwaysTrue {-mesmo q o anterior porem para true-}

bool :: Parser Bool
bool = boolTrue<|>boolFalse {-tenta com o valor true cc false-}

jsonBool :: Parser Jvalue
jsonBool = Jb <$> bool {-mapeia a função bool para o texto lido do  parser-}
{-fim do parse para bool -}    


{-retorna o valor literal lido-}
stringLiteral :: Parser String
stringLiteral = char '"' *> (many (noneOf ['"']))<* char '"'


jstring :: Parser Jvalue
jstring = Js <$> stringLiteral



{-
  Tenta casar com algum dos valores em json e retornar um "Parser Jvalue"
-}

jsonValue :: Parser Jvalue
jsonValue = jstring <|> jsonBool <|> jsonArray <|> jsonDouble <|> jsonObject


atributeParse :: Parser (String,Jvalue)
atributeParse = do
  key <- stringLiteral
  (char ':')
  value <- jsonValue
  return (key,value)

jsonAtribute :: Parser Jatribute
jsonAtribute = Jatribute <$> atributeParse


objectParse :: Parser [Jatribute]
objectParse = (char '{' )*> (jsonAtribute `sepBy` (char ',')) <*(char '}')


 


{-
  Arrays: Em Json, arrays podem ser de diferentes tipos, diferente de haskell.
  Logo, o array em haskell deve ser de Jvalue.
-}

jsonObject :: Parser Json
jsonObject =  Json <$> objectParse

array :: Parser [Jvalue]
array = (char '[') *> (jsonValue `sepBy` (char ',')) <* (char ']')



jsonArray :: Parser Jvalue
jsonArray = Ja <$> array


{-

Numeros

-}

--number :: Parser Jvalue
--number = numberDouble (readStringAsDouble (getParserValue numberString))

--AUXILIARES-----------------------------
-- Um numero vai ser representado primeiramente por uma string

getParserValue (Right a) = a



jsonDouble :: Parser Jvalue
jsonDouble = Jnum <$> double

-----------------------------------------








