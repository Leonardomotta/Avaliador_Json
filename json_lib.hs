import Data.Typeable
import Control.Applicative
import Control.Monad
import Text.ParserCombinators.Parsec hiding ((<|>),many)

{-Json-}
{-Conjunto de valores possiveis para o Json-}
data Jvalue  =   Js String | Ji Integer | Jb Bool | Jf Float | Jsa [String] |
 Jia [Integer] | Jba [Bool] | Jfa [Float]| Jn |Jo Json | Joa [Json] deriving (Show, Eq, Ord)
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

getInteger :: Jvalue -> Integer
getInteger (Ji int) =int

getBool :: Jvalue -> Bool
getBool (Jb bool) = bool

getFloat :: Jvalue -> Float
getFloat (Jf float) = float

getArrayString :: Jvalue -> [String]
getArrayString (Jsa a) = a

getArrayInteger :: Jvalue -> [Integer]
getArrayInteger (Jia i) = i

getArrayBool :: Jvalue -> [Bool]
getArrayBool (Jba b) = b

getArrayFloat :: Jvalue -> [Float]
getArrayFloat (Jfa f) = f

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






{-jsonObject = 0 <$> ((char '{')*>(Jvalue `sepBy` (char ','))<*(char '}'))-}
