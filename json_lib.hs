import Data.Typeable
import Data.List.Split


{-Conjunto de valores possiveis para o Json-}
data Jvalue  =   Js String | Ji Integer | Jb Bool | Jf Float | Jsa [String] |
 Jia [Integer] | Jba [Bool] | Jfa [Float]| Jn |Jo Json deriving (Show, Eq, Ord)
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






