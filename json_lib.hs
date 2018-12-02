module Json_lib where

import ParseDouble
import Data.Typeable
import Control.Applicative
import Control.Monad
import Text.Read (readMaybe)
import Text.ParserCombinators.Parsec hiding ((<|>),many)
import Data.List


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

{-Parse principal para valores -}
jsonValue :: Parser Jvalue
jsonValue = spaces*>(jsonNull <|> jstring <|> jsonBool <|> jsonArray <|> jsonDouble <|> jsonObjectValue  ) <* spaces



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


{-Parse para string-}
stringLiteral :: Parser String
stringLiteral = spaces*> char '"' *> spaces*> (many (noneOf ['"']))<*spaces<* char '"'


jstring :: Parser Jvalue
jstring = Js <$> stringLiteral

{-fim do parse para string-}




{-Parser para atributos-}
atributeParse :: Parser (String,Jvalue)
atributeParse = do
  key <- stringLiteral
  (spaces)
  (char ':')
  value <- jsonValue
  return (key,value)

jsonAtribute :: Parser Jatribute
jsonAtribute = Jatribute <$> atributeParse
{-Fim do parse par atributos-}




 {-Parse para objetos-}

objectParse :: Parser [Jatribute]
objectParse = spaces*>(char '{' )*> spaces*> (jsonAtribute  `sepBy` (char ','))<*spaces <*(char '}')

jsonObject :: Parser Json   {-Deriva o objeto principal -}
jsonObject =  Json <$> objectParse

jsonObjectValue :: Parser Jvalue
jsonObjectValue = Jo <$> jsonObject {-Deriva valores para o atributo que tb podem ser json-}

{- Fim do parser para objetos-}

{-Parse para array-}
array :: Parser [Jvalue]
array = (char '[') *> (jsonValue `sepBy` (char ',')) <* (char ']')

jsonArray :: Parser Jvalue
jsonArray = Ja <$> array
{-Fim do parse para array-}

{-Parser para numeros-}

jsonDouble :: Parser Jvalue
jsonDouble = Jnum <$> double

{-fim do parser para numeros-}



{-FUNCOES DE ACESSO AOS ATRIBUTOS-}
getJsonAttributeValue :: Json -> String -> Maybe Jvalue
getJsonAttributeValue (Json atributos) atrName = if (length atributos == 0) then Nothing
                                              else if((getAttributeName (head atributos)) == atrName) 
                                                then Just (getAttributeValue (head atributos))
                                              else getJsonAttributeValue (Json (tail atributos)) atrName


--Auxiliares--
--Dado um atributo do Json, pega o valor dele.
getAttributeValue :: Jatribute -> Jvalue
getAttributeValue (Jatribute (name, value)) = value
--Dado um atributo do Json, pega sua key/nome dele
getAttributeName :: Jatribute -> String
getAttributeName (Jatribute (name, value)) = name
--Dado um Parser correto retorna seu valor
getParserValue (Right a) = a


--remove espaços em branco--

removeBlank :: [Char] -> [Char] -> [Char]
removeBlank [] res = res
removeBlank (x:xs) res = removeBlank xs (res ++ (if x == ' ' then "" else [x]))

trim :: [Char] -> [Char]
trim xs = removeBlank xs ""



{-Fim das funções de acesso-}

{-Utilitarios-}

--recebe um string e constroi um Json a partir dela--
jsonParse :: String -> Json
jsonParse str = getParserValue(parse jsonObject "erro" (trim str))

-- recebe string referente ao path e retorna um IO(jvalue) gerado de acordo com o arquivo--

jsonFileParser :: String -> IO(Jvalue)
jsonFileParser path = do
                      io <- (getJsonFile path)
                      return  ((getParserValue io))




-- metodo auxiliar faz o parse e retorna um IO (Either ParseError Jvalue)--
getJsonFile str  = parseFromFile jsonValue str

{-fim-}


{-Parser para null-}

jnull :: Parser String
jnull  = string "null"

jsonNull :: Parser Jvalue
jsonNull = jnull *> pure Jn 

{-fim-}




{-
    STRINGIFY
    Usage examples: 
    - stringifyJson (Json [Jatribute ("nome", Js "Pedro"), Jatribute ("idade", Jnum 20)])
    - stringifyJson (Json [Jatribute ("nome", Js "Pedro"), Jatribute ("info", Jo (Json [Jatribute ("idade", Jnum 20)])), Jatribute ("idade", Jnum 20)])
-}
stringifyJson :: Json -> String
stringifyJson (Json attributes) = "{" ++ (intercalate ", " (map (stringifyAttribute) (attributes))) ++ "}"  


--Auxiliar: Recebe um Jatribute em e retorna o seu formato correto em String
stringifyAttribute :: Jatribute -> String
stringifyAttribute (Jatribute (key, value)) = key ++ ": " ++ (stringifyValue value)

--Auxiliares: Recebem um Jvalue e retornam seu valor como em string, usando pattern matching
stringifyValue :: Jvalue -> String --Usage example: stringifyValue (Js "Hello")
stringifyValue (Js string) = string
stringifyValue (Jb True) = "true"
stringifyValue (Jb False) = "false"
stringifyValue (Jnum number) = (show number)
stringifyValue (Jn) = "null"
stringifyValue (Ja valuesArray) = "[" ++ (intercalate "," (map (stringifyValue) (valuesArray))) ++ "]"
stringifyValue (Jo json) = stringifyJson json 








