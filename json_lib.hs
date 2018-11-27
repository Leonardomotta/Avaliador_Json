data Json = Json {
                    chave :: [String],
                    valor :: [String] {- @TODO modificar para um tipo generico-}
                            } deriving Show

{-retorna o conjunto de chaves-}
chaves (Json chave _) = chave
{-retorna o conjuto de valores-}
valores (Json _ valor) = valor

{-retorna o valor do elemento com determinada chave-}

value (Json chaves valores) chave = elemByIndex valores (indexBykey chaves chave)


{-retorna o elemento pelo indice -}

elemByIndex array 0 = head array
elemByIndex array x = elemByIndex (tail array) (x-1)

{-retorna o indice do elemnto-}
indexBykey array a = indexBykeyAux array a 0
indexBykeyAux array a x = if (head array == a) then x
                          else indexBykeyAux (tail array) a (x+1)

{-stringfy-}
{-@TODO-}
{- deve tratar o atributo caso seja diferente de string -}
stringfy (Json chaves valores) = "{" ++make chaves valores++ "}"
make::[String] -> [String] -> String
make [] [] = ""
make a b = head a ++ ":" ++ head b ++ x (tail a)(tail b) ++ make (tail a) (tail b)
x::[String] -> [String] -> String
x [] [] = " "
x _ _ = ","


