# Avaliador de Json


#### Essa biblioteca foi produzida na disciplina de Programação Funcional, do curso de Ciência da Computação - UFCG. Ela é essencialmente um Parser de Json para Haskell e vice-versa. 


#### Funções disponíveis:

* Recebe um tipo de dados Json, o nome do atributo que se deseja encontrar e retorna: Just {Valor do atributo} (se ele existe) ou Nothing se o atributo não existe:
```
getJsonAttributeValue :: Json -> String -> Maybe Jvalue
```

*  Recebe uma String referente ao path do arquivo que representa o Json e retorna um IO(Jvalue) gerado de acordo com o arquivo (Currently inconsistent)
```
jsonFileParser :: String -> IO(Jvalue)
```

* Recebe um String (que representa o Json) e retorna um tipo de dado Json
```
jsonParse :: String -> Json
```

* Realiza o stringify do tipo de dado Json
```
stringifyJson :: Json -> String
```


