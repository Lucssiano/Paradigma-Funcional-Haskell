{- FUNCIONES DE ORDEN SUPERIOR -}

data Superficie = UnaSuperficie
  { situacion :: String,
    valor :: Float
  }
  deriving (Show)

hacerDosVeces :: AccionHacerDosVeces
hacerDosVeces accion elemento = accion (accion elemento)

type AccionHacerDosVeces = (Superficie -> Superficie) -> Superficie -> Superficie {- Resume la definición de los tipos de las funciones por si tengo que usarlo más de una vez -}

mecha :: Superficie -> Superficie
mecha superficie = superficie {situacion = "Agujero " ++ situacion superficie}

lija :: Superficie -> Superficie
lija superficie = superficie {situacion = "lisa", valor = valor superficie + 1}

destornillador :: Superficie -> Superficie
destornillador algo = algo

destornillador2 :: String -> String
destornillador2 "tornillo" = "tornilloAjustado"
destornillador2 "tornilloAjustado" = "tornilloFlojo"

{- map, filter, all ,any, concatMap , zipWith, foldl (FUNCIONES DE ORDEN SUPERIOR) -}
{- flip, $ , . -}

{- Map -}
-- map (^2) [3,10,5] = [9,100,25]
-- map (2^) [3,10,5] = [8,1024,32]
-- map even [3,10,5] = [False,True,False]

{- Pensar un ejemplo -}
data Persona = UnaPersona
  { nombre :: String,
    cursos :: [String]
  }
  deriving (Show, Eq)

luciano :: Persona
luciano = UnaPersona "Luciano" ["Python", "Javascript", "C++"]

{- Filter -}
numerosPares :: [Int] -> [Int]
numerosPares = filter even

{- all -}

{- any -}

{- concatMap -}

{- zipWith -}
-- zipWith (+) [3,5,4] [2,1,5,8] = [5,6,9]

{- foldl -}

{- flip -}

{- . -}

{- find -}

{- concat (NO ES DE ORDEN SUPERIOR) -}
-- concat [[1,2],[5,6,7], [100,300]] = [[1,2,5,6,7,100,300]