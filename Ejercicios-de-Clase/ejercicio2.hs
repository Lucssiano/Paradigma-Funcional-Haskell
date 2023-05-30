{- Impuesto automotor
Dado un conjunto de personas con sus respecivos autos, queremos averiguar cuántos millonarios honestos hay.

Tenemos información sobre los autos:

data Auto = UnAuto {
    marca::String,
    modelo:: Int,
    kilometraje:: Float } deriving (Show, Eq)Algunos ejemplos de autos

ferrari,fitito, reno :: Auto
ferrari = UnAuto "ferrari" 1990 100
fitito = UnAuto "fiat" 1960 1000000
reno = UnAuto "renault" 2023 0

Se debe agregar información sobre las personas, asumiendo que de cada una sabemos su nombre, si paga sus impuestos y el conjunto de autos que tiene registrados a su nombre.
- Se considera millonario a una persona si la sumatoria del valor de sus autos es mayor a 1000000
- Se considera honesto a quien paga sus impuestos
- El valor de un auto se calcula con una extraña fórmula en la que interviene el kilometraje y modelo del año, con un incremento si se trata de una marca importada. (Inventarla!!)
Se sabe cuáles son las marcas importadas. -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}

data Auto = UnAuto
  { marca :: String,
    modelo :: Int,
    kilometraje :: Float
  }
  deriving (Show, Eq)

ferrari :: Auto
ferrari = UnAuto "ferrari" 1990 100

fitito :: Auto
fitito = UnAuto "fiat" 1960 1000000

reno :: Auto
reno = UnAuto "renault" 2023 0

data Persona = UnaPersona
  { nombre :: String,
    pagaImpuestos :: Bool,
    autos :: [Auto]
  }
  deriving (Show, Eq)

marcos :: Persona
marcos = UnaPersona "Marcos" True [ferrari, reno]

lucas :: Persona
lucas = UnaPersona "Lucas" False [fitito]

luciano :: Persona
luciano = UnaPersona "Luciano" True [fitito, reno, ferrari]

fede :: Persona
fede = UnaPersona "Fede" False [reno]

{- El valor de un auto se calcula con una extraña fórmula en la que interviene el kilometraje y modelo del año,
con un incremento si se trata de una marca importada. (Inventarla!!)
Se sabe cuáles son las marcas importadas. -}
importe :: Auto -> Float
importe auto = 0.1 * kilometraje auto + 10 * fromIntegral (modelo auto) + adicional (marca auto)

adicional :: String -> Float
adicional marca
  | elem marca marcasImportadas = 1000
  | otherwise = 0

-- Se considera millonario a una persona si la sumatoria del valor de sus autos es mayor a 1000000
esMillonario :: Persona -> Bool
esMillonario persona = sum (map importe (autos persona)) > 100000

-- Si quiero saber cuantos millonarios hay, puedo hacer:
cuantosMillonarios :: [Persona] -> Int
cuantosMillonarios personas = length (filter esMillonario personas)

marcasImportadas :: [String]
marcasImportadas = ["ferrari", "bmw"]

esHonesto :: Persona -> Bool
esHonesto = pagaImpuestos
