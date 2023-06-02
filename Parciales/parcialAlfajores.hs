import Data.List (isInfixOf)
import Text.Show.Functions ()

{- Parte 1: ¿Qué es un alfajor? -}

data Alfajor = UnAlfajor
  { capasDeRelleno :: [Relleno],
    peso :: Float,
    dulzor :: Float,
    nombre :: String
  }
  deriving (Show, Eq)

data Relleno = UnRelleno
  { sabor :: String,
    precio :: Int
  }
  deriving (Show, Eq)

dulceDeLeche, mousse, fruta :: Relleno
dulceDeLeche = UnRelleno "Dulce de Leche" 12
mousse = UnRelleno "Mousse" 15
fruta = UnRelleno "Fruta" 10

-- a)
jorgito :: Alfajor
jorgito = UnAlfajor [dulceDeLeche] 80 8 "Jorgito"

havanna :: Alfajor
havanna = UnAlfajor [mousse, mousse] 60 12 "Havanna"

capitanDelEspacio :: Alfajor
capitanDelEspacio = UnAlfajor [dulceDeLeche] 40 12 "Capitan Del Espacio"

-- b)
coeficienteDeDulzor :: Alfajor -> Float -- i)
coeficienteDeDulzor alfajor = dulzor alfajor / peso alfajor

-- precioAlfajor :: Alfajor -> Float -- ii)
-- precioAlfajor alfajor = 2 * peso alfajor + sum precioRellenos

-- precioRellenos :: Alfajor -> Float
-- precioRellenos alfajor =

esPotable :: Alfajor -> Bool -- iii)
esPotable alfajor = length (capasDeRelleno alfajor) > 0 && todasLasCapasDelMismoSabor alfajor && coeficienteDeDulzor alfajor >= 0.1

todasLasCapasDelMismoSabor :: Alfajor -> Bool
todasLasCapasDelMismoSabor alfajor = all (\x -> x == head (capasDeRelleno alfajor)) (capasDeRelleno alfajor)

{- Parte 2: Escalabilidad Vertical -}
abaratarAlfajor :: Alfajor -> Alfajor -- a)
abaratarAlfajor = reducirPeso 10 . reducirDulzor 7

reducirPeso :: Float -> Alfajor -> Alfajor
reducirPeso nro alfajor
  | (peso alfajor - nro) < 5 = alfajor {peso = 5.0}
  | otherwise = alfajor {peso = peso alfajor - nro}

reducirDulzor :: Float -> Alfajor -> Alfajor
reducirDulzor nro alfajor = alfajor {dulzor = dulzor alfajor - nro}

renombrarAlfajor :: String -> Alfajor -> Alfajor -- b)
renombrarAlfajor nuevoNombre alfajor = alfajor {nombre = nuevoNombre}

agregarCapaDeRelleno :: Relleno -> Alfajor -> Alfajor -- c)
agregarCapaDeRelleno capa alfajor = alfajor {capasDeRelleno = capa : capasDeRelleno alfajor} -- arreglar

hacerAlfajorPremium :: Alfajor -> Alfajor -- d)
hacerAlfajorPremium alfajor
  | not (esPotable alfajor) = alfajor
  | otherwise = renombrarAlfajor (nombre alfajor ++ " premium") (agregarCapaDeRelleno (head (capasDeRelleno alfajor)) alfajor) -- arreglar

hacerPremiumCiertoGrado :: Int -> Alfajor -> Alfajor -- e)
hacerPremiumCiertoGrado grado alfajor
  | grado == 0 = error "Debe indicar un grado > 0"
  | grado == 1 = hacerAlfajorPremium alfajor
  | otherwise = hacerPremiumCiertoGrado (grado - 1) (hacerAlfajorPremium alfajor)

-- f) Modelar alfajores:
jorgitito :: Alfajor -> Alfajor -- i)
jorgitito = abaratarAlfajor . renombrarAlfajor "Jorgitito"

jorgelin :: Alfajor -> Alfajor -- ii)
jorgelin = agregarCapaDeRelleno dulceDeLeche . renombrarAlfajor "Jorgelin"

capitanDelEspacioCostaACosta :: Alfajor -> Alfajor -- iii)
capitanDelEspacioCostaACosta = abaratarAlfajor . hacerPremiumCiertoGrado 4 . renombrarAlfajor "Capitan del espacio costa a costa"

{- Parte 3: Clientes del kiosco -}
type Criterio = Alfajor -> Bool

data Cliente = UnCliente
  { nombreCliente :: String,
    dineroDisponible :: Int,
    alfajoresComprados :: [Alfajor],
    gustosPersonales :: [Criterio]
  }
  deriving (Show)

-- a) Modelar clientes
emi :: Cliente -- i)
emi = UnCliente "Emi" 120 [] [tieneNombre "Capitan del Espacio"]

tieneNombre :: String -> Criterio
tieneNombre nombreABuscar alfajor = nombreABuscar `isInfixOf` nombre alfajor

tomi :: Cliente -- ii)
tomi = UnCliente "Tomi" 100 [] [tieneNombre "premium", tieneCoeficienteDulzorMayorA 0.15]

tieneCoeficienteDulzorMayorA :: Float -> Criterio
tieneCoeficienteDulzorMayorA coeficiente alfajor = coeficienteDeDulzor alfajor > coeficiente

dante :: Cliente -- iii)
dante = UnCliente "Dante" 200 [] [noTieneSaborDe dulceDeLeche, pretencioso]

pretencioso :: Criterio
pretencioso alfajor = not (esPotable alfajor)

noTieneSaborDe :: Relleno -> Criterio
noTieneSaborDe gusto alfajor = gusto `notElem` capasDeRelleno alfajor

juan :: Cliente -- iv)
juan = UnCliente "Juan" 500 [] [tieneCoeficienteDulzorMayorA 0.15, tieneNombre "Jorgito", pretencioso, noTieneSaborDe mousse]

-- indicar, dada una lista de alfajores, cuáles le gustan a cierto cliente.
alfajoresQueLeGustan :: Cliente -> [Alfajor] -> [Alfajor]
alfajoresQueLeGustan cliente = filter (leGusta cliente)

leGusta :: Cliente -> Alfajor -> Bool
leGusta cliente alfajor = all (\criterio -> criterio alfajor) (gustosPersonales cliente)

-- comprarAlfajor :: Alfajor -> Cliente -> Cliente
-- comprarAlfajor alfajor cliente = cliente {alfajores = alfajores cliente ++ [calculoAlfajoresAComprar alfajor cliente]}

-- calculoAlfajoresAComprar :: Alfajor -> Cliente -> [Alfajor]
-- calculoAlfajoresAComprar alfajor cliente =