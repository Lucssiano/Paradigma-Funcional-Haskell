import Data.List
import Text.Read.Lex (Number)
import Text.Show.Functions ()

{- 1. Postres
A) Modelar los postres. Un mismo postre puede tener muchos sabores, tiene un peso y se sirve a
cierta temperatura. Por ejemplo, un bizcocho borracho de fruta y crema de 100 gramos servido a 25°C. -}
data Postres = UnPostre
  { sabores :: [String],
    peso :: Float,
    temperatura :: Float
  }
  deriving (Show, Eq)

{- DATOS PARA PROBAR -}

postre1 :: Postres
postre1 = UnPostre ["frutilla", "menta"] 10 25

otro :: Postres
otro = UnPostre ["chocolate"] 150 0

tarta :: Postres
tarta = UnPostre ["manzana"] 50 0

bizcocho :: Postres
bizcocho = UnPostre ["alcohol", "fruta", "crema"] 100 25

{- --------------------------------------- -}

{- B) Modelar los hechizos, sabiendo que deberían poderse agregar más sin modificar el código
existente. Por ahora existen los siguientes: -}

type Hechizo = Postres -> Postres

modificarPeso :: (Float -> Float) -> Hechizo
modificarPeso modificador postre = postre {peso = modificador (peso postre)}

modificarTemperatura :: (Float -> Float) -> Hechizo
modificarTemperatura modificador postre = postre {temperatura = modificador (temperatura postre)}

cambioEnSabores :: String -> Hechizo
cambioEnSabores sabor postre = postre {sabores = sabor : sabores postre}

-- Incendio: calienta el postre 1 grado y lo hace perder 5% de su peso.
incendio :: Hechizo
incendio = modificarTemperatura (+ 1) . modificarPeso (* 0.95)

-- Immobulus: congela el postre, llevando su temperatura a 0.
immobulus :: Hechizo
immobulus = modificarTemperatura (* 0)

-- Wingardium Leviosa: levanta el postre en el aire y lo deja caer, lo que agrega a sus sabores el sabor “concentrado”. Además, pierde 10% de su peso.
wingardiumLeviosa :: Hechizo
wingardiumLeviosa = cambioEnSabores "concentrado" . modificarPeso (* 0.1)

-- Diffindo: Corta el postre, disminuyendo su peso en el porcentaje indicado.
diffindo :: Float -> Hechizo
diffindo porcentaje = modificarPeso (* (1 - porcentaje / 100))

-- Riddikulus: Requiere como información adicional un sabor y lo agrega a los sabores que tiene un postre, pero invertido.
riddikulus :: String -> Hechizo
riddikulus sabor = cambioEnSabores (reverse sabor)

-- Avada kedavra: Hace lo mismo que el immobulus pero además hace que el postre pierda todos sus sabores.
avadaKedavra :: Hechizo
avadaKedavra postre = immobulus (postre {sabores = []})

{- C) Dado un conjunto de postres en la mesa, saber si hacerles un determinado hechizo los dejará
listos (un postre está listo cuando pesa algo más que cero, tiene algún sabor y además no está
congelado).
Por ejemplo, si en la mesa está el bizcocho mencionado anteriormente y una tarta de melaza de
0 grados y 50 gramos, y les hago el hechizo incendio, quedan listos, pero si les hago el hechizo
riddikulus con el sabor “nomil” no, porque la tarta sigue congelada -}
postresListos :: [Postres] -> [Postres]
postresListos = filter estaListo

estaListo :: Postres -> Bool
estaListo postre = peso postre > 0 && sabores postre /= [] && temperatura postre > 0

postresListosDespHechizo :: Hechizo -> [Postres] -> [Postres]
postresListosDespHechizo hechizo = postresListos . map hechizo

{- D) Dado un conjunto de postres en la mesa, conocer el peso promedio de los postres listos. -}
pesoPromedio :: [Postres] -> Float
pesoPromedio = promedio . map peso . postresListos

promedio :: [Float] -> Float
promedio lista = sum lista / fromIntegral (length lista)

{- 2. Magos
De un mago se conocen sus hechizos aprendidos y la cantidad de horrorcruxes que tiene. -}

data Mago = UnMago
  { hechizosAprendidos :: [Hechizo],
    cantHorrocruxes :: Int
  }
  deriving (Show)

{- DATOS PARA PROBAR -}
lucho :: Mago
lucho = UnMago [avadaKedavra, wingardiumLeviosa] 10

dante :: Mago
dante = UnMago [immobulus, incendio] 20

iñaki :: Mago
iñaki = UnMago [immobulus] 5

{- --------------------------------------- -}

{- A) Hacer que un mago asista a la clase de defensa contra las cocinas oscuras y practique con un
hechizo sobre un postre (se espera obtener el mago). Cuando un mago practica con un hechizo,
lo agrega a sus hechizos aprendidos.
Además si el resultado de usar el hechizo en el postre es el mismo que aplicarle “avada
kedavra” al postre, entonces suma un horrorcrux. -}
practicarHechizo :: Hechizo -> Postres -> Mago -> Mago
practicarHechizo hechizo postre = sumarHorrocruxSiCumple hechizo postre . agregarHechizo hechizo

agregarHechizo :: Hechizo -> Mago -> Mago
agregarHechizo hechizo mago = mago {hechizosAprendidos = hechizo : hechizosAprendidos mago}

sumarHorrocruxSiCumple :: Hechizo -> Postres -> Mago -> Mago
sumarHorrocruxSiCumple hechizo postre mago
  | hechizo postre == avadaKedavra postre = sumarHorrocrux mago
  | otherwise = mago

sumarHorrocrux :: Mago -> Mago
sumarHorrocrux mago = mago {cantHorrocruxes = cantHorrocruxes mago + 1}

{- B) Dado un postre y un mago obtener su mejor hechizo, que es aquel de sus hechizos que deja al
postre con más cantidad de sabores luego de usarlo. -}
mejorHechizo :: Postres -> Mago -> Hechizo
mejorHechizo postre = masCantidadDeSabores postre . hechizosAprendidos

cantSabores :: Postres -> Int
cantSabores = length . sabores

masCantidadDeSabores :: Postres -> [Hechizo] -> Hechizo
-- Supongo que no existen los magos que no sepan hechizos
masCantidadDeSabores _ [a] = a
masCantidadDeSabores postre (h1 : h2 : hs)
  | (cantSabores . h1) postre > (cantSabores . h2) postre = masCantidadDeSabores postre (h1 : hs)
  | otherwise = masCantidadDeSabores postre (h2 : hs)

{- 3. Infinita Magia
A) Construir una lista infinita de postres, y construir un mago con infinitos hechizos.-}
listaInfinitaDePostres :: [Postres]
listaInfinitaDePostres = tarta : listaInfinitaDePostres

infinitosHechizos :: [Hechizo]
infinitosHechizos = avadaKedavra : infinitosHechizos

magoInfinitosHechizos :: Mago
magoInfinitosHechizos = UnMago infinitosHechizos 10

{- B) Suponiendo que hay una mesa con infinitos postres, y pregunto si algún hechizo los deja listos
¿Existe alguna consulta que pueda hacer para que me sepa dar una respuesta? Justificar
conceptualmente.-}

{- C) Suponiendo que un mago tiene infinitos hechizos ¿Existe algún caso en el que se puede
encontrar al mejor hechizo? Justificar conceptualmente. -}
