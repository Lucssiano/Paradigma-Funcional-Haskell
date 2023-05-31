import Data.List
import Text.Show.Functions ()

type Material = String

data Receta = UnaReceta
  { listaMateriales :: [Material],
    tiempoConstruccion :: Int,
    resultado :: Material
  }
  deriving (Show)

data Jugador = UnJugador
  { nombre :: String,
    puntaje :: Int,
    inventario :: [Material]
  }
  deriving (Show)

fogata, polloAsado, sueter :: Material
fogata = "fogata"
polloAsado = "pollo asado"
sueter = "sueter"

recetaFogata :: Receta
recetaFogata = UnaReceta ["madera", "fosforo"] 100 fogata

recetaPolloAsado :: Receta
recetaPolloAsado = UnaReceta [fogata, "pollo"] 300 polloAsado

recetaSueter :: Receta
recetaSueter = UnaReceta ["lana", "agujas", "tintura"] 600 sueter

lucho :: Jugador
lucho = UnJugador "lucho" 1000 [sueter, fogata, "pollo", "pollo", "madera", "fosforo"]

{- 1) Hacer las funciones necesarias para que un jugador craftee un nuevo objeto -}
intentarCraftearObjeto :: Receta -> Jugador -> Jugador
intentarCraftearObjeto receta jugador
  | tieneMaterialesRequeridos receta jugador = craftearObjeto receta jugador
  | otherwise = alterarPuntaje (-100) jugador

tieneMaterialesRequeridos :: Receta -> Jugador -> Bool
tieneMaterialesRequeridos receta jugador = all (\x -> elem x (inventario jugador)) (listaMateriales receta) -- Cada elemento x del inventario del jugador debe estar en la lista de materiales de la receta

craftearObjeto :: Receta -> Jugador -> Jugador
craftearObjeto receta = alterarPuntaje (10 * tiempoConstruccion receta) . actualizarInventario receta

actualizarInventario :: Receta -> Jugador -> Jugador
actualizarInventario receta jugador = jugador {inventario = foldr quitarUnaVez (inventario jugador) (listaMateriales receta) ++ [resultado receta]} -- falta agregar aquellos elementos que estan en la receta pero estan mas de una vez entonces no se sacan del inventario
-- lista a foldear = (listaMateriales receta)
-- operacion = quitarUnaVez
-- semilla = valor donde se va a aplicar lo foldeado (inventario jugador) (la semilla es lo ultimo que opera, en cambio en foldl es lo primero)

quitarUnaVez :: Material -> [Material] -> [Material]
quitarUnaVez _ [] = []
quitarUnaVez materialInventario (materialReceta : materialesReceta)
  | materialInventario == materialReceta = materialesReceta
  | otherwise = materialReceta : quitarUnaVez materialInventario materialesReceta

alterarPuntaje :: Int -> Jugador -> Jugador
alterarPuntaje modificador jugador = jugador {puntaje = puntaje jugador + modificador}

{- 2) Dado un personaje y una lista de recetas: -}
-- Encontrar los objetos que podría craftear un jugador y que le permitirían como mínimo duplicar su puntaje.
objetosDuplicanPuntaje :: Jugador -> [Receta] -> [Receta]
objetosDuplicanPuntaje jugador = filter (duplicaPuntaje jugador)

duplicaPuntaje :: Jugador -> Receta -> Bool
duplicaPuntaje jugador receta = puntaje (intentarCraftearObjeto receta jugador) >= (2 * puntaje jugador)

-- Hacer que un personaje craftee sucesivamente todos los objetos indicados en la lista de recetas.
craftearTodosLosObjetos :: Jugador -> [Receta] -> Jugador
craftearTodosLosObjetos = foldr intentarCraftearObjeto

-- Averiguar si logra quedar con más puntos en caso de craftearlos a todos sucesivamente en el orden indicado o al revés.
quedaConMasPuntosEnOrdenIndicado :: Jugador -> [Receta] -> Bool -- siempre da false???
quedaConMasPuntosEnOrdenIndicado jugador listaRecetas = puntaje (craftearTodosLosObjetos jugador listaRecetas) > puntaje (craftearTodosLosObjetos jugador (reverse listaRecetas))

-- 3) Mine
data Bioma = UnBioma
  { materialesDelBioma :: [Material],
    materialNecesario :: Material
  }

type Herramienta = [Material] -> Material

hielo, iglues, lobos :: Material
hielo = "hielo"
iglues = "iglues"
lobos = "lobos"

biomaArtico :: Bioma
biomaArtico = UnBioma [hielo, iglues, lobos] sueter

hacha :: Herramienta
hacha = last

espada :: Herramienta
espada = head

pico :: Int -> Herramienta
pico posicion matBioma = matBioma !! posicion

-- 2) Definir las herramientas mencionadas y agregar dos nuevas. Mostrar ejemplos de uso. Hacerlo de manera que agregar en el futuro otras herramientas no implique modificar la función minar.
pala :: Herramienta
pala = reverse . espada -- reverse head

hoz :: Herramienta
hoz = reverse . hacha -- reverse last

-- 1) Hacer una función minar, que dada una herramienta, un personaje y un bioma, permita obtener cómo queda el personaje.
minar :: Herramienta -> Bioma -> Jugador -> Jugador
minar herramienta bioma jugador
  | tieneElementoRequerido jugador bioma = agregarMaterialDeBioma herramienta bioma (alterarPuntaje 10 jugador)
  | otherwise = jugador

agregarMaterialDeBioma :: Herramienta -> Bioma -> Jugador -> Jugador
agregarMaterialDeBioma herramienta bioma jugador = jugador {inventario = inventario jugador ++ [herramienta (materialesDelBioma bioma)]}

tieneElementoRequerido :: Jugador -> Bioma -> Bool
tieneElementoRequerido jugador bioma = elem (materialNecesario bioma) (inventario jugador)

-- 3) Utilizando la función composición, usar una que permita obtener un material del medio del conjunto de materiales
materialMedioConjunto :: Herramienta
materialMedioConjunto conjMateriales = last . take (length conjMateriales `div` 2) $ conjMateriales

{- matBioma !! (length matBioma `div` 2) -}

-- 4) Utilizando una expresión lambda, inventar una nueva herramienta, diferente a las anteriores
-- herramientaConLambda :: Herramienta
-- herramientaConLambda matBioma
--   |
--   | otherwise =
-- ¿Qué pasa al intentar minar en un bioma con infinitos materiales? Mostrar ejemplos donde con diferentes herramientas o personajes sucedan diferentes cosas. Justificar. -}
listaInfinitaMateriales :: Material -> [Material]
listaInfinitaMateriales = repeat

biomaInfinito :: Bioma
biomaInfinito = UnBioma (listaInfinitaMateriales fogata) fogata
