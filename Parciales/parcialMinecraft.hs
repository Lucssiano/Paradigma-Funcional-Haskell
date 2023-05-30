import Data.List
import Text.Show.Functions ()

type Material = String

data Receta = UnaReceta
  { listaMateriales :: [Material],
    tiempoConstruccion :: Int,
    resultado :: Material
  }

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
recetaFogata = UnaReceta ["madera", "fosforo"] 10 fogata

recetaPolloAsado :: Receta
recetaPolloAsado = UnaReceta [fogata, "pollo"] 300 polloAsado

recetaSueter :: Receta
recetaSueter = UnaReceta ["lana", "agujas", "tintura"] 600 sueter

lucho :: Jugador
lucho = UnJugador "lucho" 1000 [sueter, fogata, "pollo", "pollo"]

{- prueba :: Receta -> Jugador -> [Material]
prueba receta jugador = foldl eliminarSiEsIgual (inventario jugador) (listaMateriales receta)

eliminarSiEsIgual :: [Material] -> Material -> [Material]
eliminarSiEsIgual lista material = filter (/= material) lista -}

{- 1- Hacer las funciones necesarias para que un jugador craftee un nuevo objeto -}
intentarCraftearObjeto :: Receta -> Jugador -> Jugador
intentarCraftearObjeto receta jugador
  | tieneMaterialesRequeridos receta jugador = craftearObjeto receta jugador
  | otherwise = alterarPuntaje (-100) jugador

tieneMaterialesRequeridos :: Receta -> Jugador -> Bool
tieneMaterialesRequeridos receta jugador = all (\x -> elem x (inventario jugador)) (listaMateriales receta) -- Cada elemento x del inventario del jugador debe estar en la lista de materiales de la receta

craftearObjeto :: Receta -> Jugador -> Jugador
craftearObjeto receta = alterarPuntaje (10 * tiempoConstruccion receta) . actualizarInventario receta

actualizarInventario :: Receta -> Jugador -> Jugador
actualizarInventario receta jugador = jugador {inventario = elemInvNoRec receta jugador ++ [resultado receta]} -- falta agregar aquellos elementos que estan en la receta pero estan mas de una vez entonces no se sacan del inventario

alterarPuntaje :: Int -> Jugador -> Jugador
alterarPuntaje modificador jugador = jugador {puntaje = puntaje jugador + modificador}

elemInvNoRec :: Receta -> Jugador -> [Material] -- aquellos elementos que estan en el inventario pero no en la lista de materiales necesarios para la receta
elemInvNoRec receta jugador = filter (\x -> notElem x (listaMateriales receta)) (inventario jugador)

{- Dado un personaje y una lista de recetas: -}
-- Encontrar los objetos que podría craftear un jugador y que le permitirían como mínimo duplicar su puntaje.
-- objetosDuplicanPuntaje ::  Jugador -> [Receta] -> [Receta]
-- objetosDuplicanPuntaje jugador recetas =

-- Hacer que un personaje craftee sucesivamente todos los objetos indicados en la lista de recetas.
-- craftearTodosLosObjetos :: Jugador -> [Receta] -> Jugador
-- craftearTodosLosObjetos jugador recetas

-- Averiguar si logra quedar con más puntos en caso de craftearlos a todos sucesivamente en el orden indicado o al revés.
-- uso de foldl con flip $ y foldr con $