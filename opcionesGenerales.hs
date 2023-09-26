module OpcionesGenerales where
import Data.IORef
import Archivos
import Data.List (nubBy, intercalate)
import Data.Function (on)
import OpcionesOperativas

data Parqueo = Parqueo { 
    idParqueo :: String, 
    nombre :: String, 
    barrio :: String, 
    provincia :: String, 
    xCoord :: Double, 
    yCoord :: Double 
} deriving (Show)

convertirListaAListaDeParqueos :: [[String]] -> [Parqueo]
convertirListaAListaDeParqueos lista =
    [ Parqueo (head subLista) (subLista !! 1) (subLista !! 2) (subLista !! 3) (read (subLista !! 4) :: Double) (read (subLista !! 5) :: Double) | subLista <- lista ]

-- Función que calcula la distancia euclidiana entre dos puntos (x1, y1) y (x2, y2)
distanciaEuclidiana :: Double -> Double -> Double -> Double -> Double
distanciaEuclidiana x1 y1 x2 y2 = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- Función que elimina duplicados basados en el primer valor de cada sublista
eliminarRepetidosPorId :: [Parqueo] -> [Parqueo]
eliminarRepetidosPorId = nubBy (\parq1 parq2 -> idParqueo parq1 == idParqueo parq2)

-- SECCION DE USUARIOS 👇
-- SECCION DE bicicletas 👇
-- Función para encontrar el parqueo más cercano
encontrarParqueoCercano :: Parqueo -> [Parqueo] -> Parqueo
encontrarParqueoCercano usuarioCoord parqueos =
  let parqueoCercano = foldl1 (\acc parq ->
                                if distanciaEuclidiana (xCoord acc) (yCoord acc) (xCoord usuarioCoord) (yCoord usuarioCoord) <=
                                   distanciaEuclidiana (xCoord parq) (yCoord parq) (xCoord usuarioCoord) (yCoord usuarioCoord)
                                then acc
                                else parq)
                            parqueos
  in parqueoCercano

quitarCaracter :: String -> String
quitarCaracter cadena = filter (/= '\r') cadena

quitarCaracterTercerElemento :: [[String]] -> [[String]]
quitarCaracterTercerElemento lista = map (\[x, y, z] -> [x, y, quitarCaracter z]) lista


consultarBicicletasAux :: IO (String)
consultarBicicletasAux = do
    putStrLn "Ingrese la cordenada x: "
    x <- getLine
    putStrLn "Ingrese la cordenada y: "
    y <- getLine
    let parqueo2 = Parqueo "0" "0" "0" "0" (read x :: Double) (read y :: Double)
    parqueos <- cargarParqueosSistema
    let parqueosNuevos = convertirListaAListaDeParqueos parqueos
    let parqueoCercano = encontrarParqueoCercano parqueo2 parqueosNuevos
    let parqueoCercanoString = convertirListaAString [[idParqueo parqueoCercano, nombre parqueoCercano, barrio parqueoCercano, provincia parqueoCercano]]
    print parqueoCercanoString
    let idParqueoCercano = idParqueo parqueoCercano
    return idParqueoCercano

-- funcion de pruebas
cargarUsuariosAutomatico :: IO [[String]]
cargarUsuariosAutomatico = do
    datos <- leerArchivo "./data/prueba.csv"
    lista2 <- cargarUsuariosDesdeArchivo "./data/usuarios.txt"
    let lista = convertirStringALista datos
    let lista3 = lista ++ lista2
    let lista4 = eliminarRepetidosPorPrimerValor lista3
    let datos3 = convertirListaAString lista4
    print datos3
    escribirArchivo "./data/usuarios.txt" datos3
    return lista4



