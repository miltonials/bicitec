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

data Alquiler = Alquiler {
    idAlquiler :: String,
    idBicicleta :: String,
    idUsuario :: String,
    idParqueoSalida :: String,
    idParqueoLlegada :: String,
    estado :: String
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



-- SECCIÓN ALQUILER 👇
{-
El sistema le solicitará al usuario que indique: usuario (cedula), un parqueo de salida y el parqueo de llegada. Posteriormente le mostrará las bicicletas disponibles (identificador y tipo) en el parqueo de salida.
El usuario indicará el identificador de la bicicleta y se le generará un identificador de alquiler por parte del
sistema. La bicicleta cambia de ubicación a “tránsito”, se debe generar una estructura de datos con la información
del alquiler (al menos salida, destino, código de la bicicleta, código del alquiler), este quedará “activo”.
-}

alquilar :: IO()
alquilar = do 
    putStrLn "Ingrese su cedula: "
    cedula <- getLine
    datosUsuario <- readFile "./data/usuarios.csv"
    let usuarios = convertirStringALista datosUsuario
    let usuario = filter(\x -> x!!0 == cedula) usuarios
    if usuario == [] then do
        putStrLn "El usuario no existe"
    else do
        putStrLn "Ingrese el parqueo de salida: "
        nombreParqueoSalida <- getLine
        parqueos <- cargarParqueosSistema
        let parqueoSalida = filter(\x -> x!!1 == nombreParqueoSalida) parqueos
        if parqueoSalida == [] then do
            putStrLn "El parqueo no existe"
        else do
            putStrLn "Ingrese el parqueo de llegada: "
            nombreParqueoLlegada <- getLine
            let parqueoLlegada = filter(\x -> x!!1 == nombreParqueoLlegada) parqueos
            if parqueoLlegada == [] then do
                putStrLn "El parqueo no existe"
            else do
                bicicletas <- cargarBicicletas
                let bicicletasEnParqueo = filter (\x -> x !! 2 == head parqueoSalida !! 0) bicicletas
                mostrarBicicletas bicicletasEnParqueo
                if bicicletasEnParqueo == [] then do
                    putStrLn "No hay bicicletas disponibles en el parqueo seleccionado. 😭"
                else do
                    putStrLn "Ingrese el id de la bicicleta: "
                    idBicicleta <- getLine
                    let bicicletaFiltrada = filter(\x -> x !! 0 == idBicicleta) bicicletasEnParqueo
                    if bicicletaFiltrada == [] then do
                        putStrLn "La bicicleta no existe"
                    else do
                        let bicicleta = head bicicletaFiltrada
                        putStrLn "alquilando bicicleta"
                        alquileres <- cargarAlquileres
                        print alquileres -- para cerrar el archivo en memoria.
                        putStrLn "\ESC[2J" -- limpiar consola de lo que se imprimió.
                        --id,idBici,idUsuario,idParqueoSalida,idParqueoLlegada
                        let idNuevoAlquiler = show (length alquileres + 1)
                        let nuevoAlquiler = [[idNuevoAlquiler, idBicicleta, cedula, head parqueoSalida !! 0, head parqueoLlegada !! 0, "1"]]
                        let datosNuevoAlquiler = convertirListaAString nuevoAlquiler
                        let bicicletasGeneral = filter(\x -> x !! 0 /= idBicicleta) bicicletas
                        let listaBicicletasActualizadas = bicicletasGeneral ++ [[bicicleta !! 0, bicicleta !! 1, "transito"]]
                        let datosBicicletasActualizadas = convertirListaAString listaBicicletasActualizadas
                        agregarArchivo "./data/alquileres.csv" datosNuevoAlquiler
                        escribirArchivo "./data/bicicletas.csv" datosBicicletasActualizadas
                        putStrLn "Bicicleta alquilada exitosamente. 🥳"
    putStr "Presione enter para continuar"
    opcion <- getLine
    putStrLn "\ESC[2J"
    return ()