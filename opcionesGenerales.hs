module OpcionesGenerales where
import Archivos
import Data.List (nubBy, intercalate)
import OpcionesOperativas
import Text.Read (readMaybe)

{-
    @function esDouble
    @description verifica que un string sea de tipo numérico double
    @params {String}
    @
-}
esDouble :: String -> Bool
esDouble dato =
    case readMaybe dato :: Maybe Double of
        Just _ -> True
        Nothing -> False

{-
    @constructor Parqueo
    @params {String} idParqueo
    @params {String} nombre
    @params {String} barrio
    @params {String} provincia
    @params {Double} xCoord
    @params {Double} yCoord
    @deriving Show
-}
data Parqueo = Parqueo { 
    idParqueo :: String, 
    nombre :: String, 
    barrio :: String, 
    provincia :: String, 
    xCoord :: Double, 
    yCoord :: Double 
} deriving (Show)

{-
    @constructor Alquiler
    @params {String} idAlquiler
    @params {String} idBicicleta
    @params {String} idUsuario
    @params {String} idParqueoSalida
    @params {String} idParqueoLlegada
    @params {String} estado
    @deriving Show
-}
data Alquiler = Alquiler {
    idAlquiler :: String,
    idBicicleta :: String,
    idUsuario :: String,
    idParqueoSalida :: String,
    idParqueoLlegada :: String,
    estado :: String
} deriving (Show)

{-
@function convertirListaAListaDeParqueos
@description Convierte una lista de listas de strings a una lista de parqueos
@params {[[String]]} lista
@return {[Parqueo]} lista de parqueos
-}
convertirListaAListaDeParqueos :: [[String]] -> [Parqueo]
convertirListaAListaDeParqueos lista =
    [ Parqueo (head subLista) (subLista !! 1) (subLista !! 2) (subLista !! 3) (read (subLista !! 4) :: Double) (read (subLista !! 5) :: Double) | subLista <- lista ]

{-
@function distanciaEuclidiana
@description Calcula la distancia euclidiana entre dos puntos
@params {Double} x1
@params {Double} y1
@params {Double} x2
@params {Double} y2
@return {Double} distancia euclidiana
-}
distanciaEuclidiana :: Double -> Double -> Double -> Double -> Double
distanciaEuclidiana x1 y1 x2 y2 = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

{-
@function eliminarRepetidosPorId
@description Elimina los elementos repetidos de una lista de parqueos
@params {[Parqueo]} lista de parqueos
@return {[Parqueo]} lista de parqueos sin elementos repetidos
-}
eliminarRepetidosPorId :: [Parqueo] -> [Parqueo]
eliminarRepetidosPorId = nubBy (\parq1 parq2 -> idParqueo parq1 == idParqueo parq2)

{-
@function encontrarParqueoCercano
@description Encuentra el parqueo más cercano a las coordenadas del usuario
@params {Parqueo} usuarioCoord
@params {[Parqueo]} parqueos
@return {Parqueo} parqueo más cercano
-}
encontrarParqueoCercano :: Parqueo -> [Parqueo] -> Parqueo
encontrarParqueoCercano usuarioCoord parqueos =
  let parqueoCercano = foldl1 (\acc parq ->
                                if distanciaEuclidiana (xCoord acc) (yCoord acc) (xCoord usuarioCoord) (yCoord usuarioCoord) <=
                                   distanciaEuclidiana (xCoord parq) (yCoord parq) (xCoord usuarioCoord) (yCoord usuarioCoord)
                                then acc
                                else parq)
                            parqueos
  in parqueoCercano

{-
@function quitarCaracter
@description Quita el caracter \r de una cadena de texto
@params {String} cadena
@return {String} cadena sin el caracter \r
-}
quitarCaracter :: String -> String
quitarCaracter cadena = filter (/= '\r') cadena

{-
@function quitarCaracterTercerElemento
@description Quita el caracter \r del tercer elemento de una lista de listas de strings
@params {[[String]]} lista
@return {[[String]]} lista sin el caracter \r en el tercer elemento
-}
quitarCaracterTercerElemento :: [[String]] -> [[String]]
quitarCaracterTercerElemento lista = map (\[x, y, z] -> [x, y, quitarCaracter z]) lista

{-
@function quitarCaracterQuintaColumna
@description Quita el caracter \r de la quinta columna de una lista de listas de strings
@params {[[String]]} lista
@return {[[String]]} lista sin el caracter \r en la quinta columna
-}
quitarCaracterQuintaColumna :: [[String]] -> [[String]]
quitarCaracterQuintaColumna lista = map (\[x, y, z, w, a] -> [x, y, z, w, quitarCaracter a]) lista

{-
@function quitarCaracterSextaColumna
@description Quita el caracter \r de la sexta columna de una lista de listas de strings
@params {[[String]]} lista
@return {[[String]]} lista sin el caracter \r en la sexta columna
-}
quitarCaracterSextaColumna :: [[String]] -> [[String]]
quitarCaracterSextaColumna lista = map (\[x, y, z, w, a, b] -> [x, y, z, w, a, quitarCaracter b]) lista

{-
@function consultarBicicletaAux
@description Consulta la bicicleta más cercana a las coordenadas del usuario
@params {Parqueo} usuarioCoord
@params {[Parqueo]} parqueos
@return {String} id de la bicicleta más cercana
-}
consultarBicicletasAux :: IO (String)
consultarBicicletasAux = do
    putStrLn "Ingrese la cordenada x: "
    x <- getLine
    putStrLn "Ingrese la cordenada y: "
    y <- getLine
    --validar que se ingrese un numero double puede ser positivo o negativo
    -- if ((read x :: Double) < 0 && (read y :: Double) < 0)  || ((read x :: Double) > 0 && (read y :: Double) > 0) then do
    -- se valida que x y sean double positivo o negativo
    if esDouble(x) && esDouble(y) then do
        let parqueo2 = Parqueo "0" "0" "0" "0" (read x :: Double) (read y :: Double)
        parqueos <- cargarParqueosSistema
        let parqueosNuevos = convertirListaAListaDeParqueos parqueos
        let parqueoCercano = encontrarParqueoCercano parqueo2 parqueosNuevos
        putStrLn "\ESC[2J"
        putStrLn "🅿️ Parqueo más cercano: "
        putStr "Id: "
        putStrLn (idParqueo parqueoCercano)
        putStr "Nombre: "
        putStrLn (nombre parqueoCercano)
        putStr "Barrio: "
        putStrLn (barrio parqueoCercano)
        putStr "Provincia: "
        putStrLn (provincia parqueoCercano)
        putStrLn "\n\n\n"
        let idParqueoCercano = idParqueo parqueoCercano
        return idParqueoCercano
    else do
        putStrLn "Ingrese un numero double"
        consultarBicicletasAux


-- SECCIÓN ALQUILER 👇

{-
@function alquiler :: IO()
@description Permite al usuario alquilar una bicicleta
@return {IO()}
-}
alquilar :: IO()
alquilar = do 
    putStr "Ingrese su cedula: "
    cedula <- getLine
    datosUsuario <- readFile "./data/usuarios.csv"
    let usuarios = convertirStringALista datosUsuario
    let usuario = filter(\x -> x!!0 == cedula) usuarios
    if usuario == [] then do
        putStrLn "El usuario no existe"
    else do
        putStr "Ingrese el parqueo de salida: "
        nombreParqueoSalida <- getLine
        parqueos <- cargarParqueosSistema
        let parqueoSalida = filter(\x -> x!!1 == nombreParqueoSalida) parqueos
        if parqueoSalida == [] then do
            putStrLn "El parqueo no existe"
        else do
            putStr "Ingrese el parqueo de llegada: "
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
                        imprimirResumenAlquiler nuevoAlquiler

    putStr "Presione enter para continuar"
    opcion <- getLine
    putStrLn "\ESC[2J"
    return ()

{-
    @function imprimirResumenAlquiler
    @description Imprime el resumen del  alquiler
    @params {[[String]]} x:xs
    @returns {IO()}
-}
imprimirResumenAlquiler::[[String]] -> IO()
imprimirResumenAlquiler listaAlquiler = do
    let idAlquiler = head listaAlquiler !! 0
    let idBicicleta = head listaAlquiler !! 1
    let idUsuario = head listaAlquiler !! 2
    let idParqueoSalida = head listaAlquiler !! 3
    let idParqueoLlegada = head listaAlquiler !! 4
    let estado = head listaAlquiler !! 5

    putStr "Resumen del alquiler: "
    putStrLn idAlquiler
    putStr "Id de la bicicleta: "
    putStrLn idBicicleta
    putStr "Id del usuario: "
    putStrLn idUsuario
    putStr "Id del parqueo de salida: "
    putStrLn idParqueoSalida
    putStr "Id del parqueo de llegada: "
    putStrLn idParqueoLlegada
    putStrLn "Estado: activo"


{-
@function filtrarPorIdAlquiler
@description Filtra una lista de listas de strings por el id del alquiler
@params {String} idBuscado
@params {[[String]]} lista
@return {[[String]]} lista filtrada
-}
filtrarPorIdAlquiler :: String -> [[String]] -> [[String]]
filtrarPorIdAlquiler idBuscado lista = filter (\sublista -> sublista !! 0 == idBuscado) lista

{-
@function contarFacturas
@description Cuenta la cantidad de facturas que hay en el sistema
@return {IO Int} cantidad de facturas
-}
contarFacturas :: IO Int 
contarFacturas = do 
    facturas <- cargarFacturasSistema
    let cantidadFacturas = length facturas
    --se le suma 1 para que el id de la factura sea el siguiente
    let idFactura = cantidadFacturas + 1
    return idFactura

{-
@function buscarBicicletaPorId
@description Busca una bicicleta por su id
@params {String} idBicicleta
@return {IO [String]} bicicleta
-}
buscarBicicletaPorId :: String -> IO [String]
buscarBicicletaPorId idBicicleta = do
    bicicletas <- cargarBicicletas
    print bicicletas
    putStrLn "\ESC[2J"
    let bicicleta = filter(\x -> x !! 0 == idBicicleta) bicicletas
    let bicicleta2 = head bicicleta
    return bicicleta2

{-
@function calcularKilometros
@description Calcula la cantidad de kilometros que hay entre dos parqueos
@params {[String]} alquiler
@return {IO Double} cantidad de kilometros
-}
calcularKilometros :: [String] -> IO Double
calcularKilometros alquiler = do 
    let idParqueoSalida = alquiler !! 3
    let idParqueoLlegada = alquiler !! 4
    parqueos <- cargarParqueosSistema
    let parqueoConfigurado = quitarCaracterSextaColumna parqueos
    let listaParqueos = convertirListaAListaDeParqueos parqueoConfigurado
    let parqueoSalida = filter(\x -> idParqueoSalida == idParqueo x) listaParqueos
    let parqueoLlegada = filter(\x -> idParqueoLlegada == idParqueo x) listaParqueos
    let x1 = xCoord (head parqueoSalida)
    let y1 = yCoord (head parqueoSalida)
    let x2 = xCoord (head parqueoLlegada)
    let y2 = yCoord (head parqueoLlegada)
    let distancia = distanciaEuclidiana x1 y1 x2 y2
    return distancia

{-
@function seleccionarTarifa
@description Selecciona la tarifa dependiendo del tipo de bicicleta
@params {String} tipoBicicleta
@return {Double} tarifa
-}
seleccionarTarifa :: String -> Double
seleccionarTarifa tipoBicicleta =
    if tipoBicicleta == "TR" then 100
    else if tipoBicicleta == "AE" then 200
    else 300

{-
@function facturarAux
@description Permite al usuario facturar un alquiler
@return {IO()}
-}
facturarAux :: IO ()
facturarAux = do 
    putStr "Ingrese el id del alquiler: "
    idAlquiler <- getLine
    alquileres <- cargarAlquileresSistema
    -- print alquileres
    infoComercial <- leerArchivo "./data/infoComercial.txt"
    let alquileres2 = quitarCaracterSextaColumna alquileres
    -- print alquileres2
    let alquileresActivos = filter (\x -> x !! 5 == "1" ) alquileres2
    -- let alquileresSegunId = filtrarPorIdAlquiler idAlquiler alquileresActivos
    let alquileresSegunId = filter(\x -> x !! 0  == idAlquiler) alquileresActivos
    -- print alquileresSegunId
    if alquileresSegunId == [] then do
        putStrLn "\ESC[2J"
        putStrLn "El alquiler no existe"
    else do
        idFactura <- contarFacturas
        let idFacturaString = show idFactura
        -- saco el id de la bicicleta de la lista de alquileres, el id de la bicicleta es el segundo elemento de la lista
        let idBicicleta = (head alquileresSegunId) !! 1
        bicicleta <- buscarBicicletaPorId idBicicleta
        let tipoBicicleta = bicicleta !! 1
        kilometros <- calcularKilometros (head alquileresSegunId)
        let kilometrosString = show kilometros
        parqueos <- cargarArchivoEnLista "./data/parqueos.csv"
        print parqueos
        putStrLn "\ESC[2J"
        let parqueo = filter(\x -> x !! 0 == (head alquileresSegunId) !! 4) parqueos
        let parqueo2 = filter(\x -> x !! 0 == (head alquileresSegunId) !! 3) parqueos
        let parqueoSalida = head parqueo2
        let parqueoLlegada = head parqueo
        let nombreParqueoSalida = parqueoSalida !! 1
        let nombreParqueoLlegada = parqueoLlegada !! 1
        let tarifa = seleccionarTarifa tipoBicicleta
        let tarifaString = show tarifa

        let total = kilometros * tarifa
        let totalString = show total
        putStr "Factura: "
        putStrLn idFacturaString
        putStr "Informacion comercial: "
        putStrLn infoComercial
        putStr "Usuario: "
        putStrLn (head alquileresSegunId !! 2)
        putStr "Salida: "
        putStrLn nombreParqueoSalida
        putStr "Destino: "
        putStrLn nombreParqueoLlegada
        putStr "Bicicleta: "
        putStrLn (head alquileresSegunId !! 1)
        putStr "Tipo: "
        putStrLn tipoBicicleta
        putStr "Kilometros: "
        putStrLn kilometrosString
        putStr "Tarifa por kilometro: "
        putStrLn tarifaString
        putStr "Total: "
        putStrLn totalString
        putStrLn "Presione enter para continuar"
        opcion <- getLine
        bicicletas <- cargarArchivoEnLista "./data/bicicletas.csv"
        print bicicletas -- para cerrar el archivo en memoria.
        putStrLn "\ESC[2J" -- limpiar consola de lo que se imprimió.
        -- guardar factura en archivo, datos de la factura: id, idBicicleta, idUsuario, idParqueoSalida, idParqueoLlegada,distancia, tarifa, total
        let nuevaFactura = [[idFacturaString, (head alquileresSegunId) !! 1, (head alquileresSegunId) !! 2, (head alquileresSegunId) !! 3, (head alquileresSegunId) !! 4, kilometrosString, tarifaString, totalString]]
        -- guardarFacturaEnArchivo nuevaFactura alquileres bicicletas 
        let datosNuevaFactura = convertirListaAString nuevaFactura
        agregarArchivo "./data/facturas.csv" datosNuevaFactura
        -- cambiar estado del alquiler a facturado
        let alquileresGeneral = filter(\x -> x !! 0 /= (head alquileresSegunId) !! 0) alquileres
        let listaAlquileresActualizados = alquileresGeneral ++ [[(head alquileresSegunId) !! 0, (head alquileresSegunId) !! 1, (head alquileresSegunId) !! 2, (head alquileresSegunId) !! 3, (head alquileresSegunId) !! 4, "0"]]
        let datosAlquileresActualizados = convertirListaAString listaAlquileresActualizados
        escribirArchivo "./data/alquileres.csv" datosAlquileresActualizados
        -- cambiar ubicacion de la bicicleta al parqueo de llegada
        let bicicletasGeneral = filter(\x -> x !! 0 /= idBicicleta) bicicletas
        let listaBicicletasActualizadas = bicicletasGeneral ++ [[bicicleta !! 0, bicicleta !! 1, (head alquileresSegunId) !! 4 ++ "\r"]]
        let datosBicicletasActualizadas = convertirListaAString listaBicicletasActualizadas
        -- print 
        escribirArchivo "./data/bicicletas.csv" datosBicicletasActualizadas
        putStrLn "Factura generada exitosamente. "
        putStr "Presione enter para continuar"
        opcion <- getLine
        putStrLn "\ESC[2J"
