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

quitarCaracterQuintaColumna :: [[String]] -> [[String]]
quitarCaracterQuintaColumna lista = map (\[x, y, z, w, a] -> [x, y, z, w, quitarCaracter a]) lista

quitarCaracterSextaColumna :: [[String]] -> [[String]]
quitarCaracterSextaColumna lista = map (\[x, y, z, w, a, b] -> [x, y, z, w, a, quitarCaracter b]) lista

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
                        -- putStrLn "Bicicleta alquilada exitosamente. 🥳"
                        putStrLn "Bicicleta alquilada exitosamente. "

    putStr "Presione enter para continuar"
    opcion <- getLine
    putStrLn "\ESC[2J"
    return ()

       
-- el id de la bicicleta es el primer elemento de la lista
filtrarPorIdAlquiler :: String -> [[String]] -> [[String]]
filtrarPorIdAlquiler idBuscado lista = filter (\sublista -> sublista !! 0 == idBuscado) lista

contarFacturas :: IO Int 
contarFacturas = do 
    facturas <- cargarFacturasSistema
    let cantidadFacturas = length facturas
    --se le suma 1 para que el id de la factura sea el siguiente
    let idFactura = cantidadFacturas + 1
    return idFactura

buscarBicicletaPorId :: String -> IO [String]
buscarBicicletaPorId idBicicleta = do
    bicicletas <- cargarBicicletas
    print bicicletas
    putStrLn "\ESC[2J"
    let bicicleta = filter(\x -> x !! 0 == idBicicleta) bicicletas
    let bicicleta2 = head bicicleta
    return bicicleta2

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
    -- putStr "id de los parqueo: "
    -- print idParqueoSalida
    -- print idParqueoLlegada
    -- print distancia
    return distancia

-- hay que hacer una funcion que reciba el tipo de bicicleta y devuelva la tarifa
-- existe 3 tipos de bicicletas: TR, AE, AG
seleccionarTarifa :: String -> Double
seleccionarTarifa tipoBicicleta =
    if tipoBicicleta == "TR" then 100
    else if tipoBicicleta == "AE" then 200
    else 300

-- recibe una lista de listas de strings
{-
guardarFacturaEnArchivo :: [String] -> [[String]] -> [[String]] -> IO ()
guardarFacturaEnArchivo factura alquileres bicicletas = do 
    let datosNuevaFactura = convertirListaAString factura
    agregarArchivo "./data/facturas.csv" datosNuevaFactura
    -- cambiar estado del alquiler a facturado
    let alquileresGeneral = filter(\x -> x !! 0 /= (head factura) !! 0) alquileres
    let listaAlquileresActualizados = alquileresGeneral ++ [[(head factura) !! 0, (head factura) !! 1, (head factura) !! 2, (head factura) !! 3, (head factura) !! 4, "2"]]
    let datosAlquileresActualizados = convertirListaAString listaAlquileresActualizados
    escribirArchivo "./data/alquileres.csv" datosAlquileresActualizados
    -- cambiar ubicacion de la bicicleta al parqueo de llegada
    let bicicletasGeneral = filter(\x -> x !! 0 /= (head factura) !! 1) bicicletas
    let listaBicicletasActualizadas = bicicletasGeneral ++ [[(head factura) !! 1, (head factura) !! 2, (head factura) !! 4]]
    let datosBicicletasActualizadas = convertirListaAString listaBicicletasActualizadas
    escribirArchivo "./data/bicicletas.csv" datosBicicletasActualizadas
    putStr "Presione enter para continuar"
    opcion <- getLine
    putStrLn "\ESC[2J"
-}

{-
Facturar El sistema le solicitará al usuario que indique el identificador de alquiler, 
el sistema verificará que el alquiler está activo y procederá a facturarlo. 
Se le muestra al usuario una factura en pantalla con la siguiente información: identificador (autogenerado), 
información del negocio (información comercial), usuario, salida, destino, bicicleta (identificador, tipo), 
cantidad de kilómetros (calculado con la misma fórmula de la consulta de bicicletas), 
tarifa por kilómetro aplicada (depende del tipo de bicicleta) y total en colones. Debe mostrar un formato amigable al usuario.
Al facturar, el alquiler cambia a estado “facturado” y la bicicleta se le coloca de ubicación el
parqueo destino.
-}

facturarAux :: IO ()
facturarAux = do 
    putStrLn "Ingrese el id del alquiler: "
    idAlquiler <- getLine
    alquileres <- cargarAlquileresSistema
    -- print alquileres
    infoComercial <- leerArchivo "./data/infoComercial.txt"
    let alquileres2 = quitarCaracterSextaColumna alquileres
    -- print alquileres2
    let alquileresActivos = filter (\x -> x !! 5 == idAlquiler ) alquileres2
    -- print alquileresActivos
    let alquileresSegunId = filtrarPorIdAlquiler idAlquiler alquileresActivos
    if alquileresSegunId == [] then do
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
        -- cerrar el archivo ./data/alquileres.csv, para que se pueda escribir en el archivo
        -- cerrarArchivo "./data/alquileres.csv"
        escribirArchivo "./data/alquileres.csv" datosAlquileresActualizados
        -- cambiar ubicacion de la bicicleta al parqueo de llegada
        let bicicletasGeneral = filter(\x -> x !! 0 /= idBicicleta) bicicletas
        let listaBicicletasActualizadas = bicicletasGeneral ++ [[bicicleta !! 0, bicicleta !! 1, (head alquileresSegunId) !! 4 ++ "\r"]]
        let datosBicicletasActualizadas = convertirListaAString listaBicicletasActualizadas
        -- print 
        -- cerrar el archivo ./data/bicicletas.csv, para que se pueda escribir en el archivo
        cerrarArchivo "./data/bicicletas.csv"
        escribirArchivo "./data/bicicletas.csv" datosBicicletasActualizadas
        putStrLn "Factura generada exitosamente. "
        putStr "Presione enter para continuar"
        opcion <- getLine
        putStrLn "\ESC[2J"
        {-
        -}


