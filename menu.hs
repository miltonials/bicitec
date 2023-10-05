module Menu where
import Archivos
import OpcionesOperativas
import OpcionesGenerales

{-
    @constructor Parqueo
    @param idParqueo: identificador del parqueo
    @param nombre: nombre del parqueo
    @param barrio: barrio del parqueo
    @param provincia: provincia del parqueo
    @param xCoord: coordenada x del parqueo
    @param yCoord: coordenada y del parqueo
    @deriving Show: para poder imprimir el parqueo
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
@function mmotrarUsuarios
@description: imprime los usuarios
@param usuarios: lista de listas de strings con los usuarios
@returns: IO ()
-}
mostrarUsuarios :: [[String]] -> IO ()
mostrarUsuarios usuarios = do
    putStrLn "Usuarios: "
    print usuarios

{-
@function cargarUsuariosAux
@description: carga los usuarios
@returns: IO ()
-}
cargarUsuariosAux :: IO ()
cargarUsuariosAux = do
    lista <- cargarUsuarios
    -- print lista
    if lista == [] then do
        putStrLn "No se pudo cargar los usuarios"
        menuOperativas lista
    else do
        putStrLn "Usuarios cargados con exito"
        putStrLn "Presione enter para continuar..."
        _ <- getLine
        menuOperativas lista

{-
@function top5BicicletasMasUsada
@description: imprime el top 5 de bicicletas mas usadas
@param usuarios: lista de listas de strings con los usuarios
@returns: IO ()
-}
top5BicicletasMasUsada :: [[String]] -> IO ()
top5BicicletasMasUsada usuarios = do
    putStrLn "\ESC[2J"
    top5BicicletasMasUsadaAux 
    menuEstadisticas usuarios

{-
@function top5ParqueosMasUsados
@description: imprime el top 5 de parqueos mas usados
@param usuarios: lista de listas de strings con los usuarios
@returns: IO ()
-}
top5ParqueosMasUsados :: [[String]] -> IO ()
top5ParqueosMasUsados usuarios = do
    putStrLn "\ESC[2J"
    top5ParqueosMasUsadosAux
    menuEstadisticas usuarios

{-
@function top3UsuariosMasKilometros
@description: imprime el top 3 de usuarios con mas kilometros recorridos
@param usuarios: lista de listas de strings con los usuarios
@returns: IO ()
-}
top3UsuariosMasKilometros :: [[String]] -> IO ()
top3UsuariosMasKilometros usuarios = do
    putStrLn "\ESC[2J"
    top3UsuariosMasKilometrosAux
    menuEstadisticas usuarios

{-
@function resumen
@description: imprime el resumen
@param usuarios: lista de listas de strings con los usuarios
@returns: IO ()
-}
resumen :: [[String]] -> IO ()
resumen usuarios = do
    putStrLn "\ESC[2J"
    resumenAux
    menuEstadisticas usuarios


{-
@function menuEstadisticas
@description: imprime el menu de estadisticas
@param usuarios: lista de listas de strings con los usuarios
@returns: IO ()
-}
menuEstadisticas :: [[String]] -> IO ()
menuEstadisticas usuarios = do
    putStrLn "\ESC[2J"
    putStrLn "1) Top 5 de bicicletas con más viajes"
    putStrLn "2) Top 5 de parqueos con más viajes"
    putStrLn "3) Top 3 de usuarios con más kilómetros recorridos"
    putStrLn "4) Resumen"
    putStrLn "5) Volver al menu principal"
    putStr "Ingrese una opcion: "
    opcion <- getLine
    case opcion of
        "1" -> top5BicicletasMasUsada usuarios
        "2" -> top5ParqueosMasUsados usuarios
        "3" -> top3UsuariosMasKilometros usuarios
        "4" -> resumen usuarios
        "5" -> menuPrincipal usuarios
        _ -> do
            putStrLn "Opcion invalida"
            menuEstadisticas usuarios

{-
@function menuOperativas 
@description: imprime el menu de operativas
@param usuarios: lista de listas de strings con los usuarios
@returns: IO ()
-}
menuOperativas :: [[String]] -> IO ()
menuOperativas usuarios = do 
    putStrLn "\ESC[2J"
    putStrLn "1) Cargar y Mostrar parqueos"
    putStrLn "2) Mostrar y asignar bicicletas"
    putStrLn "3) Cargar usuarios"
    putStrLn "4) Estadisticas"
    putStrLn "5) Volver al menu principal"
    putStr "Ingrese una opcion: "
    opcion <- getLine
    case opcion of
        "1" -> do 
            parqueos <- cargarParqueos
            mostrarParqueos parqueos
            putStrLn "Presione enter para volver..."
            opcion <- getLine
            menuOperativas usuarios
        "2" -> do 
            menuOperativosBicicletas
            menuOperativas usuarios
        "3" -> cargarUsuariosAux
        "4" -> menuEstadisticas usuarios
        "5" -> menuPrincipal usuarios
        _ -> do
            putStrLn "Opcion invalida"
            menuOperativas usuarios

{-
@function filtrarPorId
@description: filtra una lista de listas de strings por el id
@param idBuscado: id a buscar
@param lista: lista de listas de strings
@returns: lista de listas de strings
-}
filtrarPorId :: String -> [[String]] -> [[String]]
filtrarPorId idBuscado lista = filter (\sublista -> sublista !! 2 == idBuscado) lista

{-
@function consultarBicicletas
@description: consulta las bicicletas disponibles
@param usuarios: lista de listas de strings con los usuarios
@returns: IO ()
-}  
consultarBicicletas :: [[String]] -> IO ()
consultarBicicletas usuarios = do
    parqueoCercano <- consultarBicicletasAux
    let idParqueoCercano = parqueoCercano
    bicicletas <- cargarArchivoEnLista "./data/bicicletas.csv"
    let listaConfigurada = quitarCaracterTercerElemento bicicletas
    let bicicletasdisponible = filtrarPorId idParqueoCercano listaConfigurada
    putStr "🚲 Bicicletas disponibles: "
    putStrLn (show (length bicicletasdisponible))
    mostrarBicicletas bicicletasdisponible
    menuGenerales usuarios

{-
@function facturar
@description: factura
@param usuarios: lista de listas de strings con los usuarios
@returns: IO ()
-}
facturar :: [[String]] -> IO ()
facturar usuarios = do
    facturarAux
    putStrLn "Presione enter para volver..."
    opcion <- getLine
    menuGenerales usuarios

{-
@function menuGenerales
@description: imprime el menu de generales
@param usuarios: lista de listas de strings con los usuarios
@returns: IO ()
-}
menuGenerales :: [[String]] -> IO ()
menuGenerales usuarios = do
    putStrLn "\ESC[2J"
    putStrLn "1) Consultar bicicletas"
    putStrLn "2) Alquilar bicicletas"
    putStrLn "3) Facturar"
    putStrLn "4) Volver al menu principal"
    putStr "Ingrese una opcion: "
    opcion <- getLine
    case opcion of
        "1" -> consultarBicicletas usuarios
        "2" -> do 
            alquilar
            menuGenerales usuarios
        "3" -> facturar usuarios
        "4" -> menuPrincipal usuarios
        _ -> do
            putStrLn "Opcion invalida"
            menuGenerales usuarios

{-
@function validarInicioSession
@description: valida el inicio de session
@param usuarios: lista de listas de strings con los usuarios
@returns: IO ()
-}
validarInicioSession :: [[String]] -> IO () 
validarInicioSession usuarios = do 
    putStr "Ingrese su usuario: "
    usuario <- getLine
    if usuario == "admin" then do
        putStr "Ingrese su contraseña: "
        contrasena <- getLine
        if contrasena == "admin" then do
            putStrLn "\ESC[2J"
            putStrLn "Inicio de session exitoso"
            menuOperativas usuarios
        else do
            putStrLn "Contraseña incorrecta"
            validarInicioSession usuarios
    else if usuario == "0" then do
        putStrLn "regresando al menu principal"
        menuPrincipal usuarios
    else do
        putStrLn "Usuario incorrecto"
        validarInicioSession usuarios


{-
@function menuPrincipal
@description: imprime el menu principal
@param usuarios: lista de listas de strings con los usuarios
@returns: IO ()
-}
menuPrincipal :: [[String]] -> IO ()
menuPrincipal usuarios = do
    putStrLn "\ESC[2J"
    infoComercial <- leerArchivo "./data/infoComercial.txt"
    putStrLn infoComercial
    
    putStrLn "1) Opciones Operativas"
    putStrLn "2) Opciones Generales"
    putStrLn "3) Salir"
    putStr "Ingrese una opcion: "
    opcion <- getLine
    case opcion of
        "1" -> validarInicioSession usuarios
        "2" -> menuGenerales usuarios
        "3" -> putStrLn "Gracias por usar el sistema"
        _ -> do
            putStrLn "Opcion invalida"
            menuPrincipal usuarios