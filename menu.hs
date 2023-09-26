module Menu where
import Archivos
import OpcionesOperativas
import OpcionesGenerales
import System.IO.Unsafe
import Data.IORef

mostrarUsuarios :: [[String]] -> IO ()
mostrarUsuarios usuarios = do
    putStrLn "Usuarios: "
    print usuarios

-- cargar usuarios desde un archivo indicado por el usuario y los almacena en un archivo llamado usuarios.txt
cargarUsuariosAux :: IO ()
cargarUsuariosAux = do
    lista <- cargarUsuarios
    print lista
    if lista == [] then do
        putStrLn "No se pudo cargar los usuarios"
        menuOperativas lista
    else do
        putStrLn "Usuarios cargados con exito"
        menuOperativas lista

    
-- Menu Operativas
menuOperativas :: [[String]] -> IO ()
menuOperativas usuarios = do 
    putStrLn "1) Cargar y Mostrar parqueos"
    putStrLn "2) Mostrar y asignar bicicletas"
    putStrLn "3) Cargar usuarios"
    putStrLn "4) Estadisticas"
    putStrLn "5) Volver al menu principal"
    putStrLn "Ingrese una opcion: "
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
        "4" -> putStrLn "Estadisticas"
        "5" -> menuPrincipal usuarios
        _ -> do
            putStrLn "Opcion invalida"
            menuOperativas usuarios

menuGenerales :: [[String]] -> IO ()
menuGenerales usuarios = do
    putStrLn "1) Consultar bicicletas"
    putStrLn "2) Alquilar bicicletas"
    putStrLn "3) Facurar"
    putStrLn "4) Volver al menu principal"
    putStrLn "Ingrese una opcion: "
    opcion <- getLine
    case opcion of
        "1" -> putStrLn "Consultar bicicletas"
        "2" -> putStrLn "Alquilar bicicletas"
        "3" -> putStrLn "Facurar"
        "4" -> menuPrincipal usuarios
        _ -> do
            putStrLn "Opcion invalida"
            menuGenerales usuarios

-- se valida el inicio de session, osea que el usuario y la contraseña sean correctos
validarInicioSession :: [[String]] -> IO () 
validarInicioSession usuarios = do 
    putStrLn "Ingrese su usuario: "
    usuario <- getLine
    if usuario == "admin" then do
        putStrLn "\nIngrese su contraseña: "
        contrasena <- getLine
        if contrasena == "admin" then do
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


-- menu principal opciones: 
-- 1) opciones Operativas 
-- 2) Opciones Generales
-- 3) Salir
-- recibe una lista de listas de strings con los usuarios
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