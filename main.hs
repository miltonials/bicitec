import System.IO

-- Menu Operativas
menuOperativas :: IO ()
menuOperativas = do 
    putStrLn "1) Cargar y Mostrar parqueos"
    putStrLn "2) Mostrar y asignar bicicletas"
    putStrLn "3) Cargar usuarios"
    putStrLn "4) Estadisticas"
    putStrLn "5) Volver al menu principal"
    putStrLn "Ingrese una opcion: "
    opcion <- getLine
    case opcion of
        "1" -> putStrLn "Cargar y Mostrar parqueos"
        "2" -> putStrLn "Mostrar y asignar bicicletas"
        "3" -> putStrLn "Cargar usuarios"
        "4" -> putStrLn "Estadisticas"
        "5" -> menuPrincipal
        _ -> do
            putStrLn "Opcion invalida"
            menuOperativas

-- menu gerenciales
menuGerenciales :: IO ()
menuGerenciales = do
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
        "4" -> menuPrincipal
        _ -> do
            putStrLn "Opcion invalida"
            menuGerenciales

-- menu principal opciones: 
-- 1) opciones Operativas 
-- 2) Opciones Gerenciales
-- 3) Salir
menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "1) Opciones Operativas"
    putStrLn "2) Opciones Gerenciales"
    putStrLn "3) Salir"
    putStrLn "Ingrese una opcion: "
    opcion <- getLine
    case opcion of
        "1" -> menuOperativas
        "2" -> menuGerenciales
        "3" -> putStrLn "Gracias por usar el sistema"
        _ -> do
            putStrLn "Opcion invalida"
            menuPrincipal

-- Main
main :: IO ()
main = do
    menuPrincipal
    
