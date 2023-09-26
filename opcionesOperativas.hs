module OpcionesOperativas where
import Archivos
import Data.Char (toLower)

-- SECCION DE ALQUILERES 👇
cargarAlquileres:: IO [[String]]
cargarAlquileres = do
    datos <- leerArchivo "./data/alquileres.csv"
    let lista = convertirStringALista datos
    return lista

alquileresActivos :: [[String]] -> [[String]]
alquileresActivos alquileres = filter (\x -> x !! 5 == "1") alquileres

-- SECCIÓN DE PARQUEOS 👇
-- cargar parqueos desde el archivo ./data/parqueos.csv
cargarParqueosSistema :: IO [[String]]
cargarParqueosSistema = do
    parqueos <- leerArchivo "./data/parqueos.csv"
    let lista = convertirStringALista parqueos
    return lista

cargarParqueos :: IO [[String]]
cargarParqueos = do
    -- let nombreArchivo = "./data/parqueos.csv"
    putStrLn "Ingrese el nombre del archivo: "
    nombreArchivo <- getLine
    parqueosNuevos <- leerArchivo nombreArchivo
    parqueosSistema <- leerArchivo "./data/parqueos.csv"
    let totalParqueos = parqueosNuevos ++ parqueosSistema
    let lista = convertirStringALista totalParqueos
    --TODO: eliminar parqueos repetidos y guardarlos en el archivo parqueos.csv
    return lista

mostrarParqueos :: [[String]] -> IO ()
mostrarParqueos parqueos = do
    putStrLn "Lista de parqueos: "
    mostrarParqueosAux parqueos 1

-- función auxiliar para mostrar los parqueos
mostrarParqueosAux :: [[String]] -> Int -> IO ()
mostrarParqueosAux [] _ = do
    putStrLn "Fin de la lista de parqueos"

mostrarParqueosAux (x:xs) contador = do
    putStrLn ("Parqueo #" ++ show contador)
    putStrLn ("ID: " ++ head x)
    putStrLn ("Nombre: " ++ x !! 1)
    putStrLn ("Dirección: " ++ x !! 2)
    putStrLn ("Provincia: " ++ x !! 3)
    -- putStrLn ("Latitud: " ++ x !! 4)
    -- putStrLn ("Longitud: " ++ x !! 5)
    putStrLn("Coordenadas: " ++ x !! 4 ++ ", " ++ x !! 5)
    putStrLn ""
    mostrarParqueosAux xs (contador + 1)
    
-- Sección de bicicletas 👇

--un menu para mostrar y asignar bicicletas
menuOperativosBicicletas :: IO ()
menuOperativosBicicletas = do
    putStrLn "1) Mostrar bicicletas"
    putStrLn "2) Asignar bicicletas"
    putStrLn "3) Volver al menu operativas"
    putStrLn "Ingrese una opcion: "
    opcion <- getLine
    case opcion of
        "1" -> menuMostrarBicicletas
        "2" -> putStrLn "Asignar bicicletas"
        "3" -> putStrLn "Volver al menu operativas"
        _ -> do
            putStrLn "Opcion invalida"
            menuOperativosBicicletas

menuMostrarBicicletas :: IO ()
menuMostrarBicicletas = do
    putStrLn "Ingrese el nombre del parqueo: "
    nombreParqueo <- getLine
    bicicletas <- cargarBicicletas -- [[id, tipo, id_parqueo]]
    -- al indicar nombre “#”, muestra todas las bicicletas del sistema, al indicar “transito”, mostrará las bicicletas en tránsito (activas en alquiler).
    if nombreParqueo == "#" then do
        putStrLn "Lista de bicicletas en el sistema: "
        mostrarBicicletas bicicletas
    else if nombreParqueo == "transito" then do
        putStrLn "Lista de bicicletas en transito: "
        alquileres <- cargarAlquileres 
        mostrarBicicletas alquileres
    else do
        putStrLn "Lista de bicicletas en el parqueo: "
        parqueos <- cargarParqueosSistema -- [[id, nombre, direccion, provincia, latitud, longitud]]
        -- let parqueo = filter (\x -> x !! 1 == nombreParqueo) parqueos
        -- filtrar los parqueo que tengan un subString de nombreParqueo sin importar mayusculas o minusculas
        let parqueo = filter (\x -> map toLower (x !! 1) == map toLower nombreParqueo) parqueos
        if parqueo == [] then do
            putStrLn "No se encontro el parqueo"
        else do
            let bicicletasEnParqueos = filter (\x -> x !! 2 == head parqueo !! 0) bicicletas
            
            mostrarBicicletas bicicletasEnParqueos
            -- putStr "mostrarBicicletasEnParqueo bicicletas nombreParqueo"
    menuOperativosBicicletas

-- cargar bicicletas desde el archivo ./data/bicicletas.csv
cargarBicicletas :: IO [[String]]
cargarBicicletas = do
    datos <- leerArchivo "./data/bicicletas.csv"
    let lista = convertirStringALista datos
    return lista

-- mostrar bicicletas en el sistema
mostrarBicicletas :: [[String]] -> IO ()
mostrarBicicletas bicicletas = do
    mostrarBicicletasAux bicicletas 1

-- función auxiliar para mostrar las bicicletas
mostrarBicicletasAux :: [[String]] -> Int -> IO ()
mostrarBicicletasAux [] _ = do
    putStrLn "Fin de la lista de bicicletas"

mostrarBicicletasAux (x:xs) contador = do
    parqueos <- cargarParqueosSistema -- [[id, nombre, direccion, provincia, latitud, longitud]]
    let parqueo = filter (\y -> y !! 0 == x !! 2) parqueos

    putStrLn ("Bicicleta #" ++ show contador)
    putStrLn ("ID: " ++ x !! 0)
    putStrLn ("Tipo: " ++ x !! 1)
    if parqueo == [] then do
        putStrLn ("Parqueo: transito")
    else do
        putStrLn ("Parqueo: " ++ head parqueo !! 1)
    putStrLn ""
    mostrarBicicletasAux xs (contador + 1)