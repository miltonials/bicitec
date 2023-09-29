module OpcionesOperativas where
import Archivos
import Data.Char (toLower)
import Data.List 
import Data.List (nubBy, intercalate)
import Data.Function (on)
import Data.IORef
import Data.List (sortOn)



--SECCION DE USUARIOS 👇
{-
recibe 2 listas de listas de strings y compara si las cedulas de la primera lista son iguales a las de la segunda lista
retorna una lista de listas de strings con los usuarios sin repetir si es que hay repetidos
ejemplo: [["55454464,"andy"],["5484448","juan"],["55454464,"andy"]] -> [["55454464,"andy"],["5484448","juan"]]
-}
-- Función que elimina duplicados basados en el primer valor de cada sublista
eliminarRepetidosPorPrimerValor :: [[String]] -> [[String]]
eliminarRepetidosPorPrimerValor = nubBy (\x y -> head x == head y)

-- Función que convierte la lista de listas en un solo string
convertirListaAString :: [[String]] -> String
convertirListaAString lista = intercalate "\n" (map (intercalate ",") lista)

-- cargar los usuarios desde un archivo indicado por el usuario
-- y los almacena en una lista de usuarios y lo retorna
cargarUsuarios :: IO [[String]]
cargarUsuarios = do
    putStrLn "Ingrese el nombre del archivo: "
    nombreArchivo <- getLine
    datos <- leerArchivo nombreArchivo
    lista2 <- cargarUsuariosDesdeArchivo "./data/usuarioss.csv"
    let lista = convertirStringALista datos
    let lista3 = lista ++ lista2
    let lista4 = eliminarRepetidosPorPrimerValor lista3
    let datos3 = convertirListaAString lista4
    escribirArchivo "./data/usuarios.txt" datos3
    return lista4

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
    putStrLn "Ingrese el nombre del archivo: "
    nombreArchivo <- getLine
    if nombreArchivo == "./data/parqueos.csv" then do
        parqueos <- leerArchivo nombreArchivo
        let lista = convertirStringALista parqueos
        return lista
    else do
        parqueosNuevos <- leerArchivo nombreArchivo
        let listaParqueosNuevos = convertirStringALista parqueosNuevos
        parqueosSistema <- readFile "./data/parqueos.csv"
        putStr parqueosSistema -- para cerrar el archivo en memoria.
        putStrLn "\ESC[2J" -- limpiar consola de lo que se imprimió.
        let listaParqueosSistema = convertirStringALista parqueosSistema
        let listaParqueos = listaParqueosSistema ++ listaParqueosNuevos
        let listaFiltrada = nubBy (\x y -> head x == head y) listaParqueos
        let datosParqueos = convertirListaAString listaFiltrada
        writeFile "./data/parqueos.csv" datosParqueos
        return listaFiltrada

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
            let bicicletasEnParqueo = filter (\x -> x !! 2 == head parqueo !! 0) bicicletas
            mostrarBicicletas bicicletasEnParqueo
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

-- SECCIÓN DE ESTADÍSTICAS 👇
contarOcurrenciasBici :: [[String]] -> [[String]] -> [[String]]
contarOcurrenciasBici lista1 lista2 = map (\sublista -> [head sublista, show (length [x | x <- lista1, x !! 1 == head sublista])]) lista2

mostrarTop5BicicletasMasUsadaAux :: [[String]] -> Int -> IO ()
mostrarTop5BicicletasMasUsadaAux lista contador = do
    if lista == [] then do
        putStrLn "Fin de la lista de bicicletas"
    else if contador > 5 then do
        putStrLn "Fin de la lista de bicicletas"
    else do
        putStrLn ("Bicicleta #" ++ show contador)
        putStrLn ("ID: " ++ head (head lista))
        putStrLn ("Cantidad de viajes: " ++ head (tail (head lista)))
        putStrLn ""
        mostrarTop5BicicletasMasUsadaAux (tail lista) (contador + 1)

-- a) Top 5 de bicicletas con más viajes, indicar bicicleta y cantidad de viajes.
top5BicicletasMasUsadaAux :: IO ()
top5BicicletasMasUsadaAux = do 
    bicicletas <- cargarArchivoEnLista "./data/bicicletas.csv"
    alquileres <- cargarArchivoEnLista "./data/alquileres.csv"
    let lista = contarOcurrenciasBici alquileres bicicletas
    -- se ordena la lista de listas de strings por la cantidad de veces que se repite
    let listaOrdenada = reverse (sortOn (\x -> read (x !! 1) :: Int) lista)
    -- se imprime la lista de listas de strings
    putStrLn "Top 5 de bicicletas con más viajes"
    mostrarTop5BicicletasMasUsadaAux listaOrdenada 1
    putStr "Presione enter para continuar"
    opcion <- getLine
    putStrLn "\ESC[2J"




