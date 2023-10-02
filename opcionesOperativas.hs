module OpcionesOperativas where
import Archivos
import Data.Char (toLower)
import Data.List 



--SECCION DE USUARIOS 👇
{-
@name eliminarRepetidosPorPrimerValor
@description elimina los elementos repetidos de una lista de listas de strings
@params lista de listas de strings
@returns lista de listas de strings sin elementos repetidos
-}
eliminarRepetidosPorPrimerValor :: [[String]] -> [[String]]
eliminarRepetidosPorPrimerValor = nubBy (\x y -> head x == head y)

{-
@name convertirListaAString
@description convierte una lista de listas de strings a un string
@params lista de listas de strings
@returns string
-}
convertirListaAString :: [[String]] -> String
convertirListaAString lista = intercalate "\n" (map (intercalate ",") lista)

{-
@name cargarUsuarios
@description carga los usuarios desde el archivo ./data/usuarios.csv
@params none
@returns lista de listas de strings
-}
cargarUsuarios :: IO [[String]]
cargarUsuarios = do
    putStr "Ingrese el nombre del archivo: "
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
{-
@name cargarAlquileres
@description carga los alquileres desde el archivo ./data/alquileres.csv
@params none
@returns lista de listas de strings
-}
cargarAlquileres:: IO [[String]]
cargarAlquileres = do
    datos <- leerArchivo "./data/alquileres.csv"
    let lista = convertirStringALista datos
    return lista

{-
@name alquileresActivos
@description filtra los alquileres que estan activos
@params lista de listas de strings
@returns lista de listas de strings
-}
alquileresActivos :: [[String]] -> [[String]]
alquileresActivos alquileres = filter (\x -> x !! 5 == "1") alquileres

-- SECCIÓN DE PARQUEOS 👇
{-
@name cargarParqueosSistema
@description carga los parqueos desde el archivo ./data/parqueos.csv
@params none
@returns lista de listas de strings
-}
cargarParqueosSistema :: IO [[String]]
cargarParqueosSistema = do
    parqueos <- leerArchivo "./data/parqueos.csv"
    let lista = convertirStringALista parqueos
    return lista

{-
@name cargarParqueos
@description carga los parqueos desde el archivo ./data/parqueos.csv
@params none   
@returns lista de listas de strings
-}
cargarParqueos :: IO [[String]]
cargarParqueos = do
    putStr "Ingrese el nombre del archivo: "
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

{-
@name mostrarParqueos
@description muestra los parqueos
@params lista de listas de strings
@returns none
-}
mostrarParqueos :: [[String]] -> IO ()
mostrarParqueos parqueos = do
    putStrLn "Lista de parqueos: "
    mostrarParqueosAux parqueos 1

{-
@name mostrarParqueosAux
@description función auxiliar para mostrar los parqueos
@params lista de listas de strings, contador
@returns none
-}
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

{-
@name menuOperativosBicicletas
@description muestra el menu de operativos de bicicletas
@params none
@returns none
-}
menuOperativosBicicletas :: IO ()
menuOperativosBicicletas = do
    putStrLn "\ESC[2J"
    putStrLn "1) Mostrar bicicletas"
    putStrLn "2) Asignar bicicletas"
    putStrLn "3) Volver al menu operativas"
    putStr "Ingrese una opcion: "
    opcion <- getLine
    case opcion of
        "1" -> menuMostrarBicicletas
        "2" -> asignarBicicletas
        "3" -> putStrLn "Volver al menu operativas"
        _ -> do
            putStrLn "Opcion invalida"
            menuOperativosBicicletas

{-
@name menuMostrarBicicletas
@description muestra el menu de mostrar bicicletas
@params none
@returns none
-}
menuMostrarBicicletas :: IO ()
menuMostrarBicicletas = do
    putStr "Ingrese el nombre del parqueo: "
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

{-
@name cargarBicicletas
@description carga las bicicletas desde el archivo ./data/bicicletas.csv
@params none
@returns lista de listas de strings
-}
cargarBicicletas :: IO [[String]]
cargarBicicletas = do
    datos <- leerArchivo "./data/bicicletas.csv"
    let lista = convertirStringALista datos
    return lista

{-
@name mostrarBicicletas
@description muestra las bicicletas
@params lista de listas de strings
@returns none
-}
mostrarBicicletas :: [[String]] -> IO ()
mostrarBicicletas bicicletas = do
    mostrarBicicletasAux bicicletas 1

{-
@name mostrarBicicletasAux
@description función auxiliar para mostrar las bicicletas
@params lista de listas de strings, contador
@returns none
-}
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

{-
@name contarOcurrenciasBici
@description retorna id, cantidad de veces que se repite el id en la lista1
@params lista de listas de strings, lista de listas de strings
@returns lista de listas de strings
-}
contarOcurrenciasBici :: [[String]] -> [[String]] -> [[String]]
contarOcurrenciasBici lista1 lista2 = map (\sublista -> [head sublista, show (length [x | x <- lista1, x !! 1 == head sublista])]) lista2

{-
@name contarOcurrenciasParqueo
@description retorna id, cantidad de veces que se repite el id en la lista1
@params lista de listas de strings, lista de listas de strings
@returns lista de listas de strings
-}
contarOcurrenciasParqueo :: [[String]] -> [[String]] -> [[String]]
contarOcurrenciasParqueo lista1 lista2 = map (\sublista -> [head sublista, show (length [x | x <- lista1, x !! 3 == head sublista || x !! 4 == head sublista]), head (tail sublista)]) lista2

{-
@name mostrarTop5BicicletasMasUsadaAux
@description muestra el top 5 de bicicletas mas usadas
@params lista de listas de strings, contador    
@returns none
-}
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


{-
@name mostrarTop5PaqueosMasUsadosAux
@description muestra el top 5 de parqueos mas usados
@params lista de listas de strings, contador
@returns none
-}
mostrarTop5PaqueosMasUsadosAux :: [[String]] -> Int -> IO ()
mostrarTop5PaqueosMasUsadosAux lista contador = do
    if lista == [] then do
        putStrLn "Fin de la lista de parqueos"
    else if contador > 5 then do
        putStrLn "Fin de la lista de parqueos"
    else do
        let id = head (head lista)
        putStrLn ("Parqueo #" ++ show contador)
        putStrLn ("ID: " ++ id)
        let nombre = head (tail (tail (head lista)))
        putStrLn ("Nombre: " ++ nombre)
        putStrLn ("Cantidad de viajes: " ++ head (tail (head lista)))
        putStrLn ""
        mostrarTop5PaqueosMasUsadosAux (tail lista) (contador + 1)

{-
@name top5BicicletasMasUsadaAux
@description muestra el top 5 de bicicletas mas usadas
@params none
@returns none
-}
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

{-
@name top5ParqueosMasUsadosAux
@description muestra el top 5 de parqueos mas usados
@params none
@returns none
-}
top5ParqueosMasUsadosAux :: IO ()
top5ParqueosMasUsadosAux = do 
    alquileres <- cargarArchivoEnLista "./data/alquileres.csv"
    parqueos <- cargarArchivoEnLista "./data/parqueos.csv"
    let lista = contarOcurrenciasParqueo alquileres parqueos
    -- print lista
    -- se ordena la lista de listas de strings por la cantidad de veces que se repite
    let listaOrdenada = reverse (sortOn (\x -> read (x !! 1) :: Int) lista)
    -- se imprime la lista de listas de strings
    putStrLn "Top 5 de parqueos con más viajes"
    mostrarTop5PaqueosMasUsadosAux listaOrdenada 1
    putStr "Presione enter para continuar"
    opcion <- getLine
    putStrLn "\ESC[2J"

{-
@name contarOcurrenciasUsuarios
@description retorna id, nombre, cantidad de kilometros recorridos
@params lista de listas de strings, lista de listas de strings
@returns lista de listas de strings
-}
contarOcurrenciasUsuarios :: [[String]] -> [[String]] -> [[String]]
contarOcurrenciasUsuarios lista1 lista2 =
  let resultado = map (\sublista2 ->
                          let idUsuario = head sublista2
                              nombre =  (sublista2 !! 1)
                              cantidadRecorrido = sum (map (\sublista1 ->
                                                              let idUsuario2 = sublista1 !! 2
                                                                  cantidadRecorrido2 = (read (sublista1 !! 5) :: Double)
                                                              in if idUsuario == idUsuario2 then cantidadRecorrido2 else 0
                                                          ) lista1)
                          in [idUsuario, nombre, show cantidadRecorrido]
                      ) lista2
  in resultado

{-
@name mostrarTop3UsuariosConMayorRecorrido
@description muestra el top 3 de usuarios con mayor recorrido
@params lista de listas de strings, contador
@returns none
-}
mostrarTop3UsuariosConMayorRecorrido :: [[String]] -> Int -> IO ()
mostrarTop3UsuariosConMayorRecorrido lista contador = do
    if lista == [] then do
        putStrLn "Fin de la lista de usuarios"
    else if contador > 3 then do
        putStrLn "Fin de la lista de usuarios"
    else do
        putStrLn ("Usuario #" ++ show contador)
        putStrLn ("ID: " ++ head (head lista))
        let nombre = head (tail (head lista))
        putStrLn ("Nombre: " ++ nombre)
        putStrLn ("Cantidad de kilometros recorridos: " ++ head (tail (tail (head lista))))
        putStrLn ""
        mostrarTop3UsuariosConMayorRecorrido (tail lista) (contador + 1)


{-
@name top3UsuariosMasKilometrosAux
@description muestra el top 3 de usuarios con mayor recorrido
@params none
@returns none
-}
top3UsuariosMasKilometrosAux :: IO ()
top3UsuariosMasKilometrosAux = do 
    facturas <- cargarArchivoEnLista "./data/facturas.csv"
    usuarios <- cargarArchivoEnLista "./data/usuarios.csv" 
    print facturas
    print usuarios
    let lista = contarOcurrenciasUsuarios facturas usuarios
    print lista
    -- se ordena la lista de listas de strings por la cantidad de veces que se repite
    let listaOrdenada = reverse (sortOn (\x -> read (x !! 2) :: Double) lista)
    -- se imprime la lista de listas de strings
    putStrLn "Top 3 de usuarios con más kilómetros recorridos"
    mostrarTop3UsuariosConMayorRecorrido listaOrdenada 1
    putStr "Presione enter para continuar"
    opcion <- getLine
    putStrLn "\ESC[2J"

{-
@name resumen
@description muestra el resumen de viajes, kilometros y facturado
@params none
@returns none
-}
resumenAux :: IO ()
resumenAux = do 
    -- alquileres <- cargarArchivoEnLista "./data/alquileres.csv"
    facturas <- cargarArchivoEnLista "./data/facturas.csv"
    let totalViajes = show (length facturas)
    let totalKilometros = show (sum (map (\sublista -> read (sublista !! 5) :: Double) facturas))
    let totalFacturado = show (sum (map (\sublista -> read (sublista !! 7) :: Double) facturas))
    putStrLn ("Total de viajes: " ++ totalViajes)
    putStrLn ("Total de kilometros: " ++ totalKilometros)
    putStrLn ("Total facturado: " ++ totalFacturado)
    putStr "Presione enter para continuar"
    opcion <- getLine
    putStrLn "\ESC[2J"

{-
@name asignarBicicletas
@description asigna bicicletas a un parqueo
@params none
@returns none
-}
asignarBicicletas :: IO()
asignarBicicletas = do
    putStr "Ingrese el nombre del archivo: "
    nombreArchivo <- getLine
    datos <- leerArchivo nombreArchivo
    let lista = convertirStringALista datos
    
    if lista == [] then do
        putStrLn "El archivo que ingresaste está vacío."
    else
        asignarBicicletasAux lista
    
    putStrLn "El lote de bicicletas se ha actualizado."
    
{-
@name asignarBicicletasAux
@description función auxiliar para asignar bicicletas
@params lista de listas de strings
@returns none
-}
asignarBicicletasAux :: [[String]] -> IO()
asignarBicicletasAux [] = do
    putStrLn "Fin de la lista de bicicletas"
asignarBicicletasAux (x:xs) = do
    bicicletas <- cargarBicicletas -- [[id, tipo, id_parqueo]]
    parqueos <- cargarParqueosSistema -- [[id, nombre, direccion, provincia, latitud, longitud]]
    print bicicletas
    print parqueos
    putStrLn "\ESC[2J"
    let bicicleta = filter (\y -> y !! 0 ==  x !! 0) bicicletas
    let parqueo = filter (\y -> y !! 0 == x !! 1) parqueos
    if bicicleta == [] then do
        putStrLn ("× La bicicleta con id " ++ head x ++ " no existe")
    else if parqueo == [] then do
        putStrLn ("× El parqueo con id " ++ head (tail x) ++ " no existe")
    else if head bicicleta !! 2 == "transito" then do
        putStrLn ("× La bicicleta con id " ++ head x ++ "  está en transito")
    else if head bicicleta !! 2 == head parqueo !! 0 then do
        putStrLn ("× La bicicleta ya está en el parqueo.")
    else do
        let bicicletasSinCambios = filter (\y -> y !! 0 /=  x !! 0) bicicletas
        let bicicletaNuevaInfo  = [[head bicicleta !! 0, head bicicleta !! 1, head parqueo !! 0]]
        let nuevoLoteBicicletas = bicicletasSinCambios ++ bicicletaNuevaInfo
        let loteBicicletas_datosActualizados = convertirListaAString nuevoLoteBicicletas
        print bicicletas
        print parqueos
        putStrLn "\ESC[2J"
        writeFile "./data/bicicletas.csv" loteBicicletas_datosActualizados
    asignarBicicletasAux xs