module OpcionesGenerales where


--funcion de test
leerArchivoT2 :: String -> IO String
leerArchivoT2 nombre = do
    contenido <- readFile nombre
    return contenido