import System.Random

main = do
    let tablero = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
    mil24Cargar tablero

mil24Cargar tablero = do
    tablero <- agregarRandom tablero
    tablero <- agregarRandom tablero
    tablero <- agregarRandom tablero
    mil24 tablero 0

mil24 tablero score = do
    imprimir tablero
    putStr "score: "
    putStrLn (show score)

    let (posiblesMovimientos1, _) = moverArriba tablero 0 0 tableroLocked 3 score
    let (posiblesMovimientos2, _) = moverAbajo tablero 0 0 tableroLocked 3 score
    let (posiblesMovimientos3, _) = moverDerecha tablero 0 0 tableroLocked 3 score
    let (posiblesMovimientos4, _) = moverIzquierda tablero 0 0 tableroLocked 3 score

    if buscarNum tablero 1024
    then do
        putStrLn "   1024! WIN   "
        return()
    else do
    if tablero == posiblesMovimientos1 &&
        tablero == posiblesMovimientos2 &&
        tablero == posiblesMovimientos3 &&
        tablero == posiblesMovimientos4
    then do
        putStrLn "no hay posibles movimientos GAME OVER"
        return()
    else do
        (nuevoTablero, nuevoScore) <- insertaInstruccion tablero score
        if nuevoTablero == []
        then
            return()
        else
            if tablero == nuevoTablero
            then
                mil24 nuevoTablero score
            else do
                nuevoTablero <- agregarRandom nuevoTablero
                mil24 nuevoTablero nuevoScore

agregarRandom tablero = do
    if buscarNum tablero 0
    then do
        r <- randomRIO (1 :: Int, 2 :: Int)
        let r2 = r * 2
        x <- randomRIO (0 :: Int, 3 :: Int)
        y <- randomRIO (0 :: Int, 3 :: Int)
        let celdaVerificar = obtenerNumero tablero x y
        if celdaVerificar == 0
        then
            return (colocarNumero tablero x y r2)
        else
            agregarRandom tablero
    else
        return tablero
buscarNum::[[Int]]->Int->Bool
buscarNum [] _ = False
buscarNum (x:xs) num
    | buscarNumFila x num = True
    | otherwise = buscarNum xs num
buscarNumFila::[Int]->Int->Bool
buscarNumFila [] _ = False
buscarNumFila (x:xs) num
    | x == num = True
    | otherwise = buscarNumFila xs num

insertaInstruccion tablero score = do
    putStrLn "ingrese una instruccion:\nw<-arriba  a<-izquierda  s<-abajo  d<-derecha   p<-salir\n"
    comando <- getLine
    case comando of
        "w" -> return (moverArriba tablero 0 0 tableroLocked 3 score)
        "s" -> return (moverAbajo tablero 3 3 tableroLocked 3 score)
        "a" -> return (moverIzquierda tablero 0 0 tableroLocked 3 score)
        "d" -> return (moverDerecha tablero 3 3 tableroLocked 3 score)
        "p" -> return ([], score)
        _   -> return (tablero, score)

tableroLocked::[[Int]]
tableroLocked = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]

moverArriba::[[Int]]->Int->Int->[[Int]]->Int->Int->([[Int]], Int)
moverArriba tablero x y locked contador score
    | x == 0 && y == 4 && contador == 0 = (tablero, score)
    | x == 0 && y == 4 = moverArriba tablero 0 0 locked (contador - 1) score
    | x == 4 = moverArriba tablero 0 (y + 1) locked contador score
    | otherwise = moverArriba tablero2 (x + 1) y locked2 contador score2
    where
        (tablero2, locked2, score2) = recorre tablero x y x (y - 1) locked score
moverIzquierda::[[Int]]->Int->Int->[[Int]]->Int->Int->([[Int]], Int)
moverIzquierda tablero x y locked contador score
    | x == 0 && y == 4 && contador == 0 = (tablero, score)
    | x == 0 && y == 4 = moverIzquierda tablero 0 0 locked (contador - 1) score
    | x == 4 = moverIzquierda tablero 0 (y + 1) locked contador score
    | otherwise = moverIzquierda tablero2 (x + 1) y locked2 contador score2
    where
        (tablero2, locked2, score2) = recorre tablero x y (x - 1) y locked score
moverAbajo::[[Int]]->Int->Int->[[Int]]->Int->Int->([[Int]], Int)
moverAbajo tablero x y locked contador score
    | x == 3 && y == -1 && contador == 0 = (tablero, score)
    | x == 3 && y == -1 = moverAbajo tablero 3 3 locked (contador - 1) score
    | x == -1 = moverAbajo tablero 3 (y - 1) locked contador score
    | otherwise = moverAbajo tablero2 (x - 1) y locked2 contador score2
    where
        (tablero2, locked2, score2) = recorre tablero x y x (y + 1) locked score
moverDerecha::[[Int]]->Int->Int->[[Int]]->Int->Int->([[Int]], Int)
moverDerecha tablero x y locked contador score
    | x == 3 && y == -1 && contador == 0 = (tablero, score)
    | x == 3 && y == -1 = moverDerecha tablero 3 3 locked (contador - 1) score
    | x == -1 = moverDerecha tablero 3 (y - 1) locked contador score
    | otherwise = moverDerecha tablero2 (x - 1) y locked2 contador score2
    where
        (tablero2, locked2, score2) = recorre tablero x y (x + 1) y locked score

recorre::[[Int]]->Int->Int->Int->Int->[[Int]]->Int->([[Int]], [[Int]], Int)
recorre tablero x y x2 y2 locked score
    | x2 < 0 || x2 > 3 || y2 < 0 || y2 > 3 = (tablero, locked, score)
    | 0 == origen = (tablero, locked, score)
    | 0 == destino = (tablero_avanzado, locked, score)
    | origen /= destino || bloqueadoDestino || bloqueadoOrigen = (tablero, locked, score)
    | otherwise = (tablero_sumado, locked_agregado, (score + origen + destino))
    where
        origen = obtenerNumero tablero x y
        destino = obtenerNumero tablero x2 y2
        bloqueadoDestino = 1 == obtenerNumero locked x2 y2
        bloqueadoOrigen = 1 == obtenerNumero locked x y
        tablero_avanzado = colocarNumero (colocarNumero tablero x2 y2 origen) x y 0
        tablero_sumado = colocarNumero (colocarNumero tablero x2 y2 (origen + destino)) x y 0
        locked_agregado = colocarNumero locked x2 y2 1


colocarNumero::[[Int]]->Int->Int->Int->[[Int]]
colocarNumero (fila:restoTablero) x 0 nuevo = (colocarNumeroFila fila x nuevo) : restoTablero
colocarNumero (fila:restoTablero) x y nuevo = fila : colocarNumero restoTablero x (y - 1) nuevo
colocarNumeroFila::[Int]->Int->Int->[Int]
colocarNumeroFila (_:restoFila) 0 nuevo = nuevo : restoFila
colocarNumeroFila (num:restoFila) x nuevo = num : colocarNumeroFila restoFila (x - 1) nuevo

obtenerNumero::[[Int]]->Int->Int->Int
obtenerNumero (fila:_) x 0 = obtenerNumeroFila fila x
obtenerNumero (_:restoTablero) x y = obtenerNumero restoTablero x (y - 1)
obtenerNumeroFila::[Int]->Int->Int
obtenerNumeroFila (v:_) 0 = v
obtenerNumeroFila (_:restoFila) x = obtenerNumeroFila restoFila (x - 1)


imprimir [] = do
    return()
imprimir (x:xs) = do
    imprimirFila x
    putStrLn ""
    imprimir xs
imprimirFila [] = do
    return()
imprimirFila (x:xs) = do
    if x < 10
    then putStr "    "
    else if x < 100
        then putStr "   "
        else if x < 1000
            then putStr "  "
            else putStr " "
    putStr (show x)
    imprimirFila xs

