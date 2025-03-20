module TrabalhoFinal where

import Data.List (groupBy)
import System.IO (writeFile, readFile)
import Control.Exception (catch, IOException)

------------------------------------------------------------------------------------------------------------------
-- Tipos de dados

type Localizacao = (Int, Int, Int)
type EstadoNave = (Localizacao, Bool)
type Movimentacao = (Int, Int, Int)
type ID = String
type ListaNaves = ([Movimentacao], ID)
type Restricao = ((Int, Int, Int), (Int, Int, Int))

------------------------------------------------------------------------------------------------------------------
-- Restrições de movimentação e validação da mesma

initSpace :: IO Restricao
initSpace = do
    putStrLn "Digite os valores mínimos (x, y, z):"
    minVals <- readLn :: IO (Int, Int, Int)
    putStrLn "Digite os valores máximos (x, y, z):"
    maxVals <- readLn :: IO (Int, Int, Int)
    let restricao = (minVals, maxVals)
    putStrLn $ "Restrição configurada: " ++ show restricao
    return restricao

validaRestricao :: Restricao -> EstadoNave -> Bool
validaRestricao ((xMin, yMin, zMin), (xMax, yMax, zMax)) ((x, y, z), _) =
    x >= xMin && x <= xMax && y >= yMin && y <= yMax && z >= zMin && z <= zMax

------------------------------------------------------------------------------------------------------------------
-- Verifica embates

verificaEmbates :: [EstadoNave] -> [String] -> [String]
verificaEmbates estados ids = 
    [id1 ++ " e " ++ id2 | ((loc1, _), id1) <- zip estados ids, ((loc2, _), id2) <- zip estados ids, loc1 == loc2, id1 /= id2]

------------------------------------------------------------------------------------------------------------------
-- Movimentações das naves e validação

move :: Movimentacao -> EstadoNave -> EstadoNave
move (dx, dy, dz) ((x, y, z), ligado) = ((x + dx, y + dy, z + dz), ligado)

validaMovimento :: Restricao -> Movimentacao -> EstadoNave -> Either String EstadoNave
validaMovimento restricao movimento estado@(localizacao, ligado)
    | not ligado = Left "Erro: Nave desligada."
    | otherwise =
        let novoEstado@(novaLoc@(x, y, z), _) = move movimento estado
        in if z < 0 then Left "Erro: Coordenada Z negativa."
           else if not (validaRestricao restricao novoEstado) then Left "Erro: Fora da área permitida."
           else Right novoEstado

------------------------------------------------------------------------------------------------------------------
-- Atualização de Estado

atualizaAcao :: Bool -> EstadoNave -> EstadoNave
atualizaAcao ligacao (localizacao, _) = (localizacao, ligacao)

------------------------------------------------------------------------------------------------------------------
-- Processamento de Instruções de uma ou mais naves

processarInstrucoes :: Restricao -> EstadoNave -> [String] -> IO EstadoNave
processarInstrucoes restricao estado [] = return estado
processarInstrucoes restricao estado (instr:resto) = do
    let partes = words instr
    case partes of
        ("init":coords:estadoInicial:_) -> do
            let novaLoc = read coords :: (Int, Int, Int)
            let ligada = read estadoInicial == 1
            let novoEstado = (novaLoc, ligada)
            if validaRestricao restricao novoEstado
                then processarInstrucoes restricao novoEstado resto
                else do
                    putStrLn "Erro: Posição inicial fora da restrição."
                    processarInstrucoes restricao estado resto
        ("move":mov:_) -> do
            let movimento = read mov :: Movimentacao
            case validaMovimento restricao movimento estado of
                Left err -> do
                    putStrLn err
                    processarInstrucoes restricao estado resto
                Right novoEstado -> processarInstrucoes restricao novoEstado resto
        ("acao":"ligar":_) -> processarInstrucoes restricao (atualizaAcao True estado) resto
        ("acao":"desligar":_) -> processarInstrucoes restricao (atualizaAcao False estado) resto
        _ -> do
            --putStrLn "Erro: Instrução inválida."
            processarInstrucoes restricao estado resto

------------------------------------------------------------------------------------------------------------------
-- Leitura e Manipulação de Ficheiros

salvarFicheiro :: FilePath -> String -> IO ()
salvarFicheiro caminho conteudo = writeFile caminho conteudo

parseFicheiro :: String -> [(ID, [String])]
parseFicheiro conteudo =
    let linhas = filter (not . null) (lines conteudo) --O filter serve para remover linhas vazias
        grupos = groupBy (\a b -> head (words a) == head (words b)) linhas --groupBy para agrupar linhas por ID de nave
    in map (\grupo -> (head (words (head grupo)), map (unwords . tail . words) grupo)) grupos

readfile :: IO ()
readfile = do
    putStrLn "Lista de naves:"
    conteudo <- readFile "Alienship.txt"  -- Lê o ficheiro
    let parsed = parseFicheiro conteudo  -- Organiza o conteúdo
    mapM_ (\(id, instrs) -> do
              putStrLn ("\nID: " ++ id)
              mapM_ putStrLn instrs) parsed  -- Exibe cada nave e suas instruções

------------------------------------------------------------------------------------------------------------------
-- Execução de uma nave

executnave :: Restricao -> IO ()
executnave restricao = do
    putStrLn "Introduza o ID da nave que deseja controlar:"
    idNave <- getLine
    conteudo <- catch (readFile "Alienship.txt") (\e -> do putStrLn ("Erro ao ler o ficheiro: " ++ show (e :: IOException)); return "")
    let parsed = parseFicheiro conteudo
    case lookup idNave parsed of
        Just instrs -> do
            putStrLn $ "\nInstruções encontradas para a nave " ++ idNave ++ ":"
            mapM_ putStrLn instrs
            let estadoInicial = ((0, 0, 0), False)
            estadoFinal <- processarInstrucoes restricao estadoInicial instrs
            putStrLn $ "Estado final da nave " ++ idNave ++ ": " ++ show estadoFinal
        Nothing -> putStrLn $ "ID de nave inválido: " ++ idNave
------------------------------------------------------------------------------------------------------------------
-- Menu Principal

menu :: IO ()
menu = do
    restricao <- initSpace
    let loop = do
            putStrLn "\nSelecione a opção: \n1- Listar naves do ficheiro \n2- Executar uma nave \n3- Execução manual de uma ou mais naves \n4- Executação de todas as naves \n5- Sair"
            op <- getLine
            case op of
                "1" -> do
                    readfile
                    loop
                "2" -> do
                    executnave restricao
                    loop
                "3" -> do
                    executnaveManual restricao
                    loop
                "4" -> do
                    executarTodasNaves restricao
                    loop
                "5" -> putStrLn "Execução Terminada..."
                _ -> do
                    putStrLn "Opção inválida. Tente novamente."
                    loop
    loop


executnaveManual :: Restricao -> IO ()
executnaveManual restricao = do
    putStrLn "\nIntroduza as ações para uma ou mais naves (digite init, move, acao (ligar ou desligar) e fim para terminar):"
    let loop estados = do
            putStrLn "Digite as ações que deseja ( AB123 init (1,2,3)):"
            entrada <- getLine
            if entrada == "fim"
                then do
                    putStrLn "Execução finalizada. Estados finais:"
                    mapM_ (\(id, estado) -> putStrLn $ "ID: " ++ id ++ " -> Estado: " ++ show estado) estados
                else do
                    let palavras = words entrada
                    case palavras of
                        (id:acao:resto) -> do
                            let valores = unwords resto
                            let estadoAtual = lookup id estados
                            case (acao, estadoAtual) of
                                ("init", _) -> do
                                    let loc = read valores :: Localizacao
                                    let novoEstado = (loc, False)
                                    if validaRestricao restricao novoEstado
                                        then loop ((id, novoEstado) : filter ((/= id) . fst) estados)
                                        else do
                                            putStrLn "Erro: Posição fora das restrições."
                                            loop estados
                                ("move", Just estado) -> do
                                    let mov = read valores :: Movimentacao
                                    case validaMovimento restricao mov estado of
                                        Left err -> do
                                            putStrLn err
                                            loop estados
                                        Right novoEstado -> loop ((id, novoEstado) : filter ((/= id) . fst) estados)
                                ("acao", Just estado) -> case valores of
                                    "ligar" -> loop ((id, atualizaAcao True estado) : filter ((/= id) . fst) estados)
                                    "desligar" -> loop ((id, atualizaAcao False estado) : filter ((/= id) . fst) estados)
                                    _ -> do
                                        putStrLn "Erro: Ação inválida."
                                        loop estados
                                _ -> do
                                    putStrLn "Erro: Nave não inicializada ou comando inválido."
                                    loop estados
                        _ -> do
                            putStrLn "Erro: Formato inválido."
                            loop estados
    loop []

executarTodasNaves :: Restricao -> IO ()
executarTodasNaves restricao = do
    putStrLn "\nExecutando ações de todas as naves do ficheiro..."
    conteudo <- catch (readFile "Alienship.txt") (\e -> do putStrLn ("Erro ao ler o ficheiro: " ++ show (e :: IOException)); return "")
    let parsed = parseFicheiro conteudo
    let loop [] estados = do
            putStrLn "\nExecução finalizada. Estados finais:"
            mapM_ (\(id, estado) -> putStrLn $ "ID: " ++ id ++ " -> Estado: " ++ show estado) estados
            let embates = verificaEmbates (map snd estados) (map fst estados)
            if not (null embates)
                then putStrLn $ "Embates detectados entre as naves: " ++ unwords embates
                else putStrLn "Nenhum embate detectado."
            let estadosFinais = unlines [id ++ " " ++ show loc ++ " " ++ show ligado | (id, (loc, ligado)) <- estados]
            salvarFicheiro "Alienship_Atualizado.txt" estadosFinais
        loop ((id, instrs):resto) estados = do
            let estadoInicial = lookup id estados
            novoEstado <- case estadoInicial of
                Nothing -> processarInstrucoes restricao ((0, 0, 0), False) instrs
                Just estado -> processarInstrucoes restricao estado instrs
            loop resto ((id, novoEstado) : filter ((/= id) . fst) estados)
    loop parsed []
