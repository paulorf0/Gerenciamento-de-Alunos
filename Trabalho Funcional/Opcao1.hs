module Opcao1 (lerTurma) where

import Data.Char (toUpper)
import Data.IORef
import Importantes (Estudante (..), lerArquivo)
import System.Directory (doesFileExist)
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)
import Text.Printf

-- Exibe o menu para gerenciar a leitura ou criação de turmas.
--
-- Detalhes:
-- 1. Apresenta opções para o usuário:
-- 2. Redireciona as opções selecionadas para funções auxiliares.
-- 3. Em caso de entrada inválida, exibe uma mensagem de erro e retorna ao menu.
--
-- Parâmetros:
-- - "caminho": Um "IORef String" que armazena o caminho do arquivo de turma.
-- - "estudantes": Um "IORef [Estudante]" que contém os dados da turma carregada.
lerTurma :: IORef String -> IORef [Estudante] -> IO ()
lerTurma caminho estudantes = do
  printf "%s\n" (replicate 42 '=')
  printf "%25s\n" "Ler Turma"
  printf "%s\n" (replicate 42 '=')
  putStrLn "Opcoes:"
  putStrLn "1 - Ler turma existente"
  putStrLn "2 - Criar nova turma"
  putStrLn "3 - Voltar ao menu principal"

  putStr "Digite uma opcao: "
  opcao <- getLine
  case opcao of
    "1" -> lerTurmaOpcao1 caminho estudantes >> lerTurma caminho estudantes
    "2" -> lerTurmaOpcao2 caminho estudantes >> lerTurma caminho estudantes
    "3" -> return ()
    _ -> putStrLn "Opcao invalida."

-- Lê uma turma existente de um arquivo e atualiza os dados no "IORef" correspondente.
--
-- Funcionamento:
-- 1. Solicita ao usuário o nome do arquivo contendo os dados da turma.
-- 2. Atualiza o caminho no "IORef caminho".
-- 3. Chama a função "lerArquivo" para processar o arquivo e armazenar os dados no "IORef estudantes".
--
-- Parâmetros:
-- - "path": Um "IORef String" que armazena o caminho do arquivo de turma.
-- - "estudantes": Um "IORef [Estudante]" onde os dados da turma serão armazenados.
lerTurmaOpcao1 :: IORef String -> IORef [Estudante] -> IO ()
lerTurmaOpcao1 path estudantes = do
  putStr "Digite o nome do arquivo com a turma: "
  caminho <- getLine
  existe <- doesFileExist caminho
  if existe
    then do
      writeIORef path caminho
      lerArquivo caminho estudantes
    else do
      putStrLn "Arquivo nao existe."
      return ()

-- Cria uma nova turma em um arquivo. Se o arquivo já existir, solicita confirmação para sobrescrever.
--
-- Funcionamento:
-- 1. Solicita ao usuário o nome do arquivo para a nova turma.
-- 2. Verifica se o arquivo já existe:
--    - Caso exista:
--        - Solicita confirmação do usuário para sobrescrever o arquivo.
--        - Se confirmado, cria um arquivo vazio.
--    - Caso contrário, cria o arquivo vazio diretamente.
-- 3. Atualiza o caminho no "IORef caminho".
--
-- Parâmetros:
-- - "path": Um "IORef String" que armazena o caminho do arquivo.
--
-- Obs:
-- Os dados são armazenados no IORef "estudantes", que é inicializado como uma lista vazia. Essa lista não precisa
-- ser atualizada ao criar um novo arquivo, pois este não conterá dados inicialmente.
lerTurmaOpcao2 :: IORef String -> IORef [Estudante] -> IO ()
lerTurmaOpcao2 path estudanteRef = do
  putStr "Digite o nome do arquivo para a nova turma:"
  caminho <- getLine
  existe <- doesFileExist caminho
  writeIORef path caminho
  writeIORef estudanteRef []

  if existe
    then do
      putStrLn $ "Arquivo " ++ caminho ++ " ja existe. Deseja sobrescreve-lo? (S/N)"
      opcao <- getLine
      case toUpper (head opcao) of
        'S' -> do
          writeFile caminho ""
        'N' -> do
          lerArquivo caminho estudanteRef
          return ()
        _ -> putStrLn "Opcao invalida"
    else do
      withFile
        caminho
        WriteMode
        ( \h -> do
            hPutStrLn h ""
        )
      putStrLn $ "Arquivo " ++ caminho ++ " criado com sucesso."
