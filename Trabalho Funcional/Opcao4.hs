module Opcao4 (cadastrarEstudante) where

import Data.IORef
import Importantes (Estudante (..), informacoesFinais)
import Text.Printf
import Text.Read (readMaybe)

-- Uma função que vai lidar com a captura do valor de tipo "Double" escrito
-- pelo usuário. Caso haja algum erro na digitação do usuário, imprime uma
-- mensagem de erro e pede novamente a digitação do valor.
capturarValor :: String -> IO Double
capturarValor prompt = do
  putStr prompt
  input <- getLine
  case readMaybe input :: Maybe Double of
    Just n -> return n
    Nothing -> do
      putStrLn "Entrada inválida."
      capturarValor prompt

-- Paramêtro:
--  1. Recebe um IORef String que armazena o nome do arquivo de texto que
--    contém os dados de usuários
--  2. Recebe um IORef [Estudante] que armazena uma lista do tipo Estudante,
--    e essa lista contém todos os estudantes lidos do arquivo de texto.
--
-- Funcionalidade:
--  1. Imprime um cabeçalho.
--  2. Captura informações:
--    - Nome
--    - Matricula
--    - Nota 1
--    - Nota 2
--    - Nota 3
--    - Nota de trabalho 1
--    - Nota de trabalho 2
--    - Faltas
--  3. Calcula a nota final e a situação do estudante cadastrado e exibe na tela.
--  4. Cria um novo tipo Estudante com os dados coletados
--  5. Modifica o IORef [Estudante] adicionando a lista o novo estudante que acaba de ser cadastrado.
cadastrarEstudante :: IORef String -> IORef [Estudante] -> IO ()
cadastrarEstudante path estudanteRef = do
  -- readIORef :: IORef a -> IO a
  estudantes <- readIORef estudanteRef
  caminho <- readIORef path
  if caminho == ""
    then do
      putStrLn "Nao ha turma carregada!"
      return ()
    else do
      printf "%s\n" (replicate 100 '=')
      printf "%55s\n" "Cadastrar Novo Estudante"
      printf "%s\n" (replicate 100 '=')

      putStr "Digite o nome: "
      nome <- getLine
      putStr "Digite a matricula: "
      matricula <- getLine

      nota1 <- capturarValor "Digite a primeira nota da prova: "
      nota2 <- capturarValor "Digite a segunda nota da prova: "
      nota3 <- capturarValor "Digite a terceira nota da prova: "
      trabalho1 <- capturarValor "Digite a primeira nota do trabalho: "
      trabalho2 <- capturarValor "Digite a segunda nota do trabalho: "
      faltas <- capturarValor "Digite o numero de faltas: "

      let (nota_final, situac) = informacoesFinais nota1 nota2 nota3 trabalho1 trabalho2 (floor faltas)

      putStrLn $ "Nota final calculada: " ++ show nota_final
      putStrLn $ "Situacao final: " ++ fst situac

      let novoEstudante = Estudante nome matricula [nota1, nota2, nota3] [trabalho1, trabalho2] (floor faltas) nota_final (snd situac)
      -- modifyIORef :: IORef a -> (a -> a) -> IO ()
      modifyIORef estudanteRef (\estudantes -> estudantes ++ [novoEstudante])
      putStrLn "Estudante cadastrado!"