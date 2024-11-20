module Opcao7 (salvarESair) where

import Control.Exception
import Data.Char (toUpper)
import Data.IORef
import Importantes (Estudante (..), pegarDadosEstudante)
import System.Directory (doesFileExist)
import System.IO (IOMode (AppendMode, WriteMode), hClose, hPutStrLn, openFile, withFile)
import Text.Printf
import Prelude

-- Parâmetros:
--  1. IORef String com o nome do arquivo de texto.
--  2. IORef [Estudante] com a referência para a lista de estudantes.
--
-- Funcionalidade:
--  1. Verifica se foi fornecido um caminho de arquivo válido. Caso contrário, solicita ao usuário
--     se deseja sair ou continuar.
--  2. Se o arquivo especificado já existe, pergunta ao usuário se deseja sobrescrever o arquivo existente.
--  3. Caso o usuário opte por sobrescrever, a função chama "sobrescreverNoArquivo" para salvar os dados
--     de estudantes no arquivo e então imprime uma mensagem de despedida.
--  4. Se o arquivo não existir, pergunta ao usuário se deseja criar um novo arquivo. Caso afirmativo,
--     a função chama "sobrescreverNoArquivo" para criar o arquivo com os dados de estudantes e depois imprime
--     a mensagem de despedida.
--  5. Caso o usuário forneça uma opção inválida em qualquer momento, a função informa o erro e retorna
--     "False", voltando ao menu principal.
--  6. O valor de retorno é um "Bool" indicando se o processo foi bem-sucedido ("True" se o usuário optou
--     por sair ou confirmou a ação, "False" caso contrário).
salvarESair :: IORef String -> IORef [Estudante] -> IO Bool
salvarESair path estudantes = do
  caminho <- readIORef path

  if caminho == ""
    then do
      putStrLn "Nao foi aberto nenhum arquivo. Deseja sair? (S/N)"
      opcao <- getLine
      case toUpper (head opcao) of
        'S' -> putStrLn "Ate a proxima." >> return True
        'N' -> return False
        _ -> do
          putStrLn "Opcao inválida."
          return False
    else do
      existe <- doesFileExist caminho

      if existe
        then do
          putStrLn $ "O arquivo " ++ caminho ++ " ja existe. Deseja sobrescreve-lo? (S/N)"
          opcao <- getLine
          case toUpper (head opcao) of
            'S' -> do
              sobrescreverNoArquivo caminho estudantes
              putStrLn "Ate a proxima."
              return True
            'N' -> putStrLn "Ate a proxima." >> return True
            _ -> do
              putStrLn "Opcao inválida."
              return False
        else do
          putStrLn $ "O arquivo " ++ caminho ++ " nao existe. Deseja criar um novo? (S/N)"
          opcao <- getLine
          case toUpper (head opcao) of
            'S' -> do
              sobrescreverNoArquivo caminho estudantes
              putStrLn "Ate a proxima."
              return True
            'N' -> putStrLn "Ate a proxima." >> return True
            _ -> do
              putStrLn "Opcao inválida."
              return False

-- Parâmetros:
--  1. String com o caminho do arquivo de texto onde os dados serão salvos.
--  2. IORef [Estudante] com a referência para a lista de estudantes a ser salva no arquivo.
--
-- Funcionalidade:
--  1. A função começa lendo a lista de estudantes armazenada no IORef.
--  2. Em seguida, mapeia cada estudante para obter as informações relacionadas ao aluno (Nome, Matrícula, Notas, ...).
--  3. Utiliza a função `withFile` para abrir o arquivo no modo de escrita (`WriteMode`), garantindo que o arquivo de texto
--     esteja limpo.
--  4. A função chama `escreverLinhasArquivo`, passando o caminho do arquivo e os dados mapeados,
--     para efetivamente escrever os dados no arquivo.
--  5. O arquivo é escrito com as informações dos estudantes e o processo é concluído.
sobrescreverNoArquivo :: String -> IORef [Estudante] -> IO ()
sobrescreverNoArquivo caminho estudantes = do
  estudantes_lista <- readIORef estudantes
  let dados = map pegarDadosEstudante estudantes_lista
  escreverLinhasArquivo caminho dados

-- Parâmetros:
--  1. IORef String com o nome do arquivo de texto.
--  2. IORef [Estudante] com a referência para a lista de estudantes.
--
-- Funcionalidade:
--  1. A função converte os dados de estudantes em uma string formatada, onde cada tupla de dados é transformada
--     em uma lista de strings representando as informações de cada estudante (como nome, matrícula, notas, etc.).
--  2. O texto gerado pela conversão das tuplas é então concatenado em uma única string, onde cada linha corresponde
--     aos dados de um estudante.
--  3. A função tenta abrir o arquivo no modo de anexação (`AppendMode`) para adicionar os dados ao final do arquivo existente.
--  4. Caso o arquivo seja aberto com sucesso, os dados são escritos no arquivo usando a função `hPutStrLn`.
--  5. Caso ocorra algum erro ao tentar escrever no arquivo (como problemas de permissão ou inexistência do arquivo),
--     um erro é capturado e a mensagem "Erro em sobrescrever o arquivo." é exibida.
--  6. Se a operação for bem-sucedida, nenhum valor é retornado.
--  7. A função `capturarDados` serve para formatar cada tupla de dados de um estudante como uma lista de strings,
--     que posteriormente são convertidas em linhas de texto para escrita no arquivo.
escreverLinhasArquivo :: String -> [(String, String, [Double], [Double], Int, Double, Char)] -> IO ()
escreverLinhasArquivo caminho dados = do
  -- map capturarDados dados -> retorno [[String :: Informações, ...], ...]
  -- map (unlines . capturarDados) dados -> retorno [String :: Inf, ...]
  -- concatMap (unlines . capturarDados) dados -> Retorno String :: Todas Infor.
  let texto = concatMap (unlines . capturarDados) dados

  resultado <-
    try
      ( withFile
          caminho
          WriteMode
          ( \h -> do
              hPutStrLn h texto
          )
      ) ::
      IO (Either IOException ())
  case resultado of
    Left ex -> putStrLn "Erro em sobrescrever o arquivo."
    Right _ -> return ()
  where
    capturarDados :: (String, String, [Double], [Double], Int, Double, Char) -> [String]
    capturarDados (nome, matricula, [n1, n2, n3], [t1, t2], faltas, nota_final, situacao) =
      [ nome,
        matricula,
        show n1,
        show n2,
        show n3,
        show t1,
        show t2,
        show faltas,
        show nota_final,
        [situacao]
      ]
