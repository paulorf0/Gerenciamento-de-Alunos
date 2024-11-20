module Importantes (Estudante (..), pegarDadosEstudante, converterLinha, lerArquivo, informacoesFinais) where

import Control.DeepSeq (deepseq)
import Control.Exception
import Data.IORef
import System.Directory (doesFileExist)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetContents, openFile)

-- O tipo de dado Estudante, utilizado para armazenar informações capturadas do arquivo de texto.
data Estudante = Estudante
  { -- Nome do estudante
    nome :: String,
    -- Matrícula do estudante
    matricula :: String,
    -- Lista com três notas das provas
    notas :: [Double],
    -- Lista com duas notas dos trabalhos
    ts :: [Double],
    -- Número de faltas
    faltas :: Int,
    -- Nota final do estudante
    final :: Double,
    -- Situação do estudante ('A' = Aprovado, 'R' = Recuperação, 'F' = Reprovado por falta)
    situacao :: Char
  }

-- Retorno das informações dos estudantes.
pegarDadosEstudante :: Estudante -> (String, String, [Double], [Double], Int, Double, Char)
pegarDadosEstudante estudante =
  ( nome estudante,
    matricula estudante,
    take 3 (notas estudante ++ repeat 0),
    take 2 (ts estudante ++ repeat 0),
    faltas estudante,
    final estudante,
    situacao estudante
  )

-- Quando o programar ler o arquivo txt, essa função converter os dados do arquivo no tipo estudante.
converterLinha :: [String] -> Estudante
converterLinha dados =
  case dados of
    [n, m, n1, n2, n3, t1, t2, fal, final, s] ->
      Estudante
        n
        m
        [read n1 :: Double, read n2 :: Double, read n3 :: Double]
        [read t1 :: Double, read t2 :: Double]
        (read fal :: Int)
        (read final :: Double)
        (head s)
    _ -> error "Erro na conversao para tipo Estudante."

-- Lê um arquivo de texto contendo informações de estudantes e popula um `IORef` com os registros.
--
-- Cada estudante é representado por 10 linhas no arquivo, na seguinte ordem:
-- nome, matrícula, 3 notas, 2 notas de trabalhos, faltas, nota final e situação.
--
-- Parâmetros:
--    Caminho para o arquivo de texto.
--    estudanteRef Referência para armazenar a lista de estudantes.
--
-- Se o arquivo não existir ou houver um erro ao abri-lo, uma mensagem de erro é exibida.
lerArquivo :: String -> IORef [Estudante] -> IO ()
lerArquivo caminho estudanteRef = do
  existe <- doesFileExist caminho
  if existe
    then do
      resultado <- try (openFile caminho ReadMode) :: IO (Either IOException Handle)
      case resultado of
        Left e -> do
          putStrLn "Erro ao abrir o arquivo."
          return ()
        Right fileHandle -> do
          dados_arquivo <- hGetContents fileHandle
          deepseq dados_arquivo (hClose fileHandle)

          let linhas = lines dados_arquivo

          let processarLinhas [""] = []
              processarLinhas [] = []
              processarLinhas ["\n"] = []
              processarLinhas linhas' =
                -- A maneira que os dados são colocados no arquivo de texto, como foi mostrado no pdf de instruções, faz-se necessário pegar as 10 primeiras linhas com take.
                let dados = take 10 linhas'
                    resto = drop 10 linhas'
                    dados_do_estudante = converterLinha dados
                 in if length dados == 10
                      then dados_do_estudante : processarLinhas resto
                      else processarLinhas resto

          let estudantes_ = processarLinhas linhas
          if null estudantes_
            then return ()
            else do
              writeIORef estudanteRef estudantes_
              putStrLn $ "Arquivo " ++ caminho ++ " lido com sucesso!"
    else do
      putStrLn "Arquivo nao existe."
      return ()

-- Cálculo da Situação Final e Nota Final do estudante.
informacoesFinais :: Double -> Double -> Double -> Double -> Double -> Int -> (Double, (String, Char))
informacoesFinais n1 n2 n3 t1 t2 fal =
  let nota_final = n1 + n2 + n3 + t1 + t2
      situacao
        | fal > 18 = ("Reprovado por faltas", 'F')
        | nota_final < 60 = ("Recuperacao", 'R')
        | otherwise = ("Aprovado", 'A')
   in (nota_final, situacao)
