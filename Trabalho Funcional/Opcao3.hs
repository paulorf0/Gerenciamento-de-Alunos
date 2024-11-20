module Opcao3 (imprimirEstatisticas) where

import Data.IORef
import Data.List (sort)
import GHC.Base (IO)
import GHC.Float (Double, Floating (sqrt))
import Importantes (Estudante (..))
import Text.Printf
import Prelude

-- Parâmetro:
--    IORef do tipo lista de Estudante (IORef [Estudante])
-- Funcionalidade:
--    1. Define funções importantes que serão utilizadas para obter as estatísticas.
--      - CapturarValores : Usada especificamente para aplicar uma função "f" que é
--        aplicada sobre cada estudante e o retorno é uma lista do tipo [Double]
--      - aplicarFuncEstudante : De maneira geral, aplica uma função "f" que retorna
--        um tipo genérico "a". A função aplicarFuncEstudante retorna o tipo [a].
--      - funcoesChaves : Uma lista de funções lambdas que é utilizado para obter dados
--        de notas dos estudantes.
--      - imprimirSit : Utilizado para imprimir de maneira formatada as estatísticas de
--        aprovados, reprovados por falta e recuperação.
imprimirEstatisticas :: IORef [Estudante] -> IO ()
imprimirEstatisticas estudantesRef = do
  --  Faz a leitura do IORef, transformando do tipo IORef para uma lista [Estudante].
  estudantes <- readIORef estudantesRef
  if null estudantes
    then do
      putStrLn "Nao ha turma carregada!"
      return ()
    else do
      let capturarValores :: (Estudante -> Double) -> [Double]
          capturarValores f = map f estudantes

      -- Função genérica que aplica uma função "f" ao tipo Estudante.
      let aplicarFuncEstudante :: (Estudante -> a) -> [a]
          aplicarFuncEstudante f = map f estudantes

      let contarSituacoes s = length . filter (== s)

      --  Funções lambdas que serão utilizadas para pegar dados especificos de um estudante
      --  Essas funções serão aplicadas com "map".
      let funcoesChaves =
            [ head . notas,
              \e -> notas e !! 1,
              \e -> notas e !! 2,
              head . ts,
              \e -> ts e !! 1,
              final
            ]

      let totalEstudantes = length estudantes

      --  Bloco de código que captura informações estatísticas
      let notas = map capturarValores funcoesChaves -- [[Double]]
      let maioresNotas = map maximum notas -- [Double]
      let menoresNotas = map minimum notas -- [Double]
      let mediaNotas = map (\e -> sum e / fromIntegral (length e)) notas -- [Double]
      let desvioNotas = map desvioMedia notas -- [Double]
      let medianaNotas = map mediana notas -- [Double]

      --  Bloco de código que prepara as informações da estatística capturadas acima para ser exibidas na tela
      let textoEstatistica = ["Notas medias da turma", "Desvios da media", "Notas medianas da turma"]
      let textoMaioresMenores = ["Maiores notas da turma", "Menores notas da turma"]
      let estatisticas = zip [mediaNotas, desvioNotas, medianaNotas] textoEstatistica
      let maioresMenoresTabela = zip [maioresNotas, menoresNotas] textoMaioresMenores

      --  Bloco de código que para contar os aprovados, reprovados e que estão em recuperação.
      let textoSituacao = ["Numero de estudantes aprovados: ", "Numero de estudantes em recuperacao: ", "Numero de estudantes reprovados por falta: "]
      let situacaoEstudantes = aplicarFuncEstudante situacao -- [Char]
      let (aprovados, reprovados, reprovadosFaltas) =
            ( contarSituacoes 'A' situacaoEstudantes,
              contarSituacoes 'R' situacaoEstudantes,
              contarSituacoes 'F' situacaoEstudantes
            )
      let situacaoDados = zip [aprovados, reprovados, reprovadosFaltas] textoSituacao

      --  Exibe o cabeçalho.
      printf "%45s\n" "Estatística"
      printf "%s\n" (replicate 80 '-')
      printf "%-24s %-5s %-5s %-5s %-5s %-5s %-5s \n" "" "N1" "N2" "N3" "T1" "T2" "Final"
      printf "%s\n" (replicate 80 '-')

      mapM_ imprimirValores maioresMenoresTabela
      mapM_ imprimirEstatisticasTexto estatisticas

      let imprimirSit (valor, texto) =
            printf
              "%-45s %-3d (%.1f%%)\n"
              texto
              valor
              ((fromIntegral valor :: Double) * 100.0 / fromIntegral totalEstudantes :: Double)

      printf "%s\n" (replicate 80 '-')
      mapM_ imprimirSit situacaoDados

      putStrLn "Histograma de notas finais em grupos de 10 pontos: "
      histograma (last notas)

-- Utilizado para imprimir de maneira formatada as estatísticas de maiores e menores notas.
imprimirValores :: ([Double], String) -> IO ()
imprimirValores (valores, texto) =
  printf
    "%-24s %-5.0f %-5.0f %-5.0f %-5.0f %-5.0f %-5.0f\n"
    texto
    (head valores)
    (valores !! 1)
    (valores !! 2)
    (valores !! 3)
    (valores !! 4)
    (valores !! 5)

-- Utilizado para imprimir de maneira formatada as estatísticas de
-- Média ; Desvio de Média ; Mediana
imprimirEstatisticasTexto :: ([Double], String) -> IO ()
imprimirEstatisticasTexto (valores, texto) =
  printf
    "%-24s %-5.1f %-5.1f %-5.1f %-5.1f %-5.1f %-5.1f\n"
    texto
    (head valores)
    (valores !! 1)
    (valores !! 2)
    (valores !! 3)
    (valores !! 4)
    (valores !! 5)

-- Utilizado para calcular o desvio padrão das notas.
desvioMedia :: [Double] -> Double
desvioMedia e = sqrt (sum (map (\x -> (x - m) ^ 2) e) / tamanho)
  where
    tamanho = fromIntegral (length e)
    m = sum e / tamanho

mediana :: [Double] -> Double
mediana e
  | even (length e) = (sortList !! (middle - 1) + sortList !! middle) / 2
  | otherwise = sortList !! middle
  where
    middle = length e `div` 2
    sortList = sort e

-- Função que imprime o histograma de notas.
histograma :: [Double] -> IO ()
histograma notas = do
  -- Obtém a quantidade de notas que existe entre cada intervalo da lista de intervalos.
  let frequenciaNotas = map (\(inf, sup) -> length (filter (\x -> x >= inf && x <= sup) notas)) intervalos
  -- Cria os dados para serem impressos.
  let informacoes = zip intervalos frequenciaNotas

  let printarHistograma ((inf, sup), quant) =
        printf
          "%-3.0f - %-3.0f %-3d %s\n"
          inf
          sup
          quant
          (printarEstrela quant)

  mapM_ printarHistograma informacoes
  where
    printarEstrela 0 = ""
    printarEstrela x = "*" ++ printarEstrela (x - 1)
    intervalos = [(0, 10), (11, 20), (21, 30), (31, 40), (41, 50), (51, 60), (61, 70), (71, 80), (81, 90), (91, 100)] :: [(Double, Double)]