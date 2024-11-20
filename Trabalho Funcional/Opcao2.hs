module Opcao2 (imprimirTurma) where

import Data.IORef
import Importantes (Estudante (..), pegarDadosEstudante)
import Text.Printf

-- Função principal para imprimir a lista de estudantes.
-- Parâmetros:
--    Recebe um "IORef" que contém a lista de estudantes.
-- Funcionalidade:
--    1. Caso nenhuma turma tenha sido lida anteriormente, é exibida uma mensagem informando que não
--       há turma carregada.
--    2. Caso haja turma carregada, é impresso um cabeçalho e a função `imprimirEstudante`
--       é chamada, passando como argumento cada estudante obtido da leitura do arquivo.
imprimirTurma :: IORef [Estudante] -> IO ()
imprimirTurma estudantes = do
  estudantes_lista <- readIORef estudantes -- Lista [Estudante]
  if null estudantes_lista
    then do
      putStrLn "Nao ha turma carregada!"
      return ()
    else do
      printf "%55s\n" "Estudantes"
      printf "%s\n" (replicate 110 '-')
      printf "%-52s %-13s %-3s %-3s %-3s %-3s %-3s %-4s %-4s %-2s\n" "Nome" "Matricula" "N1" "N2" "N3" "T1" "T2" "Fal" "Final" "Sit"
      printf "%s\n" (replicate 110 '-')

      mapM_ imprimirEstudante estudantes_lista

-- Parâmetros:
--  Recebe um tipo Estudante.
-- Funcionalidade:
--    1. Captura os dados do estudante utilizando a função "pegarDadosEstudante" do módulo "Importantes".
--    2. Imprime com formatação esses dados na tela
imprimirEstudante :: Estudante -> IO ()
imprimirEstudante estudante = do
  let (nome, matricula, notasEstudante, tsEstudante, fal, n_final, sit) = pegarDadosEstudante estudante
  let [n1, n2, n3] = notasEstudante
  let [t1, t2] = tsEstudante

  printf
    "%-52s %-13s %-3.0f %-3.0f %-3.0f %-3.0f %-3.0f %-4d %-4.0f %-2c\n"
    nome
    matricula
    n1
    n2
    n3
    t1
    t2
    fal
    n_final
    sit
