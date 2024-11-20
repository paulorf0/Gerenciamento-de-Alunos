module Opcao6 (relerTurma) where

import Data.IORef
import Importantes (Estudante (..), lerArquivo)

-- Parâmetro:
--  IORef String que armazena o nome do arquivo que foi aberto anteriormente.
-- Funcionalidade:
--  Utiliza a função lerArquivo para ler novamente o arquivo anterior.
relerTurma :: IORef String -> IORef [Estudante] -> IO ()
relerTurma path estudanteRef = do
  caminho <- readIORef path

  lerArquivo caminho estudanteRef
