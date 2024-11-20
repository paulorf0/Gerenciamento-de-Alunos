module MenuPrincipal (menu) where

import Data.IORef
import Importantes (Estudante (..))
import Opcao1 (lerTurma)
import Opcao2 (imprimirTurma)
import Opcao3 (imprimirEstatisticas)
import Opcao4 (cadastrarEstudante)
import Opcao5 (editarEstudante)
import Opcao6 (relerTurma)
import Opcao7 (salvarESair)

-- Exibe o menu principal e gerencia as opções selecionadas pelo usuário.
--
-- Detalhes:
-- 1. O menu apresenta diversas opções para manipulação de dados de estudantes.
-- 2. As entradas do usuário são lidas em tempo real devido ao uso de "NoBuffering" no "main".
-- 3. A função utiliza "IORef" para manter estado entre as operações, permitindo
--    modificar dinamicamente a lista de estudantes e o caminho do arquivo.
--
-- Parâmetros:
-- - "caminho": Um "IORef String" que armazena o caminho do arquivo de dados dos estudantes.
-- - "estudantes": Um "IORef [Estudante]" que contém a lista de estudantes.
--
-- Funcionamento:
-- - O programa exibe o menu e solicita uma opção ao usuário.
-- - Após capturar a opção, executa a operação correspondente.
-- - Após concluir uma operação, retorna ao menu principal, exceto na opção de saída.
-- - Em caso de entrada inválida, exibe uma mensagem de erro e reinicia o menu.
menu :: IORef String -> IORef [Estudante] -> IO ()
menu caminho estudantes = do
  putStrLn "========================================="
  putStrLn "            Menu Principal"
  putStrLn "========================================="
  putStrLn "Opcoes:"
  putStrLn "1 - Ler turma de estudantes do arquivo"
  putStrLn "2 - Imprimir turma de estudantes"
  putStrLn "3 - Imprimir estatísticas da turma"
  putStrLn "4 - Cadastrar novo estudante"
  putStrLn "5 - Editar informacoes de um estudante"
  putStrLn "6 - Reler turma de estudantes do arquivo"
  putStrLn "7 - Salvar e Sair"
  putStr "Digite uma opcao: "

  opcao <- getLine
  case opcao of
    "1" -> lerTurma caminho estudantes >> menu caminho estudantes
    "2" -> imprimirTurma estudantes >> menu caminho estudantes
    "3" -> imprimirEstatisticas estudantes >> menu caminho estudantes
    "4" -> cadastrarEstudante caminho estudantes >> menu caminho estudantes
    "5" -> editarEstudante caminho estudantes >> menu caminho estudantes
    "6" -> relerTurma caminho estudantes >> menu caminho estudantes
    "7" -> do
      sair <- salvarESair caminho estudantes
      if sair then return () else menu caminho estudantes
    _ -> do
      putStrLn "Opcao invalida"
      menu caminho estudantes