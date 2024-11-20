import Data.IORef (IORef, newIORef)
import Importantes (Estudante)
import MenuPrincipal (menu)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin, stdout)

-- Configura o programa principal, inicializando buffers de entrada e saída
-- e criando referências para armazenar dados dinâmicos.
--
-- Detalhes:
-- 1. O buffering de "stdout" e "stdin" é desativado utilizando "hSetBuffering stdout NoBuffering"
--    e "hSetBuffering stdin NoBuffering". Isso garante que a entrada e a saída sejam processadas
--    imediatamente, sem depender do buffer do sistema operacional.
-- 2. Duas referências ("IORef") são criadas:
--    - "estudantes": Armazena uma lista de registros do tipo "Estudante".
--    - "caminho": Armazena o nome do arquivo texto que será aberto e lido.
--
-- Após a configuração inicial, o controle é passado para a função "menu", que gerencia
-- as interações do usuário.
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering

  estudantes <- newIORef []
  caminho <- newIORef ""

  menu caminho estudantes
