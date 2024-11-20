module Opcao5 (editarEstudante, escolhaAluno) where

import Control.Monad (mapM_)
import Data.Bool (Bool)
import Data.Char (toUpper)
import Data.Foldable (Foldable (length))
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Int (Int)
import Data.List (filter, isInfixOf, replicate, zip)
import Data.Maybe (Maybe (Just, Nothing))
import Data.String (String)
import Data.Tuple (uncurry)
import GHC.Base (IO)
import Importantes (Estudante (..), informacoesFinais)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Prelude
import Prelude (getLine, putStrLn)

-- Parâmetro:
--  IORef String que contém o novo do arquivo de texto.
--  IORef [Estudante] que armazena a lista de alunos que foi lido do arquivo de texto
-- Funcionalidade:
--  1. A função começa lendo a lista de estudantes armazenada em "estudantesRef".
--  2. Se a lista estiver vazia (nenhum estudante carregado), exibe a mensagem "Nenhuma turma carregada."
--  3. Caso contrário, exibe um menu com opções para buscar estudantes:
--      1. Busca por matrícula: Solicita o número de matrícula e chama a função "procurarEstudante" passando a matrícula.
--      2. Busca por nome: Solicita o nome do estudante e chama a função "procurarEstudante" passando o nome.
--      3. Retorna (opção 3): Encerra a função sem fazer mais nenhuma alteração.
--  4. Caso a opção fornecida não seja válida, exibe a mensagem "Opcao invalida."
editarEstudante :: IORef String -> IORef [Estudante] -> IO ()
editarEstudante path estudantesRef = do
  estudantes <- readIORef estudantesRef

  if null estudantes
    then do
      putStrLn "Nenhuma turma carregada."
      return ()
    else do
      opcoes

      opcao <- getLine
      case opcao of
        "1" -> do
          putStr "Digite o numero de matricula: "
          matricula <- getLine
          let procurarMatricula = 0
          procurarEstudante procurarMatricula matricula estudantesRef
        "2" -> do
          putStr "Digite o nome do estudante: "
          nome <- getLine
          let procurarNome = 1
          procurarEstudante procurarNome nome estudantesRef
        "3" -> return ()
        _ -> do
          putStrLn "Opcao invalida."
          return ()

-- Funcionalidade:
--  Exibe um menu de opções
opcoes :: IO ()
opcoes = do
  printf "%s\n" (replicate 100 '=')
  printf "%55s\n" "Editar informacoes do Estudante"
  printf "%s\n" (replicate 100 '=')

  putStrLn "Opcoes:"
  putStrLn "1 - Selecionar por numero de matricula"
  putStrLn "2 - Selecionar por nome"
  putStrLn "3 - Voltar ao menu principal"
  putStr "Digite uma opcao: "

-- Paramêtros:
--  1. Uma identificação para fazer a busca do estudante por nome ou por matrícula.
--  2. Um nome ou matrícula
--  3. Um IORef [Estudante] que armazena a lista de estudantes que foi lida do arquivo de texto.
--
-- Funcionalidade:
-- A função "procurarEstudante" realiza uma busca por estudante na lista de estudantes, com base no tipo de busca (por nome ou matrícula).
--  1. A função começa lendo a lista de estudantes armazenada em "estudantesRef".
--  2. Dependendo do valor de "id", a busca será realizada por nome ou matrícula:
--      - Se "id" for 1, a função "procurarPorNome" será chamada.
--      - Se "id" for 0, a função "procurarPorMatricula" será chamada.
--  3. Se não houver nenhum estudante encontrado (lista vazia), a função imprime "Aluno não existente" e retorna.
--  4. Se houver estudantes encontrados, a função verifica se a lista de resultados contém apenas um aluno:
--      - Se houver apenas um aluno ou se a busca tenha sido realizada com matrícula, o único aluno da lista é retornado.
--      - Se houver mais de um aluno e a busca tenha sido realizada por nome, a função "escolhaAluno" é chamada para permitir que o usuário selecione o estudante.
--  5. O estudante selecionado é então passado para a função "auxiliarProcurarEstudante", junto com a lista de estudantes e o "estudantesRef".
--
-- Funções auxiliares:
--  - "procurarPorNome": Função que busca estudantes pelo nome.
--  - "procurarPorMatricula": Função que busca estudantes pela matrícula.
--  - "escolhaAluno": Função que permite ao usuário escolher um aluno de uma lista de múltiplos resultados.
procurarEstudante :: Int -> String -> IORef [Estudante] -> IO ()
procurarEstudante id identificacao estudantesRef = do
  estudantes <- readIORef estudantesRef
  let listaDeAlunos =
        if id == 1
          then procurarPorNome identificacao estudantes
          else procurarPorMatricula identificacao estudantes

  if null listaDeAlunos
    then do
      putStrLn "Aluno nao existente"
      return ()
    else do
      estudante <-
        if id == 0 || length listaDeAlunos == 1
          then return (head listaDeAlunos)
          else escolhaAluno listaDeAlunos >>= \escolha -> return (listaDeAlunos !! escolha)

      auxiliarProcurarEstudante estudante estudantes estudantesRef

-- Paramêtros:
--  1. Um aluno do tipo Estudante.
--  2. Uma lista de estudante que tenha sido lida do arquivo de texto
--  3. Uma IORef para essa mesma lista de estudantes lida.
--
-- Funcionalidade:
--  1. Exibe um menu de opções para o usuário.
--  2. O usuário escolhe uma opção de edição.
--  3. Se a opção escolhida for "9", a função retorna e não faz mais nada (encerra a edição).
--  4. Caso contrário, a função chama a função "editar" passando a opção e o estudante encontrado, permitindo que o usuário edite os dados do estudante.
--  5. Após a edição, a função "criarNovaListaEstudantes" é chamada para criar uma nova lista de estudantes, substituindo o estudante editado pela versão atualizada.
--  6. A nova lista de estudantes é então armazenada na referência IORef de estudantes.
--  7. A função chama recursivamente "auxiliarProcurarEstudante" com o novo estudante editado, permitindo que o usuário continue editando.
--
-- Funções auxiliares:
-- - "opcoesEdicao": Função que exibe as opções de edição para o usuário.
-- - "editar": Função que aplica a edição ao estudante com base na opção fornecida.
-- - "criarNovaListaEstudantes": Função que cria uma nova lista de estudantes com o estudante editado.
auxiliarProcurarEstudante :: Estudante -> [Estudante] -> IORef [Estudante] -> IO ()
auxiliarProcurarEstudante aluno estudantes estudantesRef = do
  opcoesEdicao
  opcao <- getLine
  if opcao == "9"
    then return ()
    else do
      novoEstudante <- editar opcao aluno
      let novaListaEstudantes = criarNovaListaEstudantes (matricula aluno) novoEstudante estudantes
      writeIORef estudantesRef novaListaEstudantes

      auxiliarProcurarEstudante novoEstudante novaListaEstudantes estudantesRef

-- Parâmetros:
--  1. O nome do estudante para a busca.
--  2. A lista de estudante em que será feito a busca.
--
-- Funcionalidade:
--  A função "procurarPorNome" busca estudantes na lista "estudantes" cujos nomes contenham a string fornecida em "buscar".
--  Para realizar a busca, a função converte ambas as strings (a de busca e os nomes dos estudantes) para maiúsculas, a fim de evitar case sensitive
--     e usa a função "isInfixOf" para verificar se a string de busca é uma substring do nome do estudante.
--  Todos os termos da string "buscar" deve fazer parte do nome do estudante para que esse estudante seja incluído como resultado da busca.
--
-- Como funciona:
-- 1. A função usa "filter" para percorrer a lista de estudantes e aplicar a busca.
-- 2. A função "substring" verifica se todas as palavras da string de busca estão contidas no nome do estudante.
--
-- Exemplo:
-- Se "buscar" for "João" e a lista de estudantes contiver um estudante com o nome "João Silva", o estudante será incluído no resultado.
procurarPorNome :: String -> [Estudante] -> [Estudante]
procurarPorNome buscar estudantes = filter (\e -> substring buscar (nome e)) estudantes
  where
    substring :: String -> String -> Bool
    substring buscar_ nome_ = any (`isInfixOf` map toUpper nome_) (map (map toUpper) (words buscar_))

-- Parâmetros:
--  1. A matrícula do estudante.
--  2. A lista de busca, que é uma lista do tipo Estudante.
--
-- Funcionalidade:
-- A função "procurarPorMatricula" busca um estudante na lista "estudantes" cujos dados de matrícula correspondem à string fornecida em "buscar".
-- A busca é feita sem case sensitive.
--
-- Como funciona:
-- 1. A função percorre recursivamente a lista de estudantes.
-- 2. Para cada estudante, compara a matrícula convertida para maiúsculas com a string "buscar" também convertida para maiúsculas.
-- 3. Se a matrícula do estudante corresponde à matrícula buscada, o estudante é retornado na lista de resultados.
-- 4. Caso contrário, a busca continua no resto da lista.
--
-- Exemplo:
-- Se "buscar" for "123" e a lista de estudantes contiver um estudante com a matrícula "123", uma lista que contém apenas esse estudante.
--
-- Obs:
-- A escolha desse retorno foi feito para que pudesse generalizar as variáveis presentes na função procurarEstudante.
procurarPorMatricula :: String -> [Estudante] -> [Estudante]
procurarPorMatricula _ [] = []
procurarPorMatricula buscar (x : xs)
  | buscarUpper == matriculaUpper = [x]
  | otherwise = procurarPorMatricula buscar xs
  where
    buscarUpper = map toUpper buscar
    matriculaUpper = map toUpper (matricula x)

-- Paramêtros:
--  1. Uma lista do tipo estudante em que contém todos os estudantes atuais.
--
-- Funcionalidade:
-- A função "escolhaAluno" exibe uma lista numerada de estudantes e permite ao usuário escolher um aluno para editar.
-- O usuário insere um número que corresponde ao índice do estudante na lista. Se a escolha for válida, a função retorna o índice do estudante selecionado. Caso contrário, solicita uma nova entrada.
--
-- Como funciona:
-- 1. A função usa "zip" para associar um índice a cada estudante da lista.
-- 2. Em seguida, usa "mapM_" para aplicar a função "imprimirAlunos" a cada par índice-estudante, que imprime o número, o nome e a matrícula de cada aluno.
-- 3. Após exibir a lista, solicita ao usuário que escolha um aluno, inserindo o número correspondente.
-- 4. A entrada do usuário é lida e convertida para um valor inteiro. Se o número for válido (dentro do intervalo da lista de estudantes), o índice do estudante escolhido é retornado.
-- 5. Se o número não for válido (fora do intervalo ou não for um número inteiro), a função exibe uma mensagem de erro e solicita uma nova tentativa.
--
-- Exemplo:
-- Suponha que a lista de estudantes tenha 3 alunos: "Alice", "Bob" e "Carol".
-- A função imprime:
--   1 - Alice (123)
--   2 - Bob (456)
--   3 - Carol (789)
-- Dentro dos parênteses contém o número de matrícula do estudante.
-- O usuário escolhe "2", e a função retorna o índice 1 (correspondente ao aluno "Bob").
escolhaAluno :: [Estudante] -> IO Int
escolhaAluno estudantes = do
  let imprimirAlunos index estudante = printf "%d - %s (%s)\n" (index + 1) (nome estudante) (matricula estudante)
  let index_estudante = zip [0 ..] estudantes :: [(Int, Estudante)]

  mapM_ (uncurry imprimirAlunos) index_estudante

  putStr "Escolha o aluno para editar: "
  escolha <- getLine
  case readMaybe escolha :: Maybe Int of
    Just numero ->
      if numero >= 0 && numero <= length estudantes
        then return (numero - 1)
        else do
          putStrLn "Aluno nao existe. Tente novamente."
          escolhaAluno estudantes
    Nothing -> do
      putStrLn "Erro na digitacao. Tente novamente."
      escolhaAluno estudantes

-- Funcionalidade:
--  Exibe um menu do que o usuário pode alterar do estudante selecionado.
opcoesEdicao :: IO ()
opcoesEdicao = do
  printf "%s\n" (replicate 100 '=')
  printf "%55s\n" "Editar informacoes do Estudante"
  printf "%s\n" (replicate 100 '=')
  putStrLn "Opcoes: "
  putStrLn "1 - Alterar nome"
  putStrLn "2 - Alterar matrícula"
  putStrLn "3 - Alterar nota da prova 1"
  putStrLn "4 - Alterar nota da prova 2"
  putStrLn "5 - Alterar nota da prova 3"
  putStrLn "6 - Alterar nota do trabalho 1"
  putStrLn "7 - Alterar nota do trabalho 2"
  putStrLn "8 - Alterar número de faltas"
  putStrLn "9 - Voltar ao menu anterior"
  putStr "Digite uma opcao: "

-- Paramêtros:
--  1. Uma string que é a matrícula do estudante que deve ser atualizado.
--  2. O estudante que deve ser atualizado na lista de estudantes.
--  3. Uma lista que contém todos os alunos.
--
-- Funcionalidade:
-- A função "criarNovaListaEstudantes" cria uma nova lista de estudantes substituindo o aluno com a matrícula
--  correspondente à matrícula fornecida ("buscar") pelo estudante com as informações atualizadas ("estudanteAtualizado").
--
-- A busca e substituição são feitas sem case sensitive.
-- Quando o estudante com a matrícula buscada é encontrado, ele é substituído pelo "estudanteAtualizado". Caso contrário, o estudante
--   atual é mantido na lista.
--
-- Como funciona:
-- 1. A função recebe a matrícula ("buscar"), o estudante atualizado ("estudanteAtualizado"), e a lista de estudantes.
-- 2. A comparação entre a matrícula buscada ("buscar") e a matrícula de cada estudante da lista é feita em caixa alta (para garantir
--      que a comparação seja insensível a maiúsculas e minúsculas).
-- 3. Se a matrícula do estudante na lista for igual à matrícula buscada, o estudante é substituído pelo "estudanteAtualizado" e a função
--      retorna a nova lista com a substituição.
-- 4. Se a matrícula não for encontrada, a função continua percorrendo a lista até encontrar a correspondência ou até o final da lista.
--
-- Exemplo:
-- Lista de estudantes:
-- [ Estudante { matricula = "123", nome = "Alice" }, Estudante { matricula = "456", nome = "Bob" } ]
-- E queremos atualizar o estudante com a matrícula "123" para um novo estudante:
-- Estudante { matricula = "123", nome = "Alice Atualizada" }
--
-- A função substituirá o primeiro estudante da lista, resultando na nova lista:
-- [ Estudante { matricula = "123", nome = "Alice Atualizada" }, Estudante { matricula = "456", nome = "Bob" } ]
--
-- Obs:
--  A matrícula do estudante é passada como argumento para evitar erros na busca, caso o usuário tenha alterado o número de matrícula desse estudante.
--  Se isso acontecer, a matrícula atualizada não será compatível com nenhuma matrícula na lista dos estudantes atuais.
criarNovaListaEstudantes :: String -> Estudante -> [Estudante] -> [Estudante]
criarNovaListaEstudantes _ _ [] = []
criarNovaListaEstudantes buscar estudanteAtualizado (x : xs)
  | buscarUpper == matriculaUpper = estudanteAtualizado : xs
  | otherwise = x : criarNovaListaEstudantes buscar estudanteAtualizado xs
  where
    buscarUpper = map toUpper buscar
    matriculaUpper = map toUpper (matricula x)

-- Parâmetros:
--  1. Um valor de opção escolhido pelo usuário.
--  2. O estudante que terá uma informação atualizada.
--
-- Funcionalidade:
--  A função atualiza um campo específico do estudante com base na opção escolhida.
--  Para fazer a atualização, são utilizadas funções auxiliares.
--  Caso a opção fornecida seja inválida, uma mensagem de erro é exibida e o estudante é retornado sem alterações.
editar :: String -> Estudante -> IO Estudante
editar opcao estudante = do
  case opcao of
    "1" -> do
      nomeAtualizado <- pegarEntrada "Digite o nome: "
      return estudante {nome = nomeAtualizado}
    "2" -> do
      matriculaAtualizado <- pegarEntrada "Digite a matrícula: "
      return estudante {matricula = matriculaAtualizado}
    "3" -> atualizarNota 0 "Digite a nota da prova 1: " estudante
    "4" -> atualizarNota 1 "Digite a nota da prova 2: " estudante
    "5" -> atualizarNota 2 "Digite a nota da prova 3: " estudante
    "6" -> atualizarTrabalho 0 "Digite a nota do trabalho 1: " estudante
    "7" -> atualizarTrabalho 1 "Digite a nota do trabalho 2: " estudante
    "8" -> do
      faltasAtualizado <- lerNumero "Digite o número de faltas: "
      case faltasAtualizado of
        Just faltas -> do
          let (nota_final, situac) = informacoesFinais (head (notas estudante)) (notas estudante !! 1) (notas estudante !! 2) (head (ts estudante)) (ts estudante !! 1) faltas
          return estudante {faltas = faltas, final = nota_final, situacao = snd situac}
        Nothing -> putStrLn "Número inválido." >> return estudante
    _ -> do
      putStrLn "Opção inválida!"
      return estudante

------Funções auxiliares---------

-- Parâmetros:
--  1. Um texto que será exibido para o usuário, solicitando a entrada.
--
-- Funcionalidade:
--  Captura a entrada do usuário como uma string, tenta convertê-la para o tipo especificado (como "Float", "Int", etc.) e retorna um valor "Maybe" do tipo "a".
--  Se a conversão for bem-sucedida, retorna "Just valor"; caso contrário, retorna "Nothing".
--
-- Obs:
--  O tipo para o qual a entrada será convertida depende do tipo especificado ao chamar a função.
--  O tipo de conversão é inferido a partir do contexto ou pode ser explicitamente declarado.
lerNumero :: (Read a) => String -> IO (Maybe a)
lerNumero prompt = do
  putStr prompt
  inputStr <- getLine
  return (readMaybe inputStr)

-- Parâmetros:
--  1. Recebe um inteiro que marca o indice na lista.
--  2. Recebe um valor genérico do tipo a.
--  3. Recebe uma lista genérica de elementos do tipo a.
--
-- Funcionalidade:
--  A função substitui o valor presente no índice especificado pelo o novo valor.
--  A lista resultante contém todos os elementos originais da lista, exceto o valor no índice "indc", que é substituído por "novoValor".
--
-- Exemplo de uso:
--  atualizarLista 2 5 [1, 2, 3, 4, 5]  -- Resultado: [1, 2, 5, 4, 5]
--  O valor que estava no índice 2 (O valor 3) foi substituido pelo valor 5.
atualizarLista :: Int -> a -> [a] -> [a]
atualizarLista indc novoValor lista = take indc lista ++ [novoValor] ++ drop (indc + 1) lista

-- Parâmetros:
--  1. Um inteiro "indc" que especifica o índice da lista de notas do estudante, indicando qual nota de prova deve ser atualizada.
--  2. Um texto "prompt" que será exibido ao usuário para solicitar a entrada da nova nota.
--  3. Um estudante "estudante" do tipo "Estudante", que contém as informações do aluno, incluindo suas notas.
--
-- Funcionalidade:
--  A função solicita ao usuário que insira uma nova nota (com a ajuda da função "lerNumero"), e se a entrada for válida,
--  atualiza a nota do estudante no índice especificado da lista de notas.
--  Após a atualização, a função também recalcula a nota final e a situação do estudante, utilizando as notas atualizadas,
--  as notas de trabalhos e faltas.
--  Se a conversão da entrada não for bem-sucedida, a função exibe uma mensagem de erro e retorna o estudante sem alterações.
atualizarNota :: Int -> String -> Estudante -> IO Estudante
atualizarNota indc prompt estudante = do
  notaN <- lerNumero prompt
  case notaN of
    Just nota -> do
      let notas_ = atualizarLista indc nota (notas estudante)
      let (nota_final, situac) = informacoesFinais (head notas_) (notas_ !! 1) (notas_ !! 2) (head (ts estudante)) (ts estudante !! 1) (faltas estudante)
      return estudante {notas = notas_, final = nota_final, situacao = snd situac}
    Nothing -> putStrLn "Número inválido." >> return estudante

-- Parâmetros:
--  1. Um inteiro "indc" que especifica o índice da lista de notas do estudante, indicando qual nota de trabalho deve ser atualizada.
--  2. Um texto "prompt" que será exibido ao usuário para solicitar a entrada da nova nota.
--  3. Um estudante "estudante" do tipo "Estudante", que contém as informações do aluno, incluindo suas notas.
--
-- Funcionalidade:
--  A função solicita ao usuário que insira uma nova nota de trabalho (com a ajuda da função "lerNumero"). Se a entrada for válida,
--  ela atualiza a nota de trabalho do estudante no índice especificado.
--  Após a atualização, a função também recalcula a nota final e a situação do estudante, utilizando as notas de provas, as notas de trabalhos
--  e o número de faltas.
--  Se a conversão da entrada não for bem-sucedida, a função exibe uma mensagem de erro e retorna o estudante sem alterações.
atualizarTrabalho :: Int -> String -> Estudante -> IO Estudante
atualizarTrabalho indc prompt estudante = do
  trabalhoN <- lerNumero prompt
  case trabalhoN of
    Just t -> do
      let notasTrabalho = atualizarLista indc t (ts estudante)
      let (nota_final, situac) = informacoesFinais (head (notas estudante)) (notas estudante !! 1) (notas estudante !! 2) (head notasTrabalho) (notasTrabalho !! 1) (faltas estudante)
      return estudante {ts = notasTrabalho, final = nota_final, situacao = snd situac}
    Nothing -> putStrLn "Número inválido." >> return estudante

-- Parâmetros:
--  1. Um texto que é exibido ao usuário.
--
-- Funcionalidade:
--  Capturar uma entrada do usuário exibindo um texto.
pegarEntrada :: String -> IO String
pegarEntrada prompt = do
  putStr prompt
  getLine

--------------------------------------------
