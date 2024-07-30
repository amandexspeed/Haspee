module Main where
import Data.IORef

type Nome = String
type Preco = Float
type Quant = Float
type Produto = (Nome,Preco)
type Item = (Produto,Quant)
type Carrinho = [Item]

getPreco :: Produto -> Preco
getPreco (_,preco) = preco

getNome :: Produto -> Nome
getNome (nome, _ ) = nome

getProduto :: Item -> Produto
getProduto (prod,_) = prod

getQuantidade :: Item -> Quant
getQuantidade (_,quant) = quant

criarProduto :: Nome -> Preco -> Produto
criarProduto nome preco = (nome, preco)

criarItem :: Produto -> Quant -> Item
criarItem produto quant = (produto, quant)

adicionarCarrinho :: Item -> IORef Carrinho -> IO()
adicionarCarrinho  item car = modifyIORef car (\carrinho -> item : carrinho)

adicionarProduto :: Produto -> IORef [Produto] -> IO()
adicionarProduto prod list = modifyIORef list (\produto -> prod : produto)

-- Função para remover um item do carrinho pelo nome do produto
filtrarItensPorNome :: Nome -> Carrinho -> Carrinho
filtrarItensPorNome nome = filter (\((n, _), _) -> n /= nome)

filtrarProdutosPorNome :: Nome -> [Produto] -> [Produto]
filtrarProdutosPorNome nome = filter (\(n, _) -> n /= nome)

buscarProdutosPorNome :: Nome -> [Produto] -> [Produto]
buscarProdutosPorNome nome = filter (\(n, _) -> n == nome)

removerItem :: Nome -> IORef Carrinho -> IO ()
removerItem nome car = modifyIORef car (filtrarItensPorNome nome)

removerProduto :: Nome -> IORef [Produto] -> IO ()
removerProduto nome list = modifyIORef list (filtrarProdutosPorNome nome)

imprimirProdutos :: [Produto] -> IO()
imprimirProdutos [] = putStrLn ""
imprimirProdutos (x:xs) = do
    putStrLn ("Nome: " ++ getNome x ++ "| Preco: " ++ show (getPreco x)) 
    imprimirProdutos xs

imprimirCarrinho :: Carrinho -> IO()
imprimirCarrinho [] = putStrLn ""
imprimirCarrinho (x:xs) = do
    let produto = getProduto x
    putStrLn ("Nome: " ++ getNome produto ++ "| Preco: " ++ show (getPreco produto) ++ "| Quantidade: " ++ show (getQuantidade x)) 
    imprimirCarrinho xs

contTotal :: Carrinho -> IO Float
contTotal [] = return 0
contTotal (x:xs) = do
    parcial <- contParcial x
    soma <- contTotal xs
    return (parcial + soma)

contParcial :: Item -> IO Float
contParcial x = do
    let parcial = getPreco (getProduto x) * getQuantidade x
    putStrLn ("Nome: " ++ getNome(getProduto x) ++ " | "++ "Quantidade:  " ++ show (getQuantidade x) ++ " | " ++ "Preco:  " ++ show (getPreco (getProduto x)) ++ " | " ++ "Total parcial:  " ++ show parcial )
    return parcial

menuIO :: IORef Carrinho -> IORef [Produto] -> IO()
menuIO carrinho produtos = do
    putStrLn "----------Haspee(Haskell + Shopee)------------"
    putStrLn "Escolha uma opção:"
    putStrLn "1 - Cadastrar produto"
    putStrLn "2 - Remover produto"
    putStrLn "3 - Ver produtos cadastrados"
    putStrLn "4 - Adicionar item no carrinho"
    putStrLn "5 - Remover item no carrinho"
    putStrLn "6 - Ver itens no carrinho"
    putStrLn "7 - Calcular Total"
    op <- readLn

    case op of
        1 -> do
            putStrLn "Digite o nome do produto"
            nome <- getLine

            putStrLn "Digite o preço"
            preco <- readLn

            adicionarProduto (criarProduto nome preco) produtos

        2 -> do
            listaProds <- readIORef produtos
            imprimirProdutos listaProds
            
            putStrLn "Digite o nome do produto"
            nome <- getLine

            removerProduto nome produtos

        3 -> do

            listaProds <- readIORef produtos
            imprimirProdutos listaProds 

        4 -> do
            listaProds <- readIORef produtos
            imprimirProdutos listaProds

            putStrLn "Digite o nome do produto desejado:"
            nome <- getLine
            let prodEsc = buscarProdutosPorNome nome listaProds

            if prodEsc /= [] then do
                let prod = head prodEsc
                putStrLn "Digite a quantidade desejada:"
                quant <- readLn
                adicionarCarrinho (criarItem prod quant) carrinho 

            else putStrLn "Não achamos o produto"

        5 -> do
            itensCarrinho <- readIORef carrinho
            imprimirCarrinho itensCarrinho

            putStrLn "Digite o nome do produto"
            nome <- getLine

            removerItem nome carrinho

        6 -> do
            itensCarrinho <- readIORef carrinho
            imprimirCarrinho itensCarrinho
            
        7 -> do
            listaProds <- readIORef carrinho
            total <- contTotal listaProds
            putStrLn ("Total: " ++ show total)
        
        _ -> do
            putStrLn "Opção inválida"

loopingMenu :: IORef Carrinho -> IORef [Produto] -> IO()
loopingMenu carrinho itens = do
    menuIO carrinho itens
    putStrLn "Deseja fazer mais alguma operação? S para sim, N para não"
    opcao <- getLine

    if opcao == "S" || opcao == "s"
        then loopingMenu carrinho itens
        else putStrLn "Obrigada por usar nosso sistema!"



main :: IO()
main = do

    carrinho <- newIORef []
    listaProd <- newIORef []
    loopingMenu carrinho listaProd