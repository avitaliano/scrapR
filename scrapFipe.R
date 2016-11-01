# scrap direto na pagina da fipe

library(httr)
library(dplyr)
library(lubridate)

# APIs
api_baseurl             <- 'http://veiculos.fipe.org.br/api/veiculos'
api_tabelaReferencia    <- paste0(api_baseurl, "/ConsultarTabelaDeReferencia")
api_Marcas              <- paste0(api_baseurl, '/ConsultarMarcas')
api_Modelos             <- paste0(api_baseurl, '/ConsultarModelos')
api_Ano                 <- paste0(api_baseurl, '/ConsultarAnoModelo')
api_Valor               <- paste0(api_baseurl, '/ConsultarValorComTodosParametros')

# Constantes de tipo de veículo
tipo = c(carro ="1", moto = "2", caminhao = "3")

# Função que chama a API da FIPE
api_fipe <- function(url, data, verbose = FALSE, encode = "form"){
        # Crédito destes header options 
        # Rafael Piza
        # http://phpbrasil.com/script/QqiPgvQWXrX3/importando-indices-da-fipe-preco-automoveis-para-seu-banco
        HEADER_OPTS <-  c('Accept' = 'application/json, text/javascript, */*; q=0.01',
                          'Accept-Encoding' = 'gzip, deflate',
                          'Accept-Language' = 'pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4',
                          'Connection' = 'keep-alive',
                          'Content-Type' = 'application/x-www-form-urlencoded; charset=UTF-8',
                          'Cookie' = '_ga=GA1.3.472052299.1466616166; _gat=1',
                          'Host' = 'veiculos.fipe.org.br',
                          'Origin' = 'http://veiculos.fipe.org.br',
                          'Referer' = 'http://veiculos.fipe.org.br/',
                          'User-Agent' = 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                          'X-Requested-With' = 'XMLHttpRequest') 
        if(verbose){
                post <- POST(url = url, config = add_headers(.headers = HEADER_OPTS), 
                             body = data, encode = encode, verbose())        
        } else {
                post <- POST(url = url, config = add_headers(.headers = HEADER_OPTS), 
                             body = data, encode = encode)
        }
        return(content(post))
}

# Chama API para buscar meses de referência.
busca_mesReferencia <- function(){
        tabRef_result <- api_fipe(api_tabelaReferencia, data = "")
        tabRef <- data.frame(matrix(unlist(tabRef_result), ncol = 2, byrow = TRUE),
                             stringsAsFactors = FALSE)

        names(tabRef) <- c("mes.id", "mes.nome")
        databases <- paste0("01/",tabRef$mes.nome)
        tabRef$database <- as.Date(databases, "%d/%B/%Y")
        return(tabRef)
}

# Busca as marcas
busca_marcas <- function(tipo, mesRef, somente_principais = FALSE){
        
        # parametros para chamar api
        marcas_data <- list(codigoTabelaReferencia = mesRef$mes.id, 
                            codigoTipoVeiculo = tipo)
        # retorno
        marcas_return <- api_fipe(url = api_Marcas, marcas_data)
        
        # joga retorno em um data.frame
        marcas <- data.frame(matrix(unlist(marcas_return), ncol = 2, byrow = TRUE), 
                             stringsAsFactors = FALSE)
        
        # concatena dados do mes de referencia
        marcas <- cbind(mesRef$mes.id,
                        mesRef$mes.nome,
                        mesRef$database,
                        marcas)
        names(marcas) <- c(names(mesRef), "marca.nome", "marca.id")
        
        if(somente_principais){
                # seleciona somente as principais marcas no Brasil
                marcas_principais <- c("Fiat", "Ford", "GM - Chevrolet",
                                       "Honda", "Hyundai", "JAC",
                                       "Jeep", "Mitsubishi", "Nissan",
                                       "Peugeot", "Renault", "Suzuki",
                                       "Toyota", "VW - VolksWagen")
                marcas <- marcas %>% filter(marca.nome %in% marcas_principais)        
        }
        
        return(marcas)
}

# Busca os modelos de uma marca
busca_modelos <- function(tipo, marca){
        
        # parametros para chamar api
        api_data <- list( codigoTipoVeiculo = tipo,
                              codigoTabelaReferencia = marca$mes.id,
                              codigoMarca = marca$marca.id)
        # codigoModelo = '',
        # ano = '',
        # codigoTipoCombustivel = '',
        # anoModelo = '',
        # modeloCodigoExterno = '')
        modelos_return <- api_fipe(url = api_Modelos, data = api_data)
        modelos <- modelos_return[['Modelos']]
        modelos <- data.frame(matrix(unlist(modelos), ncol = 2, byrow = TRUE), 
                              stringsAsFactors = FALSE)
        
        # concatena informacoes da marca ao data frame
        modelos <- cbind( marca, modelos)
        
        names(modelos) <- c(names(marca), "modelo.nome", "modelo.id")
        return(modelos)
}

# Busca os anos de um modelo
busca_anos <- function(tipo, modelo){
        
        # parametros para chamar api
        api_data <- list( codigoTipoVeiculo = tipo,
                              codigoTabelaReferencia = modelo$mes.id,
                              codigoMarca = modelo$marca.id,
                              codigoModelo = modelo$modelo.id)
        
        anos_return <- api_fipe(url = api_Ano, data = api_data)
        anos <- data.frame(matrix(unlist(anos_return), ncol = 2, byrow = TRUE), 
                              stringsAsFactors = FALSE)
        
        # concatena informacoes da marca ao data frame
        anos <- cbind( modelo, anos)
        
        names(anos) <- c(names(modelo), "ano.nome", "ano.id")
        return(anos)
}

# Busca as informações de um modelo/ano específico
busca_valor <- function(tipo, ano){
        
        anoModelo<- strsplit(ano$ano.id, split = "-")[[1]][1]
        tipoCombustivel <- strsplit(ano$ano.id, split = "-")[[1]][2]
        
        # parametros para chamar api
        api_data <- list( codigoTipoVeiculo = tipo,
                          codigoTabelaReferencia = ano$mes.id,
                          codigoMarca = ano$marca.id,
                          codigoModelo = ano$modelo.id,
                          ano = ano$ano.id,
                          codigoTipoCombustivel = tipoCombustivel,
                          anoModelo = anoModelo,
                          tipoConsulta = 'Tradicional')
        
        valor <- api_fipe(url = api_Valor, data = api_data)
        valor <- data.frame(t(unlist(valor)))
        return(valor)
}

downloadFipe <- function(mesRef, reset = FALSE, download_dir = "fipe_data"){

        # cria diretorio se ele nao existe
        if(!file.exists(download_dir)){
                dir.create(download_dir)
        }
        
        # se nao informar a mes de referencia, buscar database mais recente
        if(missing(mesRef)){
                # seleciona meses disponíveis
                tabRef <- busca_mesReferencia()
                # Seleciona o mes mais recente
                mesRef <- tabRef %>% arrange(desc(database)) %>% slice(1)
        }else{
                # cast para data.frame, para chamadas da funcao por 'apply'
                # nao consegui pensar em nada mais elegante
                if(class(mesRef) != "data.frame"){
                        df <- data.frame( mes.id = mesRef['mes.id'],
                                          mes.nome = mesRef['mes.nome'],
                                          database = as.Date(mesRef['database']))
                        mesRef = df
                }
        }
        
        controle_arquivo <- paste0(download_dir, "/", 
                                   format(mesRef$database, "%Y%m"), 
                                   "-controleFipeDownload.RDS")

        # cria arquivo de controle se é primeira execução ou reset == TRUE
        if( file.exists(controle_arquivo) && !reset){
                print(paste0("Retomando download FIPE. Database: ", mesRef$mes.nome))
                controle <- readRDS(controle_arquivo)
        }else{
                # se nao, retoma download da marca que ainda não foi baixada.
                print(paste0("Iniciando download FIPE. Database: ", mesRef$mes.nome))
                
                # seleciona marcas
                marcas <- busca_marcas(tipo = tipo['carro'], mesRef = mesRef, 
                                       somente_principais = TRUE)
                
                # constroi o data frame de controle dos downloads
                controle <- marcas
                controle$downloaded <- FALSE
        }
        
        for(i in 1:nrow(controle)){
                
                marca_valores <- data.frame()
                marca <- controle[i,]
                
                if(!marca$downloaded){
                        print(paste("Baixando marca", marca$marca.nome))
                        
                        modelos <- busca_modelos(tipo = tipo['carro'], marca = marca )
                        for(j in 1:nrow(modelos)){
                                modelo <- modelos[j, ]
                                #print(paste("   Modelo",modelo$modelo.nome))
                                
                                anos <- busca_anos(tipo = tipo['carro'], modelo = modelo)
                                for(k in 1:nrow(anos)){
                                        ano <- anos[k,]
                                        #print (paste("      Ano", ano$ano.nome))
                                        
                                        valor <- busca_valor(tipo = tipo['carro'], ano = ano)
                                        marca_valores <- rbind(marca_valores, valor)
                                }
                        }
                        marca_arquivo <- paste0(download_dir, "/fipe-", 
                                                format(marca$database, "%Y%m"),
                                                "-",
                                                marca$marca.nome,".csv")
                        write.csv(marca_valores, file = marca_arquivo)
                        
                        # atualiza o arquivo de controle
                        controle[i,]$downloaded <- TRUE
                        saveRDS(controle, file = controle_arquivo)
                }
        }
        # grava arquivo de controle
        print("Download FIPE finalizado.")
}

load_fipe_DF <- function(download_dir){
        fipe_files <- list.files(path = download_dir, pattern = "fipe*")
        fipe_files <- paste0(download_dir, "/", fipe_files)
        fipe.df <- do.call(rbind, lapply(fipe_files, 
                                         function(x) read.csv(x, stringsAsFactors = FALSE)))
        return(fipe.df)
}

# init 
# baixa o mes de referencia mais recente
# download_fipe(download_dir = "fipe_data")

# baixa varios meses de referencia de uma vez
downloadFipe_PorAno <- function(ano, ...){
        
        if(missing(ano)) stop("Informar o ano")
        
        mesesRef = busca_mesReferencia()
        mesesAno = mesesRef %>% filter(year(database) == ano)
        apply(mesesAno, 1, downloadFipe, ...)
}

downloadFipe_VariosAnos <- function(ano_inicial, ano_final, ...){
        
        # verifica os parametros
        if(is.numeric(ano_inicial) && is.numeric(ano_final) && ano_inicial <= ano_final){
                sapply(ano_final:ano_inicial, downloadFipe_PorAno, ...)
        }else{
                stop("parametros invalidos")
        }
}

downloadFipe_MesAnual <- function(mes = 1, ...){
        
        # verifica os parametros
        if(is.numeric(mes) && mes >= 1 && mes <= 12){
                print(paste0("Iniciando downloadFipe para o mês ", mes, " de cada ano"))
        }else{
                stop("parametros invalidos")
        }
        
        tabRef = busca_mesReferencia()
        mesesRef <- tabRef %>% filter(month(database) == mes)
        apply(mesesRef, 1, downloadFipe, ...)
}
