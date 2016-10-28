# scrap direto na pagina da fipe

library(httr)
library(rvest)
library(XML)
library(jsonlite)
library(dplyr)

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
tabRef_result <- api_fipe(api_tabelaReferencia, data = "")
tabRef <- data.frame(matrix(unlist(tabRef_result), ncol = 2, byrow = TRUE),
                     stringsAsFactors = FALSE)
names(tabRef) <- c("mes.id", "mes.nome")
tabRef

# Seleciona o mes atual, como o primeiro elemento da lista.
mes_atual <- tabRef[1,]
mes_atual

# Busca as marcas
marcas_data <- list(codigoTabelaReferencia = mes_atual$mes.id, 
                    codigoTipoVeiculo = tipo['carro'])
marcas_data
marcas_return <- api_fipe(url = api_Marcas, marcas_data)
marcas <- data.frame(matrix(unlist(marcas_return), ncol = 2, byrow = TRUE), 
                     stringsAsFactors = FALSE)
names(marcas) <- c("marca.nome", "marca.id")

# seleciona somente as principais marcas no Brasil
marcas_principais <- c("Fiat", "Ford", "GM - Chevrolet",
                    "Honda", "Hyundai", "JAC",
                    "Jeep", "Mitsubishi", "Nissan",
                    "Peugeot", "Renault", "Suzuki",
                    "Toyota", "VW - VolksWagen")
marcas <- marcas %>% filter(marca.nome %in% marcas_principais)
marcas
