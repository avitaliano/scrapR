# scrap direto na pagina da fipe

library(httr)
library(rvest)
library(XML)
library(jsonlite)

api_baseurl <- 'http://veiculos.fipe.org.br/api/veiculos'
api_tabelaReferencia <- paste0(api_baseurl, "/ConsultarTabelaDeReferencia")        

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

tabRef_result <- content(POST(url = api_tabelaReferencia, config = add_headers(.headers = HEADER_OPTS), body = ""))
tabRef <- data.frame(matrix(unlist(tabRef_result), ncol = 2, byrow = TRUE))
names(tabRef) <- c("codigo", "mes")
