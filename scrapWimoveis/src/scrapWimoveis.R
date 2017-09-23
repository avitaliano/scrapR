
# scrap dos imoveis da página wimoveis

library(httr)
library(rvest)
library(stringr)
library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)

################
# constantes
download_dir <- "scrapWimoveis/data"
url_base <- "http://www.wimoveis.com.br"
url_anuncios <- c( "http://www.wimoveis.com.br/apartamentos-venda-asa-norte-brasilia.html")
# url_anuncios <- c( "http://www.wimoveis.com.br/apartamentos-venda-asa-sul-brasilia.html",
#                  "http://www.wimoveis.com.br/apartamentos-venda-asa-norte-brasilia.html",
#                  "http://www.wimoveis.com.br/apartamentos-venda-sudoeste-brasilia.html",
#                  "http://www.wimoveis.com.br/apartamentos-venda-noroeste-brasilia.html")

# url_anuncios <- c("http://www.wimoveis.com.br/apartamentos-aluguel-asa-norte-brasilia.html",
#                   "http://www.wimoveis.com.br/apartamentos-aluguel-asa-sul-brasilia.html",
#                   "http://www.wimoveis.com.br/apartamentos-aluguel-sudoeste-brasilia.html",
#                   "http://www.wimoveis.com.br/apartamentos-aluguel-noroeste-brasilia.html")

set_download_dir <- function(dir){
        download_dia <<- dir
}

set_url_base <- function(url){
        url_base <<- url
}

set_url_anuncios <- function(url){
        url_anuncios <<- url
}


################
# funcoes auxiliares para HTML

# remove caracteres invalidos do texto
clean_html <- function(x) {
        return(str_trim(str_replace_all(str_replace_all(x, "\\t", ''), "\\n", "")))
}

# extrai campos de valor, limpando o texto
parse_valor <- function(x){
        ini <- str_locate(x, "R\\$")[2] + 1
        
        if (is.na(ini)) return(NA)
        
        valor <- str_sub(x, start = ini)
        valor <- str_replace(valor, "[.]", "" )
        
        if (is.numeric(valor)){
                return(as.numeric(valor))      
        } else {
                return(valor)
        }
}

# realiza o parse de cada card item <li> do HTML
parse_card <- function(li){
        atributo <- clean_html(li %>% html_nodes(".nombre") %>% html_text())
        valor <- clean_html(li %>% html_nodes(".valor") %>% html_text())
        return(c(atributo, valor))
}


################
# funcao principal, que baixa as listas de anuncios, e depois baixa os anuncios jogando para o arquivo csv.
scrap_wimoveis <- function(file_prefix){
        
        set_download_dir(download_dir)
        download_lista_anuncios(file_prefix = file_prefix)
        download_anuncios(file_prefix = file_prefix)
}

# funcao que baixa as paginas que contem os anuncios a partir da pagina inicial dos anuncios
download_lista_anuncios <- function(file_prefix){
        
        # cria os diretorios 
        if(!file.exists(download_dir)){
                dir.create(download_dir)
        }
        
        links_dir <- file.path(download_dir, "links")
        if(!file.exists(links_dir)){
                dir.create(links_dir)
        }
        
        i <- 1L
        repeat{
                print (paste("Iniciando donwload do link ", url_anuncios))
                pag_anuncios <- list_anuncios(url_anuncios)     
                
                # nome do arquivo da lista
                filename <- file.path(links_dir, paste0("wimoveis-links-", file_prefix, as.character(i),".csv"))
                i <- i + 1 
                
                # salva em arquivo
                write.csv2(pag_anuncios$urls, file = filename, row.names = FALSE)
                print(paste("Salvo arquivo", filename))
                
                if (length(pag_anuncios$next_url) == 0){
                        break
                } else{
                        url_anuncios <- pag_anuncios$next_url
                }
        }
}

# constroi uma lista com a url de todos os anuncios de uma página de lista de anuncios do wimoveis
list_anuncios <- function(url){
        
        url <- url %>% read_html()
        
        urls <- url %>% 
                html_nodes( xpath = "//ul[@class='list-posts']/li") %>% 
                html_attr("data-href")
        
        urls <- paste0(url_base, urls)
        
        next_url <- url %>% 
                html_nodes( xpath = "//li[@class='pagination-action-next ']/a") %>% 
                html_attr("href")
        return(list(urls = urls, next_url = next_url))
}

# extrai os campos principais do anuncio a partir de uma lista e retorna um data.frame
extract_dados_principais <- function(l){
        
        tipo_imovel = NA
        valor.aluguel = NA
        valor.venda = NA
        valor.temporada = NA
        valor.condominio = NA
        valor.iptu = NA
        area_total = NA
        area_util = NA
        quartos = NA
        banheiros = NA
        suites = NA
        vagas = NA
        idade.imovel = NA

        decode_item <- function(x){

                print(x)
                item <- str_trim(paste(x, collapse = " "))
                
                if ( str_detect(item, "Área total") ) {
                        area_total <<- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Área útil") ) {
                        area_util <<- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Quarto") ) {
                        quartos <<- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Suite") ) {
                        suites <<- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Banheiro") ) {
                        banheiros <<- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Vaga") ) {
                        vagas <<- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Idade do imóvel") ) {
                        idade.imovel <<- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Valor Aluguel") ) {
                        valor.aluguel <<- parse_valor(item)
                        
                } else if ( str_detect(item, "Valor Venda") ) {
                        valor.venda <<- parse_valor(item)
                        
                } else if ( str_detect(item, "Valor Temporada") ) {
                        valor.temporada <<- parse_valor(item)
                        
                } else if ( str_detect(item, "Valor Condominio") ) {
                        valor.condominio <<- parse_valor(item)
                        
                } else if ( str_detect(item, "IPTU") ) {
                        valor.iptu <<- parse_valor(item)
                        
                } else {
                        tipo_imovel <<- item
                }
        }
        
        lapply(l, decode_item)
        dados_principais <- data.frame (tipo_imovel = tipo_imovel,
                                        valor.aluguel = valor.aluguel,
                                        valor.venda = valor.venda,
                                        valor.temporada = valor.temporada,
                                        valor.condominio = valor.condominio,
                                        valor.iptu = valor.iptu,
                                        area_total = area_total,
                                        area_util = area_util,
                                        quartos = quartos,
                                        banheiros = banheiros,
                                        suites = suites,
                                        vagas = vagas,
                                        idade.imovel = idade.imovel)        
        print(dados_principais)
        return(dados_principais)
}

# extrai os campos de publicacao do anuncio a partir de uma lista e retorna um data.frame
extract_dados_publicacao <- function(l){
        
        cod.anuncio = NA
        cod.wimoveis = NA
        data.anuncio = NA
        
        decode_publicacao <- function(x){
                item <- str_trim(paste(x, collapse = " "))
                
                if ( str_detect(item, "Código do anunciante") ) {
                        cod.anuncio <<- str_split(item, ":")[[1]][2]
                } else if (str_detect(item, "Código Wimoveis") ) {
                        cod.wimoveis <<- str_split(item, ":")[[1]][2]
                        
                } else if ( str_detect(item, "Publicado faz")) {
                        dias.publicacao <<- str_extract(item, '[0-9]+')
                        data.anuncio <- Sys.Date() - days(dias.publicacao)
                } else if ( str_detect(item, "Publicado hoje")) {
                        data.anuncio <<- Sys.Date()
                }                
        }

        lapply(l, decode_publicacao)
        return(data.frame(cod.anuncio = cod.anuncio, 
                          cod.wimoveis = cod.wimoveis,
                          data.anuncio = data.anuncio))
}

# extrai o anuncio a partir da sua url
extract_anuncio <- function(url_anuncio){
        
        print(url_anuncio)
        
        anuncio_html <- url_anuncio %>% read_html()        
        
        # titulo do anuncio
        titulo <- clean_html(anuncio_html %>% 
                                     html_nodes(".card-title") %>% 
                                     html_text(trim = TRUE))
        
        # local da propriedade
        local <- anuncio_html %>% 
                html_nodes(xpath = "//div[@class='list list-directions']/ul/li") %>% 
                html_text(trim = TRUE)
        
        if(length(local) == 0){
                local <- NA
        }
        
        # dados de local e titulo do anuncio
        anuncio <- data.frame(local = local,
                              titulo = titulo, 
                              url = url_anuncio,
                              stringsAsFactors = FALSE)
        
        # dados principais
        dados_principais <- anuncio_html %>% html_nodes(xpath = "//div[@class='card aviso-datos']/div/ul/li")
        dados_principais.itens <- lapply(dados_principais, parse_card)
        
        anuncio <- cbind (extract_dados_principais(dados_principais.itens), anuncio)
        
        # dados anunciante
        anunciante <- anuncio_html %>% html_nodes(xpath = "//div[@class='card aviso-datos-anunciante']/div/ul/li")
        l <- lapply(anunciante, parse_card)
        
        anuncio <- cbind(extract_dados_publicacao(l), anuncio)
        
        return(anuncio)
}

download_anuncios <- function(prefix, reset = FALSE){

        # cria um arquivo de controle contendo todos os links das propriedades
        controle_arquivo <- file.path(download_dir, "controleWimoveisDownload.RDS")
        
        if( file.exists(controle_arquivo) && !reset){
                print("Retomando download Wimoveis: ")
                controle <- readRDS(controle_arquivo)
        }else{
                # se nao, constroi arquivo de controle da lista de links
                print("Iniciando download Wimoveis")
                
                # seleciona arquivos de lista de links baixados usando a funcao download_lista_anuncios
                anuncios.links.csv <- file.path(download_dir, "links",
                                                list.files(path = download_dir, pattern = "wimoveis-links*"))
                
                # le todos os links de anuncios
                anuncios.links <- do.call(rbind, lapply(anuncios.links.csv, 
                                                     function(x) read.csv2(x, 
                                                                           stringsAsFactors = FALSE,
                                                                           col.names = "url" )))
                controle <- anuncios.links
                
                # seta flag que nenhum anuncio já foi baixado
                controle$downloaded <- FALSE
                saveRDS(controle, file = controle_arquivo)
        }
        
        # inicia o download dos anuncios
        for(i in 1:nrow(controle)){
                
                anuncio <- controle[i,]
                
                # se anuncio ainda nao foi baixado, faz o download e parse
                if( ! anuncio$downloaded){
                        anuncio.df <- extract_anuncio(anuncio$url)
                        
                        # grava fazendo append no arquivo (ignora repetidos, trata depois)
                        file <- file.path(download_dir, paste0(prefix, ".csv"))
                        
                        # writes header only once
                        if( ! file.exists(file)){
                                write.table(anuncio.df, 
                                            file = file,
                                            quote = T, 
                                            sep = ";")
                        }else {
                                write.table(anuncio.df, 
                                            file = file,
                                            append =  T, 
                                            quote = T, 
                                            sep = ";", 
                                            col.names = F)
                        }
                        
                        # atualiza arquivo de controle
                        controle[i,]$downloaded <- TRUE
                        saveRDS(controle, file = controle_arquivo)
                }
        }
}

read_anuncios_csv <- function(prefix){
        
        filename <- paste0(prefix, ".csv")
        file_anuncios <- file.path(download_dir, filename)
        anuncios.df <- read.csv2(file_anuncios, stringsAsFactors = FALSE, row.names = NULL )
        return(anuncios.df)
}
