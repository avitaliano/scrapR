# scrap dos imoveis da página wimoveis

library(httr)
library(rvest)
library(stringr)
library(lubridate)

# urls 
url_base <- "http://www.wimoveis.com.br"

urls_venda <- c( "http://www.wimoveis.com.br/apartamentos-venda-asa-sul-brasilia.html",
        "http://www.wimoveis.com.br/apartamentos-venda-sudoeste-brasilia.html",
        "http://www.wimoveis.com.br/apartamentos-venda-noroeste-brasilia.html",
        "http://www.wimoveis.com.br/apartamentos-venda-asa-norte-brasilia.html")

urls_aluguel <- c("http://www.wimoveis.com.br/apartamentos-aluguel-asa-norte-brasilia.html",
                "http://www.wimoveis.com.br/apartamentos-aluguel-asa-sul-brasilia.html",
                "http://www.wimoveis.com.br/apartamentos-aluguel-sudoeste-brasilia.html",
                "http://www.wimoveis.com.br/apartamentos-aluguel-noroeste-brasilia.html")

# funcoes auxiliares
clean_html <- function(x) {
        return(str_trim(str_replace_all(str_replace_all(x, "\\t", ''), "\\n", "")))
}

parse_card <- function(li){
        atributo <- clean_html(li %>% html_nodes(".nombre") %>% html_text())
        valor <- clean_html(li %>% html_nodes(".valor") %>% html_text())
        return(c(atributo, valor))
}

extract_valor <- function(x){
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

# lista todos os anuncios da pagina
list_anuncios <- function(url, url_base){
        
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

        for (i in 1:length(l)){
                item <- str_trim(paste(l[[i]], collapse = " "))
                
                if ( str_detect(item, "Área total") ) {
                        area_total <- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Área útil") ) {
                        area_util <- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Quarto") ) {
                        quartos <- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Suite") ) {
                        suites <- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Banheiro") ) {
                        banheiros <- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Vaga") ) {
                        vagas <- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Idade do imóvel") ) {
                        idade.imovel <- str_extract(item, '[0-9]+')
                        
                } else if ( str_detect(item, "Valor Aluguel") ) {
                        valor.aluguel <- extract_valor(item)
                        
                } else if ( str_detect(item, "Valor Venda") ) {
                        valor.venda <- extract_valor(item)
                        
                } else if ( str_detect(item, "Valor Temporada") ) {
                        valor.temporada <- extract_valor(item)
                        
                } else if ( str_detect(item, "Valor Condominio") ) {
                        valor.condominio <- extract_valor(item)
                        
                } else if ( str_detect(item, "IPTU") ) {
                        valor.iptu <- extract_valor(item)
                        
                } else {
                        tipo_imovel <- item
                }
        }

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

        return(dados_principais)
}

extract_dados_publicacao <- function(l){
        
        cod.anuncio = NA
        cod.wimoveis = NA
        data.anuncio = NA
        
        for (i in 1:length(l)){
                item <- str_trim(paste(l[[i]], collapse = " "))
                
                if ( str_detect(item, "Código do anunciante") ) {
                        cod.anuncio <- str_split(item, ":")[[1]][2]
                } else if (str_detect(item, "Código Wimoveis") ) {
                        cod.wimoveis <- str_split(item, ":")[[1]][2]
                        
                } else if ( str_detect(item, "Publicado faz")) {
                        dias.publicacao <- str_extract(item, '[0-9]+')
                        data.anuncio <- Sys.Date() - days(dias.publicacao)
                } else if ( str_detect(item, "Publicado hoje")) {
                        data.anuncio <- Sys.Date()
                }
        }
        return(data.frame(cod.anuncio = cod.anuncio, 
                          cod.wimoveis = cod.wimoveis,
                          data.anuncio = data.anuncio))
}

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

download_anuncios <- function(url_anuncios, url_base, file_prefix, download_dir = "wimoveis"){

        # cria diretorio se ele nao existe
        if(!file.exists(download_dir)){
                dir.create(download_dir)
        }

        repeat{
                pag_anuncios <- list_anuncios(url_anuncios, url_base)     
                print (paste("Iniciando donwload do link ", url_anuncios))

                # nome do arquivo
                filename <- paste0(download_dir, format(Sys.time(), "/%Y%m%d%H%M%S-"),
                                   "wimoveis-",
                                   file_prefix,
                                   ".csv")

                # busca anuncios
                anuncios.df <- do.call(rbind, lapply(pag_anuncios$urls, extract_anuncio))
                
                # salva em arquivo
                write.csv2(anuncios.df, file = filename)
                print(paste("Salvo arquivo", filename))
                
                if (is.na(pag_anuncios$next_url)){
                        break
                } else{
                        url_anuncios <- pag_anuncios$next_url
                }
        }
}

lapply(urls_venda, function(x) download_anuncios(x, url_base, file_prefix = "venda"))


