# script para baixar tabela fipe de veiculos

library(stringr)
library(dplyr)
library(jsonlite)

api_base <- "http://fipeapi.appspot.com/api/1/carros/"
api_veiculos <- paste(api_base, "veiculos/", sep = "")
api_veiculo <- paste(api_base, "veiculo/", sep = "")
api_marcas <- paste(api_base, "marcas.json", sep = "")

get_marcas <- function(){
        marcas <- GET(api_marcas)        
        marcas <- fromJSON(content(m, "text"))
        names(marcas) <- c("marca.name", "marca.fipe_name", 
                           "marca.order", "marca.key", "marca.id")
        return(marcas)
}

get_modelos <- function(marca.id){
        get_uri <- paste(api_veiculos, marca.id, ".json", sep = "")
        veiculos <- GET(get_uri)
        veiculos <- fromJSON(content(veiculos, "text"))
        return(veiculos)
}

loadModelos <- function(marcas){
        modelos <- do.call(rbind, lapply(marcas$marca.id, get_modelos))
        names(modelos) <- c("marca.fipe_name", "modelo.name", "marca.name", "modelo.key", 
                            "modelo.id", "modelo.fipe_name" )
        modelos <- merge(marcas, modelos, by = "marca.name")
        return(modelos)
}

get_modelo_anos <- function(marca.id, modelo.id){
        get_uri <- paste(api_veiculo, marca.id, "/", modelo.id ,".json", sep = "")
        modelo.anos <- GET(get_uri)
        modelo.anos <- fromJSON(content(modelo.anos, "text"))
        print(modelo.anos)
        return(modelo.anos)
}

loadModelosAnos <- function(modelos){
        anos <- NULL
        for(i in 1:nrow(modelos.honda)){ 
                anos <- rbind(anos, get_modelo_anos(modelos.honda[i, ]$marca.id, 
                                              modelos.honda[i, ]$modelo.id))
        }
        
        names(anos) <- c("marca.fipe_name", "ano.fipe_codigo", "ano.name", "marca.name",
                                   "ano.key", "modelo.name", "ano.id")
        anos <- merge(modelos, anos, by = "modelo.name")
        anos$marca.fipe_name.x <- NULL
        anos$marca.fipe_name.y <- NULL
        anos$marca.name.x <- NULL
        anos$marca.name.y <- NULL
        return(anos)
}
# test set
marcas <- get_marcas()
principais_marcas <- c("CITROEN", "FIAT", "FORD", "CHEVROLET",
                       "HONDA", "HYUNDAI", "MITSUBISHI", "NISSAN",
                       "PEUGEOT", "RENAULT", "SUZUKI", "TOYOTA",
                       "VOLKSWAGEN")
principais_marcas <- marcas %>% filter(marca.name %in% principais_marcas)
modelos <- loadModelos(principais_marcas)
modelos.honda <- modelos %>% filter(marca.name == "HONDA" 
                              & str_detect(pattern = "Fit", modelo.name) 
                              & str_detect(pattern = "Aut.", modelo.name))


modelos.anos.honda <- loadModelosAnos(modelos.honda)

get_preco_veiculo <- function(marca.id, modelo.id, ano.id){
        get_uri <- paste(api_veiculo, marca.id, "/", modelo.id , "/", ano.id ,".json", sep = "")
        preco <- GET(get_uri)
        preco <- fromJSON(content(preco, "text"))
        return(preco)
}

loadModelosPrecos <- function(modelos.anos){
        preco.df <- NULL
        for( i in 1:nrow(modelos.anos)){
                marca.id <- modelos.anos[i, ]$marca.id
                modelo.id <- modelos.anos[i, ]$modelo.id
                ano.id <- modelos.anos[i, ]$ano.id
                p <- get_preco_veiculo(marca.id, modelo.id, ano.id)
                preco <- data.frame(p$referencia, p$fipe_codigo, p$name, p$combustivel,
                                p$marca, p$ano_modelo, p$preco, p$key, p$time,
                                p$veiculo, p$id)
                names(precos) <- names(p)
                preco.df <- rbind(preco.df, preco)
        }
        return(preco.df)
}

precos <- loadModelosPrecos(modelos.anos.honda)
precos
