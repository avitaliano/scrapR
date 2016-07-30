# script para baixar tabela fipe de veiculos

library(rvest)
library(XML)
library(stringr)
library(dplyr)
library(jsonlite)
library(httr)


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

# debug
modelos.honda

modelos.anos.honda <- loadModelosAnos(modelos.honda)

modelos.anos.honda <- mapply(get_modelo_anos, modelos.honda[2,]$marca.id, modelos.honda[2,]$modelo.id)
#modelos.anos.honda <- get_modelo_anos(modelos.honda[1,]$marca.id, modelos.honda[1,]$modelo.id)
modelos.anos.honda[2]




str(honda.fit.anos)

get_preco_veiculo <- function(api_veiculo, marca.id, modelo.id, ano.id){
        get_uri <- paste(api_veiculo, marca.id, "/", modelo.id , "/", ano.id ,".json", sep = "")
        preco <- GET(get_uri)
        preco <- fromJSON(content(preco, "text"))
        return(preco)
}

preco <- with(honda.fit.anos[1, ], get_preco_veiculo(api_veiculo, marca.id, modelo.id, ano.id))
preco <- as.data.frame(preco)
names(preco) <- c("database", "")


