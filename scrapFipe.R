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

marcas = paste(api_base, "marcas.json", sep = "")
m <- GET(marcas)
marcas <- fromJSON(content(m, "text"))

names(marcas) <- c("marca.name", "marca.fipe_name", "marca.order", "marca.key", "marca.id")
principais_marcas <- marcas[1:18,]
str(marcas)

get_modelos <- function(api_veiculos, marca.id){
        get_uri <- paste(api_veiculos, marca.id, ".json", sep = "")
        veiculos <- GET(get_uri)
        veiculos <- fromJSON(content(veiculos, "text"))
        return(veiculos)
}

modelos <- do.call(rbind, lapply(principais_marcas$marca.id, get_modelos, api_veiculos = api_veiculos))
str(modelos)
names(modelos) <- c("marca.fipe_name", "modelo.name", "marca.name", "modelo.key", 
                    "modelo.id", "modelo.fipe_name" )

modelos <- merge(marcas, modelos, by = "marca.name")
str(modelos)
m_honda <- modelos %>% filter(marca.name == "HONDA" 
                              & str_detect(pattern = "Fit", modelo.name) 
                              & str_detect(pattern = "Aut.", modelo.name)
                              & str_detect(pattern = "EX", modelo.name))
str(m_honda)

get_modelo_anos <- function(api_veiculo, marca.id, modelo.id){
        get_uri <- paste(api_veiculo, marca.id, "/", modelo.id ,".json", sep = "")
        modelo.anos <- GET(get_uri)
        modelo.anos <- fromJSON(content(modelo.anos, "text"))
        return(modelo.anos)
}

honda.fit.anos <- get_modelo_anos(api_veiculo, m_honda$marca.id, m_honda$modelo.id)
str(honda.fit.anos)

names(honda.fit.anos) <- c("marca.fipe_name", "ano.fipe_codigo", "ano.name", "marca.name",
                           "ano.key", "modelo.name", "ano.id")

m_honda
honda.fit.anos <- merge(m_honda, honda.fit.anos, by = "modelo.name")
honda.fit.anos$marca.fipe_name.x <- NULL
honda.fit.anos$marca.fipe_name.y <- NULL
honda.fit.anos$marca.name.x <- NULL
honda.fit.anos$marca.name.y <- NULL
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


