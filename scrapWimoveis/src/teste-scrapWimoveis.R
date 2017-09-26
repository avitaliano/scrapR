# teste

source("scrapWimoveis/src/scrapWimoveis.R")

scrap_wimoveis(prefix = "teste-venda-asanorte")
download_anuncios(prefix = "teste-venda-asanorte", reset = TRUE)
