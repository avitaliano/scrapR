# teste

source("scrapWimoveis/src/scrapWimoveis.R")

url_anuncios <- "http://www.wimoveis.com.br/apartamentos-aluguel-asa-norte-brasilia-3-quartos-80-110-m2.html"

download_lista_anuncios(prefix = "similares")
download_anuncios(prefix = "similares", reset = TRUE)
df <- read_anuncios_csv(prefix = "similares")

nrow(df)
names(df)
