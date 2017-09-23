source("scrapWimoveis.R")
library(stringr)
library(dplyr)


# lapply(urls_venda, function(x) download_lista_anuncios(x, 
#                                                        url_base, 
#                                                        file_prefix = "venda"))

#download_lista_anuncios(urls_aluguel[1], url_base, file_prefix = "aluguel-asanorte-atualizado") 
download_anuncios(filename = "wimoves-anuncios-aluguel-asanorte.csv")



anuncios.df <- load_anuncios_csv(filename = "wimoveis-anuncios-aluguel.csv")
addColumns <- function(anuncios.df){
        
        anuncios.dt <- data.table(anuncios.df)
        anuncios.dt[str_detect(local, "Asa Norte"), bairro := "Asa Norte"]
        anuncios.dt[str_detect(local, "Asa Sul"), bairro := "Asa Sul"]
        anuncios.dt[str_detect(local, "Sudoeste"), bairro := "Sudoeste"]
        anuncios.dt[str_detect(local, "Noroeste"), bairro := "Noroeste"]
        anuncios.dt[, valor.aluguel.m2 := as.numeric(valor.aluguel) / as.integer(area_total)]
        anuncios.dt[, valor.aluguel := as.numeric(valor.aluguel)]
        
        return(anuncios.dt)
}

anuncios.df <- load_anuncios_csv(filename = 'anuncios-consolidados.csv')

View(anuncios.df)

anuncios.dt <- addColumns(anuncios.df)

anuncios.dt %>% 
        filter( bairro == "Asa Norte" #str_detect(local, "SQN") 
               & quartos == 3
               & banheiros == 2
               & valor.aluguel > 0) %>%
        select(local, valor.aluguel, area_total, valor.aluguel.m2, quartos, banheiros, suites) %>%
        arrange(as.numeric(area_total)) %>% View
        



