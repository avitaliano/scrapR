source("scrapWimoveis.R")


# lapply(urls_venda, function(x) download_lista_anuncios(x, 
#                                                        url_base, 
#                                                        file_prefix = "venda"))
# 
# download_anuncios(filename = "wimoves-anuncios-venda.csv")

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

anuncios.df <- load_anuncios_csv(filename = 'wimoveis-anuncios-aluguel.csv')
anuncios.dt <- addColumns(anuncios.df)

anuncios.dt




