# scrap direto na pagina da fipe

library(httr)
library(rvest)
library(XML)

fipe_baseurl <- "http://veiculos.fipe.org.br/"

doc <- read_html(fipe_baseurl)
str(doc)
