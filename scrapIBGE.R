# download endereços censo ibge


library(RCurl)
# library(httr)
# library(XML)
dirs <- "ftp://anonymous:anonymous@ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Cadastro_Nacional_de_Enderecos_Fins_Estatisticos/"
setwd("C:/Users/deinf.arnaldo/Desktop/ibge-setores-censitarios")

uf.list = unlist(strsplit(getURL(dirs, dirlistonly = TRUE), split = "\r\n"))
uf.list <- uf.list[ uf.list != "Layout.zip" ]

download_folder_content <- function(basedir, subdir){

  # constroi link subdir
  dir.path <- paste0(basedir, subdir, "/")

  # baixa a lista de arquivos
  files.names <- unlist(strsplit(getURL(dir.path, verbose = TRUE, dirlistonly = TRUE), split = '\r\n'))

  # constroi o link dos arquivos
  files.path <- paste0(dir.path, files.name)

  # define os nomes locais dos arquivos
  local.names <- paste0(subdir, ".", files.names)

  # faz o download
  sapply(files.path, download.file, destfile = local.names)

  return(dir())
}

download_folder_content(dirs, 'AC')


# uf.dir <- paste0(dirs, uf.list[1], "/")
# files.name <- unlist(strsplit(getURL(uf.dir, verbose = TRUE, dirlistonly = TRUE), split = '\r\n'))
# files.path <- paste0(uf.dir, files.name)
# local.name <- paste0(uf.list[1], ".", files.name)

