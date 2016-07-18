# download enderecos censo ibge


library(RCurl)
dirs <- "ftp://anonymous:anonymous@ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Cadastro_Nacional_de_Enderecos_Fins_Estatisticos/"

# download lista de estados
uf.list = unlist(strsplit(getURL(dirs, dirlistonly = TRUE), split = "\r\n"))
uf.list <- uf.list[ uf.list != "Layout.zip" ]

download_folder_content <- function(basedir, subdir){

  # constroi link subdir
  dir.path <- paste0(basedir, subdir, "/")

  # baixa a lista de arquivos
  files.names <- unlist(strsplit(getURL(dir.path, verbose = TRUE, dirlistonly = TRUE), split = '\r\n'))

  # constroi o link dos arquivos
  files.path <- paste0(dir.path, files.names)

  # define os nomes locais dos arquivos
  local.names <- paste0("Data/", subdir, ".", files.names)

  # faz o download
  #sapply(files.path, ?download.file, destfile = local.names)
  mapply(download.file, url = files.path, destfile = local.names)

  return(dir("Data"))
}

# faz o download para todos os estados
lapply(uf.list, download_folder_content, basedir = dirs)
