# Scrap informações disponibilizadas pelo BCB via webservices

# tutorial em
# http://cienciadosdados.com.br/2015/09/01/acesse-a-base-de-dados-do-banco-central-via-r/

library(RCurl)
library(XML)
#Versão antiga do SSOAP
install.packages("SSOAP_0.9-0.tar.gz", type="source", repos = "https://cran.r-project.org/")

# obtem o wsdl do site
wsdl <- getURL("https://www3.bcb.gov.br/sgspub/JSP/sgsgeral/FachadaWSSGS.wsdl",
               ssl.verifypeer = FALSE,.encoding="UTF-8")
doc <- xmlInternalTreeParse(wsdl)
doc
