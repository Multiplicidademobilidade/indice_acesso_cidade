#> Esse script faz Download, Limpeza dos dados brutos e geolocalização dos CRAS

# Setup
source('fun/setup.R')

# Criar as pastas de trabalho para os dados de saúde
ano = 2019 # Atualizar conforme o caso
files_folder <- "../../indice-mobilidade_dados"
cras_base_folder <- sprintf("%s/08_cras_assist_social", files_folder)
subfolder8 <- sprintf("%s/%s", cras_base_folder, ano)
dir.create(sprintf("%s", subfolder8), recursive = TRUE, showWarnings = FALSE)

###### 1. Download arquivos originais ###################
data_base_folder <- sprintf("%s/00_Originais", files_folder)
cras_files_folder <- sprintf("%s/CRAS", data_base_folder)
dir.create(sprintf("%s/%s", cras_files_folder, ano), recursive = TRUE, showWarnings = FALSE)

# ----------------------------------
# CRAS 2019
# ----------------------------------
# Os arquivos podem ser encontrados neste link caso dê problema em baixar pelo R:
# http://aplicacoes.mds.gov.br/sagi/snas/vigilancia/index2.php
# No linux, os arquivos vêm com problema de encoding - descompactar usando:
# 7z x CRAS\(5\).zip
# download.file("https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/CRAS(5).zip" ,
#               destfile = sprintf("%s/Censo_SUAS_2019_CRAS.zip", cras_files_folder))
# unzip(sprintf("%s/%s/Censo_SUAS_2019_CRAS.zip", cras_files_folder, ano), exdir = cras_files_folder)


# ----------------------------------
# # CRAS 2018
# download.file('https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/CRAS(3).zip',
#               destfile = "../../data-raw/CRAS/Censo_SUAS_2018_CRAS.zip")
# unzip("../../data-raw/CRAS/Censo_SUAS_2018_CRAS.zip", exdir = "../../data-raw/CRAS/2018")
# 
# # CRAS 2017
# download.file('http://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/Censo_SUAS/2017/Censo_SUAS_2017_CRAS.zip',
#               destfile = "../../data-raw/CRAS/Censo_SUAS_2017_CRAS.zip")
# unzip("../../data-raw/CRAS/Censo_SUAS_2017_CRAS.zip",exdir = "../../data-raw/CRAS/2017")


########### 2. Limpeza e Geocode

# carregar funcoes
source('fun/cras/cras.R')

# Aplicar funcao 
cras_geocode(ano = ano, 
             raw_data_folder = sprintf("%s/%s", cras_files_folder, ano), 
             out_folder = cras_base_folder, 
             run_gmaps = TRUE)



