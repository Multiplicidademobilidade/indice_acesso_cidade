# Esse script a limpeza dos dados brutos e geolocalização dos CRAS. Dados de CRAS
# acabaram não sendo utilizados para a criação dos índices, mas o processo fica 
# aqui para registro e eventual uso futuro

# Setup
source('fun/setup.R')
source('fun/cras/cras.R')

# Estrutura de pastas
ano = 2019 # Atualizar conforme o caso
files_folder <- "../../indice_acesso_cidade_dados"
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



########### 2. Limpeza e Geocode

# Aplicar funcao de limpeza e geocode
cras_geocode(ano = ano, 
             raw_data_folder = sprintf("%s/%s", cras_files_folder, ano), 
             out_folder = cras_base_folder, 
             run_gmaps = TRUE)