#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.1 Criacao de graphs no R5R

# Para usar o r5r é preciso instalar o Java SDK (ver https://ipeagit.github.io/r5r/);
# para o Debian 11, instalar os pacotes default-jdk, openjdk-11-jdk, default-jre e
# openjdk-11-jre
# options(java.parameters = '-Xmx50G')
options(java.parameters = '-Xmx14G')
library(r5r)
source("fun/setup.R")



# FUNCAO PARA CONSTRUIR network -------------------------
# graph.obj é salvo na pasta de destino

construir_graph_muni <- function(sigla_muni, ano, ano_pbf) {
  
  # sigla_muni <- 'cam'
  
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder11 <- sprintf("%s/11_malha_viaria/%s", files_folder, ano_pbf)
  subfolder15A <- sprintf("%s/15_otp/01_graphs/%s/%s", files_folder, ano, sigla_muni)
  dir.create(subfolder15A, recursive = TRUE, showWarnings = FALSE)
  
  # Criar cópia do arquivo .pbf na nova pasta
  file.copy(sprintf('%s/%s/%s_%s.osm.pbf', subfolder11, sigla_muni, sigla_muni, ano), subfolder15A)
  
  # Rodar o r5r
  r5r::setup_r5(data_path = subfolder15A, use_elevation = TRUE, overwrite = TRUE)

  # Apagar cópia do arquivo .pbf  
  file.remove(sprintf('%s/%s_%s.osm.pbf', subfolder15A, sigla_muni, ano))
  
}


# aplicar funcao ------------------------------------------------------------------------------
# lapply(munis_list$munis_metro[ano_metro == 2017]$abrev_muni, construir_graph_muni, ano = 2017)
# lapply(munis_list$munis_metro[ano_metro == 2018]$abrev_muni, construir_graph_muni, ano = 2018)
lapply(munis_list$munis_metro[ano_metro == 2019]$abrev_muni, construir_graph_muni, ano = 2019, ano_pbf = 2021)


# construir_graph_muni(sigla_muni = 'tsa', ano = 2019, ano_pbf = 2021)
