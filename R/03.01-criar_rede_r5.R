# Cria os grafos dos sistemas de transporte pelo r5r

# options(java.parameters = '-Xmx50G')
options(java.parameters = '-Xmx14G') # Mudar parâmetro de acordo com a memória
source("fun/setup.R")


# Constrói a rede dos municípios - o resultado é um arquivo chamado graph.obj
construir_graph_muni <- function(sigla_muni, ano, ano_pbf) {
  
  # Estrutura de pastas
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder11  <- sprintf("%s/11_malha_viaria/%s", files_folder, ano_pbf)
  subfolder15A <- sprintf("%s/15_r5r/01_graphs/%s/%s", files_folder, ano, sigla_muni)
  dir.create(subfolder15A, recursive = TRUE, showWarnings = FALSE)
  
  # Criar cópia do arquivo .pbf na nova pasta
  file.copy(sprintf('%s/%s/%s_%s.osm.pbf', subfolder11, sigla_muni, sigla_muni, ano), subfolder15A)
  
  # Rodar o r5r
  r5r::setup_r5(data_path = subfolder15A, use_elevation = TRUE, overwrite = TRUE)

  # Apagar cópia do arquivo .pbf  
  file.remove(sprintf('%s/%s_%s.osm.pbf', subfolder15A, sigla_muni, ano))
  
}


# Aplicar função
lapply(munis_list$munis_metro[ano_metro == 2019]$abrev_muni, construir_graph_muni, ano = 2019, ano_pbf = 2021)