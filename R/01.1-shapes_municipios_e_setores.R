#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.1 Download de shape file de municipios e setores censitarios dos  municipios incluidos no projeto

# carregar bibliotecas
source("fun/setup.R")
# função do geobr para dissolver polígonos
source("fun/dissolve_polygons.R")


# 1. Funcao para download de shape file dos municipios e setores censitarios
download_muni_setores <- function(ano, munis = "all") {

  download_muni_setores_un <- function(sigla_muni) {
    # Extrair código do município da tabela munis_metro
    code_munis <- 
      munis_list$munis_metro[abrev_muni == sigla_muni & ano_metro == ano]$code_muni %>%
      # munis_list$munis_metro[abrev_muni == 'sgo' & ano_metro == 2019]$code_muni %>%
      unlist()
    
    # O state code são os primeiros dois dígitos de code_munis
    state_code <- substr(code_munis[1], 1, 2) %>% as.numeric()
    
    # Criar pastas para armazenamento dos dados (original era 'data-raw'); a estrutura é:
    # ../../-mobilidade_dados/municipios/[ano]/arquivos_municipios.rds e
    # ../../-mobilidade_dados/setores_censitarios/[ano]/arquivos_setores_censitarios.rds
    files_folder <- "../../indice-mobilidade_dados"
    subfolder1 <- sprintf("%s/01_municipios", files_folder)
    subfolder1A <- sprintf("%s/%s", subfolder1, ano)
    subfolder2 <- sprintf("%s/02_setores_censitarios", files_folder)
    subfolder2A <- sprintf("%s/%s", subfolder2, ano)
    if ("indice-mobilidade_dados" %nin% list.dirs('../../', recursive = FALSE, full.names = FALSE)){
      dir.create(files_folder)
    }
    if ("municipios" %nin% list.dirs(files_folder, recursive = FALSE, full.names = FALSE)){
      dir.create(subfolder1)
    }
    if ("setores_censitarios" %nin% list.dirs(files_folder, recursive = FALSE, full.names = FALSE)){
      dir.create(subfolder2)
    }
    if (ano %nin% list.dirs(subfolder1, recursive = FALSE, full.names = FALSE)){
      dir.create(subfolder1A)
    }
    if (ano %nin% list.dirs(subfolder2, recursive = FALSE, full.names = FALSE)){
      dir.create(subfolder2A)
    }
    
    # Fazer download de arquivos - shapes dos municipios
    out_file1 <- sprintf("municipio_%s_%s.rds", sigla_muni, ano)
    # Baixar somente se arquivo não existir na pasta
    if (out_file1 %nin% list.files(subfolder1A)){
        uf_sf <- geobr::read_municipality(code_muni = state_code, year = ano)
        muni_sf <- uf_sf %>% filter(code_muni %in% code_munis)
        # muni_sf <- purrr::map_dfr(code_munis, geobr::read_municipality, year=ano, simplified = F)
        # Dissolver os polígonos dos municípios componentes da RM
        muni_sf <- dissolve_polygons(muni_sf, group_column = "code_state")
        # Transformar em WGS84
        muni_sf <- st_transform(muni_sf, 4326)
        
        # salvar municipios
        readr::write_rds(muni_sf, sprintf("%s/%s", subfolder1A, out_file1), compress = 'gz')
    }
    
    
    # Download de arquivos - shapes dos setores censitarios
    out_file2 <- sprintf("setores_%s_%s.rds", sigla_muni, ano)
    # Baixar somente se arquivo não existir na pasta
    if (out_file2 %nin% list.files(subfolder2A)){
      uf_sf_tracts <- geobr::read_census_tract(state_code)
      ct_sf <- uf_sf_tracts %>% filter(code_muni %in% code_munis)
      ct_sf <- st_transform(ct_sf, 4326)
      
      # salvar setores censitarios
      readr::write_rds(ct_sf, sprintf("%s/%s", subfolder2A, out_file2), compress = 'gz')
    }
  }
  
  # 2. Aplicar função
  if (munis == "all") {
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
  } else (x = munis)
  
  lapply(X=x, FUN=download_muni_setores_un)
}


# Baixar shapes e dados censitários de municípios - usar uma sigla das presentes
# em munis_list$munis_metro[ano_metro == ano]$abrev_muni ou 'all' para todos
# download_muni_setores(ano = 2019, munis = 'sgo')
# download_muni_setores(ano = 2019, munis = 'all')

download_muni_setores(ano = 2019, munis = 'all')
