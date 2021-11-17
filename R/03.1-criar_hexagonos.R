
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.1 Cria grade de hexagonos para os municipios

# carregar bibliotecas
source('fun/setup.R')
library(h3jsr)

#### 1) Funcao para criar hexagonos ------------

criar_hexagonos <- function(ano, munis = "all") {
  
  # Select the corerspondent munis_df
  # munis_df <- get(sprintf("munis_df_%s", ano))
  
  shape_to_hexagon <- function(sigla_muni) {
    
    # Leitura do(s) municipio(s)
    #muni <- readr::read_rds(sprintf("~/repos/acesso_oport/data-raw/municipios/%s/municipio_%s_%s.rds", 
                                   #ano, sigla_muni, ano) ) %>%
    muni <- readr::read_rds(sprintf("../../indice-mobilidade_dados/01_municipios/%s/municipio_%s_%s.rds", 
                                  ano, sigla_muni, ano) ) %>%
      # Buffer para extender a area do municipio e assim evitar que os hexagonos nao considerem areas de borda
      st_buffer(dist = 0.003)
    
    # mapview(muni)
    
    # Definição da resolução
    # A Tabela de resoluções H3 pode ser encontrada em: https://h3geo.org/docs/core-library/restable

    res_todas <- c(8, 9) # Inicialmente foram propostas as resolucoes 8 e 9
    
    # Criar pastas para armazenamento dos dados (original era 'data-raw'); a estrutura é:
    # ../../-mobilidade_dados/municipios/[ano]/arquivos_municipios.rds e
    # ../../-mobilidade_dados/setores_censitarios/[ano]/arquivos_setores_censitarios.rds
    files_folder <- "../../indice-mobilidade_dados"
    subfolder1 <- sprintf("%s/03_hex_municipios", files_folder)
    subfolder1A <- sprintf("%s/%s", subfolder1, ano)
    
    if ("03_hex_municipios" %nin% list.dirs(files_folder, recursive = FALSE, full.names = FALSE)){
      dir.create(subfolder1)
    }
    if (ano %nin% list.dirs(subfolder1, recursive = FALSE, full.names = FALSE)){
      dir.create(subfolder1A)
    }
    # resolution <- res_todas[2]
    
    make_hex <- function(resolution) {
      
      res_fim <- paste0("0", resolution)
      out_file <- sprintf("hex_%s_%s_%s.rds", sigla_muni, ano, res_fim)
      
      if (out_file %nin% list.files(subfolder1A)){
          # get the unique h3 ids of the hexagons intersecting your polygon at a given resolution
          hex_ids <- polyfill(muni, res = resolution, simple = TRUE)
          
          
          # pass the h3 ids to return the hexagonal grid
          hex_grid <- hex_ids %>%  
            h3_to_polygon(simple = FALSE) %>%
            rename(id_hex = h3_address) %>%
            as_tibble() %>% 
            mutate(sigla_muni = sigla_muni)
          
          # delete possible duplicates
          hex_grid <- distinct(hex_grid, id_hex, .keep_all = TRUE) %>% st_sf()
          
          # mapview(select(hex_grid, geometry))
          
          # salvar ------------------------------------------------------------------
          
          write_rds(hex_grid, 
                    sprintf("%s/%s", subfolder1A, out_file), compress = 'gz')
      }
        }
    
    # salve arquivo de cada resolucao
    purrr::walk(res_todas, make_hex)
    
    
  }
  
  #### Aplicando funcao em paralelo para salvar grades de hexagonos ---------------------------------------------------------------
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
    
  } else (x = munis)
  
  # Parallel processing using future.apply
  # future::plan(future::multiprocess)
  # future.apply::future_lapply(X = x, FUN=shape_to_hexagon, future.packages=c('h3jsr','sf', 'dplyr'))
  lapply(X = x, FUN = shape_to_hexagon)
  
}


#### 2) Aplicar funcao ------------
criar_hexagonos(ano = 2019, munis = 'all')

