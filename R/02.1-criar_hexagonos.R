
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.1 Cria grade de hexagonos para os municipios

# carregar bibliotecas
source('fun/setup.R')


#### 1) Funcao para criar hexagonos ------------

criar_hexagonos <- function(ano, res = '08', munis = "all") {
  
  # Select the correspondent munis_df
  # munis_df <- get(sprintf("munis_df_%s", ano))
  
  # sigla_muni <- 'nat'; ano <- '2019'; res = '07'; resolution = as.numeric(res)
  
  shape_to_hexagon <- function(sigla_muni) {
    
    # Estrutura de pastas
    files_folder <- "../../indice-mobilidade_dados"
    subfolder1 <- sprintf("%s/01_municipios/%s", files_folder, ano)
    subfolder12 <- sprintf("%s/12_hex_municipios/%s", files_folder, ano)
    dir.create(subfolder12, recursive = TRUE, showWarnings = FALSE)
    
    # Leitura do(s) municipio(s)
    muni_orig <- readr::read_rds(sprintf("%s/municipio_%s_%s.rds", subfolder1, sigla_muni, ano) )
    
    # Buffer para extender a área do município e evitar que os hexágonos não 
    # considerem áreas de borda - vamos fazer um buffer bem grande e depois
    # descartar os que não intersecionam com o shape do município
    if (res == '07'){
      muni <- muni_orig %>% st_buffer(dist = 3000)
    } else if (res == '08') {
      muni <- muni_orig %>% st_buffer(dist = 1200)
    } else {
      stop(sprintf("Buffer para a resolução %s não definido. Defina no código e volte a rodar o script", res))
    }
    
    # # Checagem dos buffers e grids
    # out_path <- '../../indice-mobilidade_dados/00_Originais'
    # st_write(muni_orig, sprintf('%s/01_shape_municipio.gpkg', out_path), driver = 'GPKG', append = FALSE)
    # st_write(muni, sprintf('%s/02_cidade_com_buffer.gpkg', out_path), driver = 'GPKG', append = FALSE)
    
    # mapview(muni)
    
    # Definição da resolução
    # A Tabela de resoluções H3 pode ser encontrada em: https://h3geo.org/docs/core-library/restable
    # res_todas <- c(7, 8, 9)
    # resolution <- res_todas[2] 
    res_todas <- as.numeric(res)
    # resolution <- res_todas[1]
    
    # Criar grades hexagonais
    make_hex <- function(resolution) {
      
      # Checar se arquivo resultante já existe. Se sim, avisar e pular a cidade
      out_file <- sprintf("hex_%s_0%s_%s.rds", sigla_muni, resolution, ano)
      
      if (out_file %nin% list.files(subfolder12)){
          # get the unique h3 ids of the hexagons intersecting your polygon at a given resolution
          hex_ids <- polyfill(muni, res = resolution, simple = TRUE)
          
          # pass the h3 ids to return the hexagonal grid
          hex_grid <- hex_ids %>% h3_to_polygon(simple = FALSE) 
          # # Checagem dos buffers e grids
          # st_write(hex_grid, sprintf('%s/03_hexgrid_grande.gpkg', out_path), driver = 'GPKG', append = FALSE)
          
          # Filtrar somente os hexágonos que intersecionam com o shape do município
          hex_grid <- hex_grid %>% filter(st_intersects(hex_grid, muni_orig, sparse = FALSE))
          # # Checagem dos buffers e grids
          # st_write(hex_grid, sprintf('%s/04_hexgrid_filtrado.gpkg', out_path), driver = 'GPKG', append = FALSE)
          
          # Ajustes finais no hex_grid para exportar
          hex_grid <- 
            hex_grid %>%
            rename(id_hex = h3_address) %>%
            as_tibble() %>% 
            mutate(sigla_muni = sigla_muni)
          
          # delete possible duplicates
          hex_grid <- distinct(hex_grid, id_hex, .keep_all = TRUE) %>% st_sf()
          
          # mapview(select(hex_grid, geometry))
          
          # salvar ------------------------------------------------------------------
          write_rds(hex_grid, sprintf("%s/%s", subfolder12, out_file), compress = 'gz')
          st_write(hex_grid, sprintf("%s/%s.gpkg", subfolder12, out_file), driver = 'GPKG', append = FALSE)
          
          
      } else {
        message('Arquivo para a cidade ', sigla_muni, " já existe, pulando...\n")
      }
        }
    
    # Salvar arquivo de cada resolução
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
criar_hexagonos(ano = 2019, res = '08', munis = 'all')

