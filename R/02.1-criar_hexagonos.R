
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
    subfolder11  <- sprintf("%s/11_malha_viaria/2021/%s", files_folder, sigla_muni)
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
          
          
          # --------------------------------------------------------------------
          #  Mover centróides dos hexágonos para ponto mais próximo no viário
          # --------------------------------------------------------------------
          # Rodar apenas quando a resolução for menor do que 8
          if (resolution < 8){
            # Abrir viário do municipio (rede OSM)
            map_file <- sprintf("%s/%s_%s.osm.pbf", subfolder11, sigla_muni, ano)
            viario_muni <- read_sf(map_file, 'lines')
            
            # Isolar somente tipos de ruas em que circulam veículos
            # ver https://wiki.openstreetmap.org/wiki/Key:highway
            non_road_types <- c('track', 'escape, ', 'raceway', 'footway', 
                                'service', 'unclassified',
                                'bridleway', 'steps', 'corridor', 'path', 
                                'proposed', 'construction')
            viario_muni <- viario_muni %>% filter(highway %nin% non_road_types & !is.na(highway))
            
            # Filtrar somente as linhas de viário que intersecionam com o shape do município - vamos
            # transformar o shape de linha em pontos para poder usar o st_intersects(), que é bem
            # rápido, e para que a feature mais próxima do centróide seja também um ponto
            viario_muni <- viario_muni %>% st_cast('POINT')
            viario_muni <- viario_muni %>% filter(st_intersects(viario_muni, muni_orig, sparse = FALSE))
            
            
            # Pegar centróide do viário mais próximo ao centróide do hexágono
            pegar_viario_mais_proximo <- function(hex_geom, muni_streets){
              
              # Calcular o centróide do hexágono
              centroide <- st_centroid(hex_geom) %>% st_sfc(crs = 4326)
              
              # Recortar as ruas do shape do OSM que estão dentro do hexágono
              ruas_recortadas <- muni_streets %>% filter(st_intersects(muni_streets, hex_geom, sparse = FALSE))
              
              # Pegar a via (feature) mais próxima ao centróide do hexágono - o
              # resultado é o index da feature mais próxima
              idx_vmp <- st_nearest_feature(centroide, ruas_recortadas)
              # print(idx_vmp)
              
              if (is.na(idx_vmp)){
                # Se viário não foi encontrado, criar ponto de latlong 0.0 como placeholder
                geometry = st_sfc(st_point(c(0, 0)))
                new_centroid <- st_sf(geometry, crs = 4326)
                # Transformar em dataframe para padronizar resultados
                new_centroid <- new_centroid %>% as.data.frame() %>% cbind(centroide)
                
              } else {
                # Se foi encontrada, isolar linha com o feature mais próximo e pegar seu centróide
                new_centroid <- ruas_recortadas %>% slice(idx_vmp:idx_vmp) %>% st_centroid()
                # Transformar em dataframe para padronizar resultados
                new_centroid <- new_centroid %>% dplyr::select(geometry) %>% as.data.frame() %>% cbind(centroide)
                
              }
              # print(new_centroid)
  
              return(new_centroid)
              
            }
            
            # Pegar coordenadas do ponto viário mais próximo aos centróides dos hexágonos
            # e dos próprios centróides dos hexágonos
            new_centroids <- 
              lapply(hex_grid$geometry, pegar_viario_mais_proximo, muni_streets = viario_muni) %>% 
              rbindlist() %>% 
              setNames(c('ponto_viario', 'centroide'))
            
            # Juntar resultados ao hex_grid
            hex_grid <- hex_grid %>% cbind(new_centroids)
            
            # Checar resultados
            # hex_grid %>% as.data.frame() %>% dplyr::select(-c(geometry, ponto_viario)) %>% st_as_sf() %>% mapview()
            # hex_grid %>% as.data.frame() %>% dplyr::select(-c(geometry, centroide)) %>% st_as_sf() %>% mapview()
             
            # Transformar para salvar em .csv
            hex_grid_csv <-
              hex_grid %>%
              mutate(centroide = as.character(centroide),
                     centroide = str_replace(centroide, 'c\\(', ''),
                     centroide = str_replace(centroide, '\\)', ''),
                     ponto_viario = as.character(ponto_viario),
                     ponto_viario = str_replace(ponto_viario, 'c\\(', ''),
                     ponto_viario = str_replace(ponto_viario, '\\)', '')) %>%
              separate(centroide, into = c('long_centroide', 'lat_centroide'), sep = ', ', remove = TRUE) %>%
              separate(ponto_viario, into = c('long_viario', 'lat_viario'), sep = ', ', remove = TRUE)

            write_delim(hex_grid_csv, sprintf("%s/%s.csv", subfolder12, out_file), delim = ';')
          }
          
          
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

