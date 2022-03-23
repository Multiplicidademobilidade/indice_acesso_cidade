
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### Cria grade de hexágonos para os municípios

# carregar bibliotecas
source('fun/setup.R')


#### 1) Função para criar hexágonos ------------

criar_hexagonos <- function(ano, res = '08', munis = "all") {
  
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
    if (res == '07') {
      muni <- muni_orig %>% st_buffer(dist = 3000)
    } else if (res == '08') {
      muni <- muni_orig %>% st_buffer(dist = 1200)
    } else {
      stop(sprintf("Buffer para a resolução %s não definido. Defina no código e volte a rodar o script", res))
    }
    
    # # Checagem dos buffers e grids
    out_path <- '../../indice-mobilidade_dados/00_Originais'
    # st_write(muni_orig, sprintf('%s/01_shape_municipio.gpkg', out_path), driver = 'GPKG', append = FALSE)
    # st_write(muni, sprintf('%s/02_cidade_com_buffer.gpkg', out_path), driver = 'GPKG', append = FALSE)
    
    # mapview(muni)
    
    # Definição da resolução - a tabela de resoluções H3 pode ser encontrada em: 
    # https://h3geo.org/docs/core-library/restable
    # res_todas <- c(7, 8, 9)
    # resolution <- res_todas[2] 
    res_todas <- as.numeric(res)
    
    # Criar grades hexagonais
    make_hex <- function(resolution) {
      
      # Checar se arquivo resultante já existe. Se sim, avisar e pular a cidade
      out_file <- sprintf("hex_%s_0%s_%s.rds", sigla_muni, resolution, ano)
      
      if (out_file %nin% list.files(subfolder12)) {
        message('\nComeçando cidade ', sigla_muni, "...\n")
        # Pegar os ids únicos h3 dos hexágonos que fazem interseção com o polígono
        # a uma determinada resolução
        hex_ids <- polyfill(muni, res = resolution, simple = TRUE)
        
        # Passar os ids h3 de forma a retornar o grid hexagonal
        hex_grid <- hex_ids %>% h3_to_polygon(simple = FALSE) 
        # # Checagem dos buffers e grids
        # st_write(hex_grid, sprintf('%s/03_hexgrid_grande.gpkg', out_path), driver = 'GPKG', append = FALSE)
        
        # Filtrar somente os hexágonos que intersecionam com o shape do município
        hex_grid <- hex_grid %>% filter(st_intersects(hex_grid, muni_orig, sparse = FALSE))
        # # Checagem dos buffers e grids
        # st_write(hex_grid, sprintf('%s/04_hexgrid_filtrado.gpkg', out_path), driver = 'GPKG', append = FALSE)
        
        
        # --------------------------------------------------------------------
        #  Calcular centróides somente das áreas que estão no município
        # --------------------------------------------------------------------
        
        # Pegar áreas dos hexágonos que intersecionam com shape do município
        hex_muni <- hex_grid %>% st_intersection(muni_orig)
        # st_write(hex_muni, sprintf('%s/05_hexmuni.gpkg', out_path), driver = 'GPKG', append = FALSE)
        
        # Calcular centróide somente para essas áreas
        hex_muni_centroid <- hex_muni %>% st_centroid()
        # st_write(hex_muni_centroid, sprintf('%s/06_hexmuni_centroid.gpkg', out_path), driver = 'GPKG', append = FALSE)
        
        # Alguns centróides podem ter caído fora do shape do município - separar
        # os que caíram dentro dos que caíram fora
        centroide_dentro_muni <- hex_muni_centroid %>% filter(st_intersects(hex_muni_centroid, muni_orig, sparse = FALSE))
        centroide_fora_muni <- hex_muni_centroid %>% filter(!st_intersects(hex_muni_centroid, muni_orig, sparse = FALSE))
        
        # Para os que caíram fora, usar st_point_on_surface() em vez de st_centroid()
        # https://stackoverflow.com/questions/52522872/r-sf-package-centroid-within-polygon
        point_on_surface_fora_muni <- hex_muni %>% filter(hex_muni$h3_address %in% centroide_fora_muni$h3_address) %>% st_point_on_surface()
        # st_write(point_on_surface_fora_muni, sprintf('%s/07_hexmuni_point_on_surface.gpkg', out_path), driver = 'GPKG', append = FALSE)
        
        # Juntar os dois resultados
        centroides_muni <- 
          centroide_dentro_muni %>% 
          rbind(point_on_surface_fora_muni) %>% 
          as.data.frame() %>% 
          mutate(centroides_muni = as.character(geometry)) %>% 
          dplyr::select(id_hex = h3_address, centroides_muni)
        
        
        # Ajustes finais no hex_grid para exportar
        hex_grid <- 
          hex_grid %>%
          rename(id_hex = h3_address) %>%
          as_tibble() %>% 
          mutate(sigla_muni = sigla_muni) %>% 
          left_join(centroides_muni, by = 'id_hex')
        
        # Apagar possíveis duplicatas
        hex_grid <- distinct(hex_grid, id_hex, .keep_all = TRUE) %>% st_sf()
        
        # mapview(select(hex_grid, geometry))
        
        
        # --------------------------------------------------------------------
        #  Calcular pontos mais próximos do centróide no viário
        # --------------------------------------------------------------------
        # Rodar apenas quando a resolução for menor do que 8
        if (resolution < 8) {
          # Abrir viário do municipio (rede OSM)
          map_file <- sprintf("%s/%s_%s.osm.pbf", subfolder11, sigla_muni, ano)
          
          # Arquivo de viário para o município pode ser muito grande e não ler 
          # inteiro com o read_sf() abaixo. Em especial, isso vai acontecer com
          # São Paulo - todos os arquivos de malha viária das cidades têm menos
          # de 25 MB, enquanto o de SP tem quase 120 MB. Na dúvida, vamos ler
          # todos esses arquivos como .gpkg para garantir que o viário seja lido
          # em sua totalidade
          # viario_muni <- read_sf(map_file, layer = 'lines') # não usar
          
          # Ler arquivos de viário como .gpkg. Sobre este tema, ver:
          # https://github.com/ropensci/osmextract/issues/12
          read_from_gpkg <- function(path) {
            gpkg_file <- paste0(tempfile(), ".gpkg")
            gdal_utils(
              util = "vectortranslate",
              source = path, 
              destination = gpkg_file, 
              options = c("-f", "GPKG", "lines")
            )
            res <- st_read(gpkg_file, quiet = TRUE)
            names(res)[which(names(res) == "geom")] <- "geometry"
            st_geometry(res) <- "geometry"
            res
          }
          
          viario_muni <- read_from_gpkg(map_file)
          
          
          # Isolar somente tipos de ruas em que circulam veículos
          # ver https://wiki.openstreetmap.org/wiki/Key:highway
          # Uma observação: a tag 'unclassified' pode significar retirar estradas
          # em zonas rurais com pouca estrutura de viário. Para garantir que
          # tais hexágonos não fiquem sem pontos (pois sem viário, o latlong
          # resultante deste script será 0.0, 0.0 como placeholder), na fase 
          # seguinte de filtragem dos hexágonos para a matriz de tempos de
          # ônibus, vamos incluir um fallback: se o hexágono tem população
          # residente ou oportunidades mas está com laglong zero, usar o latlong
          # do centróide do hexágono.
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
            
            # hex_geom <- hex_grid %>% as.data.frame() %>% filter(id_hex == '87a810176ffffff') %>% dplyr::select(geometry) %>% pull()
            # muni_streets = viario_muni
            
            # Calcular o centróide do hexágono
            centroide <- st_centroid(hex_geom) %>% st_sfc(crs = 4326)
            
            # Recortar as ruas do shape do OSM que estão dentro do hexágono
            ruas_recortadas <- muni_streets %>% filter(st_intersects(muni_streets, hex_geom, sparse = FALSE))
            
            # Pegar a via (feature) mais próxima ao centróide do hexágono - o
            # resultado é o index da feature mais próxima
            idx_vmp <- st_nearest_feature(centroide, ruas_recortadas)
            # print(idx_vmp)
            
            if (is.na(idx_vmp)) {
              # Se viário não foi encontrado, criar ponto de latlong 0.0 como placeholder
              # geometry = st_sfc(st_point(c(0, 0)))
              # new_centroid <- st_sf(geometry, crs = 4326)
              # Transformar em dataframe para padronizar resultados
              # new_centroid <- new_centroid %>% as.data.frame() %>% cbind(centroide)
              
              # Retornar como dataframe, com as colunas de centróide e placeholder de ponto_viário como character
              new_centroid <- 
                as.data.frame(centroide) %>% 
                mutate(ponto_viario = 'c(0, 0)', centroide = as.character(geometry)) %>% 
                dplyr::select(-geometry)
              
            } else {
              # Se foi encontrada, isolar linha com o feature mais próximo e pegar seu centróide
              new_centroid <- ruas_recortadas %>% slice(idx_vmp:idx_vmp) %>% st_centroid()
              
              # Retornar como dataframe, com as colunas de centróide e ponto_viário como character
              new_centroid <- 
                new_centroid %>% 
                as.data.frame() %>% 
                dplyr::select(ponto_viario = geometry) %>% 
                cbind(centroide) %>% 
                mutate(across(everything(), ~ as.character(.))) %>% 
                rename(centroide = geometry)
              
            }
            # print(new_centroid)
            
            return(new_centroid)
              
          }
            
          # Pegar coordenadas do ponto viário mais próximo aos centróides dos hexágonos
          # e dos próprios centróides dos hexágonos
          new_centroids <- 
            lapply(hex_grid$geometry, pegar_viario_mais_proximo, muni_streets = viario_muni) %>% 
            rbindlist()
          
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
                   centroides_muni = as.character(centroides_muni),
                   centroides_muni = str_replace(centroides_muni, 'c\\(', ''),
                   centroides_muni = str_replace(centroides_muni, '\\)', ''),
                   ponto_viario = as.character(ponto_viario),
                   ponto_viario = str_replace(ponto_viario, 'c\\(', ''),
                   ponto_viario = str_replace(ponto_viario, '\\)', '')) %>%
            separate(centroide, into = c('long_centroide', 'lat_centroide'), sep = ', ', remove = TRUE) %>%
            separate(centroides_muni, into = c('long_centroides_muni', 'lat_centroides_muni'), sep = ', ', remove = TRUE) %>%
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
