#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### Leitura e filtro de elevacao/topografia

#' info:
#' Os dados de topografia são provenientes da missão SRTM (_Shuttle Radar 
#' Topography Mission_), que é um esforço de pesquisa internacional que obtém 
#' dados de elevação numa precisão de 30 metros. Os dados de elevação do SRTM são 
#' divididos por quadrículo de 1 grau de latidude e 1 longitude, então é 
#' necessário:
#'   1 - identificar os quadrículos que cobrem a cidade escolhida;
#'   2 - baixar o raster correspondente a cada quadrículos em uma pasta temporária;
#'   3 - unir os grids em um grid único, usando a função raster::mosaic();
#'   4 - recortar do raster a área correspondente ao bounding box do município;
#'   5 - salvar o raster do município na pasta correspondente.
#'   
#' Para ter acesso a esses dados, é necessário criar um login no site
#' https://urs.earthdata.nasa.gov, e informar os dados de usuário e senha quando
#' for rodar esse script.


# carregar bibliotecas
source('fun/setup.R')

message("Informar usuário e password para acessar https://urs.earthdata.nasa.gov ")
username <- readline("Informe o username : ")
password <- readline("Informe o password : ")


download_srtm <- function(ano, sigla_muni) {
  # Estrutura de pastas
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder1 <- sprintf("%s/01_municipios/%s", files_folder, ano)
  subfolder10 <- sprintf("%s/10_topografia/%s", files_folder, sigla_muni)
  dir.create(sprintf("%s", subfolder10), recursive = TRUE, showWarnings = FALSE)
  
  # Rodar somente caso arquivo final não exista na pasta
  out_file <- sprintf("topografia_%s.tif", sigla_muni)
  if (out_file %nin% list.files(subfolder10)) {
    
    message('\nComeçando processo para a cidade: ', sigla_muni)
    
    # Ler limites do município
    muni_sf <- readr::read_rds(sprintf("%s/municipio_%s_%s.rds", subfolder1, sigla_muni, ano))
    
    # muni_sf %>% mapview()
    
    # Extrair a bounding box do município
    bbox <- st_bbox(muni_sf)
    bbox <- as.integer(bbox) - 1
    
    # Identificar quais tiles são necessários para cobrir toda a área de estudo
    lons <- seq(floor(bbox[1]), ceiling(bbox[3]), by = 1)
    lats <- seq(floor(bbox[2]), ceiling(bbox[4]), by = 1)
    tiles <- expand.grid(lat = lats, lon = lons) %>%
      mutate(hx = if_else(lon < 0, "W", "E"),
             hy = if_else(lat < 0, "S", "N"))
    tile = sprintf("%s%02d%s%03d", tiles$hy, abs(tiles$lat), tiles$hx, abs(tiles$lon))
    
    # Construir a URL para cada tile
    urls <- paste0("https://e4ftl01.cr.usgs.gov/MEASURES/SRTMGL1.003/2000.02.11/",
                   tile, ".SRTMGL1.hgt.zip")
    
    # Baixar zip files e extrair os raster tiles
    outputdir <- tempdir()
    zipfiles <- paste0(outputdir, "/", tile, ".hgt.zip")
    rstfiles <- paste0(outputdir, "/", tile, ".hgt")
    
    walk2(urls, zipfiles, function(url, filename) {
      httr::GET(url = url, 
                authenticate(username, password),
                write_disk(path = filename, overwrite = TRUE),
                progress())
    })
    
    walk(zipfiles, unzip, exdir = outputdir)
    
    # Ler todos os raster tiles, juntá-los e fazer o recorte de acordo com o bounding box
    rst <- map(rstfiles, raster)
    if (length(rst) == 1) {
      rst_layer <- rst[[1]]
    } else {
      rst_layer <- do.call(raster::mosaic, args = c(rst, fun = mean))
    }
    rst_layer_crop <- raster::crop(rst_layer, st_bbox(muni_sf))
    
    # Salvar arquivos processados
    raster::writeRaster(rst_layer_crop, 
                        sprintf("%s/%s", subfolder10, out_file), 
                        overwrite = TRUE)
    # plot(rst_layer_crop)
    
  } else {
    message(paste0('Arquivo para a cidade ', sigla_muni, " já existe, pulando...\n"))
  }
}


walk('munis_list$munis_df$abrev_muni', download_srtm, ano = 2019)

