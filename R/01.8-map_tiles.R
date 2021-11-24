### Este script faz download e edicao dos tiles que serao utilizados como fundo
## dos mapas do projeto


# carregar bibliotecas
source('fun/setup.R')
# não usada
# source("fun/crop_ggmap.R")

# 2) MAPBOX MAP TILES --------------------------------

# register mapbox api key
my_api <- data.table::fread("../../mapbox_token.txt", header = FALSE)
Sys.setenv(MAPBOX_API_KEY = my_api$V1)


# 2.1) Baixar e salvar os maps tiles de todos os municipios ---------------------
baixar_map_tile_ceramic <- function(ano, munis = "all") {
  
  # Criar pasta para salvar arquivos
  files_folder <- "../../indice-mobilidade_dados"
  subfolder1 <- sprintf("%s/01_municipios", files_folder)
  subfolder1A <- sprintf("%s/%s", subfolder1, ano)
  subfolder10 <- sprintf("%s/10_maptiles_crop", files_folder)
  subfolder10A <- sprintf("%s/%s", subfolder10, ano)
  subfolder10B <- sprintf("%s/mapbox", subfolder10A)
  dir.create(sprintf("%s", subfolder10B, ano), recursive = TRUE)
  
  baixar_map_tile_ceramic_muni <- function(sigla_muni) {
    
    # sigla_muni <- "oco"
    
    # read shape
    temp_sf <- read_rds(sprintf("%s/municipio_%s_%s.rds", subfolder1A, sigla_muni, ano))
    mapbox_url <- "https://api.mapbox.com/styles/v1/ciclocidade/cktkjh2ek5ckc17s16nr036kn/tiles/{zoom}/{x}/{y}"
    
    # download tile based on custom template (style)
    tile_for <- cc_location(loc = raster::extent(st_bbox(temp_sf)), 
                            base_url = mapbox_url
                            # type = "styles/v1/kauebraga/ck34n83gd0dli1cnvtnrlgber/tiles" 
                            # , debug = TRUE
    )
    
    plotRGB(tile_for)
    
    # as rgb data.frame
    tab <- as.data.frame(tile_for, xy = TRUE)
    names(tab) <- c("x", "y", "red", "green", "blue")
    tab$hex <- rgb(tab$red, tab$green, tab$blue, maxColorValue = 255)
    
    # save tile
    readr::write_rds(tab, sprintf("%s/maptile_crop_mapbox_%s_%s.rds", subfolder10B, sigla_muni, ano))
  }
  
  # aplicar funcao
  if (munis == "all") {
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
  } else (x = munis)
  lapply(x, baixar_map_tile_ceramic_muni)
}



# 2.2) Aplicar funcao -----------------------------------------------------------------------------

# baixar_map_tile_ceramic(ano = 2017)
# baixar_map_tile_ceramic(ano = 2018)
baixar_map_tile_ceramic(ano = 2019, munis = 'all')
