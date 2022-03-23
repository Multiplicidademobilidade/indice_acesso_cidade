### Este script faz download e edição dos tiles que serão utilizados como fundo
## dos mapas do projeto


# carregar bibliotecas
source('fun/setup.R')

# MAPBOX MAP TILES --------------------------------
# register mapbox api key
my_api <- data.table::fread("../../mapbox_token.txt", header = FALSE)
Sys.setenv(MAPBOX_API_KEY = my_api$V1)


# Baixar e salvar os maps tiles de todos os municipios
baixar_map_tile_ceramic <- function(ano, munis = "all") {
  
  # Criar pasta para salvar arquivos
  files_folder <- "../../indice-mobilidade_dados"
  subfolder1 <- sprintf("%s/01_municipios/%s", files_folder, ano)
  subfolder9 <- sprintf("%s/09_maptiles_crop/%s", files_folder, ano)
  dir.create(sprintf("%s", subfolder9), recursive = TRUE, showWarnings = FALSE)
  
  baixar_map_tile_ceramic_muni <- function(sigla_muni) {
    
    # Rodar somente caso arquivo final não exista na pasta
    out_file <- sprintf("maptile_crop_mapbox_%s_%s.rds", sigla_muni, ano)
    
    if (out_file %nin% list.files(subfolder9)) {
      # read shape
      temp_sf <- read_rds(sprintf("%s/municipio_%s_%s.rds", subfolder1, sigla_muni, ano))
      mapbox_url <- "https://api.mapbox.com/styles/v1/ciclocidade/cktkjh2ek5ckc17s16nr036kn/tiles/{zoom}/{x}/{y}"
      
      # download tile based on custom template (style)
      tile_for <- cc_location(loc = raster::extent(st_bbox(temp_sf)), 
                              base_url = mapbox_url
      )
      
      plotRGB(tile_for)
      
      # as rgb data.frame
      tab <- as.data.frame(tile_for, xy = TRUE)
      names(tab) <- c("x", "y", "red", "green", "blue")
      tab$hex <- rgb(tab$red, tab$green, tab$blue, maxColorValue = 255)
      
      # save tile
      readr::write_rds(tab, sprintf("%s/%s", subfolder9, out_file))
    } else {
      message('\nArquivo para a cidade ', sigla_muni, " já existe, pulando...\n")
    }
  }
  
  # aplicar funcao
  if (munis == "all") {
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
  } else (x = munis)
  lapply(x, baixar_map_tile_ceramic_muni)
}


# Aplicar função
baixar_map_tile_ceramic(ano = 2019, munis = 'all')
