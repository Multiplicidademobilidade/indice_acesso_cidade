Sys.setenv(TZ='UTC') # Fuso horario local

# Carregar bibliotecas

# Estes pacotes todos estão no Tidyverse
# https://www.tidyverse.org/packages/
library(tidyverse)
library(tidylog)
# library(ggplot2)      # visualizacao de dados
# library(dplyr)
# library(tidyr)        # manipulacao de dados
# library(readr)        # rapida leitura de dados 
# library(purrr)
# library(stringr)      # operacoes em strings
# library(forcats)

library(beepr)
library(bit64)        # viz large numbers
library(data.table)   # manipulacao de dados
library(extrafont)    # fontes de texto
library(fasttime)     # rapido processamento de dados em data/horario
library(furrr)
library(future.apply) # Aplicar funcoes em paralelo
library(geobr)        # dados espaciais do brasil
library(ggmap)        # geocoding
library(ggthemes)     # temas para visualizacao de dados
library(h3jsr)        # h3 hex remotes::install_github("obrl-soil/h3jsr"), requer install.packages("V8")
library(hrbrthemes)   # requer hrbrthemes::import_roboto_condensed()
library(janitor)
library(knitr)
library(lubridate)    # dados em data/horario
library(maptools)
library(mapview)      # visualizacao interativa dos dados
library(opentripplanner) # Usar OTP de dentro do R: https://github.com/ITSLeeds/opentripplanner
library(patchwork)
library(pbapply)      # progress bar
library(quantreg)
library(raster)
library(RColorBrewer) # paleta de cores
library(rgeos)
library(sf)           # leitura e manipulacao de dados espaciais

# Não utilizadas (vindas do script original do IPEA)
# library(bit.64)       # lidar com numeros ee 64bits
# library(h3jsr) # H3 grade hexagonal
# library(Hmisc) # calcular quantis ponderados
# library(read.dbc)     # leitura de bases relacionais em Microsoft Access
# library(osmdata) # Download de dados do OpenStreeteMaps (OSM)


# library(hrbrthemes)
# library(leaflet.minicharts)
# extrafont::loadfonts(device="win")
#library(htmltools)
#library(htmlwidgets)
#library(tmap)

# Opções gerais após carregamento das bibliotecas
options(scipen=10000)
`%nin%` = Negate(`%in%`)
`%nlike%` = Negate(`%like%`)

# Use GForce Optimisations in data.table operations
# details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
options(datatable.optimize=Inf)

# set number of threads used in data.table
data.table::setDTthreads(percent = 100)

munis_list <- list(
  # Siglas de novas cidades além das originais do IPEA definidas com base em
  # https://informacoes.anatel.gov.br/legislacao/resolucoes/2005/403-resolucao-424
  munis_df = tribble(
    ~code_muni, ~abrev_muni, ~name_muni,        ~abrev_estado,  ~map_plot_ratio_wh,
    1302603,    "man",         "Manaus",                "AM",           1.27,
    2211001,    "tsa",         "Teresina",              "PI",           0.74, # 0.74 estimado
    2304400,    "for",         "Fortaleza",             "CE",           1.2,
    2408102,    "nat",         "Natal",                 "RN",           0.70,
    2507507,    "jpa",         "Joao Pessoa",           "PB",           0.74, # 0.74 estimado
    2611606,    "rec",         "Recife",                "PE",           0.68,
    2704302,    "mac",         "Maceio",                "AL",           0.74,
    2927408,    "sal",         "Salvador",              "BA",           1.36,
    3106200,    "bho",         "Belo Horizonte",        "MG",           0.69,
    3170206,    "ula",         "Uberlandia",            "MG",           0.74, # 0.74 estimado
    3205309,    "vta",         "Vitoria",               "ES",           0.74, # 0.74 estimado
    3304557,    "rio",         "Rio de Janeiro",        "RJ",           1.91,
    3509502,    "cam",         "Campinas",              "SP",           1.20,
    3534401,    "oco",         "Osasco",                "SP",           0.74, # 0.74 estimado
    3547809,    "sne",         "Santo Andre",           "SP",           0.74, # 0.74 estimado
    3549904,    "sjc",         "Sao Jose dos Campos",   "SP",           0.74, # 0.74 estimado
    3550308,    "spo",         "Sao Paulo",             "SP",           0.65,
    4106902,    "cur",         "Curitiba",              "PR",           0.62,
    4113700,    "lda",         "Londrina",              "PR",           0.74, # 0.74 estimado
    5002704,    "cgr",         "Campo Grande",          "MS",           0.87,
    5208707,    "goi",         "Goiania",               "GO",           0.93
#    1501402,    "bel",       "Belem",           "PA",           0.65,
#    2111300,    "slz",       "Sao Luis",        "MA",           0.78,
#    3301702,    "duq",       "Duque de Caxias", "RJ",           0.61,
#    3304904,    "sgo",       "Sao Goncalo",     "RJ",           1.21,
#    3518800,    "gua",       "Guarulhos",       "SP",           0.91,
#    4314902,    "poa",       "Porto Alegre",    "RS",           0.75,
#    5300108,    "bsb",       "Brasilia",        "DF",           1.71
  ) %>% setDT(),
  
  
  munis_modo = tribble(
    ~abrev_muni, ~`2017`,  ~`2018`,  ~`2019`,  ~`2020`,
    "for",       "todos",  "todos",  "todos",  "todos",
    "spo",       "todos",  "todos",  "todos",  "todos",
    "rio",       "ativo",  "todos",  "todos",  "todos",
    "cur",       "todos",  "todos",  "todos",  "todos",
    "poa",       "todos",  "todos",  "todos",  "todos",
    "bho",       "todos",  "todos",  "todos",  "todos",
    "bsb",       "ativo",  "ativo",  "ativo",  "ativo",
    "sal",       "ativo",  "ativo",  "todos",  "todos",
    "man",       "ativo",  "ativo",  "ativo",  "ativo",
    "rec",       "ativo",  "ativo",  "todos",  "todos",
    "goi",       "ativo",  "ativo",  "todos",  "todos",
    "bel",       "ativo",  "ativo",  "ativo",  "ativo",
    "gua",       "ativo",  "ativo",  "ativo",  "ativo",
    "cam",       "todos",  "todos",  "todos",  "todos",
    "slz",       "ativo",  "ativo",  "ativo",  "ativo",
    "sgo",       "ativo",  "ativo",  "ativo",  "ativo",
    "mac",       "ativo",  "ativo",  "ativo",  "ativo",
    "duq",       "ativo",  "ativo",  "ativo",  "ativo",
    "cgr",       "ativo",  "ativo",  "ativo",  "ativo",
    "nat",       "ativo",  "ativo",  "ativo",  "ativo",
# Novas cidades, por enquanto definidas como todos os modos
    "tsa",       "todos",  "todos",  "todos",  "todos",
    "jpa",       "todos",  "todos",  "todos",  "todos",
    "ula",       "todos",  "todos",  "todos",  "todos",
    "vta",       "todos",  "todos",  "todos",  "todos",
    "oco",       "todos",  "todos",  "todos",  "todos",
    "sne",       "todos",  "todos",  "todos",  "todos",
    "sjc",       "todos",  "todos",  "todos",  "todos",
    "lda",       "todos",  "todos",  "todos",  "todos",

  ) %>% 
    pivot_longer(cols = `2017`:`2020`, names_to = "ano_modo", values_to = "modo") %>% 
    setDT(),
  
  munis_metro = tribble(
    ~abrev_muni, ~ano_metro,  ~code_muni,
#    "bel",       2017,     1501402,
    "bho",       2017,     3106200,
#    "bsb",       2017,     5300108,
    "cam",       2017,     3509502,
    "cgr",       2017,     5002704,
    "cur",       2017,     4106902,
#    "duq",       2017,     3301702,
    "for",       2017,     2304400,
    "goi",       2017,     5208707,
#    "gua",       2017,     3518800,
    "mac",       2017,     2704302,
    "man",       2017,     1302603,
    "nat",       2017,     2408102,
#    "poa",       2017,     4314902,
    "rec",       2017,     2611606,
    "rio",       2017,     3304557,
    "sal",       2017,     2927408,
#    "sgo",       2017,     3304904,
#    "slz",       2017,     2111300,
    "spo",       2017,     3550308,
# Novas cidades, ano base 2017
    "tsa",       2017,     2211001,
    "jpa",       2017,     2507507,
    "ula",       2017,     3170206,
    "vta",       2017,     3205309,
    "oco",       2017,     3534401,
    "sne",       2017,     3547809,
    "sjc",       2017,     3549904,
    "lda",       2017,     4113700,

#    "bel",       2018,     1501402,
    "bho",       2018,     3106200,
#    "bsb",       2018,     5300108,
    "cam",       2018,     3509502,
    "cgr",       2018,     5002704,
    "cur",       2018,     4106902,
#    "duq",       2018,     3301702,
    "for",       2018,     2304400,
    "goi",       2018,     5208707,
#    "gua",       2018,     3518800,
    "mac",       2018,     2704302,
    "man",       2018,     1302603,
    "nat",       2018,     2408102,
#    "poa",       2018,     4314902,
    "rec",       2018,     2611606,
    "rio",       2018,     3304557,
    "sal",       2018,     2927408,
#    "sgo",       2018,     3304904,
#    "slz",       2018,     2111300,
    "spo",       2018,     3550308,
# Novas cidades, ano base 2018
    "tsa",       2018,     2211001,
    "jpa",       2018,     2507507,
    "ula",       2018,     3170206,
    "vta",       2018,     3205309,
    "oco",       2018,     3534401,
    "sne",       2018,     3547809,
    "sjc",       2018,     3549904,
    "lda",       2018,     4113700,

#    "bel",       2019,     1501402,
    "bho",       2019,     3106200,
#    "bsb",       2019,     5300108,
    "cam",       2019,     3509502,
    "cgr",       2019,     5002704,
    "cur",       2019,     4106902,
#    "duq",       2019,     3301702,
    "for",       2019,     2304400,
    "goi",       2019,     c(5208707,5200050,5201405,5201801,5203302,5203559,5203609,5204557,5205208,5208400,5208806,5209200,5209705,5214507,5215009,5219100,5219738,5220454,5221197,5221403),
#    "gua",       2019,     3518800,
    "mac",       2019,     2704302,
    "man",       2019,     1302603,
    "nat",       2019,     2408102,
#    "poa",       2019,     4314902,
    "rec",       2019,     2611606,
    "rio",       2019,     3304557,
    "sal",       2019,     2927408,
#    "sgo",       2019,     3304904,
#    "slz",       2019,     2111300,
    "spo",       2019,     3550308,
# Novas cidades, ano base 2019
    "tsa",       2019,     2211001,
    "jpa",       2019,     2507507,
    "ula",       2019,     3170206,
    "vta",       2019,     3205309,
    "oco",       2019,     3534401,
    "sne",       2019,     3547809,
    "sjc",       2019,     3549904,
    "lda",       2019,     4113700,

#    "bel",       2020,     1501402,
    "bho",       2020,     3106200,
#    "bsb",       2020,     5300108,
    "cam",       2020,     3509502,
    "cgr",       2020,     5002704,
    "cur",       2020,     4106902,
#    "duq",       2020,     3301702,
    "for",       2020,     2304400,
    "goi",       2020,     c(5208707,5200050,5201405,5201801,5203302,5203559,5203609,5204557,5205208,5208400,5208806,5209200,5209705,5214507,5215009,5219100,5219738,5220454,5221197,5221403),
#    "gua",       2020,     3518800,
    "mac",       2020,     2704302,
    "man",       2020,     1302603,
    "nat",       2020,     2408102,
#    "poa",       2020,     4314902,
    "rec",       2020,     2611606,
    "rio",       2020,     3304557,
    "sal",       2020,     2927408,
#    "sgo",       2020,     3304904,
#    "slz",       2020,     2111300,
    "spo",       2020,     3550308,
# Novas cidades, ano base 2020
    "tsa",       2020,     2211001,
    "jpa",       2020,     2507507,
    "ula",       2020,     3170206,
    "vta",       2020,     3205309,
    "oco",       2020,     3534401,
    "sne",       2020,     3547809,
    "sjc",       2020,     3549904,
    "lda",       2020,     4113700,
  ) %>% setDT()
  
  
) 


# para manaus
ylim = c(-353979.8550, -326309.6987)
xlim = c(-6696609.8722, -6658735.3079)


to_spatial <- function(df1, coordenada = c("lon", "lat")) {
  x <- st_as_sf(df1, coords = coordenada, crs = 4326)
}


rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}


#' Transforme um objeto `sf` de pontos para coordenadas lon e lat
#' `sfc_as_cols` retorna um data.frame com colunas de latitude e longitude a partir de um objeto `sf` 
#' Adaptado de https://github.com/r-spatial/sf/issues/231
#' 
#' @param x Um objeto `sf` com pontos do tipo 
#' @param names Um vetor com as colunas desejadas de output de longitude e latitute
sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))                                                                                                                                                                                                                                            
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  ui <- dplyr::bind_cols(x,ret)
  st_set_geometry(ui, NULL)
}


# Function to switch between git remotes in a project
change_remotes <- function(remote, branch = "master") {
  command <- sprintf("git branch --set-upstream-to=%s/%s", remote, branch)
  shell(command)
}


# ggplot themes
theme_aop_map <- function(base_size, ...) {
  # theme_void(base_family="Roboto Condensed") %+replace%
  theme_void() %+replace%
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      # plot.title = element_text(hjust = 0, vjust = 4),
      ...
    )
}


# create Natural Jenks function
jenks_natural <- function(data, var, breaks){
  # data <- copy(iris)
  # var <- "Petal.Length"
  # breaks <- 5
  # 
  # conver df to data.table  
  setDT(data)
  
  # name of new column
  newvar <- paste0(var,"_jenks")
  
  # calculate jenks natural breaks
  data[, paste0(newvar) := as.character(cut(get(var), breaks= getJenksBreaks(get(var), breaks), include.lowest = TRUE, dig.lab=3)) ]
  
  # Edit factor text
  data[, paste0(newvar) := str_replace_all(get(newvar), "\\[|\\(|\\]", "") ]
  data[, paste0(newvar) := stri_replace_all_regex(get(newvar), "[,]", " - ") ]
  
  # get factor labels
  jenks_labels  <- data[, get(newvar)]  %>% table %>% names() %>% sort(decreasing = F) 
  
  # recode variable
  data[, paste0(newvar) := factor(get(newvar), levels = jenks_labels)]
  
  return(data)
}
