# Definir fuso horario local
Sys.setenv(TZ = 'UTC')

# Carregar bibliotecas

library(tidyverse)
library(tidylog)        # opcional, mas útil - pode ser desligado quando performance for um problema
# Estes pacotes serão usados mas todos estão no Tidyverse
# https://www.tidyverse.org/packages/
# library(ggplot2)      # visualizacao de dados
# library(dplyr)
# library(tidyr)        # manipulacao de dados
# library(readr)        # rapida leitura de dados 
# library(purrr)
# library(stringr)      # operacoes em strings
# library(forcats)

# library(beepr)
# library(bit64)        # viz large numbers
# library(ceramic)
library(data.table)   # manipulacao de dados
# library(extrafont)    # fontes de texto
# library(fastDummies)  # para a criação de dummies
# library(fasttime)     # rapido processamento de dados em data/horario
# library(furrr)
library(future.apply) # Aplicar funcoes em paralelo
library(geobr)        # dados espaciais do brasil
# library(ggmap)        # geocoding
# library(gtfsio)        #
# library(ggthemes)     # temas para visualizacao de dados
library(h3jsr)        # h3 hex remotes::install_github("obrl-soil/h3jsr"), requer install.packages("V8")
library(Hmisc)
# library(hrbrthemes)   # requer hrbrthemes::import_roboto_condensed()
# library(httr)
# library(janitor)
# library(knitr)
# library(leafgl)
# library(lubridate)    # dados em data/horario
# library(maptools)
# library(mapview)      # visualizacao interativa dos dados
# library(patchwork)
# library(pbapply)      # progress bar
# library(quantreg)
# library(rmapshaper)
library(r5r)
library(raster)
# library(RColorBrewer) # paleta de cores
library(RCurl) # paleta de cores
# library(rgeos)
library(sf)           # leitura e manipulacao de dados espaciais
# library(tmap)
library(XML)

# Para usar o r5r é preciso instalar o Java SDK (ver https://ipeagit.github.io/r5r/);
# para o Debian 11, instalar os pacotes default-jdk, openjdk-11-jdk, default-jre e
# openjdk-11-jre

# Opções gerais após carregamento das bibliotecas
options(scipen = 10000)
`%nin%` = Negate(`%in%`)
`%nlike%` = Negate(`%like%`)

# Use GForce Optimisations in data.table operations
# details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
options(datatable.optimize = Inf)

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
    3106200,    "bho",         "Belo Horizonte",        "MG",           0.69,
    3170206,    "ula",         "Uberlandia",            "MG",           0.74, # 0.74 estimado
    3205309,    "vta",         "Vitoria",               "ES",           0.74, # 0.74 estimado
    3304557,    "rio",         "Rio de Janeiro",        "RJ",           1.91,
    3509502,    "cam",         "Campinas",              "SP",           1.20,
    3547809,    "sne",         "Santo Andre",           "SP",           0.74, # 0.74 estimado
    3549904,    "sjc",         "Sao Jose dos Campos",   "SP",           0.74, # 0.74 estimado
    3550308,    "spo",         "Sao Paulo",             "SP",           0.65,
    4106902,    "cur",         "Curitiba",              "PR",           0.62,
    5002704,    "cgr",         "Campo Grande",          "MS",           0.87,
    5208707,    "goi",         "Goiania",               "GO",           0.93
   # Algumas cidades e parâmetros, vindos do estudo original do IPEA - ver:
   # https://github.com/ipeaGIT/acesso_oport
   # 1501402,    "bel",       "Belem",           "PA",           0.65,
   # 2111300,    "slz",       "Sao Luis",        "MA",           0.78,
   # 3301702,    "duq",       "Duque de Caxias", "RJ",           0.61,
   # 3304904,    "sgo",       "Sao Goncalo",     "RJ",           1.21,
   # 3518800,    "gua",       "Guarulhos",       "SP",           0.91,
   # 4314902,    "poa",       "Porto Alegre",    "RS",           0.75,
   # 5300108,    "bsb",       "Brasilia",        "DF",           1.71
  ) %>% setDT(),
  

  munis_metro = tribble(
    ~abrev_muni, ~ano_metro,  ~code_muni,
    "bho",       2019,     3106200,
    "cam",       2019,     3509502,
    "cgr",       2019,     5002704,
    "cur",       2019,     4106902,
    "for",       2019,     2304400,
    "goi",       2019,     5208707,
    "jpa",       2019,     2507507,
    "man",       2019,     1302603,
    "nat",       2019,     2408102,
    "rec",       2019,     2611606,
    "rio",       2019,     3304557,
    "sjc",       2019,     3549904,
    "sne",       2019,     3547809,
    "spo",       2019,     3550308,
    "tsa",       2019,     2211001,
    "ula",       2019,     3170206,
    "vta",       2019,     3205309,
    # Algumas cidades e parâmetros, vindos do estudo original do IPEA - ver:
    # https://github.com/ipeaGIT/acesso_oport
   # "bel",       2019,     1501402,
   # "bsb",       2019,     5300108,
   # "duq",       2019,     3301702,
   # "gua",       2019,     3518800,
   # "mac",       2019,     2704302,
   # "poa",       2019,     4314902,
   # "sal",       2019,     2927408,
   # "sgo",       2019,     3304904,
   # "slz",       2019,     2111300,
  ) %>% setDT()
  
  
) 

# para manaus
ylim = c(-353979.8550, -326309.6987)
xlim = c(-6696609.8722, -6658735.3079)


# # ggplot themes
# theme_aop_map <- function(base_size, ...) {
#   # theme_void(base_family="Roboto Condensed") %+replace%
#   theme_void() %+replace%
#     theme(
#       legend.position = "bottom",
#       plot.margin = unit(c(2,0,0,0),"mm"),
#       legend.key.width = unit(2,"line"),
#       legend.key.height = unit(0.2,"cm"),
#       legend.text = element_text(size = rel(0.5)),
#       legend.title = element_text(size = rel(0.5)),
#       # plot.title = element_text(hjust = 0, vjust = 4),
#       ...
#     )
# }

