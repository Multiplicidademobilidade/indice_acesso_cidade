# Esse arquivo foi criado para corrigir o shapefile da cidade de Vitória

# Primeiramente, foi baixado um shape contendo os limites municipais do estado do Espirito Santo
# Fonte: http://www.ijsn.es.gov.br/mapas/
# No QGIS a cidade de vitória foi isolada e o arquivo salvo num formato shp, que será carregado aqui

# Setup
source("fun/setup.R")
source("03.1-criar_hexagonos.R") # Comentei a última linha que executa a função no arquivo original

# TESTANDO A FONTE ORIGINAL (igual ao arquivo .rds guardado em 01_municipios/2019)
muni <- read_municipality(code_muni = 3205309, year=2010)
# Ao plotar verificamos que existem dois elementos que distorcem o mapa a oeste
ggplot() +
  geom_sf(data=muni, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE)

# Pasta onde os arquivos foram tratatos
load_folder <- "../../indice-mobilidade_dados/01_municipios/vta"
out_file1 <- "municipio_vta_2019.rds"

# Arquivo shp
vitoria <- read_sf(dsn = load_folder, layer="vitoria")
# Transformar em WGS84
vitoria <- st_transform(vitoria, 4326)
# Preparar colunas
vitoria <- 
  vitoria %>% 
  mutate(code_state = str_sub(.$cod_ibge, start = 1, end = 2), .after = 'cod_ibge')%>%
  dplyr::select(code_state, geometry)

# salvar municipio
readr::write_rds(vitoria, sprintf("%s/%s", load_folder, out_file1), compress = 'gz')


# Gerar hexágonos

# Primeiro é preciso substituir o arquivo .rds gerado aqui na pasta 01/municipios

criar_hexagonos(ano = 2019, munis = 'vta')

# Carregando na resolução 08
vta_hex <- read_rds("../../indice-mobilidade_dados/09_hex_municipios/2019/hex_vta_2019_08.rds")

# Plotando mapas
# Shape
ggplot() +
  geom_sf(data=vitoria, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE)

# Hexágonos
ggplot() +
  geom_sf(data=vta_hex, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE)

