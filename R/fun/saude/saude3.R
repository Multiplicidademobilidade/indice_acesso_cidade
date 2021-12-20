
# Revisar o geocode dos estabelecimentos de saúde usando o gmaps

# Estrutura de pastas
ano = 2019 # Atualizar conforme o caso
files_folder <- "../../indice-mobilidade_dados"
subfolder6 <- sprintf("%s/06_cnes_saude/%s", files_folder, ano)

# Arquivo CNES saúde vindo do script anterior
path_saude <- sprintf("%s/saude_%s_filter_geocoded.rds", subfolder6, ano)
cnes_saude <- readr::read_rds(path_saude)

# Há vários estabelecimentos do CNES com as mesmas coordenadas latlong. O
# primeiro passo é identificar quais locais possuem mais de 3 estabelecimentos
# no mesmo ponto, separá-los e refazer o georreferenciamento via gmaps

# Criar id único para latlong
cnes_saude <- 
  cnes_saude %>% 
  # Criar novas colunas de latlong limitadas a 10 caracteres cada e coluna de id
  mutate(latitude   = str_sub(as.character(NU_LATITUDE), start = 1, end = 10),
         longitude  = str_sub(as.character(NU_LONGITUDE), start = 1, end = 10),
         latlong_id = sprintf('%s%s', latitude, longitude))


# Quais são os pontos em que há mais de três estabelecimentos? Estamos usando
# o threshold de 3 estabelecimentos porque existem locais que concentram
# mais de um tipo de atendimento (e, por consequência, de estebelecimento),
# mas acima de três começa a parecer estranho - há pontos que estão concentrando
# mais de 100 estabelecimentos
lats_repetidos_saude <- 
  cnes_saude %>% 
  group_by(latlong_id) %>% 
  tally() %>% 
  # Filtrar somente pontos com mais de 3 estabelecimentos e retirar pontos 
  # que não tinham latlong e estão como 'NANA'
  filter(n > 3 & latlong_id != 'NANA') %>% 
  arrange(-n)


# Separar em dois dataframes - os que estão com latlong ok e os que precisam
# ser revistos
cnes_saude_ok  <- 
  cnes_saude %>% 
  filter(latlong_id %nin% lats_repetidos_saude$latlong_id) %>% 
  # Criar identificador sobre se o geocode foi ou não revisto
  mutate(rev_latlong = 0)

cnes_saude_rev <- 
  cnes_saude %>% 
  filter(latlong_id %in% lats_repetidos_saude$latlong_id) %>% 
  # Apagar latitude e longitude dos estabelecimentos a serem revistos
  dplyr::select(-c(latitude, longitude)) %>% 
  # Criar identificador sobre se o geocode foi ou não revisto
  mutate(rev_latlong = 1)


# Incluir nome dos municípios em cnes_saude_rev de acordo com seus códigos ibge
munis <- 
  as.data.frame(munis_list$munis_df) %>% 
  mutate(IBGE = str_sub(code_muni, start = 0, end = 6)) %>% 
  dplyr::select(IBGE, name_muni) 

cnes_saude_rev <- cnes_saude_rev %>% left_join(munis, by = 'IBGE')


# Criar coluna de endereço para geocode
cnes_saude_rev <- 
  cnes_saude_rev %>%
  # Precisaremos de uma coluna temporária de número sem o 'S/N'
  mutate(numero = str_replace(NU_ENDERECO, 'S/N', ''),
         end_geocode = sprintf('%s, %s, %s, %s, %s', NO_LOGRADOURO, numero, NO_BAIRRO, name_muni, CO_CEP),
         end_geocode = str_replace(end_geocode, ', , ', ', ')) %>% 
  # Descartar colunas temporárias
  dplyr::select(-c(numero, name_muni))


# registrar Google API Key
my_api <- data.table::fread("../../api.txt", header = FALSE)
register_google(key = my_api$V1)

# Fazer o geocode usando o ggmap e a coluna 'end_geocode'
geocode_ggmap <- geocode(location = cnes_saude_rev$end_geocode, output = "more", source = "google")
# write_delim(geocode_ggmap, sprintf('%s/saude_%s_geocode_revisto_gmaps.csv', subfolder6, ano), delim = ';')
# geocode_ggmap <- read_delim(sprintf('%s/saude_%s_geocode_revisto_gmaps.csv', subfolder6, ano), delim = ';')
 

# Simplificar geocode_ggmap para juntar ao cnes_saude_rev
geocode_ggmap <- 
  geocode_ggmap %>% 
  dplyr::select(-c(north, south, east, west)) %>% 
  rename(latitude = lat,
         longitude = lon,
         gmaps_type = type,
         gmaps_loctype = loctype,
         gmaps_address = address)

# Juntar novo georreferenciamento ao cnes_saude_rev
cnes_saude_rev <- cnes_saude_rev %>% cbind(geocode_ggmap)

  

# Juntar tudo ao cnes_saude_ok para exportar
cnes_saude_out <- 
  cnes_saude_ok %>% 
  # Adicionar colunas que não existiam originalmente
  mutate(end_geocode = NA,
         gmaps_type = NA,
         gmaps_loctype = NA,
         gmaps_address = NA) %>% 
  rbind(cnes_saude_rev)

# Salvar resultados
write_rds(cnes_saude_out, sprintf('%s/saude_%s_filter_geocode_revisto_gmaps.rds', subfolder6, ano), compress = 'gz')

cnes_saude_out2 <- cnes_saude_out %>% mutate(indice_linha = 1:nrow(.))
write_delim(cnes_saude_out2, sprintf('%s/saude_%s_filter_geocode_revisto_gmaps.csv', subfolder6, ano), delim = ';')


