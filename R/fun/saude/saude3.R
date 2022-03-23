
# Revisar o geocode dos estabelecimentos de saúde usando o gmaps

# Estrutura de pastas
ano = 2019 # Atualizar conforme o caso
files_folder <- "../../indice-mobilidade_dados"
subfolder6 <- sprintf("%s/06_cnes_saude/%s", files_folder, ano)

# Arquivo CNES saúde vindo do script anterior
path_saude <- sprintf("%s/saude_%s_filter_geocoded.rds", subfolder6, ano)
cnes_saude <- readr::read_rds(path_saude)

# Retirar unidades móveis da base de dados
cnes_saude <- cnes_saude %>% filter(!str_detect(NO_FANTASIA, ' M[OÓ]VEL'))

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

# ----------------------------------------
# Primeiro processo: Gmaps
# ----------------------------------------

# registrar Google API Key
my_api <- data.table::fread("../../api.txt", header = FALSE)
register_google(key = my_api$V1)

# Fazer o geocode usando o ggmap e a coluna 'end_geocode'
geocode_ggmap <- geocode(location = cnes_saude_rev$end_geocode, output = "more", source = "google")
# write_delim(geocode_ggmap, sprintf('%s/saude_%s_geocode_revisto_gmaps.csv', subfolder6, ano), delim = ';')
geocode_ggmap <- read_delim(sprintf('%s/saude_%s_geocode_revisto_gmaps.csv', subfolder6, ano), delim = ';')
 

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

# cnes_saude_out2 <- cnes_saude_out %>% mutate(indice_linha = 1:nrow(.))
write_delim(cnes_saude_out2, sprintf('%s/saude_%s_filter_geocode_revisto_gmaps.csv', subfolder6, ano), delim = ';')

# ----------------------------------------
# Segundo processo: Gmaps após rechecagem
# ----------------------------------------

# Após o primeiro georreferenciamento, vamos fazer uma checagem dos endereços
# resultantes (reverse geocode) com os originais e o que não bater será
# georreferenciado novamente somente pelo CEP
open_file = sprintf('%s/saude_%s_filter_geocode_revisto_gmaps.rds', subfolder6, ano)
cnes_saude_rev2 <- read_rds(open_file)

# Separar linhas onde o georreferenciamento foi refeito
cnes_saude_ok2 <- cnes_saude_rev2 %>% filter(rev_latlong == 0)
cnes_saude_rev2 <- cnes_saude_rev2 %>% filter(rev_latlong == 1)
# nrow(cnes_saude_ok2)+nrow(cnes_saude_rev2) == nrow(cnes_saude)

# cnes_check são os endereços georreferenciados via gmaps com tratamento 
# de colunas para checagem da qualidade do geocode
cnes_check <- 
  cnes_saude_rev2 %>%
  # Adicionar info de municipios
  left_join(munis, by = 'IBGE') %>% 
  # select(IBGE, latlong_id, NO_LOGRADOURO, NU_ENDERECO, name_muni, CO_CEP, gmaps_address) %>%     
  mutate(
    # Criar coluna de CEP do endereço encontrado pelo Google
    gmaps_CEP = str_replace(gmaps_address, '(.+, ){2,}(\\d{5})(-)?(\\d{3})?(,.+)', '\\2\\4'),
    # Criar coluna do logradouro
    gmaps_log = str_replace(gmaps_address, '(.+){1}, (\\d+)? - (.+, ){1}(.+){1} - (.+, ){1}(\\d{5})(-)?(\\d{3})?(,.+)', '\\1'),
    gmaps_log = toupper(rm_accent(gmaps_log)),
    gmaps_log = str_replace(gmaps_log, '^AV\\.', 'AV'),
    gmaps_log = str_replace(gmaps_log, '^AV ', 'AVENIDA '),
    gmaps_log = str_replace(gmaps_log, '^R\\.', 'RUA'),
    gmaps_log = str_replace(gmaps_log, '^TV\\.', 'TRAVESSA'),
    gmaps_log = str_replace(gmaps_log, 'DR\\.', 'DR'),
    gmaps_log = str_replace(gmaps_log, 'MAL\\.', 'MARECHAL'),
    gmaps_log = str_replace(gmaps_log, 'SR\\.', 'SENHOR'),
    # Ajeitar coluna de NO_LOGRADOURO
    logradouro_rev = str_replace(NO_LOGRADOURO, '^AV\\.', 'AV'),
    logradouro_rev = str_replace(NO_LOGRADOURO, '^AV ', 'AVENIDA '),
    # Criar coluna do numeral
    gmaps_num = str_replace(gmaps_address, '(.+){1}, (\\d+)? - (.+, ){1}(.+){1} - (.+, ){1}(\\d{5})(-)?(\\d{3})?(,.+)', '\\2'),
    # Criar coluna da cidade
    gmaps_cid = str_replace(gmaps_address, '(.+){1}, (\\d+)? - (.+, ){1}(.+){1} - (.+, ){1}(\\d{5})(-)?(\\d{3})?(,.+)', '\\4'),
    # Ajeitar coluna de cidade para se assemelhar à name_muni
    gmaps_cid = toupper(rm_accent(gmaps_cid)),
    # Ajeitar coluna name_muni
    name_muni = toupper(name_muni),
    
    # Para endereços sem número, precisamos de uma nova regex para isolar o logradouro...
    gmaps_log2 = str_replace(gmaps_log, '(.+){1} - (.+)', '\\1'),
    # ... para isolar a cidade...
    gmaps_cid2 = str_replace(gmaps_log2, '(.+){1}, (.+)', '\\2'),
    # ... e finalmente ajeitar o logradouro
    gmaps_log2 = str_replace(gmaps_log2, '(.+){1}, (.+)', '\\1'),
    gmaps_log2 = str_replace(gmaps_log2, '(.+){1} - (.+)', '\\1'),
  )

# Fazer marcações sobre a qualidade do georreferenciamento
# Marcação 1: Onde gmaps_address for NA, marcar como rever
cnes_check <- cnes_check %>% mutate(rever_geocode = case_when(is.na(gmaps_address) ~ 'sim', TRUE ~ 'não'))

# Marcação 2: Onde CEPs forem diferentes (original x geocode), marcar como rever
cnes_check <- cnes_check %>% mutate(rever_geocode = case_when(gmaps_CEP != CO_CEP ~ 'sim', 
                                                              TRUE ~ rever_geocode))

# Marcação 3: Mesmo que os CEPs sejam diferentes, se o endereço, número 
# e cidade forem iguais, marcar que não precisa rever
cnes_check <- 
  cnes_check %>% 
  mutate(rever_geocode = case_when(logradouro_rev == gmaps_log &
                                     NU_ENDERECO == gmaps_num &
                                     name_muni == gmaps_cid
                                   ~ 'não',
                                   TRUE ~ rever_geocode))

# Marcação 4: Para endereços que já não possuíam numeral, se 
# endereço e cidade forem iguais, marcar que não precisa rever
cnes_check <- 
  cnes_check %>% 
  mutate(rever_geocode = case_when(logradouro_rev == gmaps_log2 &
                                     NU_ENDERECO == 'S/N' &
                                     name_muni == gmaps_cid2
                                   ~ 'não',
                                   TRUE ~ rever_geocode))

# Marcação 5: Se CEP for NA, não adianta fazer novo georreferenciamento
cnes_check <- 
  cnes_check %>% 
  mutate(rever_geocode = case_when(is.na(CO_CEP) ~ 'não',
                                   TRUE ~ rever_geocode))

# # Quantos endereços precisam ser novamente georreferenciados?
# cnes_check %>% group_by(rever_geocode) %>% tally()
 
# Retirar colunas temporárias criadas
cnes_check <- 
  cnes_check %>% 
  dplyr::select(-c(name_muni, logradouro_rev, gmaps_CEP, gmaps_log, gmaps_num, 
                   gmaps_cid, gmaps_log2, gmaps_cid2))

# Dividir os dataframes entre o que precisa ser georreferenciado novamente e
# o que não precisa. Com isso, teremos 3 dataframes junto com o cnes_saude_ok2
cnes_saude_ok3 <- cnes_check %>% filter(rever_geocode == 'não') %>% dplyr::select(-rever_geocode)
cnes_saude_rev2 <- cnes_check %>% filter(rever_geocode == 'sim')

# Fazer o geocode usando o ggmap e a coluna 'CO_CEP'
geocode_ggmap2 <- geocode(location = cnes_saude_rev2$CO_CEP, output = "more", source = "google")
# write_delim(geocode_ggmap, sprintf('%s/saude_%s_geocode_revisto_gmaps.csv', subfolder6, ano), delim = ';')
# geocode_ggmap <- read_delim(sprintf('%s/saude_%s_geocode_revisto_gmaps.csv', subfolder6, ano), delim = ';')

# Simplificar geocode_ggmap para juntar ao cnes_saude_rev
geocode_ggmap2 <- 
  geocode_ggmap2 %>% 
  dplyr::select(-c(north, south, east, west)) %>% 
  rename(latitude2 = lat,
         longitude2 = lon,
         gmaps_type2 = type,
         gmaps_loctype2 = loctype,
         gmaps_address2 = address)

# Juntar novo georreferenciamento ao cnes_saude_rev
cnes_saude_rev2 <- cnes_saude_rev2 %>% cbind(geocode_ggmap2)

# Atualizar colunas de georreferenciamento: se o segundo processo
# não resultou em um endereço válido, vamos usar o do primeiro
cnes_saude_rev2 <- 
  cnes_saude_rev2 %>% 
  mutate(latitude = as.double(latitude),
         longitude = as.double(longitude),
         gmaps_type = as.character(gmaps_type),
         gmaps_loctype = as.character(gmaps_loctype),
         latitude2 = as.double(latitude2),
         longitude2 = as.double(longitude2),
         gmaps_type2 = as.character(gmaps_type2),
         gmaps_loctype2 = as.character(gmaps_loctype2),
         ) %>% 
  # Onde não houver dados novos (is.na(latitude2)), manter os atuais; se houver, atualizar
  mutate(latitude  = case_when(is.na(latitude2) ~ latitude,  TRUE ~ latitude2),
         longitude = case_when(is.na(latitude2) ~ longitude, TRUE ~ longitude2),
         gmaps_type    = case_when(is.na(latitude2) ~ gmaps_type,    TRUE ~ gmaps_type2),
         gmaps_loctype = case_when(is.na(latitude2) ~ gmaps_loctype, TRUE ~ gmaps_loctype2),
         gmaps_address = case_when(is.na(latitude2) ~ gmaps_address, TRUE ~ gmaps_address2))


# Retirar colunas temporárias criadas
cnes_saude_rev2 <- 
  cnes_saude_rev2 %>% 
  dplyr::select(-c(rever_geocode, latitude2, longitude2, gmaps_type2, 
                   gmaps_loctype2, gmaps_address2))


# Juntar tudo ao cnes_saude_ok2 para exportar
cnes_saude_out2 <-
  cnes_saude_ok2 %>%
  rbind(cnes_saude_ok3) %>% 
  rbind(cnes_saude_rev2)


# Salvar resultados
write_rds(cnes_saude_out2, sprintf('%s/saude_%s_filter_geocode_revisto_gmaps_duas_etapas.rds', subfolder6, ano), compress = 'gz')

# cnes_saude_out2 <- cnes_saude_out %>% mutate(indice_linha = 1:nrow(.))
write_delim(cnes_saude_out2, sprintf('%s/saude_%s_filter_geocode_revisto_gmaps_duas_etapas.csv', subfolder6, ano), delim = ';')
