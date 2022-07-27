## Script para o calculo do Indicador de Acesso às Oportunidades em prol da
## Redução de Desigualdades (IAOD)

# Indice de mobilidade
source('fun/setup.R')
source('fun/indice_acesso_oportunidades.R')


# Essa funcao calcula o indice de todas as cidades e organiza os resultados em um dataframe que sera salvo em .csv
indice_mobilidade_entorno <- function(muni_list, ano = 2019){
  # Essa função calcula uma série de indicadores intermediários que serão 
  # utilizados para compor o índice final. São eles:
  # IAOD - Índice final, que é uma média ponderada dos indicadores por modo de transporte
  
  # im_bus - indicador de acesso por ônibus, peso = 4
  # im_car - indicador de acesso por carro compartilhado peso = 1
  # im_walk - indicador de acesso a pé = peso = 2
  # im_bike - indicador de acesso por bicicleta = peso = 3
  
  # Cada um dos indicadores acima é formado por componentes que representam o acesso 
  # aos tres tipos de oportunidade, por exemplo:
  
  # im_bus_trab - ônibus > trabalho, peso = 5
  # im_bus_educ - ônibus > educacao, peso = 3
  # im_bus_saud - ônibus > saúde, peso = 2

  # Estrutura de pastas
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/%s", files_folder, ano)
  subfolder17 <- sprintf("%s/17_acesso_oportunidades/%s", files_folder, ano)


  # Criar um dataframe vazio, para ser povoado em seguida
  dados_indice <- data.frame(matrix(nrow = 0, ncol = 18))
  colnames(dados_indice) <- c('muni', 'im', 'im_bus', 'im_walk', 'im_bike', 'im_carbus',
                              'im_walk_educ', 'im_walk_saud', 'im_walk_trab',
                              'im_bike_educ', 'im_bike_saud', 'im_bike_trab',
                              'im_bus_educ',  'im_bus_saud',  'im_bus_trab',
                              'im_carbus_educ', 'im_carbus_saud', 'im_carbus_trab')


  # Cidades que não possuem dados de ônibus
  skip_bus <- c('jpa', 'tsa', 'vta')

  # Comecando
  for (muni in muni_list) {
    # Carregar e preparar as bases - usaremos as matrizes de dados agregados e de acessibilidade

    # Dados censitários de oportunidades agregados por hexágono (resolução 8, modos ativos)
    hex_agregados_8 <- sprintf('%s/hex_agregado_%s_08_2019.rds', subfolder14, muni)
    hex_agregados_8 <- read_rds(hex_agregados_8) %>% st_drop_geometry()

    # Juntar dados de acessibilidade - modos ativos (res 8) aos hexágonos
    oport_ativos <- read_rds(sprintf('%s/acess_08_%s_modos_ativos_2019.rds', subfolder17, muni))
    data_ativos <- left_join(hex_agregados_8, oport_ativos, by = c('id_hex' = 'origin'))

    # Juntar dados de acessibilidade ideal - modos ativos (res 8) aos hexágonos
    oport_ativos_ideal <- read_rds(sprintf('%s/acess_ideal_08_%s_modos_ativos_2019.rds', subfolder17, muni))
    data_ativos_ideal <- left_join(hex_agregados_8, oport_ativos_ideal, by = c('id_hex' = 'origin'))


    # Dados censitários de oportunidades agregados por hexágono (resolução 7, motorizados)
    hex_agregados_7 <- sprintf('%s/hex_agregado_%s_07_2019.rds', subfolder14, muni)
    hex_agregados_7 <- read_rds(hex_agregados_7) %>% st_drop_geometry()

    # Juntar dados de acessibilidade - carro compartilhado (res 7) aos hexágonos
    oport_carro <- read_rds(sprintf('%s/acess_07_%s_carro_compart_2019.rds', subfolder17, muni))
    data_carro  <- left_join(hex_agregados_7, oport_carro, by = c('id_hex' = 'origin')) %>% filter(!is.na(CMATT60))

    # # Juntar dados de acessibilidade ideal - carro compartilhado (res 7) aos hexágonos
    # oport_carro_ideal <- read_rds(sprintf('%s/acess_ideal_07_%s_carro_compart_2019.rds', subfolder17, muni))
    # data_carro_ideal  <- left_join(hex_agregados_7, oport_carro_ideal, by = c('id_hex' = 'origin'))

    # Juntar dados de acessibilidade - ônibus (res 7) aos hexágonos
    if (muni %nin% skip_bus) {
      oport_onibus <- read_rds(sprintf('%s/acess_07_%s_onibus_2019.rds', subfolder17, muni))
      data_bus     <- left_join(hex_agregados_7, oport_onibus, by = c('id_hex' = 'origin'))
      rm(oport_onibus)
    }

    # Limpar ambiente de trabalho
    rm(hex_agregados_8, oport_ativos, oport_ativos_ideal, hex_agregados_7, oport_carro) # oport_carro_ideal
    
    
    # Criar indicadores de acesso para ônibus e para a combinação carro 
    # compartilhado + ônibus, para cidades que têm dados de ônibus
    if (muni %nin% skip_bus) {
      # 1. Calcular para integração carro compartilhado + ônibus:
      # Selecionar colunas de interesse - como o cálculo será feito com 
      # o input dos dados de carro compartilhado, o data_carro entra aqui
      data_rh_bus <- simplificar_colunas(data_carro, modo = 'carro_onibus') # corrigido aqui
      
      # Categorizar os hexágonos de acordo população total e população negra
      data_rh_bus <- categorizar_populacao(data_rh_bus)
      
      # Calcular componentes de acesso para trabalho, educação e saúde
      im_carro_onibus <- calcular_indices_iaod(data_rh_bus, modo = 'carro_onibus')
      
      
      # 2. Calcular somente para ônibus:
      # Selecionar colunas de interesse
      data_bus <- simplificar_colunas(data_bus, modo = 'onibus')
      
      # Categorizar os hexágonos de acordo população total e população negra
      data_bus <- categorizar_populacao(data_bus)
      
      # Calcular componentes de acesso para trabalho, educação e saúde
      im_onibus <- calcular_indices_iaod(data_bus, modo = 'onibus')
      
    } else { 
      # Para as cidades sem dados de onibus
      im_carro_onibus <- data.frame(im_carbus_educ = 0,
                                    im_carbus_saud = 0,
                                    im_carbus_trab = 0)
      
      im_onibus <- data.frame(im_bus_educ = 0,
                              im_bus_saud = 0,
                              im_bus_trab = 0)
      
    }
    
    
    # Criar indicadores de acesso para carro compartilhado (cálculo substituído
    # pela integração carro + ônibus)
    
    # # Selecionar colunas de interesse - carro compartilhado
    # data_carro <- simplificar_colunas(data_carro, modo = 'carro_compart')
    # 
    # # Selecionar colunas de interesse - carro compartilhado (acessibilidade ideal)
    # data_carro_ideal <- simplificar_colunas(data_carro_ideal, modo = 'carro_compart', acess_ideal = TRUE)
    # 
    # # Juntar acessibilidades (calculada e ideal) no mesmo dataframe
    # data_carro <- left_join(data_carro, data_carro_ideal, by = 'id_hex')
    # rm(data_carro_ideal)
    # 
    # # Categorizar os hexágonos de acordo população total e população negra
    # data_carro <- categorizar_populacao(data_carro)
    # 
    # # Calcular componentes de acesso para trabalho, educação e saúde
    # im_carro <- calcular_indices_iaod(data_carro, modo = 'carro_compart')
    
    
    # Criar indicadores de acesso para modos ativos
     
    # Selecionar colunas de interesse - modos ativos
    data_ativos <- simplificar_colunas(data_ativos, modo = 'modos_ativos')
    
    # Selecionar colunas de interesse - modos ativos (acessibilidade ideal)
    data_ativos_ideal <- simplificar_colunas(data_ativos_ideal, modo = 'modos_ativos', acess_ideal = TRUE) 
    
    
    # Separar mobilidade a pé
    data_ape       <- data_ativos %>% filter(mode == 'walk')
    data_ape_ideal <- data_ativos_ideal %>% filter(mode == 'walk')
    
    # Juntar acessibilidades (calculada e ideal) no mesmo dataframe
    data_ape <- left_join(data_ape, data_ape_ideal, by = 'id_hex')
    rm(data_ape_ideal)
    
    # Categorizar os hexágonos de acordo população total e população negra
    data_ape <- categorizar_populacao(data_ape)
    
    # Calcular componentes de acesso para trabalho, educação e saúde
    im_ape <- calcular_indices_iaod(data_ape, modo = 'a_pe')
    
    
    # Separar mobilidade por bicicletas
    data_bici       <- data_ativos %>% filter(mode == 'bike')
    data_bici_ideal <- data_ativos_ideal %>% filter(mode == 'bike')
    
    # Juntar acessibilidades (calculada e ideal) no mesmo dataframe
    data_bici <- left_join(data_bici, data_bici_ideal, by = 'id_hex')
    rm(data_bici_ideal)
    
    # Categorizar os hexágonos de acordo população total e população negra
    data_bici <- categorizar_populacao(data_bici)
    
    # Calcular componentes de acesso para trabalho, educação e saúde
    im_bici <- calcular_indices_iaod(data_bici, modo = 'bicicleta')
    
    
    
    # Indicadores de acesso por modo de transporte
    
    # Considerando a Política Nacional de Mobilidade Urbana = PNMU:
    # Onibus: 60 minutos; Res 7; Entorno = 10 hex;
    # Carro:  30 minutos; Res 7;
    # Bike:   30 minutos; Res 8;
    # A pé:   30 minutos; Res 8
    
    # Ônibus
    im_onibus <- 
      im_onibus %>% 
      mutate(im_bus   = (im_bus_educ * 3 + im_bus_saud * 2 + im_bus_trab * 5) / 10,
             peso_bus = 4)
    
    # Combinação Carro compartilhado + Ônibus
    im_carro_onibus <- 
      im_carro_onibus %>% 
      mutate(im_carbus   = (im_carbus_educ * 3 + im_carbus_saud * 2 + im_carbus_trab * 5) / 10,
             peso_carbus = 1)
    
    # Carro compartilhado
    # im_carro <-
    #   im_carro %>% 
    #   mutate(im_car   = (im_car_educ * 3 + im_car_saud * 2 + im_car_trab * 5) / 10,
    #          peso_car = 1)
    
    # Mobilidade a pé
    im_ape <- 
      im_ape %>% 
      mutate(im_walk   = (im_walk_educ * 3 + im_walk_saud * 2 + im_walk_trab * 5) / 10,
             peso_walk = 2)
    
    # Bicicleta
    im_bici <- 
      im_bici %>% 
      mutate(im_bike   = (im_bike_educ * 3 + im_bike_saud * 2 + im_bike_trab * 5) / 10,
             peso_bike = 3)
    
    
    # Juntar todos os indicadores em um único dataframe
    im <- cbind(im_onibus, im_carro_onibus, im_ape, im_bici) # im_carro
    
    # Calcular índice de acesso às oportunidades (IAOD) consolidado
    # Considerando o carro integrado ao onibus
    im <- im %>% 
      mutate(
             #im_educ = (peso_bus*im_bus_educ + peso_walk*im_walk_educ + peso_bike*im_bike_educ + peso_car*im_carbus_educ) / (peso_bus + peso_walk + peso_bike + peso_carbus),
             #im_saud = (peso_bus*im_bus_saud + peso_walk*im_walk_saud + peso_bike*im_bike_saud + peso_car*im_carbus_saud) / (peso_bus + peso_walk + peso_bike + peso_carbus),
             #im_trab = (peso_bus*im_bus_trab + peso_walk*im_walk_trab + peso_bike*im_bike_trab + peso_car*im_carbus_trab) / (peso_bus + peso_walk + peso_bike + peso_carbus),
             iaod = (peso_bus*im_bus + peso_walk*im_walk + peso_bike*im_bike + peso_carbus*im_carbus) / (peso_bus + peso_bike + peso_walk + peso_carbus),
             muni = muni
             )
    
    # Reordenar e selecionar colunas
    im <- im %>% dplyr::select(muni, iaod, im_bus, im_walk, im_bike, im_carbus,
                               im_walk_educ, im_walk_saud, im_walk_trab,
                               im_bike_educ, im_bike_saud, im_bike_trab,
                               im_bus_educ,  im_bus_saud,  im_bus_trab,
                               im_carbus_educ,  im_carbus_saud,  im_carbus_trab)
    
    
    # # Exibir resultados na tela
    # print("=========================================")
    # print(paste("Cálculo do IAOD da cidade: ", muni))
    # print(paste("Acessibilidade - ônibus: ", im$im_bus))
    # print(paste("Acessibilidade - a pé:   ", im$im_walk))
    # print(paste("Acessibilidade - bici:   ", im$im_bike))
    # print(paste("Acessibilidade - carro_c:", im$im_car))
    # print(paste("Índice de mobilidade:    ", im$indice_mobilidade))
    
    
    # Anexar dados ao dataframe que junta todos os indicadores de todas as cidades
    dados_indice <- rbind(dados_indice, im)
    
  }
  
  # Retorna o dataframe
  return(dados_indice)
  
}

# Rodar função de cálculo dos índices de mobilidade para todos os municípios
ano = 2019
indices_acesso_oportunidades <- lapply(munis_list$munis_metro[ano_metro == ano]$abrev_muni,
                                       indice_mobilidade_entorno, ano = ano) %>% 
                                rbindlist()


# Estrutura de pastas para salvar o arquivo
files_folder <- "../../indice_acesso_cidade_dados"
subfolder18 <- sprintf('%s/18_indices/%s', files_folder, ano)
dir.create(subfolder19, recursive = TRUE, showWarnings = FALSE)

# Salvar resultados
out_file <- sprintf('%s/IAOD_%s.csv', subfolder18, ano)
write_delim(indices_acesso_oportunidades, out_file, delim = ';')
