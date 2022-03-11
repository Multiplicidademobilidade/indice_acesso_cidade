# Funções para o cálculo do Indicador de Mobilidade

# Simplificar colunas dos dataframes a serem usados no cálculo
simplificar_colunas <- function(df, modo, acess_ideal = FALSE) {
  
  # Selecionar colunas de acessibilidade de acordo com o modo
  if (modo == 'onibus') {
    # Acessibilidades CMATT60, CMAST60, CMAET60
    matches_acessibilidade <- '^CMA[TSE]T60'
    
    # Garantir que cálculo para ônibus jamais vai ser por acessibilidade ideal
    acess_ideal <- FALSE
    
  } else if (modo == 'carro_compart' | modo == 'modos_ativos') {
    # Acessibilidades CMATT30, CMAST30, CMAET30
    matches_acessibilidade <- '^CMA[TSE]T30'
    
  }
  
  # Fazer a seleção de colunas - ônibus jamais vai ter a opção de seleção
  # por acesso ideal
  if (acess_ideal == FALSE) {
    df_out <- df %>%
      # Descartar linhas que não tiveram os tempos de viagem calculados - seja
      # porque não possuem população na origem, seja porque não possuem 
      # oportunidades no destino
      filter(!is.na(mode) & pop_total > 0) %>% 
      # Selecionar colunas de interesse
      dplyr::select('id_hex', 'sigla_muni',
                    # População por cor
                    'mode', 'cor_negra',
                    # Colunas de totais, exceto renda, matrículas e cras
                    matches('^[pes](.+)_total'),
                    # Acessibilidades CMATT, CMAST, CMAET
                    matches(matches_acessibilidade))
    
  } else { # Acessibilidade ideal
    df_out <- df %>%
      # Descartar linhas que não tiveram os tempos de viagem calculados - seja
      # porque não possuem população na origem, seja porque não possuem 
      # oportunidades no destino
      filter(!is.na(mode) & pop_total > 0) %>% 
      # Selecionar colunas de interesse
      dplyr::select('id_hex', mode, matches(matches_acessibilidade)) %>% 
      # Substituir valores 0 de CMATT, CMAST, CMAET para 1, para que cálculo
      # posterior não seja uma divisão por zero
      mutate(CMATT30 = ifelse(CMATT30 == 0, 1, CMATT30),
             CMAST30 = ifelse(CMAST30 == 0, 1, CMAST30),
             CMAET30 = ifelse(CMAET30 == 0, 1, CMAET30)) %>% 
      # Renomear variáveis para acessibilidade ideal
      rename(CMATT30_ideal = CMATT30,
             CMAST30_ideal = CMAST30,
             CMAET30_ideal = CMAET30)
    
  }
  
  return(df_out)
  
}


# Categoriza os hexágonos de acordo com a população total e a população negra
categorizar_populacao <- function(df) {
  
  # Categorizar a população dos hexágonos conforme seus quartis:
  # 1: hexágonos com população menor do que 25%
  # 2: hexágonos com população de 25% a menos de 50%
  # 3: hexágonos com população de 50% a menos de 75%
  # 4: hexágonos com população de 75% ou acima
  
  # Categorizar população negra dos hexágonos conforme os quartis de seus percentuais:
  # 1: hexágonos com percentual de população negra menor do que 25%
  # 2: hexágonos com percentual de população negra de 25% a menos de 50%
  # 3: hexágonos com percentual de população negra de 50% a menos de 75%
  # 4: hexágonos com percentual de população negra de 75% ou acima
  
  # Criar coluna com o percentual da população negra por hexágono
  df <- df %>% mutate(perc_pop_negra = cor_negra / pop_total)
  
  # Registrar os quantis das populações total e negra
  quantis_pop_total <- quantile(df$pop_total)
  quantis_pop_negra <- quantile(df$perc_pop_negra)
  
  # Executar as categorizações
  df <- df %>% 
    mutate(cat_pop_total = case_when(pop_total >= quantis_pop_total[4] ~ 4, # 75% ou mais
                                     pop_total >= quantis_pop_total[3] & pop_total < quantis_pop_total[4] ~ 3, 
                                     pop_total >= quantis_pop_total[2] & pop_total < quantis_pop_total[3] ~ 2,
                                     TRUE ~1),
           cat_pop_negra = case_when(perc_pop_negra >= quantis_pop_negra[4] ~ 4, # 75% ou mais
                                     perc_pop_negra >= quantis_pop_negra[3] & perc_pop_negra < quantis_pop_negra[4] ~ 3, 
                                     perc_pop_negra >= quantis_pop_negra[2] & perc_pop_negra < quantis_pop_negra[3] ~ 2,
                                     TRUE ~1))
  
  return(df)
}


# Calcula indicadores de mobilidade para trabalho, educação e saúde
calcular_indicadores_mobilidade <- function(df, modo) {
  
  # Cálculo para ônibus é a partir de hexágonos do entorno; demais modos
  # é a partir de uma acessibilidade ideal
  if (modo == 'onibus') {
    
    # Definir uma distância de 10 hexágonos de entorno para ônibus - 
    # resultado é lista de hexágonos de entorno para cada hexágono inicial
    viz_bus <- get_kring(h3_address = df$id_hex, ring_size = 10, simple = TRUE)
    
    # Definir entorno - A partir de cada hexágono, será estabelecido um
    # entorno, onde serão agrupados o total de oportunidades por tipo
    df$saud_entorno <- 0
    df$educ_entorno <- 0
    df$trab_entorno <- 0
    
    # Calcular quantidade de oportunidades no entorno de cada hexágono
    for (i in 1:nrow(df)) {
      
      df[i,]$saud_entorno <- sum(df[df$id_hex %in% viz_bus[[i]],]$saude_total)
      df[i,]$educ_entorno <- sum(df[df$id_hex %in% viz_bus[[i]],]$edu_total)
      df[i,]$trab_entorno <- sum(df[df$id_hex %in% viz_bus[[i]],]$empregos_total)
      
    }
    
    # Substituir valores 0 por 1 nas colunas de saud_entorno, educ_entorno e
    # trab_entorno - este passo é necessário devido à divisão a seguir
    df$saud_entorno <- ifelse(df$saud_entorno == 0, 1, df$saud_entorno)
    df$educ_entorno <- ifelse(df$educ_entorno == 0, 1, df$educ_entorno)
    df$trab_entorno <- ifelse(df$trab_entorno == 0, 1, df$trab_entorno)
    
    # Calcular a razão entre CMA para 60 minutos e quantidade de oportunidades no entorno
    df$educ_perc <- df$CMAET60 / df$educ_entorno
    df$saud_perc <- df$CMAST60 / df$saud_entorno
    df$trab_perc <- df$CMATT60 / df$trab_entorno
    
    
  } else { # Demais modos: carro compartilhado, modos ativos
    # Calcular primeiro as acessibilidades ponderadas
    df <- df %>% 
      # Calcular a razão entre CMA 30 minutos e quantidade de oportunidades 
      # ideal, calculada sem restrições
      mutate(educ_perc = CMAET30 / CMAET30_ideal,
             saud_perc = CMAST30 / CMAST30_ideal,
             trab_perc = CMATT30 / CMATT30_ideal)
  }
  
  
  # Se valor do cálculo anterior der maior do que 1, significa que todas as
  # oportunidades já estão sendo acessadas - transformar valor para 1
  df <- df %>% 
    mutate(educ_perc = ifelse(educ_perc > 1, 1, educ_perc),
           saud_perc = ifelse(saud_perc > 1, 1, saud_perc),
           trab_perc = ifelse(trab_perc > 1, 1, trab_perc)) %>% 
    # Calcular acessibilidades ponderadas de acordo com população negra
    mutate(acess_pond_educ = educ_perc * cat_pop_negra,
           acess_pond_saud = saud_perc * cat_pop_negra,
           acess_pond_trab = trab_perc * cat_pop_negra)
  
  # Calcular os indicadores de mobilidade
  im <- data.frame(im_educ = sum(df$acess_pond_educ) / sum(df$cat_pop_negra),
                   im_saud = sum(df$acess_pond_saud) / sum(df$cat_pop_negra),
                   im_trab = sum(df$acess_pond_trab) / sum(df$cat_pop_negra))
  
  
  # Renomear colunas de acordo com o modo
  if (modo == 'onibus') {
    names(im) <- names(im) %>% str_replace('im_', 'im_bus_')
  } else if (modo == 'carro_compart') {
    names(im) <- names(im) %>% str_replace('im_', 'im_car_')
  } else if (modo == 'a_pe') {
    names(im) <- names(im) %>% str_replace('im_', 'im_walk_')
  } else if (modo == 'bicicleta') {
    names(im) <- names(im) %>% str_replace('im_', 'im_bike_')
  }
  
  return(im)
  
}