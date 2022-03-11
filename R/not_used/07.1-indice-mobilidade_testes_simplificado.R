## Script para o calculo do Indice de Mobilidade (IM)

# Indice de mobilidade
source('fun/setup.R')
library(scales)

indice_mobilidade_entorno <- function(muni_list, ano=2019, pop="pop_total"){
  # Essa funcao calcula o indice de todas as cidades e organiza os resultados em um dataframe que sera salvo em .csv
  
  # Essa funcao calcula uma serie de indices intermediarios que serao utilizados para compor o indice final, sao eles:
  # IM - Indice de mobilidade final, que e uma media ponderada dos indices por modo de transporte
  
  # IM_bus - onibus
  # IM_car - automovel compartilhado
  # IM_walk - a pe
  # IM_bike - bicicleta
  
  # Cada um dos indices acima e resultado da media aritmetica dos Indices por tipo de opotunidade, por exemplo:
  # IM_bus_edu - onibus > educacao
  # IM_bus_saude - onibus > saude
  # IM_bus_trab - onibus > trabalho
  
  
  # 0.0 Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/%s", files_folder, ano)
  subfolder17 <- sprintf("%s/17_acesso_oportunidades/%s", files_folder, ano)
  save_folder <- sprintf('%s/19_indice_mobilidade/%s', files_folder, ano)
  
  # Cidades que nao possuem dados de onibus
  skip_bus <- c('jpa', 'tsa', 'vta')
  
  # 1.0 Carrega e prepara as bases
  # Usaremos a matriz de dados agregados e a matriz de acessibilidade
  
  dados_indice <- data.frame(matrix(nrow = 0, ncol = 21))
  colnames(dados_indice) <- c('municipio','IM', #'IM_edu', 'IM_saude', 'IM_trab',
                              'IM_bus', 'IM_walk', 'IM_bike', 'IM_car',
                              'IM_bus_edu', 'IM_bus_saude', 'IM_bus_trab',
                              'IM_walk_edu', 'IM_walk_saude', 'IM_walk_trab',
                              'IM_bike_edu', 'IM_bike_saude', 'IM_bike_trab', 
                              'IM_car_edu', 'IM_car_saude', 'IM_car_trab'
                              )
  # Comecando
  for (muni in muni_list){
    munic <- muni
    # Bus (res = 7)
    file1 <- readRDS(sprintf('%s/hex_agregado_%s_07_2019.rds', subfolder14, muni))%>%
      st_drop_geometry()
    
    if (muni %nin% skip_bus){
      file2 <- readRDS(sprintf('%s/acess_07_%s_onibus_2019.rds', subfolder17, muni))
      data_bus <- left_join(file1, file2, by = c('id_hex' = 'origin'))
    }
    
    # Ativos (res = 8)
    file3 <- readRDS(sprintf('%s/hex_agregado_%s_08_2019.rds', subfolder14, muni))%>%
      st_drop_geometry()
    file4 <- readRDS(sprintf('%s/acess_08_%s_modos_ativos_2019.rds', subfolder17, muni))
    file4_i <- readRDS(sprintf('%s/acess_ideal_08_%s_modos_ativos_2019.rds', subfolder17, muni))
    data_ativos <- left_join(file3, file4, by = c('id_hex' = 'origin'))
    data_ativos_i <- left_join(file3, file4_i, by = c('id_hex' = 'origin'))
    
    # Carro compartilhado (res = 7)
    file5 <- readRDS(sprintf('%s/acess_07_%s_carro_compart_2019.rds', subfolder17, muni))
    file5_i <- readRDS(sprintf('%s/acess_ideal_07_%s_carro_compart_2019.rds', subfolder17, muni))
    data_carro <- left_join(file1, file5, by = c('id_hex' = 'origin'))
    data_carro_i <- left_join(file1, file5_i, by = c('id_hex' = 'origin'))
    
    # 3.0 Selecao de colunas relevantes
    # 3.1 Onibus
    if (muni %nin% skip_bus){
      data_bus <- data_bus%>%
        drop_na(any_of('mode'))%>%
        dplyr::select('id_hex', 'sigla_muni', 'cor_branca', 'cor_amarela','cor_indigena','cor_negra', 'pop_total',
                      'empregos_total', 'saude_total', 'edu_total', 'mode', 'CMATT15', 'CMATT30', 'CMATT45', 'CMATT60',
                      'CMAST15', 'CMAST30', 'CMAST45', 'CMAST60', 'CMAET15', 'CMAET30', 'CMAET45', 'CMAET60')%>%
        dplyr::filter(pop_total>0)
      
      # Agrega populacoes 
      # CORRIGIR NOMENCLATURA
      data_bus$pop_total_q <- ifelse(data_bus$pop_total >= quantile(data_bus$pop_total, .75), 4,
                                     ifelse(data_bus$pop_total >= quantile(data_bus$pop_total, 0.5), 3, 
                                            ifelse(data_bus$pop_total >= quantile(data_bus$pop_total, 0.25), 2,1)))
      
      data_bus$perc_negra <- data_bus$cor_negra/data_bus$pop_total
      data_bus$perc_negra_q <- ifelse(data_bus$perc_negra >= quantile(data_bus$perc_negra, .75), 4,
                                     ifelse(data_bus$perc_negra >= quantile(data_bus$perc_negra, 0.5), 3, 
                                            ifelse(data_bus$perc_negra >= quantile(data_bus$perc_negra, 0.25), 2,1)))
      
      data_bus$cor_negra_q <- ifelse(data_bus$cor_negra >= quantile(data_bus$cor_negra, .75), 4,
                                        ifelse(data_bus$cor_negra >= quantile(data_bus$cor_negra, 0.5), 3, 
                                               ifelse(data_bus$cor_negra >= quantile(data_bus$cor_negra, 0.25), 2,1)))
      
      # Definicao do entorno
      # A partir de cada hexagono sera estabelecido um entorno, onde serao agrupados o total de oportunidades por tipo
      data_bus$saude_entorno <- 0
      data_bus$edu_entorno <- 0
      data_bus$trab_entorno <- 0
    
      viz_bus <- get_kring(h3_address = data_bus$id_hex, ring_size = 10, simple = TRUE) # Quanto sera a distancia do carro?
      for(i in 1:nrow(data_bus)){
        
        data_bus[i,]$saude_entorno <- sum(data_bus[data_bus$id_hex %in% viz_bus[[i]],]$saude_total)
        data_bus[i,]$edu_entorno <- sum(data_bus[data_bus$id_hex %in% viz_bus[[i]],]$edu_total)
        data_bus[i,]$trab_entorno <- sum(data_bus[data_bus$id_hex %in% viz_bus[[i]],]$empregos_total)
        
#        if(data_bus[i,]$edu_entorno == 0){
#          #data_bus[i,]$edu_entorno <- data_bus[i,]$CMAET30+1
#        }
#        if(data_bus[i,]$saude_entorno == 0){
#          #data_bus[i,]$saude_entorno <- data_bus[i,]$CMAST30+1
#        }
#        if(data_bus[i,]$trab_entorno == 0){
#          #data_bus[i,]$trab_entorno <- data_bus[i,]$CMATT30+1
#        }
      }
      
      data_bus$saude_entorno <- ifelse(data_bus$saude_entorno == 0, 1, data_bus$saude_entorno)
      data_bus$edu_entorno <- ifelse(data_bus$edu_entorno == 0, 1, data_bus$edu_entorno)
      data_bus$trab_entorno <- ifelse(data_bus$trab_entorno == 0, 1, data_bus$trab_entorno)
      
      # Calculo do razao CMA/Oportunidades no entorno
      data_bus$E_perc <- data_bus$CMAET60/data_bus$edu_entorno
      data_bus$S_perc <- data_bus$CMAST60/data_bus$saude_entorno
      data_bus$T_perc <- data_bus$CMATT60/data_bus$trab_entorno
      
      data_bus$E_perc <- ifelse(data_bus$E_perc>1, 1, data_bus$E_perc)
      data_bus$S_perc <- ifelse(data_bus$S_perc>1, 1, data_bus$S_perc)
      data_bus$T_perc <- ifelse(data_bus$T_perc>1, 1, data_bus$T_perc)

     # 4.0 Calculo do IM
      # Onibus
      if (pop == "pop_total"){
        data_bus$A_edu <-(data_bus$E_perc)*data_bus$pop_total_q
        data_bus$A_saude <-(data_bus$S_perc)*data_bus$pop_total_q
        data_bus$A_trab <-(data_bus$T_perc)*data_bus$pop_total_q
        
        im_bus_edu <- sum(data_bus$A_edu)/sum(data_bus$pop_total_q)
        im_bus_saude <- sum(data_bus$A_saude)/sum(data_bus$pop_total_q)
        im_bus_trab <- sum(data_bus$A_trab)/sum(data_bus$pop_total_q)
      
      }else if(pop == "media"){
        im_bus_edu <- mean(data_bus$E_perc)
        im_bus_saude <- mean(data_bus$S_perc)
        im_bus_trab <- mean(data_bus$T_perc)
      }
      else{ # pop == pop_negra
        data_bus$A_edu <-(data_bus$E_perc)*data_bus$perc_negra_q
        data_bus$A_saude <-(data_bus$S_perc)*data_bus$perc_negra_q
        data_bus$A_trab <-(data_bus$T_perc)*data_bus$perc_negra_q
        
        im_bus_edu <- sum(data_bus$A_edu)/sum(data_bus$perc_negra_q)
        im_bus_saude <- sum(data_bus$A_saude)/sum(data_bus$perc_negra_q)
        im_bus_trab <- sum(data_bus$A_trab)/sum(data_bus$perc_negra_q)
      }
      
      # antes era mean(data_bus$A_edu)
      #im_bus_edu <- sum(data_bus$A_edu)/sum(data_bus$cor_negra_q)
      #im_bus_saude <- sum(data_bus$A_saude)/sum(data_bus$cor_negra_q)
      #im_bus_trab <- sum(data_bus$A_trab)/sum(data_bus$cor_negra_q)
      
    }else{ # Para as cidades sem dados de onibus
      im_bus_edu <- 0
      im_bus_saude <- 0
      im_bus_trab <- 0
    }
    
    # Carro
    data_carro <- data_carro%>%
      drop_na(any_of('mode'))%>%
      dplyr::select('id_hex', 'sigla_muni', 'cor_branca', 'cor_amarela','cor_indigena','cor_negra', 'pop_total',
                    'empregos_total', 'saude_total', 'edu_total', 'mode', 'CMATT30',
                    'CMAST30', 'CMAET30')%>%
      dplyr::filter(pop_total>0)
    
    data_carro_i<- data_carro_i%>%
      drop_na(any_of('mode'))%>%
      dplyr::filter(pop_total>0)%>%
      dplyr::select('id_hex', 'CMATT30', 'CMAST30', 'CMAET30')
    setnames(data_carro_i, old = c('CMATT30', 'CMAST30', 'CMAET30'), new = c('CMATT30_', 'CMAST30_', 'CMAET30_'))
    
    data_carro_i$CMAET30_ <- ifelse(data_carro_i$CMAET30_ == 0, 1, data_carro_i$CMAET30_)
    data_carro_i$CMAST30_ <- ifelse(data_carro_i$CMAST30_ == 0, 1, data_carro_i$CMAST30_)
    data_carro_i$CMATT30_ <- ifelse(data_carro_i$CMATT30_ == 0, 1, data_carro_i$CMATT30_)
      
    data_carro <- left_join(data_carro, data_carro_i, by = 'id_hex')
    
    # Agrega populacoes
    #data_carro$pop_negra <- data_carro$cor_negra + data_carro$cor_indigena
    #data_carro$pop_branca <- data_carro$cor_branca + data_carro$cor_amarela
    
    data_carro$pop_total_q <- ifelse(data_carro$pop_total >= quantile(data_carro$pop_total, .75), 4,
                                   ifelse(data_carro$pop_total >= quantile(data_carro$pop_total, 0.5), 3, 
                                          ifelse(data_carro$pop_total >= quantile(data_carro$pop_total, 0.25), 2,1)))
    
    data_carro$cor_negra_q <- ifelse(data_carro$cor_negra >= quantile(data_carro$cor_negra, .75), 4,
                                   ifelse(data_carro$cor_negra >= quantile(data_carro$cor_negra, 0.5), 3, 
                                          ifelse(data_carro$cor_negra >= quantile(data_carro$cor_negra, 0.25), 2,1)))
    
    data_carro$perc_negra <- data_carro$cor_negra/data_carro$pop_total
    data_carro$perc_negra_q <- ifelse(data_carro$perc_negra >= quantile(data_carro$perc_negra, .75), 4,
                                    ifelse(data_carro$perc_negra >= quantile(data_carro$perc_negra, 0.5), 3, 
                                           ifelse(data_carro$perc_negra >= quantile(data_carro$perc_negra, 0.25), 2,1)))
    
#    data_carro$saude_entorno <- 0
#    data_carro$edu_entorno <- 0
#    data_carro$trab_entorno <- 0
    
#    viz_car <- get_kring(h3_address = data_carro$id_hex, ring_size = 10, simple = TRUE)
#    for(i in 1:nrow(data_carro)){
#      
#      data_carro[i,]$saude_entorno <- sum(data_carro[data_carro$id_hex %in% viz_car[[i]],]$saude_total)
#      data_carro[i,]$edu_entorno <- sum(data_carro[data_carro$id_hex %in% viz_car[[i]],]$edu_total)
#      data_carro[i,]$trab_entorno <- sum(data_carro[data_carro$id_hex %in% viz_car[[i]],]$empregos_total)
#      
#      if(data_carro[i,]$edu_entorno == 0){
#        data_carro[i,]$edu_entorno <- data_carro[i,]$CMAET30+1
#      }
#      if(data_carro[i,]$saude_entorno == 0){
#        data_carro[i,]$saude_entorno <- data_carro[i,]$CMAST30+1
#      }
#      if(data_carro[i,]$trab_entorno == 0){
#        data_carro[i,]$trab_entorno <- data_carro[i,]$CMATT30+1
 #     }
 #   }
    
#    data_carro$saude_entorno <- ifelse(data_carro$saude_entorno == 0, 1, data_carro$saude_entorno)
#    data_carro$edu_entorno <- ifelse(data_carro$edu_entorno == 0, 1, data_carro$edu_entorno)
#    data_carro$trab_entorno <- ifelse(data_carro$trab_entorno == 0, 1, data_carro$trab_entorno)
    
#    data_carro$E_perc <- data_carro$CMAET30/data_carro$edu_entorno
#    data_carro$S_perc <- data_carro$CMAST30/data_carro$saude_entorno
#    data_carro$T_perc <- data_carro$CMATT30/data_carro$trab_entorno
    
    data_carro$E_perc <- data_carro$CMAET30/data_carro$CMAET30_
    data_carro$S_perc <- data_carro$CMAST30/data_carro$CMAST30_
    data_carro$T_perc <- data_carro$CMATT30/data_carro$CMATT30_
    
    data_carro$E_perc <- ifelse(data_carro$E_perc>1, 1, data_carro$E_perc)
    data_carro$S_perc <- ifelse(data_carro$S_perc>1, 1, data_carro$S_perc)
    data_carro$T_perc <- ifelse(data_carro$T_perc>1, 1, data_carro$T_perc)
    
    if (pop == "pop_total"){
        data_carro$A_edu <-(data_carro$E_perc)*data_carro$pop_total_q
        data_carro$A_saude <-(data_carro$S_perc)*data_carro$pop_total_q
        data_carro$A_trab <-(data_carro$T_perc)*data_carro$pop_total_q
        
        im_car_edu <- sum(data_carro$A_edu)/sum(data_carro$pop_total_q)
        im_car_saude <- sum(data_carro$A_saude)/sum(data_carro$pop_total_q)
        im_car_trab <- sum(data_carro$A_trab)/sum(data_carro$pop_total_q)

    }else if(pop == "media"){
      im_car_edu <- mean(data_carro$E_perc)
      im_car_saude <- mean(data_carro$S_perc)
      im_car_trab <- mean(data_carro$T_perc)
    }else{
      data_carro$A_edu <-(data_carro$E_perc)*data_carro$perc_negra_q
      data_carro$A_saude <-(data_carro$S_perc)*data_carro$perc_negra_q
      data_carro$A_trab <-(data_carro$T_perc)*data_carro$perc_negra_q
      
      im_car_edu <- sum(data_carro$A_edu)/sum(data_carro$perc_negra_q)
      im_car_saude <- sum(data_carro$A_saude)/sum(data_carro$perc_negra_q)
      im_car_trab <- sum(data_carro$A_trab)/sum(data_carro$perc_negra_q)
    }
    
    # 3.2 Ativos
    data_ativos <- data_ativos%>%
      drop_na(any_of('mode'))%>%
      dplyr::select('id_hex', 'sigla_muni', 'cor_branca', 'cor_amarela','cor_indigena','cor_negra', 'pop_total',
                    'empregos_total', 'saude_total', 'edu_total', 'mode', 'CMATT30',
                    'CMAST30', 'CMAET30')
    
    data_ativos_i<- data_ativos_i%>%
      drop_na(any_of('mode'))%>%
      dplyr::filter(pop_total>0)%>%
      dplyr::select('id_hex', 'mode', 'CMATT30', 'CMAST30', 'CMAET30')
      
    setnames(data_ativos_i, old = c('CMATT30', 'CMAST30', 'CMAET30'), new = c('CMATT30_', 'CMAST30_', 'CMAET30_'))
    
    data_ativos_i$CMAET30_ <- ifelse(data_ativos_i$CMAET30_ == 0, 1, data_ativos_i$CMAET30_)
    data_ativos_i$CMAST30_ <- ifelse(data_ativos_i$CMAST30_ == 0, 1, data_ativos_i$CMAST30_)
    data_ativos_i$CMATT30_ <- ifelse(data_ativos_i$CMATT30_ == 0, 1, data_ativos_i$CMATT30_)
    
    # # Agrega populacoes
    # data_ativos$pop_negra <- data_ativos$cor_negra + data_ativos$cor_indigena
    # data_ativos$pop_branca <- data_ativos$cor_branca + data_ativos$cor_amarela
  
    
    # Separa em walk e bike e calcula percentuais 
    data_walk <- dplyr::filter(data_ativos, mode=='walk')%>%
      dplyr::filter(pop_total>0)
    
    data_walk_i <- dplyr::filter(data_ativos_i, mode=='walk')%>%
      dplyr::select('id_hex', 'CMATT30_', 'CMAST30_', 'CMAET30_')
    
    data_walk <- left_join(data_walk, data_walk_i, by = 'id_hex')
    
    data_walk$pop_total_q <- ifelse(data_walk$pop_total >= quantile(data_walk$pop_total, .75), 4,
                                     ifelse(data_walk$pop_total >= quantile(data_walk$pop_total, 0.5), 3, 
                                            ifelse(data_walk$pop_total >= quantile(data_walk$pop_total, 0.25), 2,1)))
    
    data_walk$cor_negra_q <- ifelse(data_walk$cor_negra >= quantile(data_walk$cor_negra, .75), 4,
                                      ifelse(data_walk$cor_negra >= quantile(data_walk$cor_negra, 0.5), 3, 
                                             ifelse(data_walk$cor_negra >= quantile(data_walk$cor_negra, 0.25), 2,1)))
    
    data_walk$perc_negra <- data_walk$cor_negra/data_walk$pop_total
    data_walk$perc_negra_q <- ifelse(data_walk$perc_negra >= quantile(data_walk$perc_negra, .75), 4,
                                      ifelse(data_walk$perc_negra >= quantile(data_walk$perc_negra, 0.5), 3, 
                                             ifelse(data_walk$perc_negra >= quantile(data_walk$perc_negra, 0.25), 2,1)))
    
    #walk
#    data_walk$saude_entorno <- 0
#    data_walk$edu_entorno <- 0
#    data_walk$trab_entorno <- 0
    
#    viz_walk <- get_kring(h3_address = data_walk$id_hex, ring_size = 4, simple = TRUE)
#    for(i in 1:nrow(data_walk)){
#      
#      data_walk[i,]$saude_entorno <- sum(data_walk[data_walk$id_hex %in% viz_walk[[i]],]$saude_total)
#      data_walk[i,]$edu_entorno <- sum(data_walk[data_walk$id_hex %in% viz_walk[[i]],]$edu_total)
#      data_walk[i,]$trab_entorno <- sum(data_walk[data_walk$id_hex %in% viz_walk[[i]],]$empregos_total)
      
#      if(data_walk[i,]$edu_entorno == 0){
#        data_walk[i,]$edu_entorno <- data_walk[i,]$CMAET30+1
#      }
#      if(data_walk[i,]$saude_entorno == 0){
#        data_walk[i,]$saude_entorno <- data_walk[i,]$CMAST30+1
#      }
#      if(data_walk[i,]$trab_entorno == 0){
#        data_walk[i,]$trab_entorno <- data_walk[i,]$CMATT30+1
#      }
#    }
    
#    data_walk$saude_entorno <- ifelse(data_walk$saude_entorno == 0, 1, data_walk$saude_entorno)
#    data_walk$edu_entorno <- ifelse(data_walk$edu_entorno == 0, 1, data_walk$edu_entorno)
#    data_walk$trab_entorno <- ifelse(data_walk$trab_entorno == 0, 1, data_walk$trab_entorno)
    
#   data_walk$E_perc <- data_walk$CMAET30/data_walk$edu_entorno
#    data_walk$S_perc <- data_walk$CMAST30/data_walk$saude_entorno
#    data_walk$T_perc <- data_walk$CMATT30/data_walk$trab_entorno
    
    data_walk$E_perc <- data_walk$CMAET30/data_walk$CMAET30_
    data_walk$S_perc <- data_walk$CMAST30/data_walk$CMAST30_
    data_walk$T_perc <- data_walk$CMATT30/data_walk$CMATT30_
    
    data_walk$E_perc <- ifelse(data_walk$E_perc>1, 1, data_walk$E_perc)
    data_walk$S_perc <- ifelse(data_walk$S_perc>1, 1, data_walk$S_perc)
    data_walk$T_perc <- ifelse(data_walk$T_perc>1, 1, data_walk$T_perc)
     
    if (pop == "pop_total"){
      data_walk$A_edu <-(data_walk$E_perc)*data_walk$pop_total_q
      data_walk$A_saude <-(data_walk$S_perc)*data_walk$pop_total_q
      data_walk$A_trab <-(data_walk$T_perc)*data_walk$pop_total_q
      
      im_walk_edu <- sum(data_walk$A_edu)/sum(data_walk$pop_total_q)
      im_walk_saude <- sum(data_walk$A_saude)/sum(data_walk$pop_total_q)
      im_walk_trab <- sum(data_walk$A_trab)/sum(data_walk$pop_total_q)

    }else if(pop == "media"){
      im_walk_edu <- mean(data_walk$E_perc)
      im_walk_saude <- mean(data_walk$S_perc)
      im_walk_trab <- mean(data_walk$T_perc)
    }else{
      data_walk$A_edu <-(data_walk$E_perc)*data_walk$perc_negra_q
      data_walk$A_saude <-(data_walk$S_perc)*data_walk$perc_negra_q
      data_walk$A_trab <-(data_walk$T_perc)*data_walk$perc_negra_q
      
      im_walk_edu <- sum(data_walk$A_edu)/sum(data_walk$perc_negra_q)
      im_walk_saude <- sum(data_walk$A_saude)/sum(data_walk$perc_negra_q)
      im_walk_trab <- sum(data_walk$A_trab)/sum(data_walk$perc_negra_q)
    }
    
    #bike
    data_bike <- dplyr::filter(data_ativos, mode=='bike')%>%
      dplyr::filter(pop_total>0)
    
    data_bike_i <- dplyr::filter(data_ativos_i, mode=='bike')%>%
      dplyr::select('id_hex', 'CMATT30_', 'CMAST30_', 'CMAET30_')
    
    data_bike <- left_join(data_bike, data_bike_i, by = 'id_hex')
    
#    data_bike$saude_entorno <- 0
#    data_bike$edu_entorno <- 0
#    data_bike$trab_entorno <- 0
    
    data_bike$pop_total_q <- ifelse(data_bike$pop_total >= quantile(data_bike$pop_total, .75), 4,
                                    ifelse(data_bike$pop_total >= quantile(data_bike$pop_total, 0.5), 3, 
                                           ifelse(data_bike$pop_total >= quantile(data_bike$pop_total, 0.25), 2,1)))
    
    data_bike$cor_negra_q <- ifelse(data_bike$cor_negra >= quantile(data_bike$cor_negra, .75), 4,
                                    ifelse(data_bike$cor_negra >= quantile(data_bike$cor_negra, 0.5), 3, 
                                           ifelse(data_bike$cor_negra >= quantile(data_bike$cor_negra, 0.25), 2,1)))
    
    data_bike$perc_negra <- data_bike$cor_negra/data_bike$pop_total
    data_bike$perc_negra_q <- ifelse(data_bike$perc_negra >= quantile(data_bike$perc_negra, .75), 4,
                                     ifelse(data_bike$perc_negra >= quantile(data_bike$perc_negra, 0.5), 3, 
                                            ifelse(data_bike$perc_negra >= quantile(data_bike$perc_negra, 0.25), 2,1)))
    
#    viz_bike <- get_kring(h3_address = data_bike$id_hex, ring_size = 10, simple = TRUE)
    
#    for(i in 1:nrow(data_bike)){
#      data_bike[i,]$saude_entorno <- sum(data_bike[data_bike$id_hex %in% viz_bike[[i]],]$saude_total)
#      data_bike[i,]$edu_entorno <- sum(data_bike[data_bike$id_hex %in% viz_bike[[i]],]$edu_total)
#      data_bike[i,]$trab_entorno <- sum(data_bike[data_bike$id_hex %in% viz_bike[[i]],]$empregos_total)
      
#      if(data_bike[i,]$edu_entorno == 0){
#        data_bike[i,]$edu_entorno <- data_bike[i,]$CMAET30+1
#      }
#      if(data_bike[i,]$saude_entorno == 0){
#        data_bike[i,]$saude_entorno <- data_bike[i,]$CMAST30+1
#     }
#      if(data_bike[i,]$trab_entorno == 0){
#        data_bike[i,]$trab_entorno <- data_bike[i,]$CMATT30+1
#      }
#    }
#    data_bike$saude_entorno <- ifelse(data_bike$saude_entorno == 0, 1, data_bike$saude_entorno)
#    data_bike$edu_entorno <- ifelse(data_bike$edu_entorno == 0, 1, data_bike$edu_entorno)
#    data_bike$trab_entorno <- ifelse(data_bike$trab_entorno == 0, 1, data_bike$trab_entorno)
    
#    data_bike$E_perc <- data_bike$CMAET30/data_bike$edu_entorno
#    data_bike$S_perc <- data_bike$CMAST30/data_bike$saude_entorno
#    data_bike$T_perc <- data_bike$CMATT30/data_bike$trab_entorno
    
    data_bike$E_perc <- data_bike$CMAET30/data_bike$CMAET30_
    data_bike$S_perc <- data_bike$CMAST30/data_bike$CMAST30_
    data_bike$T_perc <- data_bike$CMATT30/data_bike$CMATT30_
    
    data_bike$E_perc <- ifelse(data_bike$E_perc>1, 1, data_bike$E_perc)
    data_bike$S_perc <- ifelse(data_bike$S_perc>1, 1, data_bike$S_perc)
    data_bike$T_perc <- ifelse(data_bike$T_perc>1, 1, data_bike$T_perc)
    
    if (pop == 'pop_total'){
      data_bike$A_edu <-(data_bike$E_perc)*data_bike$pop_total_q
      data_bike$A_saude <-(data_bike$S_perc)*data_bike$pop_total_q
      data_bike$A_trab <-(data_bike$T_perc)*data_bike$pop_total_q
      
      im_bike_edu <- sum(data_bike$A_edu)/sum(data_bike$pop_total_q)
      im_bike_saude <- sum(data_bike$A_saude)/sum(data_bike$pop_total_q)
      im_bike_trab <- sum(data_bike$A_trab)/sum(data_bike$pop_total_q)
      
    }else if(pop == "media"){
      im_bike_edu <- mean(data_bike$E_perc)
      im_bike_saude <- mean(data_bike$S_perc)
      im_bike_trab <- mean(data_bike$T_perc)
    }else{
      data_bike$A_edu <-(data_bike$E_perc)*data_bike$perc_negra_q
      data_bike$A_saude <-(data_bike$S_perc)*data_bike$perc_negra_q
      data_bike$A_trab <-(data_bike$T_perc)*data_bike$perc_negra_q
      
      im_bike_edu <- sum(data_bike$A_edu)/sum(data_bike$perc_negra_q)
      im_bike_saude <- sum(data_bike$A_saude)/sum(data_bike$perc_negra_q)
      im_bike_trab <- sum(data_bike$A_trab)/sum(data_bike$perc_negra_q)
    }
    
    # 4.1 IM por modo de transporte
    # Considerar PNMU
    
    # Onibus 
    # 60 minutos
    # Res 7
    # Entorno = 10 hex
    
    # Carro
    # 30 minutos
    # Res 7
    # Entorno = 10 hex
    
    # Bike
    # 30 minutos
    # Res 8
    # Entorno = 10 hex
    
    # Walk
    # 30 minutos
    # Res 8
    # Entorno = 4 hex
    
    # Onibus
    if (muni %nin% skip_bus){
      im_bus <- (im_bus_edu*3 + im_bus_saude*2 + im_bus_trab*5)/10
      p_bus <- 5;
    }else{
      im_bus <- 0
      p_bus <- 5
    }
    # Carro
    im_car <- (im_car_edu*3 + im_car_saude*2 + im_car_trab*5)/10
    p_car <- 1
    # A pe
    im_walk <- (im_walk_edu*3 + im_walk_saude*2 + im_walk_trab*5)/10
    p_walk <- 1
    # Bicicleta
    im_bike <- (im_bike_edu*3 + im_bike_saude*2 + im_bike_trab*5)/10
    p_bike <- 3
    
    # 4.2 IM consolidado
    # Por enquanto usamos a media aritmetica
    im_edu <- (p_bus*im_bus_edu + p_walk*im_walk_edu + p_bike*im_bike_edu + p_car*im_car_edu) / (p_bus + p_walk + p_bike + p_car)
    im_saude <- (p_bus*im_bus_saude + p_walk*im_walk_saude + p_bike*im_bike_saude + p_car*im_car_saude) / (p_bus + p_walk + p_bike + p_car)
    im_trab <- (p_bus*im_bus_trab + p_walk*im_walk_trab + p_bike*im_bike_trab + p_car*im_car_trab) / (p_bus + p_walk + p_bike + p_car)
    
    indice_mobilidade <- (p_bus*im_bus + p_walk*im_walk + p_bike*im_bike + p_car*im_car) / (p_bus + p_bike + p_walk + p_car)
    
    # 5.0 Imprime relatorio
    print("=========================================")
    print(sprintf("Calculo do IM da cidade: %s", muni))
    print(paste("Acessibilidade - ??nibus:", im_bus))
    print(paste("Acessibilidade - a p??:", im_walk))
    print(paste("Acessibilidade - bicicleta:", im_bike))
    print(paste("Acessibilidade - carro compart.:", im_car))
    print(paste("indice de mobilidade:", indice_mobilidade))
    
    df <- data.frame(municipio = muni, IM = indice_mobilidade, #IM_edu = im_edu, IM_saude = im_saude, IM_trab = im_trab,
                     IM_bus = im_bus,
                     IM_walk = im_walk,
                     IM_bike = im_bike,
                     IM_car = im_car,
                     IM_bus_edu = im_bus_edu, IM_bus_saude = im_bus_saude, IM_bus_trab = im_bus_trab, 
                      
                     IM_walk_edu = im_walk_edu, IM_walk_saude = im_walk_saude, IM_walk_trab = im_walk_trab, 
                     
                     IM_bike_edu = im_bike_edu, IM_bike_saude = im_bike_saude, IM_bike_trab = im_bike_trab, 
                     
                     IM_car_edu = im_car_edu, IM_car_saude = im_car_saude, IM_car_trab = im_car_trab
                     )
    
    dados_indice <- rbind(dados_indice, df)
    
    # Ajusta o nome do arquivo
    #write_csv(dados_indice, sprintf('%s/indice_mobilidade_pop_normalizado_4_%s.csv', save_folder, pop))
    # write_csv(dados_indice, sprintf('%s/indice_mobilidade_%s_elast.csv', save_folder, pop))
    
  }
  # Retorna o dataframe
  
  write_delim(dados_indice, sprintf('%s/indice_mobilidade_%s_elast.csv', save_folder, ano), delim = ';')
  return(dados_indice)
}

munis <- c('nat','rec')

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "jpa", "man", "nat", "rec",
                 "rio", "sne", "sjc", "spo", "tsa", "ula", "vta")

indice_mobilidade_entorno(muni_list = munis, ano=2019, pop="lala")

