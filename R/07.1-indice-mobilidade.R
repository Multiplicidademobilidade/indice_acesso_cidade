## Script para o calculo do Indice de Mobilidade (IM)

source('fun/setup.R')

calculo_indice_mobilidade <- function(muni){
  # A funcao calcula e imprime o relatorio sobre uma cidade de cada vez
  # Implementar a execucao para todas as cidades de forma que seja gerado um
  # arquivo .rds ao final
  
  # 0.0 Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolder17 <- sprintf("%s/17_acesso_oportunidades/2019", files_folder)
  
  # 1.0 Carrega e prepara as bases
  # Usaremos a matriz de dados agregados e a matriz de acessibilidade
  
  # Bus (res = 7)
  file1 <- readRDS(sprintf('%s/hex_agregado_%s_07_2019.rds', subfolder14, muni))%>%
    st_drop_geometry()
  file2 <- readRDS(sprintf('%s/acess_%s_07_2019.rds', subfolder17, muni))
  data_bus <- left_join(file1, file2, by = c('id_hex' = 'origin'))
  
  data_bus$perc_negra <- data_bus$cor_negra/data_bus$pop_total
  data_bus$perc_branca <- data_bus$cor_branca/data_bus$pop_total
  data_bus$OPORT <- sum(data_bus$empregos_total + data_bus$saude_total + data_bus$edu_total)
  
  # Ativos (res = 8)
  file3 <- readRDS(sprintf('%s/hex_agregado_%s_08_2019.rds', subfolder14, muni))%>%
    st_drop_geometry()
  file4 <- readRDS(sprintf('%s/acess_%s_08_2019.rds', subfolder17, muni))
  data_ativos <- left_join(file3, file4, by = c('id_hex' = 'origin'))
  
  data_ativos$perc_negra <- data_ativos$cor_negra/data_ativos$pop_total
  data_ativos$perc_branca <- data_ativos$cor_branca/data_ativos$pop_total
  data_ativos$OPORT <- sum(data_ativos$empregos_total + data_ativos$saude_total + data_ativos$edu_total)
  
  rm(file1, file2, file3, file4)
  
  # 3.0 Selecao de colunas relevantes
  # 3.1 Onibus
  im_bus <- data_bus%>%
    drop_na(any_of('mode'))%>%
    dplyr::select('id_hex', 'sigla_muni', 'cor_branca', 'cor_amarela','cor_indigena','cor_negra', 'pop_total', 'perc_negra',
                  'empregos_total', 'saude_total', 'edu_total', 'mode', 'CMATT15', 'CMATT30', 'CMATT45', 'CMATT60',
                  'CMAST15', 'CMAST30', 'CMAST45', 'CMAST60', 'CMAET15', 'CMAET30', 'CMAET45', 'CMAET60')
  # CMA total a 60 minutos
  im_bus$CMA60 <- im_bus$CMATT60 + im_bus$CMAST60 + im_bus$CMAET60
  
  # 3.2 Ativos
  im_ativos <- data_ativos%>%
    drop_na(any_of('mode'))%>%
    dplyr::select('id_hex', 'sigla_muni', 'cor_branca', 'cor_amarela','cor_indigena','cor_negra', 'pop_total', 'perc_negra',
                  'empregos_total', 'saude_total', 'edu_total', 'mode', 'CMATT15', 'CMATT30',
                  'CMAST15', 'CMAST30', 'CMAET15', 'CMAET30')
  # CMA total a 30 minutos
  im_ativos$CMA30 <- im_ativos$CMATT30 + im_ativos$CMAST30 + im_ativos$CMAET30
  
  # 4.0 Calculo do IM
  im_bus$CMAn <-im_bus$CMA60*im_bus$cor_negra
  im_bus$CMAb <-im_bus$CMA60*im_bus$cor_branca
  
  CMA_bus_negra <- sum(im_bus$CMAn)/sum(im_bus$cor_negra)
  CMA_bus_branca <- sum(im_bus$CMAb)/sum(im_bus$cor_branca)
  
  im_ativos$CMAn <-im_ativos$CMA30*im_ativos$cor_negra
  im_ativos$CMAb <-im_ativos$CMA30*im_ativos$cor_branca
  
  CMA_walk_negra <- sum(im_ativos[im_ativos$mode=='walk',]$CMAn)/sum(im_ativos$cor_negra)
  CMA_walk_branca <- sum(im_ativos[im_ativos$mode=='walk',]$CMAb)/sum(im_ativos$cor_branca)
  
  CMA_bike_negra <- sum(im_ativos[im_ativos$mode=='bike',]$CMAn)/sum(im_ativos$cor_negra)
  CMA_bike_branca <- sum(im_ativos[im_ativos$mode=='bike',]$CMAb)/sum(im_ativos$cor_branca)
  
  # 4.1 IM por modo de transporte
  im_bus <- CMA_bus_negra / CMA_bus_branca
  im_walk <- CMA_walk_negra / CMA_walk_branca
  im_bike <- CMA_bike_negra / CMA_bike_branca
  
  # 4.2 IM consolidado
  # Por enquanto usamos a media aritmetica
  indice_mobilidade <- (2*im_bus + 2*im_walk + 2*im_bike) / 6
  
  # 5.0 Imprime relatorio
  print("=========================================")
  print(sprintf("C??lculo do IM da cidade: %s", muni))
  print(paste("Acessibilidade - ??nibus:", im_bus))
  print(paste("Acessibilidade - a p??:", im_walk))
  print(paste("Acessibilidade - bicicleta:", im_bike))
  print(paste("??ndice de mobilidade:", indice_mobilidade))
  
}

# Executar funcao
calculo_indice(muni='rio')

