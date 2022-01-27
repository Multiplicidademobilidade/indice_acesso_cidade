## Script para o calculo do indice de acessibilidade
source('fun/setup.R')

files_folder <- "../../indice-mobilidade_dados"
subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
subfolder17 <- sprintf("%s/17_acesso_oportunidades/2019", files_folder)

muni <- 'spo'
file <- readRDS(sprintf('%s/hex_agregado_%s_07_2019.rds', subfolder14, muni))%>%
  st_drop_geometry()

file2 <- readRDS(sprintf('%s/acess_%s_07_2019.rds', subfolder17, muni))

file3 <- left_join(file, file2, by = c('id_hex' = 'origin'))

# Correla????o Linear de Pearson (R)

file3$perc_negra <- file3$cor_negra/file3$pop_total
file3$perc_branca <- file3$cor_branca/file3$pop_total

file_cor <- file3%>%
  drop_na(any_of('mode'))%>%
  dplyr::select(perc_branca, perc_negra, CMATT15, CMATT30, CMATT45, CMATT60)

cor(file_cor)

# Diagrama de dispers??o
file3%>%
  pivot_longer(c('CMATT15', 'CMATT30', 'CMATT45', 'CMATT60'), names_to="CMA", values_to="Total")%>%
  ggplot(aes(x=perc_negra, y=Total, color=CMA))+
  geom_point()

# Calculo do indice
# Selecao das variaveis relevantes

data_im <- file3%>%
  drop_na(any_of('mode'))%>%
  dplyr::select('id_hex', 'sigla_muni', 'cor_branca', 'cor_amarela','cor_indigena','cor_negra', 'pop_total',
                'empregos_total', 'saude_total', 'edu_total', 'mode', 'CMATT15', 'CMATT30', 'CMATT45', 'CMATT60',
                'CMAST15', 'CMAST30', 'CMAST45', 'CMAST60', 'CMAET15', 'CMAET30', 'CMAET45', 'CMAET60')

data_im$Tbus <- (data_im$CMATT60/sum(data_im$empregos_total))/(data_im$cor_negra/sum(data_im$pop_total))









