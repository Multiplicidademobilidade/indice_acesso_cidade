# Script para visualiza????es
source('fun/setup.R')
library(tmap)
library(RColorBrewer)

# Cores
paleta <- c('#38040E', '#F4442E', '#FFC145', '#05E4FF', '#6184D8') 
paleta_NA <- '#FCF7F8'

tmap_options(
  output.format = "png",
  output.dpi = 500
)

# Dados
files_folder <- "../../indice-mobilidade_dados"
subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
subfolder17 <- sprintf("%s/17_acesso_oportunidades/2019", files_folder)

muni = "bho"
file_aux <- readRDS(sprintf("%s/hex_agregado_%s_08_2019.rds", subfolder14, muni))
tt_total <- sum(file_aux$empregos_total)
st_total <- sum(file_aux$saude_total)
et_total <- sum(file_aux$edu_total)

file <- readRDS(sprintf("%s/acess_%s_08_2019.rds", subfolder17, muni))

data_plot <- st_join(file_aux, file, join=st_equals)

# Trabalharemos com percentuais
# Trabalho
data_plot$CMATT15_p <- (data_plot$CMATT15 / tt_total)*100
data_plot$CMATT30_p <- (data_plot$CMATT30 / tt_total)*100
data_plot$CMATT45_p <- (data_plot$CMATT45 / tt_total)*100
data_plot$CMATT60_p <- (data_plot$CMATT60 / tt_total)*100

rm(file_aux, file, data_plot)


# Recortes dos dados

# populacao
data_plot$perc_cor_negra <- (data_plot$cor_negra / data_plot$pop_total)*100

data_inset_pop <- data_plot%>%
  dplyr::select(origin, mode, edu_total, saude_total)%>%
  pivot_longer(c('edu_total', 'saude_total'), names_to="OPORT", values_to="Total")

# oportunidades
data_inset_oport <- data_plot%>%
  dplyr::select(origin, mode, edu_total, saude_total)%>%
  pivot_longer(c('edu_total', 'saude_total'), names_to="OPORT", values_to="Total")

# ativos
data_inset <- data_plot%>%
  dplyr::select(id_hex, mode, CMATT15_p, CMATT30_p)%>%
  pivot_longer(c('CMATT15_p', 'CMATT30_p'), names_to="CMA", values_to="Total")

# motorizados
data_inset <- data_plot%>%
  dplyr::select(origin, mode, CMATT15_p, CMATT30_p, CMATT45_p, CMATT60_p)%>%
  pivot_longer(c('CMATT15_p', 'CMATT30_p', 'CMATT45_p', 'CMATT60_p'), names_to="CMA", values_to="Total")

df = plot_data %>% 
  filter(CMA %in% c("CMATT15_p", "CMATT30_p", "CMATT45_p", "CMATT60_p"))



# Definindo padrao de mapa

# Cores
# tmaptools::palette_explorer()


# Modelo
tm_shape(data_plot) + 
  tm_layout(title = 'Acesso ao trabalho', legend.outside = TRUE, frame = FALSE,
            title.size = 1, legend.title.size = 3)+
  #tm_polygons(title = "Oportunidades em 15 minutos")+
  tm_fill(col = "CMATT60_p", 
          n=5,
          style = "pretty",
          palette = paleta, colorNA = paleta_NA, textNA = "N/D", 
          title = "Percentual de oportunidades a 15 minutos",
          legend.format = list(text.separator="a", text.or.more="ababa", scientific=TRUE, format="f"))+ # scientific e interessante
  tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("left", "bottom"))



# "Por modo por tempo por tipo de oportunidade"
tm_shape(df) + 
  tm_layout(title = 'Acesso ao trabalho - ??nibus', legend.outside = TRUE, frame = FALSE, 
            panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
            title.size = 1, legend.title.size = 3)+
  #tm_polygons(title = "Oportunidades em 15 minutos")+
  tm_fill(col = "Total", 
          n=5,
          style = "pretty",
          palette = paleta, colorNA = paleta_NA, textNA = "N/D", 
          title = "Percentual de oportunidades",
          legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
  tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
  tm_facets(by = "CMA", nrow=2, free.coords = FALSE)


# Distribuicao de oportunidades

tm_shape(data_inset_oport) + 
  tm_layout(title = 'Distribui????o de Oportunidades', legend.outside = TRUE, frame = FALSE, 
            #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
            title.size = 1, legend.title.size = 3)+
  #tm_polygons(title = "Oportunidades em 15 minutos")+
  tm_fill(col = "Total", 
          n=5,
          style = "pretty",
          palette = paleta, colorNA = paleta_NA, textNA = "N/D", 
          title = "Percentual de oportunidades",
          legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
  tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
  tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)


# Distribuicao de Populacao

plot <- tm_shape(data_inset_oport) + 
  tm_layout(title = 'Distribui????o de Oportunidades', legend.outside = TRUE, frame = FALSE, 
            #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
            title.size = 1, legend.title.size = 3)+
  #tm_polygons(title = "Oportunidades em 15 minutos")+
  tm_fill(col = "Total",
          n=5,
          style = "pretty",
          palette = paleta, colorNA = paleta_NA, textNA = "N/D", 
          title = "Percentual de oportunidades",
          legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
  tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
  tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
  tm_borders(alpha = 1)

# 3.0 gera os plots e salva

dplyr::filter(data_inset, mode=="walk")%>%
  tm_shape() + 
    tm_layout(main.title = 'Acesso ao trabalho - a p??', legend.outside = TRUE, frame = FALSE, 
              panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
              panel.label.bg.color = NA,
              title.size = 1, legend.title.size = 1,
              main.title.position = 'center',
              frame.lwd = NA)+
    #tm_polygons(title = "Oportunidades em 15 minutos")+
    tm_fill(col = "Total", 
            n=5,
            style = "pretty",
            palette = paleta, colorNA = paleta_NA, textNA = "N/D", 
            title = "Percentual de oportunidades",
            legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
    tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
    tm_facets(by = "CMA", nrow=1, free.coords = FALSE)+
  tm_legend(position = c('right','center'))

filename = "bho_oport"
tmap_save(tm=plot33, height = 1000, height = 1000, units = 'px')


criar_mapas_acesso <- function(muni, formato){
  # 0.0 Configuracoes iniciais
  # Plotagem
  tmap_options(
    output.format = formato
  )
  # Cores
  paleta <- c('#38040E', '#F4442E', '#FFC145', '#05E4FF', '#6184D8') 
  paleta_NA <- '#FCF7F8'
  
  # Estrutura de pasta
  files_folder <- "../../indice-mobilidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolder17 <- sprintf("%s/17_acesso_oportunidades/2019", files_folder)
  save_folder <- sprintf("%s/XY_imagens/2019/Acessibilidade", files_folder)
  
  # 1.0 Carrega arquivos
  # res 7
  file_aux_7 <- readRDS(sprintf("%s/hex_agregado_%s_07_2019.rds", subfolder14, muni))
  file_7 <- readRDS(sprintf("%s/acess_%s_07_2019.rds", subfolder17, muni))
  # res 8
  file_aux_8 <- readRDS(sprintf("%s/hex_agregado_%s_08_2019.rds", subfolder14, muni))
  file_8 <- readRDS(sprintf("%s/acess_%s_08_2019.rds", subfolder17, muni))
  
  data_plot_7 <- st_join(file_aux_7, file_7, join=st_equals)
  data_plot_8 <- st_join(file_aux_8, file_8, join=st_equals)
  
  # 2.0 Prepara bases
  # Trabalharemos com percentuais - RES 7
  # Trabalho
  data_plot_7$CMATT30_p <- (data_plot_7$CMATT30 / tt_total)*100
  data_plot_7$CMATT45_p <- (data_plot_7$CMATT45 / tt_total)*100
  data_plot_7$CMATT60_p <- (data_plot_7$CMATT60 / tt_total)*100
  # Saude
  data_plot_7$CMAST30_p <- (data_plot_7$CMAST30 / tt_total)*100
  data_plot_7$CMAST45_p <- (data_plot_7$CMAST45 / tt_total)*100
  data_plot_7$CMAST60_p <- (data_plot_7$CMAST60 / tt_total)*100
  # Educacao
  data_plot_7$CMAET30_p <- (data_plot_7$CMAET30 / tt_total)*100
  data_plot_7$CMAET45_p <- (data_plot_7$CMAET45 / tt_total)*100
  data_plot_7$CMAET60_p <- (data_plot_7$CMAET60 / tt_total)*100

  # Trabalharemos com percentuais - RES 8
  # Trabalho
  data_plot_8$CMATT15_p <- (data_plot_8$CMATT15 / tt_total)*100
  data_plot_8$CMATT30_p <- (data_plot_8$CMATT30 / tt_total)*100
  # Saude
  data_plot_8$CMAST15_p <- (data_plot_8$CMAST15 / tt_total)*100
  data_plot_8$CMAST30_p <- (data_plot_8$CMAST30 / tt_total)*100
  # Educacao
  data_plot_8$CMAET15_p <- (data_plot_8$CMAET15 / tt_total)*100
  data_plot_8$CMAET30_p <- (data_plot_8$CMAET30 / tt_total)*100
  
  # ativos
  data_ativos <- data_plot_8%>%
    dplyr::select(origin, mode, CMATT15_p, CMATT30_p)%>%
    pivot_longer(c('CMATT15_p', 'CMATT30_p'), names_to="CMA", values_to="Total")
  
  # motorizados
  data_motor <- data_plot_7%>%
    dplyr::select(origin, mode, CMATT30_p, CMATT45_p, CMATT60_p)%>%
    pivot_longer(c('CMATT30_p', 'CMATT45_p', 'CMATT60_p'), names_to="CMA", values_to="Total")
  
  
  # 3.0 Gera graficos e salva
  
  # ATIVOS
  # A p??
  plot1 <- dplyr::filter(data_ativos, mode == "walk")%>%
    tm_shape() + 
    tm_layout(title = 'Acesso ao trabalho - A p??', legend.outside = TRUE, frame = FALSE, 
              panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
              title.size = 1, legend.title.size = 1,
              main.title.position = 'center',
              frame.lwd = NA,
              panel.label.bg.color = NA)+
    #tm_polygons(title = "Oportunidades em 15 minutos")+
    tm_fill(col = "Total", 
            n=5,
            style = "pretty",
            palette = paleta, colorNA = paleta_NA, textNA = "N/D", 
            title = "Percentual de oportunidades",
            legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
    tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
    tm_facets(by = "CMA", nrow=1, free.coords = FALSE)+
    tm_legend(position = c('right','center'))
  
  # bicicleta
  plot2 <- dplyr::filter(data_ativos, mode == "bike")%>%
    tm_shape() + 
    tm_layout(title = 'Acesso ao trabalho - Bicicleta', legend.outside = TRUE, frame = FALSE, 
              panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
              title.size = 1, legend.title.size = 1,
              main.title.position = 'center',
              frame.lwd = NA,
              panel.label.bg.color = NA)+
    #tm_polygons(title = "Oportunidades em 15 minutos")+
    tm_fill(col = "Total", 
            n=5,
            style = "pretty",
            palette = paleta, colorNA = paleta_NA, textNA = "N/D", 
            title = "Percentual de oportunidades",
            legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
    tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
    tm_facets(by = "CMA", nrow=1, free.coords = FALSE)+
    tm_legend(position = c('right','center'))
  
  # MOTORIZADOS
  # ??nibus
  plot3 <- dplyr::filter(data_motor, mode == "transit")%>%
    tm_shape() + 
    tm_layout(title = 'Acesso ao trabalho ??nibus', legend.outside = TRUE, frame = FALSE, 
              panel.labels = c('At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
              title.size = 1, legend.title.size = 1,
              main.title.position = 'center',
              frame.lwd = NA,
              panel.label.bg.color = NA)+
    #tm_polygons(title = "Oportunidades em 15 minutos")+
    tm_fill(col = "Total", 
            n=5,
            style = "pretty",
            palette = paleta, colorNA = paleta_NA, textNA = "N/D", 
            title = "Percentual de oportunidades",
            legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
    tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
    tm_facets(by = "CMA", nrow=1, free.coords = FALSE)+
    tm_legend(position = c('right','center'))
  
  path_1 <- sprintf("%s/acess_trabalho_walk_%s_2019.%s", save_folder, muni, formato)
  tmap_save(tm = plot1, filename = path_1, height = 1200, units = "px")
  path_2 <- sprintf("%s/acess_trabalho_bike_%s_2019.%s", save_folder, muni, formato)
  tmap_save(tm = plot2, filename = path_2, height = 1200, units = "px")
  path_3 <- sprintf("%s/acess_trabalho_transit_%s_2019.%s", save_folder, muni, formato)
  tmap_save(tm = plot3, filename = path_3, height = 1200, units = "px")
}




criar_mapas_acesso(muni = "bho", formato = "png")


