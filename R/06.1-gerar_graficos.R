# Script para visualiza????es
source('fun/setup.R')
library(tmap)
library(RColorBrewer)

# Cores
# tmaptools::palette_explorer()
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

muni = "nat"
file_aux <- readRDS(sprintf("%s/hex_agregado_%s_08_2019.rds", subfolder14, muni))
file_aux2 <- readRDS(sprintf("%s/hex_agregado_%s_07_2019.rds", subfolder14, muni))%>%
  st_drop_geometry()

tt_total <- sum(file_aux$empregos_total)
st_total <- sum(file_aux$saude_total)
et_total <- sum(file_aux$edu_total)

file <- readRDS(sprintf("%s/acess_%s_08_2019.rds", subfolder17, muni))

data_plot <- st_join(file_aux, file, join=st_equals) # , 
data_plot2 <- st_join(file, file_aux)  # , join=st_equals

data_plot3 <- left_join(file_aux2, file, by = c("id_hex" = "origin"))

filter(data_plot, mode %in% c('walk', NA))

# definir breaks
# NA = "sem dados"
# filtrar NA

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

# Modelo
dplyr::filter(data_inset, mode %in% c(modo, NA))%>%
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
          breaks = c(0,20,40,60,80,100),
          #style = "pretty",
          palette = paleta, colorNA = paleta_NA, textNA = "Sem dados", 
          title = "Percentual de oportunidades",
          legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
  tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
  tm_facets(by = "CMA", nrow=1, free.coords = FALSE)+
  tm_legend(position = c('right','center'))




filename = "bho_oport"
tmap_save(tm=plot, height = 1000, height = 1000, units = 'px')


# ----- FUNCAO ACESSO OPORTUNIDADES -----
criar_mapas_acesso <- function(muni, formato){
  # 0.0 Configuracoes iniciais
  # Plotagem
  tmap_options(
    output.format = formato
  )
  
  oportunidades <- c('trabalho', 'saude', 'educacao')
  modes <- c('walk', 'bike', 'transit')
  # Cores
  paleta <- c('#38040E', '#F4442E', '#FFC145', '#05E4FF', '#6184D8') 
  paleta_NA <- '#DCDCDC'
  
  # Estrutura de pasta
  files_folder <- "../../indice-mobilidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolder17 <- sprintf("%s/17_acesso_oportunidades/2019", files_folder)
  subfolderXY <- sprintf("%s/XY_imagens/2019/Acessibilidade", files_folder)
  save_folder <- sprintf("%s/%s", subfolderXY, muni)
  
  dir.create(sprintf("%s", save_folder), recursive = TRUE, showWarnings = FALSE)
  
  
  # 1.0 Carrega arquivos
  # res 7
  file_aux_7 <- readRDS(sprintf("%s/hex_agregado_%s_07_2019.rds", subfolder14, muni))
  file_7 <- readRDS(sprintf("%s/acess_%s_07_2019.rds", subfolder17, muni))
  # res 8
  file_aux_8 <- readRDS(sprintf("%s/hex_agregado_%s_08_2019.rds", subfolder14, muni))
  file_8 <- readRDS(sprintf("%s/acess_%s_08_2019.rds", subfolder17, muni))
  
  tt_total <- sum(file_aux_7$empregos_total)
  st_total <- sum(file_aux_7$saude_total)
  et_total <- sum(file_aux_7$edu_total)
  
  data_plot_7 <- st_join(file_aux_7, file_7, join=st_equals)
  data_plot_8 <- st_join(file_aux_8, file_8, join=st_equals)
  
  # 2.0 Prepara bases
  # Trabalharemos com percentuais - RES 7
  # Trabalho
  data_plot_7$CMATT30_p <- (data_plot_7$CMATT30 / tt_total)*100
  data_plot_7$CMATT45_p <- (data_plot_7$CMATT45 / tt_total)*100
  data_plot_7$CMATT60_p <- (data_plot_7$CMATT60 / tt_total)*100
  # Saude
  data_plot_7$CMAST30_p <- (data_plot_7$CMAST30 / st_total)*100
  data_plot_7$CMAST45_p <- (data_plot_7$CMAST45 / st_total)*100
  data_plot_7$CMAST60_p <- (data_plot_7$CMAST60 / st_total)*100
  # Educacao
  data_plot_7$CMAET30_p <- (data_plot_7$CMAET30 / et_total)*100
  data_plot_7$CMAET45_p <- (data_plot_7$CMAET45 / et_total)*100
  data_plot_7$CMAET60_p <- (data_plot_7$CMAET60 / et_total)*100
  
  # Trabalharemos com percentuais - RES 8
  # Trabalho
  data_plot_8$CMATT15_p <- (data_plot_8$CMATT15 / tt_total)*100
  data_plot_8$CMATT30_p <- (data_plot_8$CMATT30 / tt_total)*100
  # Saude
  data_plot_8$CMAST15_p <- (data_plot_8$CMAST15 / st_total)*100
  data_plot_8$CMAST30_p <- (data_plot_8$CMAST30 / st_total)*100
  # Educacao
  data_plot_8$CMAET15_p <- (data_plot_8$CMAET15 / et_total)*100
  data_plot_8$CMAET30_p <- (data_plot_8$CMAET30 / et_total)*100
  
  # ativos
  #  data_ativos <- data_plot_8%>%
  #    dplyr::select(origin, mode, CMATT15_p, CMATT30_p)%>%
  #    pivot_longer(c('CMATT15_p', 'CMATT30_p'), names_to="CMA", values_to="Total")
  
  # motorizados
  #  data_motor <- data_plot_7%>%
  #    dplyr::select(origin, mode, CMATT30_p, CMATT45_p, CMATT60_p)%>%
  #    pivot_longer(c('CMATT30_p', 'CMATT45_p', 'CMATT60_p'), names_to="CMA", values_to="Total")
  
  
  # 3.0 Gera graficos e salva
  
  for (oport in oportunidades) {
    if (oport == "trabalho"){
      titulo_aux1 <- "ao trabalho"
      col_ativos <- c('origin', 'mode', 'CMATT15_p', 'CMATT30_p')
      pivot_ativos <- c('CMATT15_p','CMATT30_p')
      col_motor <- c('origin', 'mode', 'CMATT30_p', 'CMATT45_p', 'CMATT60_p')
      pivot_motor <- c('CMATT30_p', 'CMATT45_p', 'CMATT60_p')
    } else if (oport == "saude"){
      titulo_aux1 <- "?? sa??de"
      col_ativos <- c('origin', 'mode', 'CMAST15_p', 'CMAST30_p')
      pivot_ativos <- c('CMAST15_p','CMAST30_p')
      col_motor <- c('origin', 'mode', 'CMAST30_p', 'CMAST45_p', 'CMAST60_p')
      pivot_motor <- c('CMAST30_p', 'CMAST45_p', 'CMAST60_p')
    }else{
      titulo_aux1 <- "?? educa????o"
      col_ativos <- c('origin', 'mode', 'CMAET15_p', 'CMAET30_p')
      pivot_ativos <- c('CMAET15_p','CMAET30_p')
      col_motor <- c('origin', 'mode', 'CMAET30_p', 'CMAET45_p', 'CMAET60_p')
      pivot_motor <- c('CMAET30_p', 'CMAET45_p', 'CMAET60_p')
    }
    
    for (modo in modes){
      
      if (modo == "walk"){
        titulo_aux2 <- "a P??"
        titulo <- sprintf("Acesso %s - %s", titulo_aux1, titulo_aux2)
        panel_labels = c('At?? 15 minutos','At?? 30 minutos')
        dados <- data_plot_8%>%
          dplyr::select(col_ativos)%>%
          pivot_longer(pivot_ativos, names_to="CMA", values_to="Total")
      }
      else if (modo == "bike"){
        titulo_aux2 <- "Bicicleta"
        titulo <- sprintf("Acesso %s - %s", titulo_aux1, titulo_aux2)
        panel_labels = c('At?? 15 minutos','At?? 30 minutos')
        dados <- data_plot_8%>%
          dplyr::select(col_ativos)%>%
          pivot_longer(pivot_ativos, names_to="CMA", values_to="Total")
      }
      else {
        titulo_aux2 <- "??nibus"
        titulo <- sprintf("Acesso %s - %s", titulo_aux1, titulo_aux2)
        panel_labels = c('At?? 30 minutos','At?? 45 minutos','At?? 60 minutos')
        dados <- data_plot_7%>%
          dplyr::select(col_motor)%>%
          pivot_longer(pivot_motor, names_to="CMA", values_to="Total")
      }
      
      plot <- dplyr::filter(dados, mode %in% c(modo, NA))%>%
        tm_shape()+ 
        tm_layout(main.title = titulo, legend.outside = TRUE, frame = FALSE, 
                  panel.labels = panel_labels,
                  title.size = 0.8, legend.title.size = 1,
                  main.title.position = 'center',
                  frame.lwd = NA,
                  panel.label.bg.color = NA,
                  panel.label.size = 0.8)+
        tm_fill(col = "Total", 
                n=5,
                breaks = c(0,20,40,60,80,100),
                #style = "pretty",
                palette = paleta, colorNA = paleta_NA, textNA = "Sem dados", 
                title = "Percentual de oportunidades",
                legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
        tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
        tm_facets(by = "CMA", nrow=1, free.coords = FALSE)+
        tm_legend(position = c('right','center'))
      
      file_name <- sprintf("%s/acess_%s_%s_%s_2019.%s", save_folder, oport, modo, muni, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
    }
  }
}

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "man", "nat", "rec",
           "rio", "sne", "sjc", "spo", "ula")

for (muni in munis){
  criar_mapas_acesso(muni = muni, formato = "png")
}

criar_mapas_acesso(muni = "bho", formato = "png")

# ----- FUNCAO POPULACAO -----

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

muni = "ula"
file_aux <- readRDS(sprintf("%s/hex_agregado_%s_07_2019.rds", subfolder14, muni))

file$T15[i] <- ifelse(result$Time <= 900, 1, 0)
file_aux$perc_negra <- ifelse(file_aux$pop_total > 0, (file_aux$cor_negra/file_aux$pop_total)*100,0)
file_aux$perc_negra <- (file_aux$cor_negra/file_aux$pop_total)*100
file_aux$perc_pop <- (file_aux$pop_total/sum(file_aux$pop_total))*100

tmap_save(tm=plot, height = 1600, units = 'px')

# Distribuicao de Populacao

# pop total
plot <- tm_shape(file_aux) + 
  tm_layout(main.title = 'Distribui????o da popula????o res8', legend.outside = TRUE, frame = FALSE, 
            #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
            title.size = 1, legend.title.size = 3)+
  #tm_polygons(title = "Oportunidades em 15 minutos")+
  tm_fill(col = "pop_total",
          n=5,
          #breaks = breaks,
          style = "equal",
          palette = paleta, colorNA = paleta_NA, textNA = "Sem dados", 
          title = "Num. de habitantes",
          legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
  tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))#+
#tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
#tm_borders(alpha = 1)

# percentual 
tm_shape(file_aux) + 
  tm_layout(main.title = 'Distribui????o da popula????o', legend.outside = TRUE, frame = FALSE, 
            #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
            title.size = 1, legend.title.size = 3)+
  #tm_polygons(title = "Oportunidades em 15 minutos")+
  tm_fill(col = "perc_pop",
          n=5,
          #breaks = breaks,
          style = "equal",
          palette = paleta, colorNA = paleta_NA, textNA = "Sem dados", 
          title = "Percentual da popula????o",
          legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
  tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))#+
#tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
#tm_borders(alpha = 1)


tm_shape(file_aux) + 
  tm_layout(main.title = 'Distribui????o da popula????o negra', legend.outside = TRUE, frame = FALSE, 
            #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
            title.size = 1, legend.title.size = 3)+
  #tm_polygons(title = "Oportunidades em 15 minutos")+
  tm_fill(col = "perc_negra",
          n=5,
          breaks = c(0,20,40,60,80,100),
          #style = "equal",
          palette = paleta, colorNA = paleta_NA, textNA = "Sem popula????o", 
          title = "% de popula????o negra",
          legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
  tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))#+
#tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
#tm_borders(alpha = 1)

pop_total <- sum(file_aux$pop_total)

round(pop_total/5)
br <- round_any(pop_total/5, 100, f = ceiling)
breaks <- c(0, br, br*2, br*3, br*4, br*5)

criar_mapas_populacao <- function(muni, formato, res){
  # 0.0 Configuracoes iniciais
  # Plotagem
  tmap_options(
    output.format = formato
  )
  # Cores
  paleta <- c('#38040E', '#F4442E', '#FFC145', '#05E4FF', '#6184D8') 
  paleta_NA <- '#DCDCDC'
  
  # Estrutura de pasta
  files_folder <- "../../indice-mobilidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolderXY <- sprintf("%s/XY_imagens/2019/Populacao", files_folder)
  save_folder <- sprintf("%s/%s", subfolderXY, muni)
  
  # !! adicionar um condicional
  dir.create(sprintf("%s", save_folder), recursive = TRUE, showWarnings = FALSE)
  
  # 1.0 Carrega arquivos
  file <- readRDS(sprintf("%s/hex_agregado_%s_%s_2019.rds", subfolder14, muni, res))
  
  file$perc_negra <- (file$cor_negra/file$pop_total)*100 
  
  graficos <- c("total", "negra")
  
  # 3.0 Gera graficos e salva
  
  for (grafico in graficos) {
    if (grafico == "total"){
      main_title <- 'Distribui????o da popula????o'        # main_title em tm_layout
      coluna <- "pop_total"                       # col em tm_fill
      subtitulo <- "N??mero de habitantes por hex??gono"   # title em tm_fill
      estilo <- "equal"                                # style em tm_fill
      
      plot <- tm_shape(file) + 
        tm_layout(main.title = main_title, legend.outside = TRUE, frame = FALSE, 
                  #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
                  title.size = 1, legend.title.size = 3)+
        tm_fill(col = coluna, 
                n=5,
                #breaks = breaks,
                style = estilo,
                palette = paleta, colorNA = paleta_NA, textNA = "Sem popula????o", 
                title = subtitulo,
                legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
        tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))#+
      #tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
      #tm_borders(alpha = 1)
      
      file_name <- sprintf("%s/pop_%s_%s_%s_2019.%s", save_folder, grafico, muni, res, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
      
    }else{
      main_title <- "Distribui????o da popula????o negra"
      coluna <- "perc_negra"
      subtitulo <- "Percentual de popula????o negra por hex??gono"
      estilo <- "equal"
      breaks = c(0,20,40,60,80,100)
      
      plot <- tm_shape(file) + 
        tm_layout(main.title = main_title, legend.outside = TRUE, frame = FALSE, 
                  #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
                  title.size = 1, legend.title.size = 3)+
        tm_fill(col = coluna, 
                n=5,
                breaks = breaks,
                style = 'fixed',
                palette = paleta, colorNA = paleta_NA, textNA = "Sem popula????o", 
                title = subtitulo,
                legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
        tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))#+
      #tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
      #tm_borders(alpha = 1)
      
      file_name <- sprintf("%s/pop_%s_%s_%s_2019.%s", save_folder, grafico, muni, res, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
    }
  
  }
}

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "man", "nat", "rec",
           "rio", "sne", "sjc", "spo", "ula", "tsa", "vta", "jpa")

for (muni in munis){
  criar_mapas_populacao(muni = muni, res = "08", formato = "png")
}

criar_mapas_populacao(muni = "rio", res = "07", formato = "png")

# ---- FUNCAO OPORTUNIDADES
files_folder <- "../../indice-mobilidade_dados"
subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)

muni = "nat"
file_aux <- readRDS(sprintf("%s/hex_agregado_%s_08_2019.rds", subfolder14, muni))
file_aux$perc_negra <- (file_aux$cor_negra/file_aux$pop_total)*100
file_aux$perc_pop <- (file_aux$pop_total/sum(file_aux$pop_total))*100


# saude total
tm_shape(file_aux) + 
  tm_layout(main.title = 'Distribui????o da saude res8', legend.outside = TRUE, frame = FALSE, 
            #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
            title.size = 1, legend.title.size = 3)+
  #tm_polygons(title = "Oportunidades em 15 minutos")+
  tm_fill(col = "saude_total",
          n=5,
          #breaks = c(0,1,5,10,20,70),
          style = "order",
          palette = paleta, colorNA = paleta_NA, textNA = "Sem dados", 
          title = "Num. de estabs saude",
          legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
  tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))#+
#tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
#tm_borders(alpha = 1)

# edu total
tm_shape(file_aux) + 
  tm_layout(main.title = 'Distribui????o da educacao res8', legend.outside = TRUE, frame = FALSE, 
            #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
            title.size = 1, legend.title.size = 3)+
  #tm_polygons(title = "Oportunidades em 15 minutos")+
  tm_fill(col = "edu_total",
          n=5,
          #breaks = c(0,1,5,10,20,70),
          style = "order",
          palette = paleta, colorNA = paleta_NA, textNA = "Sem dados", 
          title = "Num. de estabs edu",
          legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
  tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))#+
#tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
#tm_borders(alpha = 1)

# trab total
tm_shape(file_aux) + 
  tm_layout(main.title = 'Distribui????o do trabalho res8', legend.outside = TRUE, frame = FALSE, 
            #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
            title.size = 1, legend.title.size = 3)+
  #tm_polygons(title = "Oportunidades em 15 minutos")+
  tm_fill(col = "empregos_total",
          n=5,
          #breaks = c(0,1,5,10,20,70),
          style = "order",
          palette = paleta, colorNA = paleta_NA, textNA = "Sem dados", 
          title = "Num. de empregos",
          legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
  tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))#+
#tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
#tm_borders(alpha = 1)


criar_mapas_oportunidades <- function(muni, formato, res){
  # 0.0 Configuracoes iniciais
  # Plotagem
  tmap_options(
    output.format = formato
  )
  
  oportunidades <- c('trabalho', 'saude', 'educacao')
  #modes <- c('walk', 'bike', 'transit')
  # Cores
  paleta <- c('#38040E', '#F4442E', '#FFC145', '#05E4FF', '#6184D8') 
  paleta_NA <- '#FCF7F8'
  
  # Estrutura de pasta
  files_folder <- "../../indice-mobilidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolderXY <- sprintf("%s/XY_imagens/2019/Oportunidades", files_folder)
  save_folder <- sprintf("%s/%s", subfolderXY, muni)
  
  # !! adicionar um condicional
  dir.create(sprintf("%s", save_folder), recursive = TRUE, showWarnings = FALSE)
  
  # 1.0 Carrega arquivos
  file <- readRDS(sprintf("%s/hex_agregado_%s_%s_2019.rds", subfolder14, muni, res))

  tt_total <- sum(file$empregos_total)
  st_total <- sum(file$saude_total)
  et_total <- sum(file$edu_total)

  # 3.0 Gera graficos e salva
  
  for (oport in oportunidades) {
    if (oport == "trabalho"){
      main_title <- 'Oportunidades de trabalho'        # main_title em tm_layout
      coluna <- "empregos_total"                       # col em tm_fill
      subtitulo <- "N??mero de empregos por hex??gono"   # title em tm_fill
      estilo <- "equal"                                # style em tm_fill
    } else if (oport == "saude"){
      main_title <- "Oportunidades de sa??de"
      coluna <- "saude_total"
      subtitulo <- "N??mero de estabelecimentos de sa??de"
      estilo <- "order"
    }else{
      main_title <- "Oportunidades de educa????o"
      coluna <- "edu_total"
      subtitulo <- "N??mero de estabelecimentos de educa????o"
      estilo <- "order"

    }
    
  plot <- tm_shape(file_aux) + 
      tm_layout(main.title = main_title, legend.outside = TRUE, frame = FALSE, 
                #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
                title.size = 1, legend.title.size = 3)+
      tm_fill(col = coluna, 
              n=5,
              #breaks = c(0,1,5,10,20,70),
              style = estilo,
              palette = paleta, colorNA = paleta_NA, textNA = "Sem dados", 
              title = subtitulo,
              legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
      tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))#+
    #tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
    #tm_borders(alpha = 1)
      
      file_name <- sprintf("%s/oport_%s_%s_%s_2019.%s", save_folder, oport, muni, res, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
    
  }
}

criar_mapas_oportunidades(muni = "bho", res = "07", formato = "png")
