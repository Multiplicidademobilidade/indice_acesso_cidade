# Esse script é utilizado para gerar os mapas utilizados no relatorio

# Setup
source('fun/setup.R')

# Funcoes:
# criar_mapas_acesso <- linha 23
# criar_mapas_oportunidades <- linha 328
# Criar mapas populacao <- linha 513

# Cores e configuracoes
tmaptools::palette_explorer()
paleta <- c('#38040E', '#F4442E', '#FFC145', '#05E4FF', '#6184D8') 
paleta_NA <- '#DCDCDC'

tmap_options(
  output.format = "png",
  output.dpi = 500
)


# ----- FUNCAO ACESSO OPORTUNIDADES -----

# A funcao esta modificada para gerar mapas do automovel

mapas_acessibilidade <- function(muni, formato, modo, oport){
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
  subfolder17 <- sprintf("%s/17_acesso_oportunidades/2019", files_folder)
  subfolderXY <- sprintf("%s/XY_imagens/2019/Acessibilidade", files_folder)
  save_folder <- sprintf("%s/%s", subfolderXY, muni)
  
  dir.create(sprintf("%s", save_folder), recursive = TRUE, showWarnings = FALSE)
  
  if (modo %in% c('onibus', 'carro_compart')){
    # res 7
    file_aux_7 <- readRDS(sprintf("%s/hex_agregado_%s_07_2019.rds", subfolder14, muni))
    file_7 <- readRDS(sprintf("%s/acess_07_%s_carro_compart_2019.rds", subfolder17, muni))
    data_plot_7 <- st_join(file_aux_7, file_7, join=st_equals)
  } else {
    # res 8
    file_aux_8 <- readRDS(sprintf("%s/hex_agregado_%s_08_2019.rds", subfolder14, muni))
    file_8 <- readRDS(sprintf("%s/acess_08_%s_modos_ativos_2019.rds", subfolder17, muni))
    data_plot_8 <- st_join(file_aux_8, file_8, join=st_equals)
  }
  
  
  tt_total <- sum(file_aux_7$empregos_total)
  st_total <- sum(file_aux_7$saude_total)
  et_total <- sum(file_aux_7$edu_total)
  
  
  
  
}

criar_mapas_acesso <- function(muni, formato){
  # Classifica o acesso em fatores:
  # 0
  # até 1
  # 1 a 5
  # 6 a 10
  # mais de 10
  
  # ATENCAO: MODIFICADO PARA O CARRO
  
  # 0.0 Configuracoes iniciais
  # Plotagem
  tmap_options(
    output.format = formato
  )
  
  oportunidades <- c('trabalho', 'saude', 'educacao')
  modes <- c('shared_car', 'walk', 'bike') # Fazendo para o auto
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
  file_7 <- readRDS(sprintf("%s/acess_07_%s_carro_compart_2019.rds", subfolder17, muni))
  # res 8
  file_aux_8 <- readRDS(sprintf("%s/hex_agregado_%s_08_2019.rds", subfolder14, muni))
  file_8 <- readRDS(sprintf("%s/acess_08_%s_modos_ativos_2019.rds", subfolder17, muni))
  
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
  
  # transforma acesso a saude e educacao em fatores
  cmaf_labels <- c('Zero', '1', '2 a 5', '6 a 10', '10 ou mais')
  # RES 8
  data_plot_8$CMAST15_f <-  with(data_plot_8, ifelse(CMAST15 == 0, 1,
                                                     ifelse(between(CMAST15, 0, 1), 2,
                                                            ifelse(between(CMAST15,2,5), 3,
                                                                   ifelse(between(CMAST15,6,10), 4, 5)))))
  #cmaf_labels <- c('Zero', '1', '2 a 5', '6 a 10', '10 ou mais')
  data_plot_8$CMAST15_f <- factor(data_plot_8$CMAST15_f, levels = 1:5, labels = cmaf_labels)
  #
  data_plot_8$CMAST30_f <-  with(data_plot_8, ifelse(CMAST30 == 0, 1,
                                                     ifelse(between(CMAST30, 0, 1), 2,
                                                            ifelse(between(CMAST30,2,5), 3,
                                                                   ifelse(between(CMAST30,6,10), 4, 5)))))
  #cmaf_labels <- c('Zero', '1', '2 a 5', '6 a 10', '10 ou mais')
  data_plot_8$CMAST30_f <- factor(data_plot_8$CMAST30_f, levels = 1:5, labels = cmaf_labels)
  #
  data_plot_8$CMAET15_f <-  with(data_plot_8, ifelse(CMAET15 == 0, 1,
                                                     ifelse(between(CMAET15, 0, 1), 2,
                                                            ifelse(between(CMAET15,2,5), 3,
                                                                   ifelse(between(CMAET15,6,10), 4, 5)))))
  #cmaf_labels <- c('Zero', '1', '2 a 5', '6 a 10', '10 ou mais')
  data_plot_8$CMAET15_f <- factor(data_plot_8$CMAET15_f, levels = 1:5, labels = cmaf_labels)
  #
  data_plot_8$CMAET30_f <-  with(data_plot_8, ifelse(CMAET30 == 0, 1,
                                                     ifelse(between(CMAET30, 0, 1), 2,
                                                            ifelse(between(CMAET30,2,5), 3,
                                                                   ifelse(between(CMAET30,6,10), 4, 5)))))
#cmaf_labels <- c('Zero', '1', '2 a 5', '6 a 10', '10 ou mais')
  data_plot_8$CMAET30_f <- factor(data_plot_8$CMAET30_f, levels = 1:5, labels = cmaf_labels)
  # RES 7
  #
  data_plot_7$CMAST30_f <-  with(data_plot_7, ifelse(CMAST30 == 0, 1,
                                                     ifelse(between(CMAST30, 0, 1), 2,
                                                            ifelse(between(CMAST30,2,5), 3,
                                                                   ifelse(between(CMAST30,6,10), 4, 5)))))
  #cmaf_labels <- c('Zero', '1', '2 a 5', '6 a 10', '10 ou mais')
  data_plot_7$CMAST30_f <- factor(data_plot_7$CMAST30_f, levels = 1:5, labels = cmaf_labels)
  #
  data_plot_7$CMAST45_f <-  with(data_plot_7, ifelse(CMAST45 == 0, 1,
                                                     ifelse(between(CMAST45, 0, 1), 2,
                                                            ifelse(between(CMAST45,2,5), 3,
                                                                   ifelse(between(CMAST45,6,10), 4, 5)))))
  #cmaf_labels <- c('Zero', '1', '2 a 5', '6 a 10', '10 ou mais')
  data_plot_7$CMAST45_f <- factor(data_plot_7$CMAST45_f, levels = 1:5, labels = cmaf_labels)
  #
  data_plot_7$CMAST60_f <-  with(data_plot_7, ifelse(CMAST60 == 0, 1,
                                                     ifelse(between(CMAST60, 0, 1), 2,
                                                            ifelse(between(CMAST60,2,5), 3,
                                                                   ifelse(between(CMAST60,6,10), 4, 5)))))
  #cmaf_labels <- c('Zero', '1', '2 a 5', '6 a 10', '10 ou mais')
  data_plot_7$CMAST60_f <- factor(data_plot_7$CMAST60_f, levels = 1:5, labels = cmaf_labels)
  #
  data_plot_7$CMAET30_f <-  with(data_plot_7, ifelse(CMAET30 == 0, 1,
                                                     ifelse(between(CMAET30, 0, 1), 2,
                                                            ifelse(between(CMAET30,2,5), 3,
                                                                   ifelse(between(CMAET30,6,10), 4, 5)))))
  #cmaf_labels <- c('Zero', '1', '2 a 5', '6 a 10', '10 ou mais')
  data_plot_7$CMAET30_f <- factor(data_plot_7$CMAET30_f, levels = 1:5, labels = cmaf_labels)
  #
  data_plot_7$CMAET45_f <-  with(data_plot_7, ifelse(CMAET45 == 0, 1,
                                                     ifelse(between(CMAET45, 0, 1), 2,
                                                            ifelse(between(CMAET45,2,5), 3,
                                                                   ifelse(between(CMAET45,6,10), 4, 5)))))
  #cmaf_labels <- c('Zero', '1', '2 a 5', '6 a 10', '10 ou mais')
  data_plot_7$CMAET45_f <- factor(data_plot_7$CMAET45_f, levels = 1:5, labels = cmaf_labels)
  #
  data_plot_7$CMAET60_f <-  with(data_plot_7, ifelse(CMAET60 == 0, 1,
                                                     ifelse(between(CMAET60, 0, 1), 2,
                                                            ifelse(between(CMAET60,2,5), 3,
                                                                   ifelse(between(CMAET60,6,10), 4, 5)))))
  #cmaf_labels <- c('Zero', '1', '2 a 5', '6 a 10', '10 ou mais')
  data_plot_7$CMAET60_f <- factor(data_plot_7$CMAET60_f, levels = 1:5, labels = cmaf_labels)

  
  # ativos
    data_ativos <- data_plot_8%>%
      dplyr::select(origin, mode, CMATT15_p, CMATT30_p)%>%
      pivot_longer(c('CMATT15_p', 'CMATT30_p'), names_to="CMA", values_to="Total")
  
  # motorizados
    data_motor <- data_plot_7%>%
      dplyr::select(origin, mode, CMATT30_p, CMATT45_p, CMATT60_p)%>%
      pivot_longer(c('CMATT30_p', 'CMATT45_p', 'CMATT60_p'), names_to="CMA", values_to="Total")
  
  
  # 3.0 Gera graficos e salva
  
  for (oport in oportunidades) {
    if (oport == "trabalho"){
      titulo_aux1 <- "ao trabalho"
      col_ativos <- c('origin', 'mode', 'CMATT15_p', 'CMATT30_p')
      pivot_ativos <- c('CMATT15_p','CMATT30_p')
      col_motor <- c('origin', 'mode', 'CMATT30_p', 'CMATT45_p', 'CMATT60_p')
      pivot_motor <- c('CMATT30_p', 'CMATT45_p', 'CMATT60_p')
      style <- 'fixed'
      breaks <-  c(0,20,40,60,80,100)
    } else if (oport == "saude"){
      titulo_aux1 <- "à saúde"
      col_ativos <- c('origin', 'mode', 'CMAST15_f', 'CMAST30_f')
      pivot_ativos <- c('CMAST15_f','CMAST30_f')
      col_motor <- c('origin', 'mode', 'CMAST30_f', 'CMAST45_f', 'CMAST60_f')
      pivot_motor <- c('CMAST30_f', 'CMAST45_f', 'CMAST60_f')
      style <- 'cat'
      breaks <- NULL
    }else{
      titulo_aux1 <- "à educação"
      col_ativos <- c('origin', 'mode', 'CMAET15_f', 'CMAET30_f')
      pivot_ativos <- c('CMAET15_f','CMAET30_f')
      col_motor <- c('origin', 'mode', 'CMAET30_f', 'CMAET45_f', 'CMAET60_f')
      pivot_motor <- c('CMAET30_f', 'CMAET45_f', 'CMAET60_f')
      style <- 'cat'
      breaks <- NULL
    }
    
    for (modo in modes){
      
      if (modo == "walk"){
        titulo_aux2 <- "a Pé"
        titulo <- sprintf("Acesso %s - %s", titulo_aux1, titulo_aux2)
        panel_labels = c('Até 15 minutos','Até 30 minutos')
        dados <- data_plot_8%>%
          dplyr::select(col_ativos)%>%
          pivot_longer(pivot_ativos, names_to="CMA", values_to="Total")
      }
      else if (modo == "bike"){
        titulo_aux2 <- "Bicicleta"
        titulo <- sprintf("Acesso %s - %s", titulo_aux1, titulo_aux2)
        panel_labels = c('Até 15 minutos','Até 30 minutos')
        dados <- data_plot_8%>%
          dplyr::select(col_ativos)%>%
          pivot_longer(pivot_ativos, names_to="CMA", values_to="Total")
      }
      else if (modo == 'shared_car'){
        titulo_aux2 <- "Carro compartilhado"
        titulo <- sprintf("Acesso %s - %s", titulo_aux1, titulo_aux2)
        panel_labels = c('Até 30 minutos','Até 45 minutos','Até 60 minutos')
        dados <- data_plot_7%>%
          dplyr::select(col_motor)%>%
          pivot_longer(pivot_motor, names_to="CMA", values_to="Total")
      } else{
        titulo_aux2 <- "Ônibus"
        titulo <- sprintf("Acesso %s - %s", titulo_aux1, titulo_aux2)
        panel_labels = c('Até 30 minutos','Até 45 minutos','Até 60 minutos')
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
                breaks = breaks, #c(0,20,40,60,80,100),
                style = style,
                palette = 'plasma', colorNA = paleta_NA, textNA = "Sem dados", 
                title = "Estabelecimentos acessíveis",
                legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
        tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
        tm_facets(by = "CMA", nrow=1, free.coords = FALSE)+
        tm_legend(position = c('right','center'))
      
      file_name <- sprintf("%s/acess3_%s_%s_%s_2019.%s", save_folder, oport, modo, muni, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
    }
  }
}

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "man", "nat", "rec",
           "rio", "sne", "sjc", "spo", "ula")

munis <- c('jpa', 'tsa', 'vta')

for (muni in munis){
  criar_mapas_acesso(muni = muni, formato = "png")
}

criar_mapas_acesso(muni = "vta", formato = "png")

# ----- FUNCAO POPULACAO -----

# Cores
paleta <- c('#38040E', '#F4442E', '#FFC145', '#05E4FF', '#6184D8') 
paleta_NA <- '#DCDCDC'

tmap_options(
  output.format = "png",
  output.dpi = 500
)

tmap_mode('plot')

# Dados
files_folder <- "../../indice-mobilidade_dados"
subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)

muni = "cgr"
file_aux <- readRDS(sprintf("%s/hex_agregado_%s_08_2019.rds", subfolder14, muni))

file_aux$pop_total <- ifelse(file_aux$pop_total > 0, file_aux$pop_total, NA)
file_aux$cor_negra <- ifelse(file_aux$cor_negra > 0, file_aux$pop_total, NA)

quantile(file_aux$cor_negra, na.rm=TRUE)

file$T15[i] <- ifelse(result$Time <= 900, 1, 0)
file_aux$perc_negra <- ifelse(file_aux$pop_total > 0, (file_aux$cor_negra/file_aux$pop_total)*100,0)
file_aux$perc_negra <- (file_aux$cor_negra/file_aux$pop_total)*100
file_aux$perc_pop <- (file_aux$pop_total/sum(file_aux$pop_total))*100

tmap_save(tm=plot, height = 1600, units = 'px')

# Distribuicao de Populacao

# pop total
tm_shape(file_aux) + 
  tm_layout(main.title = 'Distribuicao da populacao negra', legend.outside = TRUE, frame = FALSE, 
            #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
            title.size = 1, legend.title.size = 3)+
  #tm_polygons(title = "Oportunidades em 15 minutos")+
  tm_fill(col = "pop_total",
          n=4,
          #breaks = breaks,
          style = "jenks",
          palette = 'Oranges', colorNA = paleta_NA, textNA = "Sem populacao", 
          title = "Num. de habitantes - quartil",
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
  
  file$pop_total <- ifelse(file$pop_total > 0, file$pop_total, NA)
  file$perc_negra <- (file$cor_negra/file$pop_total)*100 
  
  graficos <- c("total", "negra")
  
  # 3.0 Gera graficos e salva
  
  for (grafico in graficos) {
    if (grafico == "total"){
      main_title <- 'Distribuição da população'        # main_title em tm_layout
      coluna <- "pop_total"                       # col em tm_fill
      subtitulo <- "Número de habitantes por hexágono"   # title em tm_fill
      estilo <- "quantile"                                # style em tm_fill
      
      plot <- tm_shape(file) + 
        tm_layout(main.title = main_title, legend.outside = TRUE, frame = FALSE, 
                  #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
                  title.size = 1, legend.title.size = 3)+
        tm_fill(col = coluna, 
                n=4,
                #breaks = breaks,
                style = estilo,
                palette = 'Greys', colorNA = paleta_NA, textNA = "Sem população", 
                title = subtitulo,
                legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
        tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))#+
      #tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
      #tm_borders(alpha = 1)
      
      file_name <- sprintf("%s/pop2_%s_%s_%s_2019.%s", save_folder, grafico, muni, res, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
      
    }else{
      main_title <- "Distribuição da população negra"
      coluna <- "perc_negra"
      subtitulo <- "Percentual de população negra por hexágono"
      estilo <- "equal"
      breaks = c(0,20,40,60,80,100)
      
      plot <- tm_shape(file) + 
        tm_layout(main.title = main_title, legend.outside = TRUE, frame = FALSE, 
                  #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
                  title.size = 1, legend.title.size = 3)+
        tm_fill(col = coluna, 
                n=4,
                breaks = breaks,
                style = 'quantile',
                palette = paleta, colorNA = paleta_NA, textNA = "Sem população", 
                title = subtitulo,
                legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
        tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))#+
      #tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
      #tm_borders(alpha = 1)
      
      file_name <- sprintf("%s/pop2_%s_%s_%s_2019.%s", save_folder, grafico, muni, res, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
    }
  
  }
}

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "man", "nat", "rec",
           "rio", "sne", "sjc", "spo", "ula", "tsa", "vta", "jpa")

munis <- 'cam'

for (muni in munis){
  criar_mapas_populacao(muni = muni, res = "08", formato = "png")
}

criar_mapas_populacao(muni = "spo", res = "08", formato = "png")

# ----- FUNCAO OPORTUNIDADES -----

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
  paleta_NA <- '#DCDCDC'
  
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
  
  file$empregos_total <- ifelse(file$empregos_total > 0, file$empregos_total, NA)
  file$saude_total <- ifelse(file$saude_total > 0, file$saude_total, NA)
  file$edu_total <- ifelse(file$edu_total > 0, file$edu_total, NA)

  # 3.0 Gera graficos e salva
  
  for (oport in oportunidades) {
    if (oport == "trabalho"){
      main_title <- 'Oportunidades de trabalho'        # main_title em tm_layout
      coluna <- "empregos_total"                       # col em tm_fill
      subtitulo <- "Total de empregos por hexágono"   # title em tm_fill
      estilo <- "jenks"                                # style em tm_fill
    } else if (oport == "saude"){
      main_title <- "Oportunidades de saúde"
      coluna <- "saude_total"
      subtitulo <- "Número de estabelecimentos de saúde"
      estilo <- "jenks"
    }else{
      main_title <- "Oportunidades de educação"
      coluna <- "edu_total"
      subtitulo <- "Número de estabelecimentos de educação"
      estilo <- "jenks"

    }
    
  plot <- tm_shape(file) + 
      tm_layout(main.title = main_title, legend.outside = TRUE, frame = FALSE, 
                #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
                title.size = 1, legend.title.size = 3)+
      tm_fill(col = coluna, 
              n=5,
              #breaks = c(0,1,5,10,20,70),
              style = estilo,
              palette = paleta, colorNA = paleta_NA, textNA = "Zero", 
              title = subtitulo,
              legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
      tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))#+
    #tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
    #tm_borders(alpha = 1)
      
      file_name <- sprintf("%s/oport_%s_%s_%s_2019.%s", save_folder, oport, muni, res, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
    
  }
}

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "man", "nat", "rec",
           "rio", "sne", "sjc", "spo", "ula")

for (muni in munis){
  criar_mapas_oportunidades(muni = muni, res = '08', formato = "png")
}

criar_mapas_oportunidades(muni = "rec", res = "08", formato = "png")


# ----- TESTES FORA DA FUNCAO -----

# Cores
tmaptools::palette_explorer()
paleta <- c('#38040E', '#F4442E', '#FFC145', '#05E4FF', '#6184D8') 
paleta_NA <- '#DCDCDC'

tmap_options(
  output.format = "png",
  output.dpi = 500
)

# Dados
files_folder <- "../../indice-mobilidade_dados"
subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
subfolder17 <- sprintf("%s/17_acesso_oportunidades/2019", files_folder)

muni = "nat"
file_aux <- readRDS(sprintf("%s/hex_agregado_%s_08_2019.rds", subfolder14, muni))%>%
  st_drop_geometry()
file_aux2 <- readRDS(sprintf("%s/hex_agregado_%s_07_2019.rds", subfolder14, muni))%>%
  st_drop_geometry()

#tt_total <- sum(file_aux$empregos_total)
#st_total <- sum(file_aux$saude_total)
#et_total <- sum(file_aux$edu_total)

file <- readRDS(sprintf("%s/acess_07_%s_carro_compart_2019.rds", subfolder17, muni))
file2 <- readRDS(sprintf("%s/acess_07_%s_onibus_2019.rds", subfolder17, muni))
file2 <- readRDS(sprintf("%s/acess_08_%s_modos_ativos_2019.rds", subfolder17, muni))

data_plot <- st_join(file_aux, file, join=st_equals) # , 
#data_plot2 <- st_join(file, file_aux)  # , join=st_equals

data_plot <- left_join(file_aux, file2, by = c("id_hex" = "origin"))

data_plot <- filter(data_plot, mode %in% c('walk', NA))

data_plot$CMAST15_f <-  with(data_plot, ifelse(CMAST15 == 0, 1,
                                               ifelse(between(CMAST15, 0, 1), 2,
                                                      ifelse(between(CMAST15,2,5), 3,
                                                             ifelse(between(CMAST15,6,10), 4, 5)))))
cmaf_labels <- c('Zero', 'Até 1', '2 a 5', '6 a 10', '10 ou mais')
data_plot$CMAST15_f <- factor(data_plot$CMAST15_f, levels = 1:5, labels = cmaf_labels)

tmap_mode('plot')

data_plot$geom <- h3_to_polygon(data_plot$id_hex)

dplyr::filter(st_as_sf(data_plot), mode %in% c('bike'))%>%
  tm_shape()+ 
  tm_layout(main.title = 'Acesso à saúde até 15 minutos - bicicleta', legend.outside = TRUE, frame = FALSE, 
            #panel.labels = panel_labels,
            title.size = 0.8, legend.title.size = 1,
            main.title.position = 'center',
            frame.lwd = NA,
            panel.label.bg.color = NA,
            panel.label.size = 0.8)+
  tm_fill(col = "CMAST30", 
          n=9,
          #breaks = c(0,20,40,60,80,100),
          style = 'quantile', #"cat",
          palette = 'plasma', colorNA = paleta_NA, textNA = "Sem dados", 
          title = "Número de estabelecimentos",
          legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
  tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
  #tm_facets(by = "CMA", nrow=1, free.coords = FALSE)+
  tm_legend(position = c('right','center'))

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
          palette = 'plasma', colorNA = paleta_NA, textNA = "Sem dados", 
          title = "Percentual de oportunidades",
          legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
  tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
  tm_facets(by = "CMA", nrow=1, free.coords = FALSE)+
  tm_legend(position = c('right','center'))




filename = "bho_oport"
tmap_save(tm=plot, height = 1000, height = 1000, units = 'px')
