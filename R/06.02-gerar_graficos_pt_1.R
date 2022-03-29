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

# ----- FUNCAO POPULACAO -----

criar_mapas_populacao <- function(muni, formato, res){
  # 0.0 Configuracoes iniciais
  # Plotagem
  tmap_options(
    output.format = formato,
    output.dpi = 500
  )
  # Cores
  paleta <- c('#e7eff0', '#b6ced1', '#86adb2', '#568c93', '#0d5b65') 
  paleta_NA <- '#808080'
  
  # Estrutura de pasta
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolder19 <- sprintf("%s/19_mapas_acessibilidades/2019/Populacao", files_folder)
  save_folder <- sprintf("%s/%s", subfolder19, muni)
  
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
      main_title <- 'População'        # main_title em tm_layout
      coluna <- "pop_total"                       # col em tm_fill
      subtitulo <- "Número de habitantes"   # title em tm_fill
      estilo <- "quantile"                                # style em tm_fill
      
      plot <- tm_shape(file) + 
        tm_layout(main.title = main_title, legend.outside = FALSE, frame = FALSE, 
                  #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
                  title.size = 1, legend.title.size = 1,
                  inner.margins = c(0.02, 0.02, 0.02, 0.3))+
        tm_fill(col = coluna, 
                n=4,
                #breaks = breaks,
                style = estilo,
                palette = paleta, colorNA = paleta_NA, textNA = "Sem população", 
                title = subtitulo,
                legend.format = list(text.separator="a", scientific=TRUE, format="f"),
                legend.position = c('left','center'))+ # scientific e interessante
        tm_scale_bar(breaks = c(0, 2, 4), text.size = .5)#, position=c("right", "bottom"))#+
      #tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
      #tm_borders(alpha = 1)
      
      file_name <- sprintf("%s/pop2_%s_%s_%s_2019.%s", save_folder, grafico, muni, res, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
      
    }else{
      main_title <- "População negra"
      coluna <- "perc_negra"
      subtitulo <- "Percentual por hexágono"
      estilo <- "equal"
      breaks = c(0,20,40,60,80,100)
      
      plot <- tm_shape(file) + 
        tm_layout(main.title = main_title, legend.outside = FALSE, frame = FALSE, 
                  #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
                  title.size = 1, legend.title.size = 1,
                  inner.margins = c(0.02, 0.02, 0.02, 0.3))+
        tm_fill(col = coluna, 
                n=4,
                breaks = breaks,
                style = 'quantile',
                palette = paleta, colorNA = paleta_NA, textNA = "Sem população", 
                title = subtitulo,
                legend.format = list(text.separator="a", scientific=TRUE, format="f"),
                legend.position = c('left','center'))+ # scientific e interessante
        tm_scale_bar(breaks = c(0, 2, 4), text.size = .5)#, position=c("right", "bottom"))#+
      #tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
      #tm_borders(alpha = 1)
      
      file_name <- sprintf("%s/pop3_%s_%s_%s_2019.%s", save_folder, grafico, muni, res, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
    }
    
  }
}

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "man", "nat", "rec",
           "rio", "sne", "sjc", "spo", "ula", "tsa", "vta", "jpa")

munis <- 'bho'


for (muni in munis){
  criar_mapas_populacao(muni = muni, res = "08", formato = "png")
}

criar_mapas_quantil <- function(muni, res = '08', formato = 'png'){
  # 0.0 Configuracoes iniciais
  # Plotagem
  tmap_options(
    output.format = formato,
    output.dpi = 500
  )
  # Cores
  paleta <- c('#e7eff0', '#b6ced1', '#86adb2', '#568c93', '#0d5b65') 
  paleta_NA <- '#808080'
  
  # Estrutura de pasta
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolder19 <- sprintf("%s/19_mapas_acessibilidades/2019/Populacao", files_folder)
  save_folder <- sprintf("%s/%s", subfolder19, muni)
  
  # !! adicionar um condicional
  dir.create(sprintf("%s", save_folder), recursive = TRUE, showWarnings = FALSE)
  
  # 1.0 Carrega arquivos
  file <- readRDS(sprintf("%s/hex_agregado_%s_%s_2019.rds", subfolder14, muni, res))
  
  file <- file %>% mutate(perc_pop_negra = cor_negra / pop_total)
  quantis_pop_negra <- quantile(file$perc_pop_negra, na.rm=TRUE)
  
  # Executar as categorizações
  file <- file %>% 
    mutate(cat_pop_negra = case_when(perc_pop_negra >= quantis_pop_negra[4] ~ 4, # 75% ou mais
                                     perc_pop_negra >= quantis_pop_negra[3] & perc_pop_negra < quantis_pop_negra[4] ~ 3, 
                                     perc_pop_negra >= quantis_pop_negra[2] & perc_pop_negra < quantis_pop_negra[3] ~ 2,
                                     TRUE ~1))
  
  main_title <- "População negra"
  coluna <- "cat_pop_negra"
  subtitulo <- "Quantil por hexágono"
  estilo <- "equal"
  breaks = c(1,2,3,4)
  
  plot <- tm_shape(file) + 
    tm_layout(main.title = main_title,
              main.title.position = c("center", "TOP"),
              legend.outside = FALSE, frame = FALSE,
              title.size = 1, legend.title.size = 1,
              inner.margins = c(0.02, 0.02, 0.02, 0.4))+
    tm_fill(col = coluna, 
            n=4,
            breaks = breaks,
            style = 'cat',
            palette = paleta, colorNA = paleta_NA, textNA = "Sem população", 
            title = subtitulo,
            legend.format = list(text.separator="a", scientific=TRUE, format="f"),
            legend.position = c('left','center'))+ # scientific e interessante
    tm_scale_bar(breaks = c(0, 2, 4), text.size = .5)#, position=c("right", "bottom"))#+
  
  file_name <- sprintf("%s/pop_negra_q_%s_%s_2019.%s", save_folder, muni, res, formato)
  tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
  
}

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "man", "nat", "rec",
           "rio", "sne", "sjc", "spo", "ula", "tsa", "vta", "jpa")

munis <- 'bho'


for (muni in munis){
  criar_mapas_quantil(muni = muni, res = "08", formato = "png")
}


# ----- FUNCAO OPORTUNIDADES -----

criar_mapas_oportunidades <- function(muni, formato='png', res='08'){
  # 0.0 Configuracoes iniciais
  # Plotagem
  tmap_options(
    output.format = formato,
    output.dpi = 500
  )
  
  oportunidades <- c('trabalho', 'saude', 'educacao')
  #modes <- c('walk', 'bike', 'transit')
  # Cores
  paleta <- c('#e7eff0', '#b6ced1', '#86adb2', '#568c93', '#0d5b65') 
  paleta_NA <- '#808080'
  
  # Estrutura de pasta
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolder19 <- sprintf("%s/19_mapas_acessibilidades/2019/Oportunidades", files_folder)
  save_folder <- sprintf("%s/%s", subfolder19, muni)
  
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
      main_title <- 'Trabalho'        # main_title em tm_layout
      coluna <- "empregos_total"                       # col em tm_fill
      subtitulo <- "Número de empregos"   # title em tm_fill
      estilo <- "jenks"                                # style em tm_fill
    } else if (oport == "saude"){
      main_title <- "Saúde"
      coluna <- "saude_total"
      subtitulo <- "Número de estabelecimentos"
      estilo <- "jenks"
    }else{
      main_title <- "Educação"
      coluna <- "edu_total"
      subtitulo <- "Número de estabelecimentos"
      estilo <- "pretty"

    }
    
  plot <- tm_shape(file) + 
      tm_layout(main.title = main_title,
                main.title.position = c("center","TOP"),
                legend.outside = FALSE, frame = FALSE, 
                #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
                title.size = 1, legend.title.size = 1,
                inner.margins = c(0.02, 0.02, 0.02, 0.4))+
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

munis <- c("bho")

for (muni in munis){
  criar_mapas_oportunidades(muni = muni, formato = "png")
}

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
files_folder <- "../../indice_acesso_cidade_dados"
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
