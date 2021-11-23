#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.3 Geocode dados do censo escolar

# carregar bibliotecas
source('fun/setup.R')

# carregar funcoes
source('fun/saude/saude.R')

# Aplicar funcao filtro
# saude_filter(2019)
source('fun/saude/saude2.R')

# Aplicar funcao geocode
# saude_geocode(2017)
# saude_geocode(2018)
saude_geocode(2019)

# Selecionar somente as obsservacoes com boa qualidade de geocode
source("fun/filter_geocode.R")
# lapply(X=2017:2019, geocode_filter, "saude")
lapply(X = 2019, geocode_filter, "saude")
