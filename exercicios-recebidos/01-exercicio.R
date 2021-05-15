library(magrittr)
library(ggplot2)

# Exercício 1 -------------------------------------------------------------

base_bruta <- read.delim(
  "dados/DadosBO_2021_3(ROUBO DE CELULAR).xls",
  fileEncoding = "UTF-16LE",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Exercício 1

# a) transforme a base bruta em uma base tidy

# Dica: A base não precisa ser perfeita, o mais
# importante é remover as duplicações proporcionadas pela
# pelas rubricas/desdobramentos etc

# Olhando a base
tibble::view(base_bruta)


# Verificando colunas
dplyr::glimpse(base_bruta)

# A unidade é o fato? (roubo de celular)
base_bruta %>% 
base_bruta %>% 
  dplyr::count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME)

# Olhando uma situação em específico
base_bruta %>% 
  dplyr::filter(NUM_BO == 34, ANO_BO == 2021, DELEGACIA_NOME == "NUCLEO ROUBO DE CARGAS-SEC GUA") %>% 
  tibble::view()





# b) arrume os nomes da base

# c) arrume as colunas de latitude e longitude

# Dica 1: as.numeric("1.05") transforma o texto "1.05" no número 1.05
# Dica 2: Essa planilha usa "," como separador decimal

# d) padronize a coluna marca_celular

# Dica 1: Use a função dplyr::case_when
# Dica 2: Você pode tentar chegar em uma coluna final
# que tenha apenas marcas escritas assim:
# APPLE, LENOVO, MICROSOFT, ..., OUTROS

# d) (opcional) faça um mapa com pontos usando latitudes e longitudes.
# use como cor a coluna marca_celular (arrumada)
