library(magrittr, include.only = "%>%")
# library(ggplot2)
library(dplyr)

# Exercício 1 -------------------------------------------------------------

# url <- "https://curso-r.github.io/202105-faxina/dados/DadosBO_2021_3(ROUBO%20DE%20CELULAR).xls"
# download.file(url,destfile = "DadosBO_2021_3(ROUBO DE CELULAR).xls")
# ou baixar o arquivo na página do curso: https://curso-r.github.io/202105-faxina/

# abrir arquivo original
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
# Quando faço isso, percebo que existem algumas repetições
base_bruta %>% 
  dplyr::count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME)


# Olhando apenas para as situações que se repetem da pra ver que são muitos itens
# com 2 casos e outros que chegam a  30. Não vou usar a coluna "DELEGACIA_CIRCUNSCRICAO"
# por não mudar o resultado
base_bruta %>% 
  
  # fazer contagem das linhas que possuem as informações iguais para as seguintes cols.
  dplyr::count(ANO_BO, NUM_BO, DELEGACIA_NOME) %>% 
  
  # filtrar tudo que é diferente de 1 na contagem
  dplyr::filter(n != 1) %>% 
  
  # fazer tabela de frequência
  janitor::tabyl(n) %>% 
  
  # formatar como percentual
  janitor::adorn_pct_formatting() %>%
  
  # re-ordenar
  dplyr::arrange(desc(n_n)) %>% 
  
  # tabela
  knitr::kable()

# ==============================================================================

# Olhando uma situação em específico (com 12 repeticões)
base_bruta %>% 
  dplyr::filter(NUM_BO == 34, 
                ANO_BO == 2021, 
                DELEGACIA_NOME == "NUCLEO ROUBO DE CARGAS-SEC GUA") %>% 
  tibble::view()

# Outra situação (com 2 repetições)
base_bruta %>%
  dplyr::filter(NUM_BO == 50, 
                ANO_BO == 2021, 
                DELEGACIA_NOME == "DEL.INV.GER. CRUZEIRO") %>% 
  tibble::view()

# Para esse caso tenho 4 linhas que correspondem à ESPECIE "localização ou devolução"
# 2 dessas possuem informação de veículo e as outras duas não

# As outras 8 linhas são referentes a um artigo que imagino que seja de "roubo", 
# por conta da RÚBRICA. Essa última se dividindo em "Roubo de carga" e "Roubo de 
# veículo", tendo 4 linhas para cada. E essas 4 linhas, apenas duas possuem info
# sobre a placa e veículo.

# Com isso, podemos concluir que existe duplicação dentro das rúbricas, e além
# disso possuímos linhas vazias de infos.

# ==============================================================================

# Diminuir essas duplicidades
base_trt_no_dupes <-
  base_bruta %>%
  
  # retirar duplicatas considerando as 4 colunas
  dplyr::distinct(ANO_BO,
                  NUM_BO,
                  RUBRICA,
                  DELEGACIA_NOME,
                  .keep_all = TRUE)

# Verificando resultado do tratamento
base_trt_no_dupes %>% 
  dplyr::count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME) %>% 
  dplyr::filter(n != 1) %>% 
  janitor::tabyl(n) %>% 
  janitor::adorn_pct_formatting() %>%
  dplyr::arrange(desc(n_n)) %>% 
  knitr::kable()

# Grande parte dos casos foi resolvida, porém ainda temos alguns itens que variam
# de 2 linhas a 6, implicando que eles não são necessariamente duplicatas, então
# é necessário explorar se a diferença ocorre apenas na RÚBRICA ou em outro lugar


# Explorando esses itens restantes
base_trt_no_dupes %>% 
  dplyr::count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME) %>% 
  dplyr::filter(n == 2) 

# Nesse caso temos uma rúbrica de recepção qualificada e outra para o roubo em si
base_trt_no_dupes %>% 
  dplyr::filter(ANO_BO == 2021,
         NUM_BO == 37,
         DELEGACIA_NOME == "DEIC-3ª DELEGACIA DA DISCCPAT") %>% 
  tibble::view()

# Retirar receptação, por não ser um roubo?
# Como prosseguir com essas "duplicatas"?


# base_trt_no_dupes %>% 
#   filter(!stringr::str_detect(RUBRICA, "Receptação")) %>% 
#   #tibble::view()
#   dplyr::count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME) %>%
#   dplyr::filter(n != 1) %>% 
#   janitor::tabyl(n) %>% 
#   janitor::adorn_pct_formatting() %>%
#   dplyr::arrange(desc(n_n))


# ==============================================================================

# b) arrume os nomes da base
base_trt_no_dupes <-
  base_trt_no_dupes %>% 
  janitor::clean_names()


# c) arrume as colunas de latitude e longitude
# Dica 1: as.numeric("1.05") transforma o texto "1.05" no número 1.05
# Dica 2: Essa planilha usa "," como separador decimal

base_arrumada_long_lat <-
  base_trt_no_dupes %>%
  mutate(
    latitude = stringr::str_replace_all(latitude, 
                                        pattern = ",", 
                                        replacement = "."),
    
    longitude = stringr::str_replace_all(longitude, 
                                         pattern = ",", 
                                         replacement = "."),
    
    latitude = as.numeric(latitude, dec = ","),
    longitude = as.numeric(longitude, dec = ",")
    
  )


# d) padronize a coluna marca_celular
# Dica 1: Use a função dplyr::case_when
# Dica 2: Você pode tentar chegar em uma coluna final
# que tenha apenas marcas escritas assim:
# APPLE, LENOVO, MICROSOFT, ..., OUTROS

# d) (opcional) faça um mapa com pontos usando latitudes e longitudes.
# use como cor a coluna marca_celular (arrumada)
