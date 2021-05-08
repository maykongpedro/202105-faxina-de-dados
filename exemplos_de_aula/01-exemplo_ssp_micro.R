library(tidyverse)
library(magrittr, include.only = '%>%')


# Essa opção não funciona, pois o arquivo não é necessariamente um excel, apesar de ter essa extensão
# No caso de não abrir, testar se o arquivo abre em um bloco de notas, para confirmar se é realmente
# a extensão que ele diz ser
base_bruta <- readxl::read_excel("dados/DadosBO_2021_3(ROUBO DE VEÍCULOS).xls")


# Se o arquivo abrir no bloco de notas, testar diversas maneiras de abrir com read.delim
# O enconding testado foi encontrado dentro da documentação da função "read.delim"
# Ele em específico foi identificado pelo texto da documentação que explica que existem 
# encondings específicos para .txt dentro do windows.
# Usar o endoding "UTF-16LE" para conseguir abrir o arquivo corretamente
base_bruta <-
    read.delim(
        "dados/DadosBO_2021_3(ROUBO DE VEÍCULOS))_completa.xls",
        fileEncoding = "UTF-16LE",
        sep = "\t",
        header = T,
        stringsAsFactors = F
    ) %>% 
    tibble::as_tibble()


# Outra forma de descobrir o encoding
readr::guess_encoding("dados/DadosBO_2021_3(ROUBO DE VEÍCULOS))_completa.xls")


# Arquivi de roubo de celular, não foi usado nesse primeiro exemplo
# base_bruta <-
#     read.delim(
#         "dados/DadosBO_2021_3(ROUBO DE CELULAR).xls",
#         fileEncoding = "UTF-16LE",
#         sep = "\t",
#         header = T,
#         stringsAsFactors = F
#     )



# Quem são as observações da base? ----------------------------------------
# Nessa seção descobriremos várias coisas analisando manualmente a base

# Olhando a base
base_bruta %>% tibble::view()


# Verificando colunas
base_bruta %>% dplyr::glimpse()


## Essa base não é tidy?
# A unidade é o fato? (roubo de carro)
base_bruta %>% 
  dplyr::count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME)



# A unidade é o carro roubado? (vítima que sofreu o roubo)
# Nessa situação encontramos várias repetições por placa, ano, delegacia e etc. 
# Demonstrando assim que a plana não é necessariamente uma chave, pq ela se repete
base_bruta %>% 
    dplyr::count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME, PLACA_VEICULO)

# Olhando uma situação em específico
base_bruta %>% 
    dplyr::filter(NUM_BO == 12, ANO_BO == 2021, PLACA_VEICULO == "FIG6050") %>% 
    tibble::view()


# Arrumando nomes e placas ------------------------------------------------

base_nomes_arrumados_preenchida <-
    base_bruta %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!is.na(placa_veiculo), # filtra placas diferentes de "NA"
                  placa_veiculo != "",   # filtra placas diferente de vazio
                  !(placa_veiculo %in% c("0000000", "SEMPLAC", "TAMPADA"))) # filtra placas diferentes dessas 3 situações

# Verificar se PARECE correto
base_nomes_arrumados_preenchida %>% 
    dplyr::count(placa_veiculo) %>% 
    tibble::view()



# Separando as bases ------------------------------------------------------

base_nomes_arrumados_preenchida %>% 
    tibble::view()

# Primeiro amos separar as três bases e depois juntar pela mesma chave
# Qual vai ser a estratégia:
# usar as colunas: num_bo, ano_bo, delegacia_nome

# Base de ocorrência
ocorrencias <-
    base_nomes_arrumados_preenchida %>% 
    dplyr::select(ano_bo:delegacia_circunscricao) %>% 
    dplyr::distinct()
    #dplyr::distinct(num_bo, .keep_all = TRUE) # nesse caso ele traz todas as outras colunas após retirar duplicatas no "num_bo"

ocorrencias %>% 
    tibble::view()

# Base de carros
carros <- 
    base_nomes_arrumados_preenchida %>% 
    dplyr::select(num_bo,
                  ano_bo,
                  delegacia_nome,
                  # as colunas acima são para manter as mesmas colunas de chave da outra base
                  placa_veiculo:ano_fabricacao) %>%
    dplyr::distinct(placa_veiculo, .keep_all = TRUE)

# Base de crimes
crimes_passo1 <-
    base_nomes_arrumados_preenchida %>% 
    dplyr::select(num_bo,
                  ano_bo,
                  delegacia_nome,
                  # precisamos das colunas de chave
                  especie, rubrica, desdobramento) %>% 
    dplyr::distinct() %>% 
    # criar uma coluna unindo outras
    tidyr::unite(crime_completo, especie, rubrica, desdobramento, sep = " @@@ ")


# Unir todos os crimes em.....
# Para isso temos pelo menos 3 opções:

# Opção A - Sumarização
crimes_passo2_a <-
    crimes_passo1 %>% 
    dplyr::group_by(num_bo, ano_bo, delegacia_nome) %>% 
    dplyr::summarise(todos_os_crimes = paste0(crime_completo, collapse = ", "))
    # ou 
    #dplyr::summarise(todos_os_crimes = stringr::str_c(crime_completo, collapse = "\n"))
crimes_passo2_a


# Opção B - Pivot_wider
crimes_passo2_b <-
    crimes_passo1 %>% 
    dplyr::group_by(num_bo, ano_bo, delegacia_nome) %>% 
    dplyr::mutate(nomes_colunas_largas = stringr::str_c("crime_", 1:dplyr::n())) %>% 
    tidyr::pivot_wider(names_from = nomes_colunas_largas,
                       values_from = crime_completo)
crimes_passo2_b


# Opção C - Nest: transformar as linhas todas em uma tabela e list-column
crimes_passo2_c <-
    crimes_passo1 %>% 
    dplyr::group_by(num_bo, ano_bo, delegacia_nome) %>% 
    tidyr::nest()
crimes_passo2_c


# Vamos usar essa nesse exemplo
crimes_passo2_a %>% 
    tibble::view()


# Construção da tabela final ----------------------------------------------

# Gerar base tidy
# Não ficou perfeita pois aumentou duas linhas dentro da base
base_final_tidy <- 
    carros %>%
    dplyr::left_join(ocorrencias) %>% 
    dplyr::left_join(crimes_passo2_a)
    

# Verificando se existe duplicidade
# Existe, pois não selecionamos o id correto, temos que selecionar delegacia_nome
# ao invés de delegacia_circunscricao
base_final_tidy %>% 
    dplyr::select(num_bo,
                  ano_bo,
                  delegacia_nome,
                  placa_veiculo) %>% 
    janitor::get_dupes()












