# Rodando Pacotes

library(dplyr)

# Importação de Dados Filtrados

# Tabela 2019
dados_filtrados_2019 <- tibble::as_tibble(
  read.csv("Dados\\Dados Enem 2019 Filtrados.csv")) |>
  dplyr::select(TP_SEXO:TP_DEPENDENCIA_ADM_ESC, Q006, MEDIA_GERAL) |>
  mutate(
    TP_COR_RACA = recode(TP_COR_RACA, "Não Declarado" = "0", "Branca" = "1",  "Preta" = "2", "Parda" = "3",
                         "Amarela" = "4", "Indigena" = "5", "Nao Dispoe" = "6"),

    TP_DEPENDENCIA_ADM_ESC = recode(TP_DEPENDENCIA_ADM_ESC, "Federal" = "1", "Estadual" = "2", "Municipal" = "3", "Privada" = "4"),

    TP_SEXO = recode(TP_SEXO, "F" = "0", "M" = "1"),

    Q006 = recode(Q006, "A" = "1", "B" = "2", "C" = "3", "D" = "4", "E" = "5", "F" = "6",
                  "G" = "7", "H" = "8", "I" = "9", "J" = "10", "K" = "11", "L" = "12", "M" = "13",
                  "M" = "14", "N" = "15", "O" = "16", "P" = "17")) |>
  mutate(
    across(c(TP_SEXO, TP_COR_RACA, TP_DEPENDENCIA_ADM_ESC, Q006, MEDIA_GERAL), as.integer),
    across(c(TP_SEXO, TP_COR_RACA, TP_DEPENDENCIA_ADM_ESC, Q006, MEDIA_GERAL), as.integer)
  )


# Tabela 2020

dados_filtrados_2020 <- tibble::as_tibble(
  read.csv("Dados\\Dados Enem 2020 Filtrados.csv")) |>
  dplyr::select(TP_SEXO:TP_DEPENDENCIA_ADM_ESC, Q006, MEDIA_GERAL) |>
  mutate(
    TP_COR_RACA = recode(TP_COR_RACA, "Não Declarado" = "0", "Branca" = "1",  "Preta" = "2", "Parda" = "3",
                         "Amarela" = "4", "Indigena" = "5", "Nao Dispoe" = "6"),

    TP_DEPENDENCIA_ADM_ESC = recode(TP_DEPENDENCIA_ADM_ESC, "Federal" = "1", "Estadual" = "2", "Municipal" = "3", "Privada" = "4"),

    TP_SEXO = recode(TP_SEXO, "F" = "0", "M" = "1"),

    Q006 = recode(Q006, "A" = "1", "B" = "2", "C" = "3", "D" = "4", "E" = "5", "F" = "6",
                  "G" = "7", "H" = "8", "I" = "9", "J" = "10", "K" = "11", "L" = "12", "M" = "13",
                  "M" = "14", "N" = "15", "O" = "16", "P" = "17")) |>
  mutate(
    across(c(TP_SEXO, TP_COR_RACA, TP_DEPENDENCIA_ADM_ESC, Q006, MEDIA_GERAL), as.integer),
    across(c(TP_SEXO, TP_COR_RACA, TP_DEPENDENCIA_ADM_ESC, Q006, MEDIA_GERAL), as.integer)
  )


# Tabela 2021

dados_filtrados_2021 <- tibble::as_tibble(
  read.csv2("Dados\\Dados Enem 2021 Filtrados.csv")) |>
  dplyr::select(TP_SEXO:TP_DEPENDENCIA_ADM_ESC, Q006, MEDIA_GERAL) |>
  mutate(
    TP_COR_RACA = recode(TP_COR_RACA, "Não Declarado" = "0", "Branca" = "1",  "Preta" = "2", "Parda" = "3",
                         "Amarela" = "4", "Indigena" = "5", "Nao Dispoe" = "6"),

    TP_DEPENDENCIA_ADM_ESC = recode(TP_DEPENDENCIA_ADM_ESC, "Federal" = "1", "Estadual" = "2", "Municipal" = "3", "Privada" = "4"),

    TP_SEXO = recode(TP_SEXO, "F" = "0", "M" = "1"),

    Q006 = recode(Q006, "A" = "1", "B" = "2", "C" = "3", "D" = "4", "E" = "5", "F" = "6",
                  "G" = "7", "H" = "8", "I" = "9", "J" = "10", "K" = "11", "L" = "12", "M" = "13",
                  "M" = "14", "N" = "15", "O" = "16", "P" = "17")) |>
  mutate(
    across(c(TP_SEXO, TP_COR_RACA, TP_DEPENDENCIA_ADM_ESC, Q006, MEDIA_GERAL), as.integer),
    across(c(TP_SEXO, TP_COR_RACA, TP_DEPENDENCIA_ADM_ESC, Q006, MEDIA_GERAL), as.integer)
  )


# Teste de Correlação

#corrplot 2019

corrplot::corrplot(cor(dados_filtrados_2019, use="pairwise.complete.obs"), method = "number", type = "upper")

#corrplot 2020

corrplot::corrplot(cor(dados_filtrados_2020, use="pairwise.complete.obs"), method = "number", type = "upper")

#corrplot 2021

corrplot::corrplot(cor(dados_filtrados_2021, use="pairwise.complete.obs"), method = "number", type = "upper")

cor_2019 <- lm(data = dados_filtrados_2019, MEDIA_GERAL ~ Q006 + TP_COR_RACA +
            TP_SEXO + TP_DEPENDENCIA_ADM_ESC)

cor_2020 <- lm(data = dados_filtrados_2020, MEDIA_GERAL ~ Q006 + TP_COR_RACA +
                 TP_SEXO + TP_DEPENDENCIA_ADM_ESC)

cor_2021 <- lm(data = dados_filtrados_2021, MEDIA_GERAL ~ Q006 + TP_COR_RACA +
                 TP_SEXO + TP_DEPENDENCIA_ADM_ESC)


summary(cor_2019)
summary(cor_2020)
summary(cor_2021)

# Equações das Retas de Regressão para colocar no R markdown

Geral

# $$ y_i = \beta_0 + \beta_1 x_{i1} + \beta_2 x_{i2} + \beta_3 x_{i3} + \beta_4 x_{i4} + \epsilon_i,$$

2019

#\hat{y} = 443.69 +  8.37030  x_{i1} - 6.38250  x_{i2} + 11.64846 x_{i3} + 13.42205 x_{i4}.

2020

#\hat{y} = 461.52 +  8.12817  x_{i1} - 8.81910  x_{i2} + 18.66507 x_{i3} + 10.46004 x_{i4}.

2021

#\hat{y} = 457.00 +  7.40265  x_{i1} - 8.28723  x_{i2} + 14.08657 x_{i3} + 9.58476 x_{i4}.


