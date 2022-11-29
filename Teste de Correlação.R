# Rodando Pacotes

library(dplyr)

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

tabelas <- rbind(dados_filtrados_2019, dados_filtrados_2020)
tabelas2 <- rbind(tabelas, dados_filtrados_2021)

# Renomenado Variáveis

tabelas2 <- rename(tabelas2, Sexo = TP_SEXO, Raca = TP_COR_RACA, Escolas = TP_DEPENDENCIA_ADM_ESC,
                   Renda = Q006, Media_Geral = MEDIA_GERAL)

# Teste de Correlação

# Corrplot

corrplot::corrplot(cor(tabelas2, use="pairwise.complete.obs"), method = "number", type = "upper")

correlacao <- lm(data = tabelas2, Media_Geral ~ Renda + Raca +
            Sexo + Escolas)

summary(correlacao)

# Texto Markdown

Modelo Utilizado

    Para realização do estudo de regressão, utilizamos de um modelo de regressão linear
múltipla em que buscamos explicar o comportamento da Média Geral das notas do enem dos últimos 3 anos
através de 4 variáveis de interesse, sendo elas: Renda, Raça, Sexo e Tipo de Escola


O Modelo de regresssão utilizado, é expressado pela equação:

# $$ y_i = \beta_0 + \beta_1 x_{i1} + \beta_2 x_{i2} + \beta_3 x_{i3} + \beta_4 x_{i4} + \epsilon_i,$$

Sendo as variáveis utilizadas:

$\beta_0 = Media Geral das Notas$
$\beta_1= Renda$
$\beta_2 = Raça$
$\beta_3 = Sexo$
$\beta_4 = Tipo de Escola$
$\epsilon_i = erro$


Após a avaliação numérica, o modelo de regressão estimado é dado por:

#\hat{y} = 452.45216 +  7.95842  x_{i1} - 7.51051  x_{i2} + 14.28080 x_{i3} + 11.35732 x_{i4}.

Em que:

$x_i1$, para $i = 1, ..., 17.$
$x_i2$, para $i = 0, ..., 6.$
$x_i3$, para $i = 0, ..., 1.$
$x_i4$, para $i = 1, ..., 4.$

O valor para o $R^2 = 0.2768$, significando que as variáveis explicativas utilizadas
no modelo de regressão conseguem explicar em 27.68% o valor da variável dependente: Media Geral das Notas do Enem.
