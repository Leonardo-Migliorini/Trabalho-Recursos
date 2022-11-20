# Carregando Pacotes

library(dplyr)

# Importando Base de Dados
enem_2019 <- as_tibble(
  read.csv2("..\\DADOS\\MICRODADOS_ENEM_2019.csv")
)

# Selecionando Colunas de interesse
enem_2019_range <- enem_2019 %>%
  select(
    NU_INSCRICAO,
    TP_SEXO,
    TP_COR_RACA,
    TP_DEPENDENCIA_ADM_ESC,
    starts_with("TP_PRESENCA_"),
    Q006,
    NU_NOTA_CN,
    NU_NOTA_CH,
    NU_NOTA_LC,
    NU_NOTA_MT
  ) %>%
  # Alterando Tipos das Variáveis
  mutate(
    across(c(NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT), as.numeric),
    across(c(NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT, starts_with("TP_PRESENCA_")), as.numeric)
  ) %>%
  mutate(
    across(c(NU_INSCRICAO, TP_SEXO, TP_COR_RACA, TP_DEPENDENCIA_ADM_ESC, starts_with("TP_PRESENCA_")), as.character),
    across(c(NU_INSCRICAO, TP_SEXO, TP_COR_RACA, TP_DEPENDENCIA_ADM_ESC,), as.character)
  )

  # Filtando as colunas



inscritos_enem_2019 <- nrow(enem_2019_range)

presencas_1_2019 <- sum(enem_2019_range$TP_PRESENCA_LC == 1 & enem_2019_range$TP_PRESENCA_CH == 1)
faltantes_1_2019 <- inscritos_enem_2019 - presencas_1_2019

presencas_2_2019 <- sum(enem_2019_range$TP_PRESENCA_CN == 1 & enem_2019_range$TP_PRESENCA_MT == 1)
faltantes_2_2019 <- inscritos_enem_2019 - presencas_2_2019

presenca_1_2_2019 <- sum(
  enem_2019_range$TP_PRESENCA_LC == 1 & enem_2019_range$TP_PRESENCA_CH == 1 & enem_2019_range$TP_PRESENCA_CN == 1 & enem_2019_range$TP_PRESENCA_MT == 1)

enem_2019_range <-enem_2019_range %>%
  mutate(
    MEDIA_GERAL = rowMeans(select(enem_2019_range, starts_with("NU_NOTA_")))
  ) %>%
  filter(
    !is.na(MEDIA_GERAL)
  ) %>%
  mutate(
    TP_COR_RACA = recode(TP_COR_RACA, "0"= "Não Declarado", "1"= "Branca", "2"= "Preta", "3"= "Parda", "4"= "Amarela", "5"= "Indigena", "6" = "Nao Dispoe"),
    TP_DEPENDENCIA_ADM_ESC = recode(TP_DEPENDENCIA_ADM_ESC, "1" = "Federal", "2" = "Estadual", "3" = "Municipal", "4" = "Privada"),
    TP_COR_RACA = recode(
      TP_COR_RACA, "0"= "Nao Declarado", "1"= "Branca", "2"= "Preta", "3"= "Parda", "4"= "Amarela", "5"= "Indigena", "6" = "Nao Dispoe"
    ),
    TP_DEPENDENCIA_ADM_ESC = recode(
      TP_DEPENDENCIA_ADM_ESC, "1" = "Federal", "2" = "Estadual", "3" = "Municipal", "4" = "Privada"
    )
  )

sexo_feminino_2019 <- sum(enem_2019_range$TP_SEXO == "F")
sexo_masculino_2019 <- sum(enem_2019_range$TP_SEXO == "M")

cor_preta_2019 <- sum(enem_2019_range$TP_COR_RACA == "Preta")
cor_branca_2019 <- sum(enem_2019_range$TP_COR_RACA == "Branca")
cor_nao_declarado_2019 <- sum(enem_2019_range$TP_COR_RACA == "Nao Declarado")
cor_parda_2019 <- sum(enem_2019_range$TP_COR_RACA == "Parda")
cor_amarela_2019 <- sum(enem_2019_range$TP_COR_RACA == "Amarela")
cor_indigena_2019 <- sum(enem_2019_range$TP_COR_RACA == "Indigena")
cor_nao_dispoe_2019 <- sum(enem_2019_range$TP_COR_RACA == "Nao Dispoe")

media_faixa1_2019 <- sapply(
  enem_2019_range[enem_2019_range$Q006 == "A"|enem_2019_range$Q006 == "B"|enem_2019_range$Q006 == "C"|enem_2019_range$Q006 == "D",
                  "MEDIA_GERAL"], mean)
media_faixa2_2019 <- sapply(
  enem_2019_range[enem_2019_range$Q006 == "E"|enem_2019_range$Q006 == "F"|enem_2019_range$Q006 == "G"|enem_2019_range$Q006 == "H",
                  "MEDIA_GERAL"], mean)
media_faixa3_2019 <- sapply(
  enem_2019_range[enem_2019_range$Q006 == "I"|enem_2019_range$Q006 == "J"|enem_2019_range$Q006 == "K"|enem_2019_range$Q006 == "L",
                  "MEDIA_GERAL"], mean)
media_faixa4_2019 <- sapply(
  enem_2019_range[enem_2019_range$Q006 == "M"|enem_2019_range$Q006 == "N"|enem_2019_range$Q006 == "O"|enem_2019_range$Q006 == "P",
                  "MEDIA_GERAL"], mean)

# write.csv(enem_2019_range, file = "Dados Enem 2019 Filtrados.csv")

  