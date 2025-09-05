## Indicadores Saúde Bucal - SB Brasil 2023
# Érika Aquino
# Agosto de 2025

# Preparando o ambiente R
library(readxl)
library(tidyverse)
library(DT)
library(RODBC)
library(dplyr)
library(sf)
library(ggplot2)
library(geobr)
library(scales)
library(sf) 
library(ggrepel) 
library(ggspatial) 
library(geojsonio)
library(patchwork)
library(haven)

# Carregando banco de dados

banco_sbbrasil_2023 <- read_sav("banco_sbbrasil_2023-20250326.sav")
View(banco_sbbrasil_2023)

# instalar e carregar a biblioteca survey 
# install.packages("survey")
library(survey)

# Definição do objeto que contém o plano amostral
options(survey.lonely.psu = "certainty")
desenhoamostral <- svydesign(id=~setor, strata=~estrato,
                               weights=~wi_pesodel, nest=TRUE, data=banco_sbbrasil_2023)

# Criação das faixas etárias
banco_sbbrasil_2023 <- banco_sbbrasil_2023 %>%
  mutate(
    faixa_etaria = cut(
      idade,
      breaks = c(-Inf, 14, 29, 59, Inf),
      labels = c("de 0 a 14 anos",
                 "de 15 a 29 anos",
                 "de 30 a 59 anos",
                 "acima de 60 anos"),
      right = TRUE
    )
  )

# Atualização do objeto do plano amostral
desenhoamostral <- update(desenhoamostral,
                          faixa_etaria = cut(idade,
                                             breaks = c(-Inf, 14, 29, 59, Inf),
                                             labels = c("de 0 a 14 anos",
                                                        "de 15 a 29 anos",
                                                        "de 30 a 59 anos",
                                                        "60 anos e mais"),
                                             right = TRUE))


svytable(~ faixa_etaria, desenhoamostral)

# Transformar a variável uf2 em nomes das Unidades Federadas
banco_sbbrasil_2023 <- banco_sbbrasil_2023 %>%
  mutate(
    nome_uf = factor(uf2,
                     levels = 1:27,
                     labels = c(
                       "Acre", "Amazonas", "Amapá", "Pará", "Rondônia", "Roraima", "Tocantins",
                       "Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe",
                       "Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo",
                       "Paraná", "Rio Grande do Sul", "Santa Catarina",
                       "Distrito Federal", "Goiás", "Mato Grosso do Sul", "Mato Grosso"
                     )
    )
  )

table(banco_sbbrasil_2023$nome_uf, useNA = "ifany")

# Atualização do objeto no plano amostral
desenhoamostral <- update(
  desenhoamostral,
  nome_uf = banco_sbbrasil_2023$nome_uf
)


# Transformar a variável capint
banco_sbbrasil_2023 <- banco_sbbrasil_2023 %>%
  mutate(
    capint2 = factor(capint,
                     levels = 1:2,
                     labels = c(
                       "Capital", "Interior")
    )
  )

table(banco_sbbrasil_2023$capint2, useNA = "ifany")

# Atualização do objeto no plano amostral
desenhoamostral <- update(
  desenhoamostral,
  capint2 = banco_sbbrasil_2023$capint2
)

###############################################################################################################
## CÁLCULO DE PARÂMETROS PARA POPULAÇÃO GERAL
###############################################################################################################

# Cobertura Atenção Básica

## BRASIL

# nt_rest_1superf	Contagem de dentes com necessidade de restauração de 1 superfície 
# nt_rest_2superf	Contagem de dentes com necessidade de restauração de 2 ou mais superfícies 
# nt_extracao	Contagem de dentes com necessidade de extração
# nt_remin	Contagem de dentes com necessidade de remineralização de mancha branca
# nt_selante	Contagem de dentes com necessidade de selante
# prevcalc	Prevalência de cálculo (1 = 1 a 6 sextantes com cálculo dentário)
# prevbolsarasa	Prevalência de bolsa rasa (4 a 5 mm)(1 = 1 a 6 sextantes com bolsa rasa)

# Criação da variável COB_AB
banco_sbbrasil_2023 <- banco_sbbrasil_2023 %>%
  mutate(
    COB_AB = as.numeric(
      nt_rest_1superf > 0 |
        nt_rest_2superf > 0 |
        nt_extracao > 0 |
        (idade < 60 & nt_remin > 0) |
        (idade < 60 & nt_selante > 0) |
        (idade > 5 & prevcalc == 1) |
        (!idade %in% c(5, 12) & prevbolsarasa == 1)
    )
  )
    

# Atualização do plano amostral
desenhoamostral <- update(
  desenhoamostral,
  COB_AB = as.numeric(
    nt_rest_1superf > 0 |
      nt_rest_2superf > 0 |
      nt_extracao > 0 |
      (idade < 60 & nt_remin > 0) |
      (idade < 60 & nt_selante > 0) |
      (idade > 5 & prevcalc == 1) |
      (!idade %in% c(5, 12) & prevbolsarasa == 1)
  )
)

# Proporção de pessoas com COB_AB == 1
svymean(~ COB_AB, 
        design = desenhoamostral, 
        na.rm = TRUE)

# Proporção por faixa etária com IC 95%
prop_cob_ab_faixa <- svyby(
  ~ COB_AB,
  ~ faixa_etaria,
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")  # inclui erro padrão e intervalo de confiança
)

# Formatar como percentual e renomear colunas
prop_cob_ab_faixa_formatado <- prop_cob_ab_faixa %>%
  mutate(across(c(COB_AB, se, ci_l, ci_u), ~ round(.x * 100, 4))) %>%
  rename(
    faixa_etaria = faixa_etaria,
    perc_necessidade = COB_AB,
    erro_padrao = se,
    IC_inf = ci_l,
    IC_sup = ci_u
  ) |>
  mutate(tipo_procedimento = 'APS')

# Exibir resultado
print(prop_cob_ab_faixa_formatado)




## UNIDADES FEDERADAS
# Proporção por UF com IC 95%
prop_cob_ab_uf <- svyby(
  ~ COB_AB,
  ~ nome_uf,
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Formatar como percentual
prop_cob_ab_uf_formatado <- 
  prop_cob_ab_uf %>%
  mutate(across(c(COB_AB, se, ci_l, ci_u), ~ round(.x * 100, 1))) %>%
  rename(
    UF = nome_uf,
    perc_necessidade = COB_AB,
    erro_padrao = se,
    IC_inf = ci_l,
    IC_sup = ci_u
  ) %>%
  arrange(UF) |>
  mutate(tipo_procedimento = 'APS')

# Exibir resultado
print(prop_cob_ab_uf_formatado)


# Calcular proporção por UF e faixa etária com IC 95%
prop_cob_ab_uf_faixa <- svyby(
  ~ COB_AB,
  ~ interaction(nome_uf, faixa_etaria),
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Organizar resultados em tabela com colunas separadas
prop_cob_ab_uf_faixa_formatado <- prop_cob_ab_uf_faixa %>%
  separate(`interaction(nome_uf, faixa_etaria)`, into = c("UF", "faixa_etaria"), sep = "\\.") %>%
  mutate(
    perc_necessidade = round(COB_AB * 100, 4),
    erro_padrao      = round(se * 100, 4),
    IC_inf           = round(ci_l * 100, 4),
    IC_sup           = round(ci_u * 100, 4)
  ) %>%
  select(UF, faixa_etaria, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  arrange(UF, match(faixa_etaria,
                    c("de 0 a 14 anos", "de 15 a 29 anos", "de 30 a 59 anos", "60 anos e mais"))) |>
  mutate(tipo_procedimento = 'APS')

# Exibir tabela final
print(prop_cob_ab_uf_faixa_formatado)



##CAPITAL E INTERIOR
# Proporção com IC 95%
prop_cob_ab_capint <- svyby(
  ~ COB_AB,
  ~ capint2,
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Formatar como percentual
prop_cob_ab_capint_formatado <- prop_cob_ab_capint %>%
  mutate(across(c(COB_AB, se, ci_l, ci_u), ~ round(.x * 100, 4))) %>%
  rename(
    capint = capint2,
    perc_necessidade = COB_AB,
    erro_padrao = se,
    IC_inf = ci_l,
    IC_sup = ci_u
  ) %>%
  arrange(capint) |>
  mutate(tipo_procedimento = 'APS')

# Exibir resultado
print(prop_cob_ab_capint_formatado)




# Calcular proporção capint e faixa etária com IC 95%
prop_cob_ab_capint_faixa <- svyby(
  ~ COB_AB,
  ~ interaction(capint2, faixa_etaria),
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Organizar resultados em tabela com colunas separadas
prop_cob_ab_capint_faixa_formatado <- prop_cob_ab_capint_faixa %>%
  separate(`interaction(capint2, faixa_etaria)`, into = c("capint", "faixa_etaria"), sep = "\\.") %>%
  mutate(
    perc_necessidade = round(COB_AB * 100, 4),
    erro_padrao      = round(se * 100, 4),
    IC_inf           = round(ci_l * 100, 4),
    IC_sup           = round(ci_u * 100, 4)
  ) %>%
  select(capint, faixa_etaria, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  arrange(capint, match(faixa_etaria,
                    c("de 0 a 14 anos", "de 15 a 29 anos", "de 30 a 59 anos", "60 anos e mais"))) |>
  mutate(tipo_procedimento = 'APS')

# Exibir tabela final
print(prop_cob_ab_capint_faixa_formatado)


###############################################################################################################
# Cobertura de endodontia

# nt_tratpulpar	Contagem de dentes com necessidade de tratamento pulpar


##BRASIL

# Criação da variável COB_ENDO
banco_sbbrasil_2023 <- banco_sbbrasil_2023 %>%
  mutate(
    COB_ENDO = as.numeric(
    nt_tratpulpar > 0)
    )
  
# Atualização do plano amostral
desenhoamostral <- update(
  desenhoamostral,
  COB_ENDO = as.numeric(nt_tratpulpar > 0)
)


# Proporção de pessoas com COB_ENDO == 1
svymean(~ COB_ENDO, design = desenhoamostral, na.rm = TRUE)

# Proporção por faixa etária com IC 95%
prop_cob_endo_faixa <- svyby(
  ~ COB_ENDO,
  ~ faixa_etaria,
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")  # erro padrão e intervalo de confiança
)

# Formatar como percentual e renomear colunas
prop_cob_endo_faixa_formatado <- prop_cob_endo_faixa %>%
  mutate(across(c(COB_ENDO, se, ci_l, ci_u), ~ round(.x * 100, 4))) %>%
  rename(
    faixa_etaria = faixa_etaria,
    perc_necessidade = COB_ENDO,
    erro_padrao = se,
    IC_inf = ci_l,
    IC_sup = ci_u
  ) |>
  mutate(tipo_procedimento = 'Endodontia')

# Exibir resultado
print(prop_cob_endo_faixa_formatado)




## UNIDADES FEDERADAS

# Proporção por UF com IC 95%
prop_cob_endo_uf <- svyby(
  ~ COB_ENDO,
  ~ nome_uf,
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Formatar como percentual
prop_cob_endo_uf_formatado <- prop_cob_endo_uf %>%
  mutate(
    perc_necessidade = round(COB_ENDO * 100, 4),
    erro_padrao = round(se * 100, 4),
    IC_inf = round(ci_l * 100, 4),
    IC_sup = round(ci_u * 100, 4)
  ) %>%
  rename(UF = nome_uf) %>%
  select(UF, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  arrange(UF) |>
  mutate(tipo_procedimento = 'Endodontia')

# Exibir resultado
print(prop_cob_endo_uf_formatado)




# Proporção por UF e faixa etária com IC 95%
prop_cob_endo_uf_faixa <- svyby(
  ~ COB_ENDO,
  ~ interaction(nome_uf, faixa_etaria),
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Organizar resultados em tabela com colunas separadas
prop_cob_endo_uf_faixa_formatado <- prop_cob_endo_uf_faixa %>%
  separate(`interaction(nome_uf, faixa_etaria)`, into = c("UF", "faixa_etaria"), sep = "\\.") %>%
  mutate(
    perc_necessidade = round(COB_ENDO * 100, 4),
    erro_padrao      = round(se * 100, 4),
    IC_inf           = round(ci_l * 100, 4),
    IC_sup           = round(ci_u * 100, 4)
  ) %>%
  select(UF, faixa_etaria, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  arrange(UF, match(faixa_etaria,
                    c("de 0 a 14 anos", "de 15 a 29 anos", "de 30 a 59 anos", "60 anos e mais"))) |>
  mutate(tipo_procedimento = 'Endodontia')

# Exibir resultado
print(prop_cob_endo_uf_faixa_formatado)



## CAPITAL E INTERIOR

# Proporção com IC 95%
prop_cob_endo_capint <- svyby(
  ~ COB_ENDO,
  ~ capint2,
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Formatar como percentual
prop_cob_endo_capint_formatado <- prop_cob_endo_capint %>%
  mutate(
    perc_necessidade = round(COB_ENDO * 100, 4),
    erro_padrao = round(se * 100, 4),
    IC_inf = round(ci_l * 100, 4),
    IC_sup = round(ci_u * 100, 4)
  ) %>%
  rename(capint = capint2) %>%
  select(capint, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  arrange(capint) |>
  mutate(tipo_procedimento = 'Endodontia')

# Exibir resultado
print(prop_cob_endo_capint_formatado)



# Proporção capint e faixa etária com IC 95%
prop_cob_endo_capint_faixa <- svyby(
  ~ COB_ENDO,
  ~ interaction(capint2, faixa_etaria),
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Organizar resultados em tabela com colunas separadas
prop_cob_endo_capint_faixa_formatado <- prop_cob_endo_capint_faixa %>%
  separate(`interaction(capint2, faixa_etaria)`, into = c("capint", "faixa_etaria"), sep = "\\.") %>%
  mutate(
    perc_necessidade = round(COB_ENDO * 100, 4),
    erro_padrao      = round(se * 100, 4),
    IC_inf           = round(ci_l * 100, 4),
    IC_sup           = round(ci_u * 100, 4)
  ) |>
  mutate(tipo_procedimento = 'Endodontia')
  select(capint, faixa_etaria, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  arrange(capint, match(faixa_etaria,
                    c("de 0 a 14 anos", "de 15 a 29 anos", "de 30 a 59 anos", "60 anos e mais"))) |>
    mutate(tipo_procedimento = 'Endodontia')

# Exibir resultado
print(prop_cob_endo_capint_faixa_formatado)


###############################################################################################################
# Cobertura de periodontia

# prevbolsaprofunda	Prevalência de bolsa profunda (>= 6 mm)

##BRASIL

# Criação da variável COB_PERIO
banco_sbbrasil_2023 <- banco_sbbrasil_2023 %>%
  mutate(
    COB_PERIO = as.numeric(
      prevbolsaprofunda > 0)
  )

# Atualização do plano amostral
desenhoamostral <- update(
  desenhoamostral,
  COB_PERIO = as.numeric(prevbolsaprofunda > 0)
)



# Proporção de pessoas com COB_PERIO == 1
svymean(~ COB_PERIO, design = desenhoamostral, na.rm = TRUE)

# Proporção por faixa etária com IC 95%
prop_cob_perio_faixa <- svyby(
  ~ COB_PERIO,
  ~ faixa_etaria,
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")  # erro padrão e intervalo de confiança
)

# Formatar como percentual e renomear colunas
prop_cob_perio_faixa_formatado <- prop_cob_perio_faixa %>%
  mutate(across(c(COB_PERIO, se, ci_l, ci_u), ~ round(.x * 100, 4))) %>%
  rename(
    faixa_etaria = faixa_etaria,
    perc_necessidade = COB_PERIO,
    erro_padrao = se,
    IC_inf = ci_l,
    IC_sup = ci_u
  ) |> 
  mutate(tipo_procedimento = 'Periodontia')

# Exibir resultado (somente faixas acima de 14 anos)
print(
  prop_cob_perio_faixa_formatado %>%
    filter(faixa_etaria != "de 0 a 14 anos")
)



## UNIDADES FEDERADAS
# Proporção por UF com IC 95%
prop_cob_perio_uf <- svyby(
  ~ COB_PERIO,
  ~ nome_uf,
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Formatar como percentual
prop_cob_perio_uf_formatado <- prop_cob_perio_uf %>%
  mutate(
    perc_necessidade = round(COB_PERIO * 100, 4),
    erro_padrao = round(se * 100, 4),
    IC_inf = round(ci_l * 100, 4),
    IC_sup = round(ci_u * 100, 4)
  ) %>%
  rename(UF = nome_uf) %>%
  select(UF, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  arrange(UF) |>
  mutate(tipo_procedimento = 'Periodontia')

# Exibir resultado
print(prop_cob_perio_uf_formatado)



# Proporção por UF e faixa etária com IC 95%
prop_cob_perio_uf_faixa <- svyby(
  ~ COB_PERIO,
  ~ interaction(nome_uf, faixa_etaria),
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Organizar resultados em tabela com colunas separadas
prop_cob_perio_uf_faixa_formatado <- prop_cob_perio_uf_faixa %>%
  separate(`interaction(nome_uf, faixa_etaria)`,
           into = c("UF", "faixa_etaria"),
           sep = "\\.") %>%
  mutate(
    perc_necessidade = round(COB_PERIO * 100, 4),
    erro_padrao = round(se * 100, 4),
    IC_inf = round(ci_l * 100, 4),
    IC_sup = round(ci_u * 100, 4)
  ) %>%
  select(UF, faixa_etaria, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  filter(faixa_etaria != "de 0 a 14 anos") %>%
  arrange(UF, match(faixa_etaria,
                    c("de 15 a 29 anos",
                      "de 30 a 59 anos",
                      "60 anos e mais"))) |> 
  mutate(tipo_procedimento = 'Periodontia')

# Exibir resultado
print(prop_cob_perio_uf_faixa_formatado)



## CAPITAL E INTERIOR
# Proporção com IC 95%
prop_cob_perio_capint <- svyby(
  ~ COB_PERIO,
  ~ capint2,
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Formatar como percentual
prop_cob_perio_capint_formatado <- prop_cob_perio_capint %>%
  mutate(
    perc_necessidade = round(COB_PERIO * 100, 4),
    erro_padrao = round(se * 100, 4),
    IC_inf = round(ci_l * 100, 4),
    IC_sup = round(ci_u * 100, 4)
  ) %>%
  rename(capint = capint2) %>%
  select(capint, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  arrange(capint) |>
  mutate(tipo_procedimento = 'Periodontia')

# Exibir resultado
print(prop_cob_perio_capint_formatado)




# Proporção capint e faixa etária com IC 95%
prop_cob_perio_capint_faixa <- svyby(
  ~ COB_PERIO,
  ~ interaction(capint2, faixa_etaria),
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Organizar resultados em tabela com colunas separadas
prop_cob_perio_capint_faixa_formatado <- prop_cob_perio_capint_faixa %>%
  separate(`interaction(capint2, faixa_etaria)`,
           into = c("capint", "faixa_etaria"),
           sep = "\\.") %>%
  mutate(
    perc_necessidade = round(COB_PERIO * 100, 4),
    erro_padrao = round(se * 100, 4),
    IC_inf = round(ci_l * 100, 4),
    IC_sup = round(ci_u * 100, 4)
  ) %>%
  select(capint, faixa_etaria, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  filter(faixa_etaria != "de 0 a 14 anos") %>%
  arrange(capint, match(faixa_etaria,
                    c("de 15 a 29 anos",
                      "de 30 a 59 anos",
                      "60 anos e mais"))) |> 
    mutate(tipo_procedimento = 'Periodontia')

# Exibir resultado
print(prop_cob_perio_capint_faixa_formatado)



###############################################################################################################
# Cobertura de prótese

# necprot:
# 0 = Não necessita
# 1 = Parcial 1 maxilar
# 2 = Parcial 2 maxilares
# 3 = Total 1 maxilar
# 4 = Parcial + total
# 5 = Total 2 maxilares
# 9 = Sem informação
#
# Critério para COB_PROTESE = 1:
# - necprot ∈ [1, 2, 3, 4, 5]
#
# Critério para COB_PROTESE = 0:
# - necprot == 0
#
# Critério para COB_PROTESE = NA:
# - necprot == 9


##BRASIL

# Criar variável COB_PROTESE
banco_sbbrasil_2023 <- banco_sbbrasil_2023 %>%
  mutate(
    COB_PROTESE = case_when(
      necprot == 9              ~ NA_real_,
      necprot %in% 1:5          ~ 1,
      necprot == 0              ~ 0,
      TRUE                      ~ NA_real_
    )
  )


# Atualizar o plano amostral com a nova variável
desenhoamostral <- update(
  desenhoamostral,
  COB_PROTESE = banco_sbbrasil_2023$COB_PROTESE
)

# Proporção total com necessidade de prótese
svymean(~ COB_PROTESE, design = desenhoamostral, na.rm = TRUE)

# Proporção por faixa etária com IC 95%
prop_cob_protese_faixa <- svyby(
  ~ COB_PROTESE,
  ~ faixa_etaria,
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Formatar como percentual
prop_cob_protese_faixa_formatado <- prop_cob_protese_faixa %>%
  mutate(across(c(COB_PROTESE, se, ci_l, ci_u), ~ round(.x * 100, 4))) %>%
  rename(
    perc_necessidade = COB_PROTESE,
    erro_padrao = se,
    IC_inf = ci_l,
    IC_sup = ci_u
  ) |> 
  mutate(tipo_procedimento = 'Prótese')

# Exibir resultado (exceto crianças de 0 a 14 anos)
print(
  prop_cob_protese_faixa_formatado %>%
    filter(faixa_etaria != "de 0 a 14 anos")
)



##UNIDADES FEDERADAS
# Proporção por UF com IC 95%
prop_cob_protese_uf <- svyby(
  ~ COB_PROTESE,
  ~ nome_uf,
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Formatar como percentual
prop_cob_protese_uf_formatado <- prop_cob_protese_uf %>%
  mutate(
    perc_necessidade = round(COB_PROTESE * 100, 4),
    erro_padrao      = round(se * 100, 4),
    IC_inf           = round(ci_l * 100, 4),
    IC_sup           = round(ci_u * 100, 4)
  ) %>%
  rename(UF = nome_uf) %>%
  select(UF, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  arrange(UF) |> 
  mutate(tipo_procedimento = 'Prótese')

# Exibir resultado
print(prop_cob_protese_uf_formatado)




# Proporção por UF e faixa etária com IC 95%
prop_cob_protese_uf_faixa <- svyby(
  ~ COB_PROTESE,
  ~ interaction(nome_uf, faixa_etaria),
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Organizar resultados em tabela com colunas separadas
prop_cob_protese_uf_faixa_formatado <- prop_cob_protese_uf_faixa %>%
  separate(`interaction(nome_uf, faixa_etaria)`,
           into = c("UF", "faixa_etaria"),
           sep = "\\.") %>%
  mutate(
    perc_necessidade = round(COB_PROTESE * 100, 4),
    erro_padrao      = round(se * 100, 4),
    IC_inf           = round(ci_l * 100, 4),
    IC_sup           = round(ci_u * 100, )
  ) %>%
  select(UF, faixa_etaria, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  filter(faixa_etaria != "de 0 a 14 anos") %>%
  arrange(UF, match(faixa_etaria,
                    c("de 15 a 29 anos",
                      "de 30 a 59 anos",
                      "60 anos e mais"))) |> 
  mutate(tipo_procedimento = 'Prótese')

# Exibir resultado
print(prop_cob_protese_uf_faixa_formatado)



##CAPITAL E INTERIOR
# Proporção com IC 95%
prop_cob_protese_capint <- svyby(
  ~ COB_PROTESE,
  ~ capint2,
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Formatar como percentual
prop_cob_protese_capint_formatado <- prop_cob_protese_capint %>%
  mutate(
    perc_necessidade = round(COB_PROTESE * 100, 4),
    erro_padrao      = round(se * 100, 4),
    IC_inf           = round(ci_l * 100, 4),
    IC_sup           = round(ci_u * 100, 4)
  ) %>%
  rename(capint = capint2) %>%
  select(capint, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  arrange(capint) |>
  mutate(tipo_procedimento = 'Prótese')

# Exibir resultado
print(prop_cob_protese_capint_formatado)




# Proporção capint e faixa etária com IC 95%
prop_cob_protese_capint_faixa <- svyby(
  ~ COB_PROTESE,
  ~ interaction(capint2, faixa_etaria),
  design = desenhoamostral,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

# Organizar resultados em tabela com colunas separadas
prop_cob_protese_capint_faixa_formatado <- prop_cob_protese_capint_faixa %>%
  separate(`interaction(capint2, faixa_etaria)`,
           into = c("capint", "faixa_etaria"),
           sep = "\\.") %>%
  mutate(
    perc_necessidade = round(COB_PROTESE * 100, 4),
    erro_padrao      = round(se * 100, 4),
    IC_inf           = round(ci_l * 100, 4),
    IC_sup           = round(ci_u * 100, 4)
  ) %>%
  select(capint, faixa_etaria, perc_necessidade, erro_padrao, IC_inf, IC_sup) %>%
  filter(faixa_etaria != "de 0 a 14 anos") %>%
  arrange(capint, match(faixa_etaria,
                    c("de 15 a 29 anos",
                      "de 30 a 59 anos",
                      "60 anos e mais"))) |> 
  mutate(tipo_procedimento = 'Prótese')

# Exibir resultado
print(prop_cob_protese_capint_faixa_formatado)


###############################################################################################################
## CÁLCULO DE PARÂMETROS PROCEDIMENTOS INDIVIDUAIS
###############################################################################################################

###############################################################################################################
# Procedimentos odontológicos individuais da Atenção Básica

##BRASIL

# Soma de necessidades dente/sextante 
desenhoamostral <- update(
  desenhoamostral,
  PROC_AB =
    nt_rest_1superf +
    nt_rest_2superf +
    nt_extracao +
    ifelse(idade < 60, nt_remin,   0) +
    ifelse(idade < 60, nt_selante, 0) +
    ifelse(idade > 5,  as.numeric(prevcalc == 1),        0) +
    ifelse(!idade %in% c(5, 12), as.numeric(prevbolsarasa == 1), 0)
)

# Média (per capita) por faixa etária, com SE e IC95%
proc_ab_percapita_faixa <- svyby(
  ~ PROC_AB,
  ~ faixa_etaria,
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  mutate(
    proc_per_capita = round(PROC_AB, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  select(faixa_etaria, proc_per_capita, se, ic_inf, ic_sup) %>%
  arrange(match(faixa_etaria,
                c("de 0 a 14 anos","de 15 a 29 anos","de 30 a 59 anos","60 anos e mais")))

# Printar o indicador per capita por faixa etária
print(proc_ab_percapita_faixa)



##UNIDADES FEDERADAS
# Criar variável PROC_AB com soma de necessidades dente/sextante
desenhoamostral <- update(
  desenhoamostral,
  PROC_AB =
    nt_rest_1superf +
    nt_rest_2superf +
    nt_extracao +
    ifelse(idade < 60, nt_remin,   0) +
    ifelse(idade < 60, nt_selante, 0) +
    ifelse(idade > 5,  as.numeric(prevcalc == 1),        0) +
    ifelse(!idade %in% c(5, 12), as.numeric(prevbolsarasa == 1), 0)
)

# Calcular média per capita por UF com erro padrão e IC95%
proc_ab_percapita_uf <- svyby(
  ~ PROC_AB,
  ~ nome_uf,  
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  mutate(
    proc_per_capita = round(PROC_AB, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  rename(UF = nome_uf) %>%
  select(UF, proc_per_capita, se, ic_inf, ic_sup) %>%
  arrange(UF)

# Visualizar resultado por UF
print(proc_ab_percapita_uf)



# Calcular média per capita por UF e faixa etária com erro padrão e IC95%
proc_ab_percapita_uf_faixa <- svyby(
  ~ PROC_AB,
  ~ interaction(nome_uf, faixa_etaria, drop = TRUE),
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  rename_with(~ "grupo", .cols = 1) %>%
  tidyr::separate(
    col = grupo,
    into = c("UF", "faixa_etaria"),
    sep = "\\."
  ) %>%
  mutate(
    proc_per_capita = round(PROC_AB, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  select(UF, faixa_etaria, proc_per_capita, se, ic_inf, ic_sup) %>%
  arrange(UF, match(faixa_etaria,
                    c("de 0 a 14 anos","de 15 a 29 anos","de 30 a 59 anos","60 anos e mais")))

# Visualizar resultado por UF e faixa etária
print(proc_ab_percapita_uf_faixa)




##CAPITAL E INTERIOR
# Calcular média per capita por capint com erro padrão e IC95%
proc_ab_percapita_capint <- svyby(
  ~ PROC_AB,
  ~ capint2,  
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  mutate(
    proc_per_capita = round(PROC_AB, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  rename(capint = capint2) %>%
  select(capint, proc_per_capita, se, ic_inf, ic_sup) %>%
  arrange(capint)

# Visualizar resultado por capint
print(proc_ab_percapita_capint)



# Calcular média per capita por capint e faixa etária com erro padrão e IC95%
proc_ab_percapita_capint_faixa <- svyby(
  ~ PROC_AB,
  ~ interaction(capint2, faixa_etaria, drop = TRUE),
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  rename_with(~ "grupo", .cols = 1) %>%
  tidyr::separate(
    col = grupo,
    into = c("capint", "faixa_etaria"),
    sep = "\\."
  ) %>%
  mutate(
    proc_per_capita = round(PROC_AB, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  select(capint, faixa_etaria, proc_per_capita, se, ic_inf, ic_sup) %>%
  arrange(capint, match(faixa_etaria,
                    c("de 0 a 14 anos","de 15 a 29 anos","de 30 a 59 anos","60 anos e mais")))

# Visualizar resultado por capint e faixa etária
print(proc_ab_percapita_capint_faixa)




###############################################################################################################
# Procedimentos de Endodontia

##BRASIL

# Atualizar plano amostral
desenhoamostral <- update(
  desenhoamostral,
  ENDO_PC = ifelse(is.na(nt_tratpulpar), 0, nt_tratpulpar)
)


# Média (per capita) por faixa etária, com SE e IC95%
endo_percapita_faixa <- svyby(
  ~ ENDO_PC,
  ~ faixa_etaria,
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  mutate(
    endo_per_capita = round(ENDO_PC, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  select(faixa_etaria, endo_per_capita, se, ic_inf, ic_sup) %>%
  arrange(match(faixa_etaria,
                c("de 0 a 14 anos","de 15 a 29 anos","de 30 a 59 anos","60 anos e mais")))


# Printar o indicador per capita por faixa etária
print(endo_percapita_faixa)


##UNIDADES FEDERADAS

# Calcular média per capita por UF com erro padrão e IC95%
endo_percapita_uf <- svyby(
  ~ ENDO_PC,
  ~ nome_uf,
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  mutate(
    endo_per_capita = round(ENDO_PC, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  rename(UF = nome_uf) %>%
  select(UF, endo_per_capita, se, ic_inf, ic_sup) %>%
  arrange(UF)

# Visualizar resultado por UF
print(endo_percapita_uf)


# Calcular média per capita por UF e faixa etária com erro padrão e IC95%
endo_percapita_uf_faixa <- svyby(
  ~ ENDO_PC,
  ~ interaction(nome_uf, faixa_etaria),
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  separate(`interaction(nome_uf, faixa_etaria)`,
           into = c("UF", "faixa_etaria"),
           sep  = "\\.") %>%
  mutate(
    endo_per_capita = round(ENDO_PC, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  select(UF, faixa_etaria, endo_per_capita, se, ic_inf, ic_sup) %>%
  arrange(UF, match(faixa_etaria,
                    c("de 0 a 14 anos", "de 15 a 29 anos", "de 30 a 59 anos", "60 anos e mais")))

# Visualizar resultado por UF e faixa etária
print(endo_percapita_uf_faixa)





##CAPITAL E INTERIOR

# Calcular média per capita por capint com erro padrão e IC95%
endo_percapita_capint <- svyby(
  ~ ENDO_PC,
  ~ capint2,
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  mutate(
    endo_per_capita = round(ENDO_PC, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  rename(capint= capint2) %>%
  select(capint, endo_per_capita, se, ic_inf, ic_sup) %>%
  arrange(capint)

# Visualizar resultado por capint
print(endo_percapita_capint)


# Calcular média per capita por capint e faixa etária com erro padrão e IC95%
endo_percapita_capint_faixa <- svyby(
  ~ ENDO_PC,
  ~ interaction(capint2, faixa_etaria),
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  separate(`interaction(capint2, faixa_etaria)`,
           into = c("capint", "faixa_etaria"),
           sep  = "\\.") %>%
  mutate(
    endo_per_capita = round(ENDO_PC, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  select(capint, faixa_etaria, endo_per_capita, se, ic_inf, ic_sup) %>%
  arrange(capint, match(faixa_etaria,
                    c("de 0 a 14 anos", "de 15 a 29 anos", "de 30 a 59 anos", "60 anos e mais")))

# Visualizar resultado por capint e faixa etária
print(endo_percapita_capint_faixa)



###############################################################################################################
# Procedimentos de Periodontia Especializada

# Número de sextantes com bolsa profunda (nsextantes_bolsaprofunda)


##BRASIL
# Atualizar plano amostral
desenhoamostral <- update(
  desenhoamostral,
  PROC_PERIO = nsextantes_bolsaprofunda
)


# Média (per capita) por faixa etária, com SE e IC95%
proc_perio_percapita_faixa <- svyby(
  ~ PROC_PERIO,
  ~ faixa_etaria,
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  mutate(
    proc_per_capita = round(PROC_PERIO, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  select(faixa_etaria, proc_per_capita, se, ic_inf, ic_sup) %>%
  arrange(match(faixa_etaria,
                c("de 0 a 14 anos","de 15 a 29 anos","de 30 a 59 anos","60 anos e mais")))

# Printar o indicador per capita por faixa etária
print(
  proc_perio_percapita_faixa %>%
    filter(faixa_etaria != "de 0 a 14 anos")
)




##UNIDADES FEDERADAS

# Calcular média per capita por UF com erro padrão e IC95%
proc_perio_percapita_uf <- svyby(
  ~ PROC_PERIO,
  ~ nome_uf,
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  mutate(
    proc_per_capita = round(PROC_PERIO, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  rename(UF = nome_uf) %>%
  select(UF, proc_per_capita, se, ic_inf, ic_sup) %>%
  arrange(UF)

# Visualizar resultado por UF
print(  proc_perio_percapita_uf)


# Calcular média per capita por UF e faixa etária com erro padrão e IC95%
proc_perio_percapita_uf_faixa <- svyby(
  ~ PROC_PERIO,
  ~ interaction(nome_uf, faixa_etaria),
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  separate(`interaction(nome_uf, faixa_etaria)`, into = c("UF", "faixa_etaria"), sep = "\\.") %>%
  mutate(
    proc_per_capita = round(PROC_PERIO, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  select(UF, faixa_etaria, proc_per_capita, se, ic_inf, ic_sup) %>%
  arrange(UF, match(faixa_etaria,
                    c("de 0 a 14 anos", "de 15 a 29 anos", "de 30 a 59 anos", "60 anos e mais")))

# Visualizar resultado por UF e faixa etária
print(
  proc_perio_percapita_uf_faixa %>%
    filter(faixa_etaria != "de 0 a 14 anos")
)




##CAPITAL E INTERIOE

# Calcular média per capita por capint com erro padrão e IC95%
proc_perio_percapita_capint <- svyby(
  ~ PROC_PERIO,
  ~ capint2,
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  mutate(
    proc_per_capita = round(PROC_PERIO, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  rename(capint = capint2) %>%
  select(capint, proc_per_capita, se, ic_inf, ic_sup) %>%
  arrange(capint)

# Visualizar resultado por capint
print(  proc_perio_percapita_capint)




# Calcular média per capita por UF e faixa etária com erro padrão e IC95%
proc_perio_percapita_capint_faixa <- svyby(
  ~ PROC_PERIO,
  ~ interaction(capint2, faixa_etaria),
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  separate(`interaction(capint2, faixa_etaria)`, into = c("capint", "faixa_etaria"), sep = "\\.") %>%
  mutate(
    proc_per_capita = round(PROC_PERIO, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  select(capint, faixa_etaria, proc_per_capita, se, ic_inf, ic_sup) %>%
  arrange(capint, match(faixa_etaria,
                    c("de 0 a 14 anos", "de 15 a 29 anos", "de 30 a 59 anos", "60 anos e mais")))

# Visualizar resultado por UF e faixa etária
print(
  proc_perio_percapita_capint_faixa %>%
    filter(faixa_etaria != "de 0 a 14 anos")
)



###############################################################################################################
# Procedimentos de Prótese

# necprot	Necessidade de prótese dentária superior e inferior	0	Não necessita prótese dentária
# necprot	Necessidade de prótese dentária superior e inferior	1	Parcial 1 maxilar
# necprot	Necessidade de prótese dentária superior e inferior	2	Parcial 2 maxilares
# necprot	Necessidade de prótese dentária superior e inferior	3	Prótese total em 1 maxilar
# necprot	Necessidade de prótese dentária superior e inferior	4	Prótese parcial e total
# necprot	Necessidade de prótese dentária superior e inferior	5	Prótese total nos 2 maxilares
# necprot	Necessidade de prótese dentária superior e inferior	9	Sem informação


# Criar a variável NUMPROT a partir de necprot
banco_sbbrasil_2023 <- banco_sbbrasil_2023 %>%
  mutate(
    NUMPROT = case_when(
      necprot == 1 ~ 1,
      necprot == 2 ~ 2,
      necprot == 3 ~ 3,
      necprot == 4 ~ 2,
      necprot == 5 ~ 2,
      TRUE         ~ 0  # Inclui necprot == 0 ou 9
    )
  )

# Atualizar plano amostral com a nova variável
desenhoamostral <- update(
  desenhoamostral,
  NUMPROT = banco_sbbrasil_2023$NUMPROT
)

# Calcular média per capita de próteses por faixa etária
proc_protese_percapita_faixa <- svyby(
  ~ NUMPROT,
  ~ faixa_etaria,
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  mutate(
    proc_per_capita = round(NUMPROT, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  select(faixa_etaria, proc_per_capita, se, ic_inf, ic_sup) %>%
  arrange(match(faixa_etaria,
                c("de 0 a 14 anos", "de 15 a 29 anos", "de 30 a 59 anos", "60 anos e mais")))

# Exibir resultados
print(
  proc_protese_percapita_faixa %>%
    filter(faixa_etaria != "de 0 a 14 anos")
)



##UNIDADES FEDERADAS

# Calcular média per capita por UF com erro padrão e IC95%
proc_perio_percapita_uf <- svyby(
  ~ PROC_PERIO,
  ~ nome_uf,  # variável com os nomes das UFs
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  mutate(
    proc_per_capita = round(PROC_PERIO, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  rename(UF = nome_uf) %>%
  select(UF, proc_per_capita, se, ic_inf, ic_sup) %>%
  arrange(UF)

# Visualizar resultado por UF
print(proc_perio_percapita_uf)



# Calcular média per capita de próteses por UF e faixa etária com SE e IC95%
proc_protese_percapita_uf_faixa <- svyby(
  ~ NUMPROT,
  ~ interaction(nome_uf, faixa_etaria),
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  separate(`interaction(nome_uf, faixa_etaria)`, into = c("UF", "faixa_etaria"), sep = "\\.") %>%
  mutate(
    proc_per_capita = round(NUMPROT, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  select(UF, faixa_etaria, proc_per_capita, se, ic_inf, ic_sup) %>%
  filter(faixa_etaria != "de 0 a 14 anos") %>%
  arrange(UF, match(faixa_etaria,
                    c("de 15 a 29 anos", "de 30 a 59 anos", "60 anos e mais")))

# Exibir resultado por UF e faixa etária (exceto 0 a 14 anos)
print(proc_protese_percapita_uf_faixa)



##CAPITAL E INTERIOR

# Calcular média per capita por capint com erro padrão e IC95%
proc_perio_percapita_capint <- svyby(
  ~ PROC_PERIO,
  ~ capint2,  
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  mutate(
    proc_per_capita = round(PROC_PERIO, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  rename(capint = capint2) %>%
  select(capint, proc_per_capita, se, ic_inf, ic_sup) %>%
  arrange(capint)

# Visualizar resultado por UF
print(proc_perio_percapita_capint)



# Calcular média per capita de próteses por capint e faixa etária com SE e IC95%
proc_protese_percapita_capint_faixa <- svyby(
  ~ NUMPROT,
  ~ interaction(capint2, faixa_etaria),
  design  = desenhoamostral,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = c("se", "ci")
) %>%
  separate(`interaction(capint2, faixa_etaria)`, into = c("capint", "faixa_etaria"), sep = "\\.") %>%
  mutate(
    proc_per_capita = round(NUMPROT, 4),
    se              = round(se, 4),
    ic_inf          = round(ci_l, 4),
    ic_sup          = round(ci_u, 4)
  ) %>%
  select(capint, faixa_etaria, proc_per_capita, se, ic_inf, ic_sup) %>%
  filter(faixa_etaria != "de 0 a 14 anos") %>%
  arrange(capint, match(faixa_etaria,
                    c("de 15 a 29 anos", "de 30 a 59 anos", "60 anos e mais")))

# Exibir resultado por UF e faixa etária (exceto 0 a 14 anos)
print(proc_protese_percapita_capint_faixa)
###############################################################################################################



# Unindo tudo -------------------------------------------------------------

ufs_cod <- data.frame(
  UF = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins",
         "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", 
         "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais", 
         "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", 
         "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", 
         "Mato Grosso", "Goiás", "Distrito Federal"),
  
  cod_uf = c(11, 12, 13, 14, 15, 16, 17,
             21, 22, 23, 24, 25, 26, 27, 28, 29,
             31, 32, 33, 35, 41, 42, 43, 50, 
             51, 52, 53),
)


faixas_cod <- data.frame(
  faixa_etaria = c("de 0 a 14 anos", 
                   "de 15 a 29 anos", 
                   "de 30 a 59 anos", 
                   "60 anos e mais"),
  cod_faixa = c(1, 2, 3, 4)
)

tabelas_cobertura <- ls(pattern = "^prop_cob_.*_uf_.*_formatado$")

tabela_cobertura <- do.call(rbind,mget(tabelas_uf)) %>%
  left_join(ufs_cod, by = "UF") %>%
  relocate(cod_uf, .after = UF) %>%
  left_join(faixas_cod, by = "faixa_etaria") %>%
  relocate(cod_faixa, .after = faixa_etaria)

tabelas_percapita <- ls(pattern = "uf.*faixa.*percapita")

tabela_percapita <- do.call(rbind,mget(tabelas_percapita)) %>%
  left_join(ufs_cod, by = "UF") %>%
  relocate(cod_uf, .after = UF) %>%


