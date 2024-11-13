

# carregando pacotes  -----------------------------------------------------


library(tidyverse)
library(arrow)
library(patchwork)


# lendo os dados ----------------------------------------------------------

resultado_teste1 <- 
  read_parquet("~/GitHub/saude_bucal/02_script/resultados/resultado1211.parquet")

# existem alguns cenarios que o resultado da especialidade ficaram iguais. Pq? Por conta de iteracao 
# pela da APS

# grafico - boxplot -------------------------------------------------------


g1 <- resultado_teste1 |> 
  filter(regiao_saude == "Jundiaí" |
         regiao_saude == "Juruá e Tarauacá/Envira" |
         regiao_saude == "Amor Perfeito"|
         regiao_saude == "São Luís") |> 
  mutate(rr = 100 * rr) |>
  ggplot(aes(x = nivel, y = rr,
             fill = regiao_saude)) + geom_boxplot() +
  theme_minimal() + 
  ggtitle("Health region: Jundiaí(SP), Juruá e Tarauacá/Envira (AC), Amor Perfeito (TO), São Luis (MA)") + 
  ylab("Relative Results (%)") + xlab("Healthcare level") +
  coord_flip()

ggsave(filename = "05_graficos/distribuicao_cenarios.jpeg",
       g1, width = 10, height = 5, dpi = 400)


# Gráficos para cenários -------------------------------------------------------

## cenario de maiores gaps (menores percentuais)
# tempo APS = 55, tempo endo = 55, tempo peri = 55, tempo prot = 55
# ttd = 1776, pd = 0.5, pl = 0.6
# Apenas profissionais SUS
# Necessidade de todos 

cenario1 <- 
  resultado_teste1 |> 
  filter(regiao_saude == "Jundiaí" & 
           cenario == '11629') |> 
  mutate(descricao_cenario = "1. Baixa produtividade, oferta não otimizada, oferta SUS, população total")

## cenario onde há maior produtividade, mas demais constantes 
# tempo APS = 25, tempo endo = 35, tempo peri = 35, tempo prot = 35
# ttd = 1776, pd = 0.5, pl = 0.6
# Apenas SUS
# Necessidade de todos

cenario2 <- 
  resultado_teste1 |> 
  filter(regiao_saude == "Jundiaí" & 
           cenario == '7777') |> 
  mutate(descricao_cenario = "2. Alta produtividade, oferta não otimizada, oferta SUS, população total")



## cenario onde há maior produtividade e oferta otimizada
# tempo APS = 25, tempo endo = 35, tempo peri = 35, tempo prot = 35
# ttd = 1576, pd = 0.7, pl = 0.8
# Apenas SUS
# Necessidade SUS dependente

cenario3 <- 
  resultado_teste1 |> 
  filter(regiao_saude == "Jundiaí" & 
           cenario == '34') |> 
  mutate(descricao_cenario = "3. Alta produtividade, oferta otimizada, oferta SUS, SUS dependente")



## cenario onde há maior produtividade, oferta otimizada e dedução do SUS dependente
# tempo APS = 25, tempo endo = 35, tempo peri = 35, tempo prot = 35
# ttd = 1576, pd = 0.7, pl = 0.8
# Apenas SUS
# Necessidade de SUS

cenario4 <- 
  resultado_teste1 |> 
  filter(regiao_saude == "Jundiaí" & 
           cenario == '36')|> 
  mutate(descricao_cenario = "4. Alta produtividade, oferta otimizada, oferta todos, SUS dependente")


cenarios <- rbind(cenario1,
                  cenario2,
                  cenario3,
                  cenario4)



cenarios |> 
  ungroup() |> 
  select(nivel, descricao_cenario, 
         oferta_rs, necessidade) |> 
  gather(key = componente, 
         value = total, 3:4) |> 
  mutate(total = round(total)) |> 
  ggplot(aes(x = descricao_cenario, 
             y = total, fill = componente)) + 
  geom_col(position = position_dodge(width = 0.9)) + 
  geom_text(aes(label = total), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3) + 
  facet_wrap(~nivel) +
  coord_flip() + 
  xlab("Descrição do cenário") +
  theme_minimal()





