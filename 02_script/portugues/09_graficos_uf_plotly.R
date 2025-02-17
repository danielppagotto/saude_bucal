
library(plotly)


cenarios_regioes <- read_excel("01_dados/cenarios_regioes.xlsx")

baseline <- 
  cenarios_regioes |> 
  filter(cenario == "baseline")


# ggplot ------------------------------------------------------------------

baseline_aes <- baseline |>
  filter(nivel == "AES") |> 
  group_by(uf_sigla) |> 
  summarise(necessidade = sum(necessidade),
            necessidade_sus = sum(necessidade_sus),
            oferta_rs = sum(oferta),
            rr = mean(rr)) |> 
  mutate(rr = round(rr, 2)) |>
  mutate(regiao = case_when(
    uf_sigla %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    uf_sigla %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
    uf_sigla %in% c("PR", "RS", "SC") ~ "Sul")) 

baseline |> 
  ggplot(aes(x = fct_reorder(uf_sigla,rr), 
             y = rr,
             fill = regiao)) +
  geom_col() + coord_flip()


# fazendo gráfico em plotly -----------------------------------------------

baseline$uf_reordered <- with(baseline, fct_reorder(uf_sigla, rr))


plot_ly(baseline,
        x = ~rr,
        y = ~uf_reordered,
        type = 'bar',
        orientation = 'h',
        color = ~regiao) %>%
  layout(xaxis = list(title = "Resultado Relativo"),
         yaxis = list(title = "UF"))

  
