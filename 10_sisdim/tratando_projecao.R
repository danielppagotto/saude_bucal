
library(readxl)
library(tidyverse)

projecoes <- read_excel("~/GitHub/saude_bucal/10_sisdim/projecoes.xlsx")

projecoes_tratadas <- projecoes |> 
  filter(uf != "BR" & 
         uf != "CO" & 
         uf != "SU" &
         uf != "SD" &
         uf != "ND" &
         uf != "NO") |> 
  filter(sexo == "Ambos") |> 
  gather(key = 'ano',
         value = 'total',
         6:41) |> 
  mutate(ano = as.numeric(ano)) |> 
  mutate(faixa_etaria = 
              case_when(
                grupo == '00-04' | grupo == '05-09' |
                grupo == '10-14'   ~ '00 a 14 anos',
                grupo == '15-19' | grupo == '20-24' |
                grupo == '25-29' ~ '15 a 29 anos',
                grupo == '30-34' | grupo == '35-39' |
                grupo == '40-44' | grupo == '45-49' |
                grupo == '50-54' | grupo == '55-59' ~ '30 a 59 anos',
                TRUE ~ '60 anos ou mais')) |> 
  group_by(cod_uf, uf, ano, faixa_etaria) |> 
  summarise(total = sum(total)) |> 
  filter(ano > 2014)

writexl::write_xlsx(projecoes_tratadas,
                    "~/GitHub/saude_bucal/10_sisdim/projecoes_tratadas.xlsx")
