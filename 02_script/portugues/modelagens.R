
library(tidyverse)

df <- read_csv("01_dados/base_modelagem_inferencial.csv") |> 
        select(-`...1`)

spdf <- 
  geojson_read("~/GitHub/saude_bucal/01_dados/shape_file_regioes_saude.json", 
               what = "sp") 

rs <- read_csv("01_dados/hierarquia_atualizada.csv") |> 
        distinct(regiao, cod_regsaud)

spdf_fortified <- 
  sf::st_as_sf(spdf) |> 
  sf::st_set_crs(4326) |> 
  unique()

# Definir limites de longitude e latitude para focar no Brasil
limite_long <- c(-75, -28)  # limites de longitude
limite_lat <- c(-33, 4)     # limites de latitude

spdf_fortified$reg_id <- as.character(spdf_fortified$reg_id)

df <- df |> 
  mutate(cod_regsaud = as.character(cod_regsaud)) |> 
  left_join(spdf_fortified, 
            by = c("cod_regsaud"="reg_id"))

df <- df |> 
  mutate(cod_regsaud = as.numeric(cod_regsaud)) |> 
  left_join(rs, 
            by = c("cod_regsaud"="cod_regsaud"))

# modelagem sem espacial --------------------------------------------------

modelo_multiplo <- 
  lm("rr_2 ~ retencao_geral + pibpc_rs + 
             matriculas + equip + regiao", 
     data = df)

summary(modelo_multiplo)

modelo_multiplo |> 
  flextable::as_flextable()

bptest(modelo_multiplo)
plot(resid(modelo_multiplo))

library(sandwich)
library(lmtest)

# Apply robust standard errors
coeftest(modelo_multiplo, vcov = vcovHC(modelo_multiplo, type = "HC1"))

modelo_log <- lm(log(rr_2) ~ retencao_geral + pibpc_rs + matriculas + equip + regiao, data = df)
summary(modelo_log)
bptest(modelo_log)


weights <- 1 / (residuals(modelo_multiplo)^2)

# Step 3: Fit weighted model
modelo_wls <- lm(rr_2 ~ retencao_geral + pibpc_rs + matriculas + equip + regiao, 
                 data = df, weights = weights)
summary(modelo_wls)


modelo_poly <- lm(rr_2 ~ retencao_geral + I(retencao_geral^2) + 
                    pibpc_rs + I(pibpc_rs^2) + 
                    matriculas + equip + regiao, 
                  data = df)
summary(modelo_poly)
bptest(modelo_poly)

par(mfrow = c(2, 3))
plot(df$retencao_geral, resid(modelo_multiplo), main = "Residuals vs. Retention")
plot(df$pibpc_rs, resid(modelo_multiplo), main = "Residuals vs. GDP per capita")
plot(df$matriculas, resid(modelo_multiplo), main = "Residuals vs. Enrollment")
plot(df$equip, resid(modelo_multiplo), main = "Residuals vs. Equipment")

modelo_transform <- lm(rr_2 ~ retencao_geral + pibpc_rs + 
                         log1p(matriculas) + log1p(equip) + regiao, 
                       data = df)

summary(modelo_transform)

bptest(modelo_transform)


library(nlme)
modelo_gls <- gls(rr_2 ~ retencao_geral + pibpc_rs + matriculas + equip + regiao, 
                  data = df, weights = varPower())

summary(modelo_gls)

plot(fitted(modelo_gls), 
     resid(modelo_gls))

library(lmtest)
dwtest(modelo_gls)

modelo_gls1 <- gls(rr_2 ~ retencao_geral + pibpc_rs + matriculas + equip + regiao, 
                   data = df, weights = varPower())
modelo_gls2 <- gls(rr_2 ~ retencao_geral + pibpc_rs + matriculas + equip + regiao, 
                   data = df, weights = varExp())
AIC(modelo_gls1, modelo_gls2)

resettest(rr_2 ~ retencao_geral + pibpc_rs + matriculas + equip + regiao, data = df)

shapiro.test(resid(modelo_gls))
qqnorm(resid(modelo_gls))
qqline(resid(modelo_gls))
