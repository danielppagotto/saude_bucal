

# Carregando pacotes -------------------------------------------------------


library(tidyverse)
library(arrow)
library(patchwork)
library(geojsonio)
library(geojsonsf)
library(geobr)
library(scales)
library(ggspatial) 
library(sf)
library(readxl)
library(leaflet)


cenarios_regioes <- read_excel("01_dados/cenarios_regioes.xlsx")

estados_br <- read_state(year = 2020,
                         showProgress = FALSE)

spdf <- 
  geojson_read("~/GitHub/saude_bucal/01_dados/shape_file_regioes_saude.json", 
               what = "sp") 

spdf_fortified <- 
  sf::st_as_sf(spdf)

# Definir limites de longitude e latitude para focar no Brasil
limite_long <- c(-75, -28)  # limites de longitude
limite_lat <- c(-33, 4)     # limites de latitude



# Mapas para cenário baseline  --------------------------------------------


baseline <- 
  cenarios_regioes |> 
    filter(cenario == "baseline")


baseline <- 
  baseline |> 
  mutate(rr = if_else(rr > 100, 100, rr)) |> 
  left_join(spdf_fortified, 
            by = c("cod_regsaud"="reg_id")) |> 
  distinct_all() |> 
  mutate(scenario = "Baseline")

if (!"geometry" %in% names(baseline)) {
  
  baseline <- st_as_sf(baseline, 
                       coords = c("longitude", 
                                  "latitude"), 
                       crs = 4326) 
}


# 1) APS ---------------------------------------------------------------------

# cenario 1 - baseline ---------------------------------------------------------------

c1_baseline <- 
  ggplot() +
  geom_sf(data = baseline |> 
            filter(nivel == "APS"), 
          aes(fill = rr, geometry = geometry), 
          color = "#f5f5f5") +
  geom_sf(data = estados_br, 
          fill = NA, 
          color = "#4c4d4a", 
          size = 0.1) + 
  geom_sf(data = estados_br |> filter(abbrev_state == "MG" &
                                        abbrev_state == "SP"), 
          fill = NA, 
          color = "black", # Cor preta mais escura
          size = 2) +
  scale_fill_gradientn(colors = c("#D92B3A", 
                                  "#d4e302",
                                  "#02592e"), 
                       values = 
                         rescale(c(0,50,100)), 
                       limits = c(0,100),
                       breaks = c(0, 50, 100)) + 
  theme_minimal() +
  labs(fill = "Relative gap") +
  annotation_north_arrow(location = "tr",  
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotate("text", x = -Inf, y = Inf, label = "1", # Canto superior esquerdo
           hjust = -0.5, vjust = 1.5, size = 6) + 
  theme(
    legend.position = "none",
    legend.justification = "center",
    legend.box = "horizontal",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.border = element_rect(color = "black", 
                                fill = NA, 
                                size = 1), 
    plot.margin = margin(10, 10, 10, 10)) +
  ggtitle("Baseline scenario")

c1_baseline

# cenario 2 ---------------------------------------------------------------

c2_productivity <- 
  cenarios_regioes |> 
  filter(cenario == "cenario 2")

c2_productivity <- 
  c2_productivity |> 
  mutate(rr = if_else(rr > 100, 100, rr)) |> 
  left_join(spdf_fortified, 
            by = c("cod_regsaud"="reg_id")) |> 
  distinct_all() |> 
  mutate(scenario = "Scenario 2")


c2 <- 
  ggplot() +
  geom_sf(data = c2_productivity |> 
            filter(nivel == "APS"), 
          aes(fill = rr, geometry = geometry), 
          color = "#f5f5f5") +
  geom_sf(data = estados_br, 
          fill = NA, 
          color = "#4c4d4a", 
          size = 0.1) + 
  geom_sf(data = estados_br |> filter(abbrev_state == "MG" &
                                        abbrev_state == "SP"), 
          fill = NA, 
          color = "black", # Cor preta mais escura
          size = 2) +
  scale_fill_gradientn(colors = c("#D92B3A", 
                                  "#d4e302",
                                  "#02592e"), 
                       values = 
                         rescale(c(0,50,100)), 
                       limits = c(0,100),
                       breaks = c(0, 50, 100)) + 
  theme_minimal() +
  labs(fill = "Relative gap") +
  annotation_north_arrow(location = "tr",  
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotate("text", x = -Inf, y = Inf, label = "2", 
           hjust = -0.5, vjust = 1.5, size = 6) + 
  theme(
    legend.position = "none",
    legend.justification = "center",
    legend.box = "horizontal",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.border = element_rect(color = "black", 
                                fill = NA, 
                                size = 1), 
    plot.margin = margin(10, 10, 10, 10)) +
  ggtitle("Improved productivity \nscenario")


# cenario 3 ---------------------------------------------------------------

c3_productivity <- 
  cenarios_regioes |> 
  filter(cenario == "cenario 3")


c3_productivity <- 
  c3_productivity |> 
  mutate(rr = if_else(rr > 100, 100, rr)) |> 
  left_join(spdf_fortified, 
            by = c("cod_regsaud"="reg_id")) |> 
  distinct_all() |> 
  mutate(scenario = "Scenario 3")

c3 <- 
  ggplot() +
  geom_sf(data = c3_productivity |> 
            filter(nivel == "APS"), 
          aes(fill = rr, geometry = geometry), 
          color = "#f5f5f5") +
  geom_sf(data = estados_br, 
          fill = NA, 
          color = "#4c4d4a", 
          size = 0.1) + 
  geom_sf(data = estados_br |> filter(abbrev_state == "MG" &
                                        abbrev_state == "SP"), 
          fill = NA, 
          color = "black", # Cor preta mais escura
          size = 2) +
  scale_fill_gradientn(colors = c("#D92B3A", 
                                  "#d4e302",
                                  "#02592e"), 
                       values = 
                         rescale(c(0,50,100)), 
                       limits = c(0,100),
                       breaks = c(0, 50, 100)) + 
  theme_minimal() +
  labs(fill = "Relative gap (%)") +
  annotation_north_arrow(location = "tr",  
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotate("text", x = -Inf, y = Inf, label = "3", # Canto superior esquerdo
           hjust = -0.5, vjust = 1.5, size = 6) + 
  theme(
    legend.position = "none",
    legend.justification = "center",
    legend.box = "horizontal",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.border = element_rect(color = "black", 
                                fill = NA, 
                                size = 1), 
    plot.margin = margin(10, 10, 10, 10)) +
    ggtitle("SUS dependent deduction \nscenario")



# cenario 4 ---------------------------------------------------------------

c4_productivity <- 
  cenarios_regioes |> 
  filter(cenario == "cenario 4")


c4_productivity <- 
  c4_productivity |> 
  mutate(rr = if_else(rr > 100, 100, rr)) |> 
  left_join(spdf_fortified, 
            by = c("cod_regsaud"="reg_id")) |> 
  distinct_all() |> 
  mutate(scenario = "Scenario 4")

c4 <- 
  ggplot() +
  geom_sf(data = c4_productivity |> 
            filter(nivel == "APS"), 
          aes(fill = rr, geometry = geometry), 
          color = "#f5f5f5") +
  geom_sf(data = estados_br, 
          fill = NA, 
          color = "#4c4d4a", 
          size = 0.1) + 
   scale_fill_gradientn(colors = c("#D92B3A", 
                                  "#d4e302",
                                  "#02592e"), 
                       values = 
                         rescale(c(0,50,100)), 
                       limits = c(0,100),
                       breaks = c(0, 50, 100)) + 
  theme_minimal() +
  labs(fill = "Relative gap") +
  annotation_north_arrow(location = "tr",  
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotate("text", x = -Inf, y = Inf, label = "4", # Canto superior esquerdo
           hjust = -0.5, vjust = 1.5, size = 6) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box = "horizontal",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.border = element_rect(color = "black", 
                                fill = NA, 
                                size = 1), 
    plot.margin = margin(10, 10, 10, 10)) +
    ggtitle("Supply of professionals with any \ntype of contract")


# grafico de linhas -------------------------------------------------------


# grafico 

grafico <- rbind(baseline,
                 c2_productivity,
                 c3_productivity,
                 c4_productivity)

evolucao <- grafico |> 
  filter(nivel == "APS") |> 
  mutate(regiao = case_when(
    uf_sigla %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    uf_sigla %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
    uf_sigla %in% c("PR", "RS", "SC") ~ "Sul")) |> 
  group_by(nivel, scenario, regiao) |> 
  summarise(necessidade = sum(necessidade),
            necessidade_sus = sum(necessidade_sus),
            oferta_rs = sum(oferta),
            rr = mean(rr)) |> 
  mutate(rr = round(rr, 2)) |> 
  rename(Região = regiao) |> 
  mutate(Região = factor(Região, levels = c("Nordeste", "Norte", "Centro-Oeste", "Sudeste", "Sul"))) |> 
  ggplot(aes(x = scenario, y = rr, fill = Região)) + 
  geom_col(position = "dodge") +
  geom_text(aes(label = rr),
            size = 5, 
            fontface = "bold",
            position = position_dodge(width = 0.9),
            vjust = -0.25
  ) +
  theme_minimal() +
  ylab("Relative gap (%)") + 
  theme(
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 16, face = "bold"),  
    axis.text.x = element_text(size = 14),                 
    axis.text.y = element_text(size = 14),                 
    legend.title = element_text(size = 16, face = "bold"), 
    legend.text = element_text(size = 14),                 
    plot.title = element_text(size = 20, face = "bold"),   
    plot.subtitle = element_text(size = 16),               
    strip.text = element_text(size = 14),
    legend.position = "bottom" 
  ) + ylim(0, 100)


evolucao

# Figura 3 ----------------------------------------------------------------


c_aps <- (c1_baseline | c2 | c3 | c4) / evolucao

ggsave(filename = "04_mapas/cenarios_aps.png",
       c_aps, dpi = 1000, height = 10, width = 20)

ggsave(filename = "04_mapas/cenarios_aps.svg",
       c_aps, dpi = 1000, height = 10, width = 20)


# 2) AES ------------------------------------------------------------------

# cenario 1 ---------------------------------------------------------------

d1_baseline <- 
  ggplot() +
  geom_sf(data = baseline |> 
            filter(nivel == "AES"), 
          aes(fill = rr, geometry = geometry), 
          color = "#f5f5f5") +
  geom_sf(data = estados_br, 
          fill = NA, 
          color = "#4c4d4a", 
          size = 0.1) + 
  geom_sf(data = estados_br |> filter(abbrev_state == "MG" &
                                        abbrev_state == "SP"), 
          fill = NA, 
          color = "black", # Cor preta mais escura
          size = 2) +
  scale_fill_gradientn(colors = c("#D92B3A", 
                                  "#d4e302",
                                  "#02592e"), 
                       values = 
                         rescale(c(0,50,100)), 
                       limits = c(0,100),
                       breaks = c(0, 50, 100)) + 
  theme_minimal() +
  labs(fill = "Relative gap") +
  annotation_north_arrow(location = "tr",  
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotate("text", x = -Inf, y = Inf, label = "1", # Canto superior esquerdo
           hjust = -0.5, vjust = 1.5, size = 6) + 
  theme(
    legend.position = "none",
    legend.justification = "center",
    legend.box = "horizontal",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.border = element_rect(color = "black", 
                                fill = NA, 
                                size = 1), 
    plot.margin = margin(10, 10, 10, 10)) +
  ggtitle("Baseline scenario")


# cenario 2 ---------------------------------------------------------------


d2 <- 
  ggplot() +
  geom_sf(data = c2_productivity |> 
            filter(nivel == "AES"), 
          aes(fill = rr, geometry = geometry), 
          color = "#f5f5f5") +
  geom_sf(data = estados_br, 
          fill = NA, 
          color = "#4c4d4a", 
          size = 0.1) + 
  geom_sf(data = estados_br |> filter(abbrev_state == "MG" &
                                        abbrev_state == "SP"), 
          fill = NA, 
          color = "black", # Cor preta mais escura
          size = 2) +
  scale_fill_gradientn(colors = c("#D92B3A", 
                                  "#d4e302",
                                  "#02592e"), 
                       values = 
                         rescale(c(0,50,100)), 
                       limits = c(0,100),
                       breaks = c(0, 50, 100)) + 
  theme_minimal() +
  labs(fill = "Relative gap") +
  annotation_north_arrow(location = "tr",  
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotate("text", x = -Inf, y = Inf, label = "2", # Canto superior esquerdo
           hjust = -0.5, vjust = 1.5, size = 6) + 
  theme(
    legend.position = "none",
    legend.justification = "center",
    legend.box = "horizontal",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.border = element_rect(color = "black", 
                                fill = NA, 
                                size = 1), 
    plot.margin = margin(10, 10, 10, 10)) +
  ggtitle("Improved productivity \nscenario")


# cenario 3 ---------------------------------------------------------------


d3 <- 
  ggplot() +
  geom_sf(data = c3_productivity |> 
            filter(nivel == "AES"), 
          aes(fill = rr, geometry = geometry), 
          color = "#f5f5f5") +
  geom_sf(data = estados_br, 
          fill = NA, 
          color = "#4c4d4a", 
          size = 0.1) + 
  geom_sf(data = estados_br |> filter(abbrev_state == "MG" &
                                        abbrev_state == "SP"), 
          fill = NA, 
          color = "black", # Cor preta mais escura
          size = 2) +
  scale_fill_gradientn(colors = c("#D92B3A", 
                                  "#d4e302",
                                  "#02592e"), 
                       values = 
                         rescale(c(0,50,100)), 
                       limits = c(0,100),
                       breaks = c(0, 50, 100)) + 
  theme_minimal() +
  labs(fill = "Relative gap") +
  annotation_north_arrow(location = "tr",  
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotate("text", x = -Inf, y = Inf, label = "3", # Canto superior esquerdo
           hjust = -0.5, vjust = 1.5, size = 6) + 
  theme(
    legend.position = "none",
    legend.justification = "center",
    legend.box = "horizontal",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.border = element_rect(color = "black", 
                                fill = NA, 
                                size = 1), 
    plot.margin = margin(10, 10, 10, 10))  + 
  ggtitle("SUS dependent deduction \nscenario")



# cenario 4 ---------------------------------------------------------------


d4 <- 
  ggplot() +
  geom_sf(data = c4_productivity |> 
            filter(nivel == "AES"), 
          aes(fill = rr, geometry = geometry), 
          color = "#f5f5f5") +
  geom_sf(data = estados_br, 
          fill = NA, 
          color = "#4c4d4a", 
          size = 0.1) + 
  scale_fill_gradientn(colors = c("#D92B3A", 
                                  "#d4e302",
                                  "#02592e"), 
                       values = 
                         rescale(c(0,50,100)), 
                       limits = c(0,100),
                       breaks = c(0, 50, 100)) + 
  theme_minimal() +
  labs(fill = "Relative gap") +
  annotation_north_arrow(location = "tr",  
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotate("text", x = -Inf, y = Inf, label = "4", # Canto superior esquerdo
           hjust = -0.5, vjust = 1.5, size = 6) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box = "horizontal",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.border = element_rect(color = "black", 
                                fill = NA, 
                                size = 1), 
    plot.margin = margin(10, 10, 10, 10)) +
  ggtitle("Supply of professionals with \nany type of contract")


# grafico de linhas -------------------------------------------------------


# grafico 


evolucao_aes <- grafico |> 
  filter(nivel == "AES") |> 
  mutate(regiao = case_when(
    uf_sigla %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    uf_sigla %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
    uf_sigla %in% c("PR", "RS", "SC") ~ "Sul")) |> 
  group_by(nivel, scenario, regiao) |> 
  summarise(necessidade = sum(necessidade),
            necessidade_sus = sum(necessidade_sus),
            oferta_rs = sum(oferta),
            rr = mean(rr)) |> 
  mutate(rr = round(rr, 2)) |> 
  rename(Região = regiao) |> 
  mutate(Região = factor(Região, levels = c("Nordeste", "Norte", "Centro-Oeste", "Sudeste", "Sul"))) |> 
  ggplot(aes(x = scenario, y = rr, fill = Região)) + 
  geom_col(position = "dodge") +
  geom_text(aes(label = rr),
            size = 5, 
            fontface = "bold",
            position = position_dodge(width = 0.9),
            vjust = -0.25
  ) +
  theme_minimal() +
  ylab("Relative gap (%)") + 
  theme(
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 16, face = "bold"),  
    axis.text.x = element_text(size = 14),                 
    axis.text.y = element_text(size = 14),                 
    legend.title = element_text(size = 16, face = "bold"), 
    legend.text = element_text(size = 14),                 
    plot.title = element_text(size = 20, face = "bold"),   
    plot.subtitle = element_text(size = 16),               
    strip.text = element_text(size = 14),
    legend.position = "bottom" 
  ) + ylim(0, 100)


evolucao_aes


# figura 4 ----------------------------------------------------------------

d_aes <- (d1_baseline | d2 | d3 | d4) / evolucao_aes

d_aes

ggsave(filename = "04_mapas/cenarios_aes.png",
       d_aes, dpi = 1000, height = 10, width = 20)


