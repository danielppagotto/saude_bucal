

# Carregando pacotes -------------------------------------------------------


library(tidyverse)
library(arrow)
library(patchwork)
library(geojsonio)
library(geojsonsf)
library(geobr)
library(scales)
library(ggspatial) 



resultado_teste2 <- 
  read_parquet("~/GitHub/saude_bucal/resultado1711.parquet")

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
  resultado_teste2 |> 
  filter(tempo_endo == '45' &
           tempo_prot == '55' & 
           tempo_aps == '45' &
           tempo_peri == '55' &
           pd == '0.4',
         pl == '0.5',
         ttd == '1576',
         sus == 'Somente profissionais SUS',
         plano == 'Necessidade de todos')


baseline <- 
  baseline |> 
  mutate(rr = 100 * rr) |> 
  mutate(rr = if_else(rr > 100, 100, rr)) |> 
  left_join(spdf_fortified, 
            by = c("cod_regsaud"="reg_id")) |> 
  distinct_all()

if (!"geometry" %in% names(baseline)) {
  
  baseline <- st_as_sf(baseline, 
                       coords = c("longitude", 
                                  "latitude"), 
                       crs = 4326) 
}


a1 <- 
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
  annotate("segment", 
           x = -44, y = -25,   # Coordenadas iniciais da seta (no oceano)
           xend = -44, yend = -19.5, # Coordenadas finais da seta (em Minas Gerais)
           arrow = arrow(length = unit(0.15, "cm")), # Tamanho menor da seta
           color = "black", # Cor da seta
           size = 1.2) +
  scale_fill_gradientn(colors = c("#D92B3A", 
                                  "#d4e302",
                                  "#02592e"), 
                       values = 
                         rescale(c(0,50,100)), 
                       limits = c(0,100),
                       breaks = c(0, 50, 100)) + 
  theme_minimal() +
  labs(fill = "Relative gap") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotate("text", x = -Inf, y = Inf, label = "A", # Canto superior esquerdo
           hjust = -0.5, vjust = 1.5, size = 6) + 
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box = "horizontal",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 16),
    panel.border = element_rect(color = "black", 
                                fill = NA, 
                                size = 1), 
    plot.margin = margin(10, 10, 10, 10)) +
  ggtitle("Primary healthcare")


b1 <- 
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
  # Seta para Minas Gerais
  annotate("segment", 
           x = -40, y = -30,   # Coordenadas iniciais da seta (no oceano)
           xend = -44, yend = -19.5, # Coordenadas finais da seta (em Minas Gerais)
           arrow = arrow(length = unit(0.15, "cm")), # Tamanho menor da seta
           color = "black", # Cor da seta
           size = 1.2) + # Espessura da seta
  # Seta para São Paulo
  annotate("segment", 
           x = -40, y = -30,   # Coordenadas iniciais da seta (no oceano, mesma origem)
           xend = -47, yend = -23.5, # Coordenadas finais da seta (em São Paulo)
           arrow = arrow(length = unit(0.15, "cm")), # Tamanho menor da seta
           color = "black", # Cor da seta
           size = 1.2) + # Espessura da seta
  scale_fill_gradientn(colors = c("#D92B3A", 
                                  "#d4e302",
                                  "#02592e"), 
                       values = 
                         rescale(c(0,50,100)), 
                       limits = c(0,100),
                       breaks = c(0, 50, 100)) + 
  theme_minimal() +
  labs(fill = "Relative gap") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", # Posição: canto superior direito
                         which_north = "true", 
                         pad_x = unit(0.1, "in"), 
                         pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering()) +
  annotate("text", x = -Inf, y = Inf, label = "B", # Canto superior esquerdo
           hjust = -0.5, vjust = 1.5, size = 6) + 
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box = "horizontal",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 16),
    panel.border = element_rect(color = "black", 
                                fill = NA, 
                                size = 1), 
    plot.margin = margin(10, 10, 10, 10)) +
  ggtitle("Specialized Healthcare")

b1

c1 <- a1 + b1 
c1

ggsave(filename = "04_mapas/cenario_baselinet.png",
       c1, dpi = 350, height = 8, width = 16)



# APS ---------------------------------------------------------------------


# cenario 1 ---------------------------------------------------------------





# cenario 1 ---------------------------------------------------------------

mapa_rs1 <-
  resultado_teste1 |> 
  filter(cenario == '11629') |> 
  mutate(rr = 100 * rr) |> 
  mutate(rr = if_else(rr > 100, 100, rr)) |> 
  left_join(spdf_fortified, 
            by = c("cod_regsaud"="reg_id")) 

# Adicionar geometria se não existir
if (!"geometry" %in% names(mapa_rs1)) {
  
  mapa_rs1 <- st_as_sf(mapa_rs1, 
                       coords = c("longitude", 
                                  "latitude"), 
                       crs = 4326) 
}


a1 <- ggplot() +
  geom_sf(data = mapa_rs1 |> filter(nivel == "APS"), 
          aes(fill = rr, geometry = geometry), 
          color = "#e9e9e9") +
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
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  #  annotation_north_arrow(location = "tr",  
  #                         which_north = "true",
  #                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl",  
                   width_hint = 0.3)  +
  ggtitle("Primary Healthcare")


b1 <- ggplot() +
  geom_sf(data = mapa_rs1 |> filter(nivel == "AES"), 
          aes(fill = rr, geometry = geometry), 
          color = "#e9e9e9") +
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
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  annotation_north_arrow(location = "tr",  
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  #  annotation_scale(location = "bl",  
  #                   width_hint = 0.3)  +
  ggtitle("Specialized Healthcare")


c1 <- a1 + b1
c1


ggsave(filename = "04_mapas/mapas - cenario1.jpeg", 
       plot = c1,
       dpi = 400, 
       width = 16, 
       height = 10)


# cenario 2 ---------------------------------------------------------------

mapa_rs2 <-
  resultado_teste1 |> 
  filter(cenario == '7777') |> 
  mutate(rr = 100 * rr) |> 
  mutate(rr = if_else(rr > 100, 100, rr)) |> 
  left_join(spdf_fortified, 
            by = c("cod_regsaud"="reg_id")) 

# Adicionar geometria se não existir
if (!"geometry" %in% names(mapa_rs2)) {
  
  mapa_rs2 <- st_as_sf(mapa_rs2, 
                       coords = c("longitude", 
                                  "latitude"), 
                       crs = 4326) 
}


a2 <- ggplot() +
  geom_sf(data = mapa_rs2 |> filter(nivel == "APS"), 
          aes(fill = rr, geometry = geometry), 
          color = "#e9e9e9") +
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
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  #  annotation_north_arrow(location = "tr",  
  #                         which_north = "true",
  #                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl",  
                   width_hint = 0.3)  +
  ggtitle("Primary Healthcare")


b2 <- ggplot() +
  geom_sf(data = mapa_rs2 |> filter(nivel == "AES"), 
          aes(fill = rr, geometry = geometry), 
          color = "#e9e9e9") +
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
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  annotation_north_arrow(location = "tr",  
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  #  annotation_scale(location = "bl",  
  #                   width_hint = 0.3)  +
  ggtitle("Specialized Healthcare")


c2 <- a2 + b2


ggsave(filename = "04_mapas/mapas - cenario2.jpeg", 
       plot = c2,
       dpi = 400, 
       width = 16, 
       height = 10)


# cenario 3 ---------------------------------------------------------------

mapa_rs3 <-
  resultado_teste1 |> 
  filter(cenario == '34') |> 
  mutate(rr = 100 * rr) |> 
  mutate(rr = if_else(rr > 100, 100, rr)) |> 
  left_join(spdf_fortified, 
            by = c("cod_regsaud"="reg_id")) 

# Adicionar geometria se não existir
if (!"geometry" %in% names(mapa_rs3)) {
  
  mapa_rs3 <- st_as_sf(mapa_rs3, 
                       coords = c("longitude", 
                                  "latitude"), 
                       crs = 4326) 
}


a3 <- ggplot() +
  geom_sf(data = mapa_rs3 |> filter(nivel == "APS"), 
          aes(fill = rr, geometry = geometry), 
          color = "#e9e9e9") +
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
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  #  annotation_north_arrow(location = "tr",  
  #                         which_north = "true",
  #                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl",  
                   width_hint = 0.3)  +
  ggtitle("Primary Healthcare")


b3 <- ggplot() +
  geom_sf(data = mapa_rs3 |> filter(nivel == "AES"), 
          aes(fill = rr, geometry = geometry), 
          color = "#e9e9e9") +
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
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  annotation_north_arrow(location = "tr",  
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  #  annotation_scale(location = "bl",  
  #                   width_hint = 0.3)  +
  ggtitle("Specialized Healthcare")


c3 <- a3 + b3


ggsave(filename = "04_mapas/mapas - cenario3.jpeg", 
       plot = c3,
       dpi = 400, 
       width = 16, 
       height = 10)

# cenario 4 ---------------------------------------------------------------

mapa_rs4 <-
  resultado_teste1 |> 
  filter(cenario == '36') |> 
  mutate(rr = 100 * rr) |> 
  mutate(rr = if_else(rr > 100, 100, rr)) |> 
  left_join(spdf_fortified, 
            by = c("cod_regsaud"="reg_id")) 

# Adicionar geometria se não existir
if (!"geometry" %in% names(mapa_rs4)) {
  
  mapa_rs4 <- st_as_sf(mapa_rs4, 
                       coords = c("longitude", 
                                  "latitude"), 
                       crs = 4326) 
}


a4 <- ggplot() +
  geom_sf(data = mapa_rs4 |> filter(nivel == "APS"), 
          aes(fill = rr, geometry = geometry), 
          color = "#e9e9e9") +
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
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  #  annotation_north_arrow(location = "tr",  
  #                         which_north = "true",
  #                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl",  
                   width_hint = 0.3)  +
  ggtitle("Primary Healthcare")


b4 <- ggplot() +
  geom_sf(data = mapa_rs4 |> filter(nivel == "AES"), 
          aes(fill = rr, geometry = geometry), 
          color = "#e9e9e9") +
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
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  annotation_north_arrow(location = "tr",  
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  #  annotation_scale(location = "bl",  
  #                   width_hint = 0.3)  +
  ggtitle("Specialized Healthcare")


c4 <- a4 + b4


ggsave(filename = "04_mapas/mapas - cenario4.jpeg", 
       plot = c4,
       dpi = 400, 
       width = 16, 
       height = 10)
