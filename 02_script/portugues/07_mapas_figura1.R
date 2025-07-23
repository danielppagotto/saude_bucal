
# Carregar pacotes
library(sf)
library(geobr)
library(ggplot2)
library(rnaturalearth)
library(ggspatial)
library(dplyr)
library(geojsonio)
library(geojsonsf)
library(patchwork)

# Dados geográficos
brasil_municipios <- read_municipality(code_muni = "all", 
                                       year = 2020)

brasil_estados <- read_state(year = 2020)
paises_vizinhos <- ne_countries(scale = "medium", 
                                continent = "South America", returnclass = "sf")

# Adicionar coluna com as regiões no shapefile de estados
brasil_estados <- brasil_estados %>%
  mutate(regiao = case_when(
    abbrev_state %in% c("PR", "RS", "SC") ~ "Sul",
    abbrev_state %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
    abbrev_state %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    abbrev_state %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    abbrev_state %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
    TRUE ~ NA_character_
  ))

# Definir cores para as regiões
cores_regioes <- c(
  "Sul" = "#66c2a5",
  "Sudeste" = "#fc8d62",
  "Centro-Oeste" = "#8da0cb",
  "Nordeste" = "#e78ac3",
  "Norte" = "#a6d854"
)


a <- 
  ggplot() +
       # Adicionar países vizinhos
       geom_sf(data = paises_vizinhos, fill = "gray90", color = "gray70") +
       # Adicionar municípios com cores baseadas em código de estado
       geom_sf(data = brasil_municipios, aes(fill = as.factor(code_state)), color = "gray80", size = 0.1, show.legend = FALSE) +
       # Adicionar estados com bordas mais destacadas e cores por região
       geom_sf(data = brasil_estados, aes(fill = regiao), color = "black", size = 0.4, alpha = 0.5) +
       scale_fill_manual(values = cores_regioes, name = "Regiões") +
       # Rosa dos ventos e escala
       annotation_north_arrow(location = "tr", which_north = "true", 
                              style = north_arrow_fancy_orienteering) +
       annotation_scale(location = "bl", width_hint = 0.3) +
       # Ajustar coordenadas para focar no Brasil
       coord_sf(xlim = c(-75, -34), ylim = c(-35, 5), expand = FALSE) +
       # Adicionar borda ao redor do gráfico
       annotate(
             "rect", 
             xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
             fill = NA, color = "black", size = 1
         ) +
       # Adicionar a letra "A" no canto superior esquerdo
       annotate(
             "text", 
             x = -Inf, y = Inf, 
             label = "A", 
             hjust = -0.5, vjust = 1.5, 
             size = 5, 
             fontface = "bold") +
       # Ajustes de tema
       theme_minimal() +
       theme(
             legend.position = "bottom",
             legend.title = element_text(size = 10),
             legend.text = element_text(size = 9),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             plot.title = element_text(hjust = 0.5, 
             size = 12, face = "bold"))


a

# Região de Saúde  --------------------------------------------------------

spdf <- 
  geojson_read("~/GitHub/saude_bucal/01_dados/shape_file_regioes_saude.json", 
               what = "sp") 

spdf_fortified_go <- 
  sf::st_as_sf(spdf) |> 
  filter(est_id == "13") |> 
  distinct_all()

municipios_go <- brasil_municipios |> 
                      filter(code_state == "13")

spdf_fortified_go <- st_set_crs(spdf_fortified_go, 4674)
municipios_go <- st_set_crs(municipios_go, 4674)

spdf_fortified_go <- st_transform(spdf_fortified_go, 4674)
municipios_go <- st_transform(municipios_go, 4674)


cores_regioes_go <- c(
  "Baixo Amazonas" = "#66c2a5",
  "Médio Amazonas" = "#fc8d62",
  "Regional Juruá" = "#8da0cb",
  "Regional Purus" = "#e78ac3",
  "Manaus, Entorno e Alto Rio Negro" = "#a6d854",
  "Rio Madeira" = "#ffd92f",
  "Rio Negro e Solimões" = "#e5c494",
  "Alto Solimões" = "#b3b3b3",
  "Triângulo" = "#e31a1c"
)

b <- ggplot() +
  geom_sf(data = municipios_go, 
          fill = NA, 
          color = "gray50", 
          size = 0.2) +
  geom_sf(data = spdf_fortified_go, 
          aes(fill = nome), 
          color = "black", 
          size = 0.3, 
          alpha = 0.7) +
  scale_fill_manual(values = cores_regioes_go, 
                    guide = "none") +
  annotation_scale(location = "bl", 
                   width_hint = 0.3) +
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         style = north_arrow_fancy_orienteering) +
  annotate(
    "rect", 
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
    fill = NA, color = "black", size = 1
  ) +
  annotate(
    "text", 
    x = -Inf, y = Inf, 
    label = "B", 
    hjust = -0.5, vjust = 1.5, 
    size = 6, 
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", # Remover a legenda
    plot.title = element_text(size = 12, face = "bold"),
    plot.caption = element_text(size = 9),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank() # Garante que o fundo fique claro
  )

b

c <- a + b

c

ggsave(filename = "mapas_brasil_am.png",
       c, dpi = 500, height = 8, width = 12)
