# criando o mapa usando Leaflet 


library(leaflet)
library(sf)
library(dplyr)
library(readxl)

cenarios_regioes <- read_excel("01_dados/cenarios_regioes.xlsx")

spdf <- 
  geojson_read("~/GitHub/saude_bucal/01_dados/shape_file_regioes_saude.json", 
               what = "sp") 


spdf_fortified <- 
  sf::st_as_sf(spdf)

estados_br <- read_state(year = 2020,
                         showProgress = FALSE)

baseline <- 
  cenarios_regioes |> 
  filter(cenario == "baseline") |> 
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

# Verificar se baseline já é um objeto sf
if (!inherits(baseline, "sf")) {
  baseline <- st_as_sf(baseline)
}

# Certifique-se de que a geometria não está vazia
#baseline <- baseline |> filter(!st_is_empty(geometry))

# Criar a paleta de cores
pal <- colorNumeric(palette = c("#D92B3A", "#d4e302", "#02592e"), 
                    domain = baseline$rr)

# Criar o mapa interativo com hover
baseline |> 
  filter(nivel == "APS") |> 
leaflet() |>
  addTiles() |>  
  addPolygons(
    fillColor = ~pal(rr),
    color = "#f5f5f5", 
    weight = 1, 
    opacity = 1,
    fillOpacity = 0.8,
    highlightOptions = highlightOptions(
      weight = 3, 
      color = "black", 
      bringToFront = TRUE),
    popup = ~paste("Nome da Região:", regiao_saude,
                   "<br>UF:",uf_sigla,
                   "<br>Lacuna(%):", rr),  # Popup no hover
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", "font-size" = "12px"),
      direction = "auto",
      textOnly = FALSE
    )
  ) |>
  addPolylines(
    data = estados_br,  # Usa os estados do IBGE
    color = "grey",  # Linha preta destacada
    weight = 2,  # Mais espesso que as linhas das regiões
    opacity = 1
  ) |>
  setView(lng = -55, lat = -14, zoom = 4)

