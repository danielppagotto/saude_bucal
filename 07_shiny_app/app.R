library(shiny)
library(dplyr)
library(plotly)
library(DT)
library(forcats)
library(patchwork)
library(tidyr)
library(stringr)
library(leaflet)
library(sf)
library(readxl)
library(geojsonio)
library(shinycssloaders)  # Para os spinners

# Evita o aviso do jsonlite
options(jsonlite.keep_vec_names = FALSE)

# ---------------------------
# Leitura dos dados
# ---------------------------
pop_brasil <- readRDS("./data/pop_brasil.rds")
relacao_municipio_rs <- readRDS("./data/relacao_municipio_rs.rds")
plano_odontologico_rs <- readRDS("./data/plano_odontologico_rs.rds")
cobertura_sb <- readRDS("./data/cobertura_sb.rds")
producao_normativa_br <- readRDS("./data/producao_normativa_br.rds")
oferta_brasil <- readRDS("./data/oferta_brasil.rds")
hierarquia_municipios <- readRDS("./data/hierarquia_municipios.rds")
estados_br <- readRDS("./data/estados_br.rds")  # Objeto sf ou compatível

# Carregar o shape das regiões de saúde via GeoJSON e converter para sf
spdf <- geojson_read("./shape_file_regioes_saude.json", 
                     what = "sp")
spdf_fortified <- st_as_sf(spdf)

# ---------------------------
# Função gap_necessidade_oferta
# ---------------------------
gap_necessidade_oferta <- function(tempo_aps, tempo_endo, tempo_prot, tempo_peri, ttd, pd, pl, sus, categoria, plano){
  
  pop_brasil_tratado <- pop_brasil %>%
    mutate(uf = substr(cod_municipiodv, 1, 2)) %>%
    select(cod_municipiodv, ibge_sb, municipio, 
           de_0_a_14_anos, de_15_a_29_anos, de_30_a_59_anos, acima_de_60_anos) %>%
    tidyr::pivot_longer(
      cols = 4:7,
      names_to = "faixa",
      values_to = "total"
    ) %>%
    mutate(faixa = gsub("_", " ", faixa),
           ibge = as.character(ibge_sb),
           ibge = substr(ibge_sb, 1, 6),
           cod_municipiodv = as.character(cod_municipiodv),
           cod_mun_loc = cod_municipiodv,
           cod_municipiodv = substr(cod_municipiodv, 1, 6),
           id_faixa = case_when(
             faixa == "de 0 a 14 anos" ~ 1,
             faixa == "de 15 a 29 anos" ~ 2,
             faixa == "de 30 a 59 anos" ~ 3,
             faixa == "acima de 60 anos" ~ 4))
  
  pop_brasil_tratado <- pop_brasil_tratado %>%
    mutate(ibge_sb = as.character(ibge_sb),
           ibge_sb = substr(ibge_sb, 1, 6))
  
  populacao_tratado <- pop_brasil_tratado %>%
    left_join(relacao_municipio_rs, by = c("cod_municipiodv" = "cod_municipio")) %>%
    left_join(plano_odontologico_rs, by = c("cod_regsaud", "id_faixa")) %>%
    select(-benef, -pop) %>%
    mutate(pop_sus = total * (1 - cobertura_plano))
  
  cobertura_sb$ibge <- as.character(cobertura_sb$ibge)
  
  pop_coberta_br <- populacao_tratado %>%
    left_join(cobertura_sb, by = c("ibge_sb" = "ibge", "id_faixa" = "id_faixa")) %>%
    select(ibge, ibge_sb, municipio.x, faixa, id_faixa, total, procedimento, cobertura,
           cobertura_plano, pop_sus, cod_municipiodv, cod_mun_loc) %>%
    rename(municipio = municipio.x, cobertura_servicos = cobertura) %>%
    mutate(populacao_coberta = round(cobertura_servicos * total, 2),
           populacao_coberta_sus = round(cobertura_servicos * pop_sus, 2))
  
  producao_brasil <- producao_normativa_br %>%
    select(-municipio, -li_cobertura, -ls_cobertura, -faixa_etaria)
  
  necessidades_servicos_br <- pop_coberta_br %>%
    left_join(producao_brasil, by = c("ibge" = "ibge", "id_faixa", "procedimento")) %>%
    mutate(nec_servicos = round(populacao_coberta * producao_pc, 2),
           nec_servicos_sus = round(populacao_coberta_sus * producao_pc, 2))
  
  necessidades_prof_br <- necessidades_servicos_br %>%
    mutate(
      nec_prof = case_when(
        procedimento == "Atenção Básica" ~ (nec_servicos * tempo_aps/60)/1576,
        procedimento == "Endodontia" ~ (nec_servicos * tempo_endo/60)/1576,
        procedimento == "Periodontia" ~ (nec_servicos * tempo_peri/60)/1576,
        procedimento == "Prótese" ~ (nec_servicos * tempo_prot/60)/1576
      ),
      nec_prof_sus = case_when(
        procedimento == "Atenção Básica" ~ (nec_servicos_sus * tempo_aps/60)/1576,
        procedimento == "Endodontia" ~ (nec_servicos_sus * tempo_endo/60)/1576,
        procedimento == "Periodontia" ~ (nec_servicos_sus * tempo_peri/60)/1576,
        procedimento == "Prótese" ~ (nec_servicos_sus * tempo_prot/60)/1576
      )
    ) %>%
    mutate(nivel = if_else(procedimento == "Atenção Básica", "APS", "AES")) %>%
    group_by(ibge, municipio, nivel, cod_municipiodv, cod_mun_loc) %>%
    summarise(necessidade = sum(nec_prof),
              necessidade_sus = sum(nec_prof_sus)) %>%
    mutate(necessidade = round(necessidade, 2),
           necessidade_sus = round(necessidade_sus, 2))
  
  oferta_prof <- oferta_brasil %>% filter(profissional == categoria)
  
  todos <- TRUE
  oferta_temp <- if (todos == sus) {
    oferta_prof %>% filter(SUS == "1")
  } else {
    oferta_prof %>% group_by(ibge, profissional, nivel) %>% summarise(fte40 = sum(fte40)) %>% ungroup()
  }
  
  oferta_temp$fte40[is.na(oferta_temp$fte40)] <- 0
  oferta_temp <- oferta_temp %>%
    mutate(FTE_40_direto = fte40 * pd,
           FTE_40_linha = FTE_40_direto * pl)
  
  oferta_temp$FTE_40_direto[is.na(oferta_temp$FTE_40_direto)] <- 0
  oferta_temp$FTE_40_linha[is.na(oferta_temp$FTE_40_linha)] <- 0
  
  flag <- plano
  
  oferta_vs_demanda <- necessidades_prof_br %>%
    left_join(oferta_temp, by = c("cod_municipiodv" = "ibge", "nivel" = "nivel")) %>%
    left_join(hierarquia_municipios, by = c("cod_municipiodv" = "cod_municipio")) %>%
    mutate(ra = if_else(flag == 1, FTE_40_linha - necessidade, FTE_40_linha - necessidade_sus),
           rr = if_else(flag == 0, FTE_40_linha/necessidade, FTE_40_linha/necessidade_sus),
           ra = round(ra, 2),
           rr = round(rr, 2),
           necessidade = round(necessidade, 2),
           FTE_40_linha = round(FTE_40_linha, 2))
  
  cd_oferta_vs_demanda_regiao_saude <- oferta_vs_demanda %>%
    group_by(nivel, cod_regsaud, uf_sigla, regiao_saude) %>%
    summarise(necessidade = sum(necessidade),
              necessidade_sus = sum(necessidade_sus),
              oferta = sum(FTE_40_linha)) %>%
    mutate(necessidade = round(necessidade, 2),
           necessidade_sus = round(necessidade_sus, 2),
           oferta = round(oferta, 2),
           ra = if_else(flag == 1, oferta - necessidade, oferta - necessidade_sus),
           rr = if_else(flag == 1, oferta/necessidade, oferta/necessidade_sus),
           ra = round(100 * ra, 2),
           rr = round(100 * rr, 2),
           rr = if_else(rr == 0.00, 0.01, rr))
  
  return(cd_oferta_vs_demanda_regiao_saude)
}

# ---------------------------
# Interface do usuário (UI)
# ---------------------------
ui <- navbarPage("Gap Necessidade vs. Oferta",
                 tabPanel("APS",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("tempo_aps", "Tempo APS (min):", min = 10, max = 60, value = 30, step = 5),
                              numericInput("ttd", "Tempo Total Disponível (TTD):", value = 1576, min = 1000, max = 2000),
                              numericInput("pd", "Produtividade Direta (PD):", value = 0.6, min = 0, max = 1, step = 0.05),
                              numericInput("pl", "Produtividade em Linha (PL):", value = 0.5, min = 0, max = 1, step = 0.05),
                              checkboxInput("sus", "Considerar apenas profissionais do SUS", value = TRUE),
                              selectInput("categoria", "Categoria Profissional:", choices = c("2232", "3224"), selected = "2232"),
                              radioButtons("plano", "Plano de Saúde:", choices = list("Todos" = 1, "Apenas SUS" = 0), selected = 1),
                              actionButton("calcular", "Calcular")
                            ),
                            mainPanel(
                              fluidRow(
                                # Linha superior: duas colunas lado a lado (cada uma com 6 colunas)
                                column(width = 6, withSpinner(leafletOutput("mapa_aps", height = 400))),
                                column(width = 6, withSpinner(plotlyOutput("grafico_aps", height = 400)))
                              ),
                              fluidRow(
                                # Linha inferior: tabela ocupando a largura total
                                column(width = 12, withSpinner(DTOutput("resultado_aps")))
                              )
                            )
                          )
                 ),
                 tabPanel("AES",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("tempo_endo", "Tempo Endodontia (min):", min = 10, max = 60, value = 30, step = 5),
                              sliderInput("tempo_prot", "Tempo Prótese (min):", min = 10, max = 90, value = 60, step = 5),
                              sliderInput("tempo_peri", "Tempo Periodontia (min):", min = 10, max = 60, value = 30, step = 5),
                              numericInput("ttd", "Tempo Total Disponível (TTD):", value = 1576, min = 1000, max = 2000),
                              numericInput("pd", "Produtividade Direta (PD):", value = 0.6, min = 0, max = 1, step = 0.05),
                              numericInput("pl", "Produtividade em Linha (PL):", value = 0.5, min = 0, max = 1, step = 0.05),
                              checkboxInput("sus", "Considerar apenas profissionais do SUS", value = TRUE),
                              selectInput("categoria", "Categoria Profissional:", choices = c("2232", "3224"), selected = "2232"),
                              radioButtons("plano", "Plano de Saúde:", choices = list("Todos" = 1, "Apenas SUS" = 0), selected = 1),
                              actionButton("calcular", "Calcular")
                            ),
                            mainPanel(
                              fluidRow(
                                column(width = 6, withSpinner(leafletOutput("mapa_aes", height = 400))),
                                column(width = 6, withSpinner(plotlyOutput("grafico_aes", height = 400)))
                              ),
                              fluidRow(
                                column(width = 12, withSpinner(DTOutput("resultado_aes")))
                              )
                            )
                          )
                 )
)

# ---------------------------
# Lógica do servidor
# ---------------------------
server <- function(input, output) {
  
  # Cálculo reativo
  resultado_calc <- eventReactive(input$calcular, {
    gap_necessidade_oferta(
      tempo_aps = input$tempo_aps,
      tempo_endo = input$tempo_endo,
      tempo_prot = input$tempo_prot,
      tempo_peri = input$tempo_peri,
      ttd = input$ttd,
      pd = input$pd,
      pl = input$pl,
      sus = input$sus,
      categoria = input$categoria,
      plano = input$plano
    )
  })
  
  # DT para APS
  output$resultado_aps <- renderDT({
    req(resultado_calc())
    datatable(
      resultado_calc() %>%
        ungroup() %>%
        filter(nivel == "APS") %>%
        select(-nivel, -cod_regsaud) %>% 
        rename(RR = rr, RA = ra, Oferta = oferta,
               Necessidade = necessidade,
               `Necessidade SUS` = necessidade_sus,
               UF = uf_sigla, 
               `Região de Saúde` = regiao_saude)
    )
  })
  
  # DT para AES
  output$resultado_aes <- renderDT({
    req(resultado_calc())
    datatable(
      resultado_calc() %>%
        ungroup() %>%
        filter(nivel == "AES") %>%
        select(-nivel, -cod_regsaud) %>% 
        rename(RR = rr, RA = ra, Oferta = oferta,
               Necessidade = necessidade,
               `Necessidade SUS` = necessidade_sus,
               UF = uf_sigla, 
               `Região de Saúde` = regiao_saude)
    )
  })
  
  # Gráfico para APS
  output$grafico_aps <- renderPlotly({
    req(resultado_calc())
    
    grafico_plotly <- resultado_calc() %>% 
      filter(nivel == "APS") %>% 
      group_by(uf_sigla) %>% 
      summarise(necessidade = sum(necessidade),
                necessidade_sus = sum(necessidade_sus),
                oferta_rs = sum(oferta),
                rr = mean(rr)) %>% 
      mutate(rr = round(rr, 2)) %>%
      mutate(regiao = case_when(
        uf_sigla %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
        uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
        uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
        uf_sigla %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
        uf_sigla %in% c("PR", "RS", "SC") ~ "Sul")) 
    
    grafico_plotly$uf_reordered <- with(grafico_plotly, fct_reorder(uf_sigla, rr))
    
    plot_ly(grafico_plotly,
            x = ~rr,
            y = ~uf_reordered,
            type = 'bar',
            orientation = 'h',
            color = ~regiao) %>%
      layout(xaxis = list(title = "Resultado Relativo"),
             yaxis = list(title = "UF"))
  })
  
  # Gráfico para AES
  output$grafico_aes <- renderPlotly({
    req(resultado_calc())
    
    grafico_plotly <- resultado_calc() %>% 
      filter(nivel == "AES") %>% 
      group_by(uf_sigla) %>% 
      summarise(necessidade = sum(necessidade),
                necessidade_sus = sum(necessidade_sus),
                oferta_rs = sum(oferta),
                rr = mean(rr)) %>% 
      mutate(rr = round(rr, 2)) %>%
      mutate(regiao = case_when(
        uf_sigla %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
        uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
        uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
        uf_sigla %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
        uf_sigla %in% c("PR", "RS", "SC") ~ "Sul")) 
    
    grafico_plotly$uf_reordered <- with(grafico_plotly, fct_reorder(uf_sigla, rr))
    
    plot_ly(grafico_plotly,
            x = ~rr,
            y = ~uf_reordered,
            type = 'bar',
            orientation = 'h',
            color = ~regiao) %>%
      layout(xaxis = list(title = "Resultado Relativo"),
             yaxis = list(title = "UF"))
  })
  
  # Mapa para APS
  output$mapa_aps <- renderLeaflet({
    req(resultado_calc())
    # Usa os dados de resultado_calc() para APS e junta com os polígonos de spdf_fortified
    baseline <- resultado_calc() %>%
      filter(nivel == "APS") %>%
      mutate(RR = if_else(rr > 100, 100, rr)) %>% 
      left_join(spdf_fortified, by = c("cod_regsaud" = "reg_id")) %>%
      distinct()
    
    if (!"geometry" %in% names(baseline)) {
      baseline <- st_as_sf(baseline, coords = c("longitude", "latitude"), crs = 4326)
    }
    if (!inherits(baseline, "sf")) {
      baseline <- st_as_sf(baseline)
    }
    
    # Paleta de cores (ajuste "RR" conforme sua variável resultante)
    pal <- colorNumeric(palette = c("#D92B3A", "#d4e302", "#02592e"), domain = baseline$RR)
    
    leaflet(data = baseline) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(RR),
        color = "#f5f5f5",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE),
        popup = ~paste("Região de Saúde:", regiao_saude,
                       "<br>UF:", uf_sigla,
                       "<br>Lacuna(%):", RR),
        labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "12px"),
                                    direction = "auto")
      ) %>%
      addPolylines(data = estados_br, color = "grey", weight = 2, opacity = 1) %>%
      addLegend(position = "bottomright", 
                pal = pal, 
                values = ~RR, 
                title = "RR",
                opacity = 1) %>%
      setView(lng = -55, lat = -14, zoom = 4)
  })
  
  # Mapa para AES
  output$mapa_aes <- renderLeaflet({
    req(resultado_calc())
    baseline <- resultado_calc() %>%
      filter(nivel == "AES") %>%
      mutate(RR = if_else(rr > 100, 100, rr)) %>% 
      left_join(spdf_fortified, by = c("cod_regsaud" = "reg_id")) %>%
      distinct()
    
    if (!"geometry" %in% names(baseline)) {
      baseline <- st_as_sf(baseline, coords = c("longitude", "latitude"), crs = 4326)
    }
    if (!inherits(baseline, "sf")) {
      baseline <- st_as_sf(baseline)
    }
    
    pal <- colorNumeric(palette = c("#D92B3A", "#d4e302", "#02592e"), domain = baseline$RR)
    
    leaflet(data = baseline) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(RR),
        color = "#f5f5f5",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE),
        popup = ~paste("Região de Saúde:", regiao_saude,
                       "<br>UF:", uf_sigla,
                       "<br>Lacuna(%):", RR),
        labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "12px"),
                                    direction = "auto")
      ) %>%
      addPolylines(data = estados_br, color = "grey", weight = 2, opacity = 1) %>%
      addLegend(position = "bottomright", 
                pal = pal, 
                values = ~RR, 
                title = "RR",
                opacity = 1) %>%
      setView(lng = -55, lat = -14, zoom = 4)
  })
}

# ---------------------------
# Executa o aplicativo Shiny
# ---------------------------
shinyApp(ui = ui, 
         server = server,
         options = list(launch.browser = TRUE))
