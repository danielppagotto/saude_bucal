

gap_necessidade_oferta <- 
  
  function(tempo, ttd, pd, pl, sus, categoria){
    
    pop_brasil_tratado <- 
      pop_brasil |>
      filter(cod_municipiodv == '4209508') |> 
      select(cod_municipiodv, 
             ibge_sb, municipio, de_0_a_14_anos,
             de_15_a_29_anos, de_30_a_59_anos,
             acima_de_60_anos) |> 
      gather(key = "faixa", 
             value = "total",
             4:7) |> 
      mutate(faixa = gsub("_"," ",faixa)) |> 
      mutate(ibge = as.character(ibge_sb)) |> 
      mutate(ibge = substr(ibge_sb, 1, 6)) |> 
      mutate(cod_municipiodv = 
               as.character(cod_municipiodv)) |> 
      mutate(cod_mun_loc = cod_municipiodv) |> 
      mutate(cod_municipiodv = 
               substr(cod_municipiodv, 1, 6)) |> 
      mutate(id_faixa = case_when(
        faixa == "de 0 a 14 anos" ~ 1,
        faixa == "de 15 a 29 anos" ~ 2,
        faixa == "de 30 a 59 anos" ~ 3,
        faixa == "acima de 60 anos" ~ 4))
    
    pop_brasil_tratado <-
      pop_brasil_tratado |> 
      mutate(ibge_sb = as.character(ibge_sb)) |> 
      mutate(ibge_sb = str_sub(ibge_sb, start = 1, end = 6))
    
    cobertura_sb$ibge <- as.character(cobertura_sb$ibge)
    
    pop_coberta_br <- 
      pop_brasil_tratado |> 
      left_join(cobertura_sb, by = c("ibge_sb" = "ibge",
                                     "id_faixa" = "id_faixa")) |> 
      select(ibge, ibge_sb, municipio.x, 
             faixa, id_faixa, 
             total, procedimento, cobertura, 
             cod_municipiodv, cod_mun_loc) |> 
      mutate(populacao_coberta = cobertura * total) |> 
      mutate(populacao_coberta = round(populacao_coberta, 2))
    
    
    producao_brasil <- producao_normativa_br |> 
      select(-municipio, 
             -li_cobertura,
             -ls_cobertura,
             -faixa_etaria)
    
    necessidades_servicos_br <- 
      pop_coberta_br |> 
      left_join(producao_brasil, 
                by = 
                  c("ibge" = "ibge",
                    "id_faixa","procedimento")) |> 
      mutate(nec_servicos = 
               populacao_coberta * producao_pc) |> 
      mutate(nec_servicos = 
               round(nec_servicos, 2))
    
    
    necessidades_prof_br <- 
      necessidades_servicos_br |>
      mutate(nec_prof = 
               (nec_servicos * tempo/60)/ttd) |> 
      mutate(nivel = 
               if_else(procedimento == "Atenção Básica","APS","AES")) |> 
      group_by(ibge, municipio.x, 
               nivel, cod_municipiodv, cod_mun_loc) |> 
      summarise(necessidade = 
                  sum(nec_prof))
    
    
    
    oferta$FTE40 <- as.numeric(oferta$FTE40)
    
    oferta_brasil <- 
      if(todos == sus)
      {
        oferta |> 
          filter(PROF_SUS == "1") |> 
          group_by(CODUFMUN, CATEGORIA, NIVEL) |> 
          summarise(FTE40 = sum(FTE40)) |> 
          filter(NIVEL != "NA") |> 
          mutate(NIVEL = if_else(NIVEL == "APS","APS","AES")) 
      } else{
        oferta |> 
          group_by(CODUFMUN, CATEGORIA, NIVEL) |> 
          summarise(FTE40 = sum(FTE40)) |> 
          filter(NIVEL != "NA") |> 
          mutate(NIVEL = if_else(NIVEL == "APS","APS","AES")) 
        
      }
    
    oferta_brasil <- 
      oferta_brasil |> 
      mutate(FTE_40_direto = FTE40 * pd) |> 
      mutate(FTE_40_linha = FTE_40_direto * pl)
    
    
    oferta_brasil_cd <- oferta_brasil |> 
      filter(CATEGORIA == categoria)
    
    # Cirurgião-dentista
    
    oferta_vs_demanda <-
      necessidades_prof_br |> 
      left_join(oferta_brasil_cd, 
                by = c("cod_municipiodv"="CODUFMUN",
                       "nivel" = "NIVEL")) |> 
      mutate(ra = FTE_40_linha - necessidade, 
             rr = (FTE_40_linha/necessidade) * 100) |> 
      mutate(ra = round(ra, 2),
             rr = round(rr, 2)) |> 
      filter(CATEGORIA != "NA")
    
    
    oferta_vs_demanda$FTE_40_linha[is.na(oferta_vs_demanda$FTE_40_linha)] <- 0
    
    
    oferta_vs_demanda$FTE40[is.na(oferta_vs_demanda$FTE40)] <- 0
    
    
    oferta_vs_demanda$FTE_40_direto[is.na(oferta_vs_demanda$FTE_40_direto)] <- 0
    
    oferta_vs_demanda <- oferta_vs_demanda |>
      mutate(necessidade = round(necessidade, 2)) |>
      mutate(FTE_40_linha = round(FTE_40_linha, 2)) |> 
      select(ibge, municipio.x, 
             nivel, necessidade, 
             FTE_40_linha, rr, ra, cod_municipiodv, cod_mun_loc)
    
  }





















# Criando lista de vetores para cada parâmetro
list_ttd <- as.numeric(c("1576"))
list_tempo <- as.numeric(c("25"))
list_pd <- as.numeric(c("0.50"))
list_pl <- as.numeric(c("0.50"))
list_cat <- c("Cirurgião-dentista","Técnico ou Auxiliar de Saúde Bucal")
list_sus <- c(TRUE, FALSE) # pegar todos os profissionais ou só SUS (false)

resultado1 <- list()
iteracao1 <- 0
total_iteracoes1 <- length(list_ttd) * length(list_tempo) * length(list_pd) * length(list_pl) * length(list_cat) * length(list_sus)

for (ttd in list_ttd) {
  for (tempo in list_tempo) {
    for (pd in list_pd) {
      for (pl in list_pl) { 
        for(sus in list_sus){
          for(cat in list_cat){
            iteracao1 <- iteracao1 + 1
            cat("Iteração:", iteracao, "de", total_iteracoes1, "\n")
            
            res1 <- 
              gap_necessidade_oferta(
                tempo = tempo, 
                ttd = ttd, 
                pd = pd, 
                pl = pl,
                sus = sus, 
                cat = cat)
            
            res1 <- cbind(res1, 
                         ttd = ttd, 
                         tempo = tempo, 
                         pd = pd, 
                         pl = pl,
                         sus = sus,
                         cat = cat)
            
            res1$atributos <- paste(ttd, 
                                   tempo, 
                                   pd, 
                                   pl, 
                                   sus,
                                   cat, 
                                   sep = "_")
            
            resultado1[[length(resultado1) + 1]] <- res1
          }
        }
      }
    }
  }
}

# Verificando o tamanho da lista resultado
length(resultado1)

resultado_teste1 <- 
  do.call(rbind, resultado1)


a <- resultado_final |> 
        group_by(cod_municipiodv, municipio) |> 
        count()









# Definindo o vetor com 5570 elementos
vetor <- hierarquia_municipios$cod_municipio

# Cada elemento do vetor será repetido 8 vezes (2 valores para 'nível', 2 valores para 'profissional' e 2 valores para 'SUS')
vetor_duplicado <- rep(vetor, each = 8)

# Criando a coluna 'nível' com valores alternados entre 'APS' e 'AES'
nivel <- rep(c("APS", "ESPECIALIZADO"), each = 4, length.out = length(vetor_duplicado))

# Criando a coluna 'profissional' com valores alternados entre 'dentista' e 'TSB'
profissional <- rep(c("Cirurgião-dentista", "Técnico ou Auxiliar de Saúde Bucal"), each = 2, length.out = length(vetor_duplicado))

# Criando a coluna 'SUS' com valores alternados entre 0 e 1
SUS <- rep(c('0', '1'), length.out = length(vetor_duplicado))

# Criando o DataFrame
df <- data.frame(elemento = vetor_duplicado, nivel = nivel, profissional = profissional, SUS = SUS)

# Visualizando o DataFrame
head(df, 16)  # Para visualizar as primeiras 16 linhas e verificar a estrutura

df <- df |> rename(ibge = elemento)

df$ibge <- as.character(df$ibge)


# f -----------------------------------------------------------------------

teste_oferta_aps_aes <- 
  df |>  
    left_join(oferta, by = c("ibge"="CODUFMUN",
                             "nivel"="NIVEL",
                             "SUS" = "PROF_SUS",
                             "profissional" = "CATEGORIA"))










