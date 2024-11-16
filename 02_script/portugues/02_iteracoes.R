

# versão 09/11 ------------------------------------------------------------

gap_necessidade_oferta <- 
  
  function(tempo_aps, tempo_endo, 
           tempo_prot, tempo_peri, 
           ttd, pd, pl, sus, 
           categoria, plano){
    
    pop_brasil_tratado <- 
      pop_brasil |>
      mutate(uf = substr(cod_municipiodv, 1, 2)) |> 
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
      mutate(cod_municipiodv = as.character(cod_municipiodv)) |> 
      mutate(cod_mun_loc = cod_municipiodv) |> 
      mutate(cod_municipiodv = substr(cod_municipiodv, 1, 6)) |> 
      mutate(id_faixa = case_when(
                                    faixa == "de 0 a 14 anos" ~ 1,
                                    faixa == "de 15 a 29 anos" ~ 2,
                                    faixa == "de 30 a 59 anos" ~ 3,
                                    faixa == "acima de 60 anos" ~ 4)) |> 
      mutate(ibge_sb = as.character(ibge_sb)) |> 
      mutate(ibge_sb = str_sub(ibge_sb, 
                               start = 1, 
                               end = 6))
    
    populacao_tratado <- 
      pop_brasil_tratado |> 
      left_join(relacao_municipio_rs,
                by = c("cod_municipiodv"="cod_municipio")) |> 
      left_join(plano_odontologico_rs,
                by = c("cod_regsaud",
                       "id_faixa")) |> 
      select(-benef, -pop) |> 
      mutate(pop_sus = total * (1 - cobertura_plano))
    
    cobertura_sb$ibge <- as.character(cobertura_sb$ibge)
    
    pop_coberta_br <- 
      populacao_tratado |> 
      left_join(cobertura_sb, 
                by = c("ibge_sb" = "ibge",
                       "id_faixa" = "id_faixa")) |> 
      select(ibge, ibge_sb, 
             municipio.x, 
             faixa, id_faixa, 
             total, procedimento, cobertura, cobertura_plano,
             pop_sus, cod_municipiodv, cod_mun_loc) |> 
      rename(municipio = municipio.x) |> 
      rename(cobertura_servicos = cobertura) |> 
      mutate(populacao_coberta = cobertura_servicos * total) |> 
      mutate(populacao_coberta_sus = cobertura_servicos * pop_sus) 
    
    producao_brasil <- producao_normativa_br |> 
      select(-municipio, 
             -li_cobertura,
             -ls_cobertura,
             -faixa_etaria)
    
    necessidades_servicos <- 
      pop_coberta_br |> 
      left_join(producao_brasil, 
                by = c("ibge" = "ibge",
                       "id_faixa",
                       "procedimento")) |> 
      mutate(nec_servicos = 
               populacao_coberta * producao_pc) |> 
      mutate(nec_servicos = 
               round(nec_servicos, 2)) |> 
      mutate(nec_servicos_sus = 
               populacao_coberta_sus * producao_pc) |> 
      mutate(nec_servicos_sus = round(nec_servicos_sus,2))
    
    necessidades_prof <- 
      necessidades_servicos |>
      mutate(nec_prof = case_when(
        procedimento == "Atenção Básica" ~ (nec_servicos * tempo_aps/60)/ttd,
        procedimento == "Endodontia" ~ (nec_servicos * tempo_endo/60)/ttd,
        procedimento == "Periodontia" ~ (nec_servicos * tempo_peri/60)/ttd,
        procedimento == "Prótese" ~ (nec_servicos * tempo_prot/60)/ttd)) |> 
      mutate(nec_prof_sus = case_when(
        procedimento == "Atenção Básica" ~ (nec_servicos_sus * tempo_aps/60)/ttd,
        procedimento == "Endodontia" ~ (nec_servicos_sus * tempo_endo/60)/ttd,
        procedimento == "Periodontia" ~ (nec_servicos_sus * tempo_peri/60)/ttd,
        procedimento == "Prótese" ~ (nec_servicos_sus * tempo_prot/60)/ttd)) |> 
    mutate(nivel = if_else(procedimento == "Atenção Básica",
                           "APS",
                           "AES")) |> 
      group_by(ibge, municipio, 
               nivel, cod_municipiodv, 
               cod_mun_loc) |> 
      summarise(necessidade = sum(nec_prof),
                necessidade_sus = sum(nec_prof_sus)) 
    
    oferta_prof <- oferta_brasil |> 
      filter(profissional == categoria)
    
    todos <- 1 # Se sus = 1 vai pegar apenas aqueles profissionais com vínculo SUS
               # Se SUS = 0 vai pegar todos os profissionais independente do vínculo
    
    oferta_temp <- 
      if(todos == sus)
      {
        oferta_prof |> 
          filter(SUS == "1") 
      }else{
      oferta_prof |> 
        group_by(ibge, profissional, nivel) |> 
        summarise(fte40 = sum(fte40)) |> 
        ungroup()
    }
    
    oferta_temp$fte40[is.na(oferta_temp$fte40)] <- 0
    
    oferta_tratada <- 
      oferta_temp |> 
      mutate(FTE_40_direto = fte40 * pd) |> 
      mutate(FTE_40_linha = FTE_40_direto * pl)
    
    oferta_tratada$FTE_40_direto[is.na(oferta_tratada$FTE_40_direto)] <- 0
    oferta_tratada$FTE_40_linha[is.na(oferta_tratada$FTE_40_linha)] <- 0
    
    
    flag <- 1 # se flag = 1, entao vamos trabalhar com as necessidades todos
              # se flag = 0, entao vamos trabalhar com as necessidades daqueles sus dependentes
    #flag <- plano
    
    oferta_vs_demanda <-
      necessidades_prof |> 
      left_join(oferta_tratada, 
                by = c("cod_municipiodv"="ibge",
                       "nivel" = "nivel")) |> 
      mutate(ra = if_else(flag == plano, FTE_40_linha - necessidade, 
                                         FTE_40_linha - necessidade_sus)) |>
      mutate(rr = if_else(flag == plano, FTE_40_linha/necessidade, 
                                         FTE_40_linha/necessidade_sus)) 
    
    
### falta colocar as flags aqui para pegar a população SUS dependente 
    
        
    cd_oferta_vs_demanda_regiao_saude <- 
      oferta_vs_demanda |>
          left_join(hierarquia, 
                    by = c("cod_mun_loc"="cod_municipiodv")) |> 
          group_by(nivel, 
                   cod_regsaud, 
                   uf_sigla,
                   regiao_saude) |> 
          summarise(necessidade = sum(necessidade),
                    necessidade_sus = sum(necessidade_sus),
                    oferta_rs = sum(FTE_40_linha)) |> 
          mutate(ra = if_else(flag == plano, oferta_rs - necessidade, 
                                             oferta_rs - necessidade_sus)) |> 
          mutate(rr = if_else(flag == plano, oferta_rs/necessidade, 
                                             oferta_rs/necessidade_sus)) |> 
          mutate(rr = if_else(rr == 0.00, 0.01, rr)) 
    
    
    
  }


# testando  ----------------------------------------------------------


list_ttd <- c(1576, 1676, 1776)  # Transformação de strings para números (não precisa de as.numeric)
list_tempo_aps <- c(25, 35, 45, 55)
list_tempo_endo <- c(35, 45, 55)
list_tempo_peri <- c(35, 45, 55)
list_tempo_prot <- c(35, 45, 55)
list_pd <- c(0.40, 0.50, 0.60)
list_pl <- c(0.50, 0.60, 0.70)
list_cat <- c("2232")  # Categoria como vetor de strings
list_sus <- c(1, 0)  # Seleciona entre todos os profissionais (0) ou apenas SUS (1)
list_plano <- c(1, 0)  # Plano como 1 (necessidades) ou 0 (necessidades_sus)


resultado1 <- list()
iteracao1 <- 0
total_iteracoes1 <- 
  length(list_ttd) * length(list_tempo_aps) * length(list_tempo_endo) * 
  length(list_tempo_peri) * length(list_tempo_prot) * length(list_pd) * 
  length(list_pl) * length(list_cat) * length(list_sus) * length(list_plano)


for (ttd in list_ttd) {
  for (tempo_aps in list_tempo_aps) {
    for (tempo_endo in list_tempo_endo) {
      for (tempo_peri in list_tempo_peri) {
        for (tempo_prot in list_tempo_prot) {
          for (pd in list_pd) {
            for (pl in list_pl) { 
              for (sus in list_sus) {
                for (cat in list_cat) {
                  for (plano in list_plano) {
                    
                    iteracao1 <- iteracao1 + 1
                    cat("Iteração:", iteracao1, "de", total_iteracoes1, "\n")
                    
                    # Chama a função gap_necessidade_oferta
                    res1 <- gap_necessidade_oferta(
                      tempo_aps = tempo_aps,
                      tempo_endo = tempo_endo,
                      tempo_peri = tempo_peri,
                      tempo_prot = tempo_prot,
                      ttd = ttd,
                      pd = pd, 
                      pl = pl, 
                      sus = sus, 
                      categoria = cat, 
                      plano = plano
                    )
                    
                    # Adiciona as variáveis de controle ao resultado
                    res1 <- res1 |> 
                      mutate(
                        tempo_aps = tempo_aps,
                        tempo_endo = tempo_endo,
                        tempo_peri = tempo_peri,
                        tempo_prot = tempo_prot,
                        ttd = ttd,
                        pd = pd,
                        pl = pl,
                        sus = sus,
                        categoria = cat,
                        plano = plano,
                        cenario = iteracao1,
                        plano = if_else(plano == 1, "Necessidade de todos", "Apenas SUS dependente"),
                        sus = if_else(sus == TRUE, "Somente profissionais SUS", "Todos profissionais independente de vínculo"),
                        atributos = paste(tempo_aps, tempo_endo, tempo_peri, tempo_prot, ttd, pd, pl, sus, cat, plano, sep = "_")
                      )
                    
                    # Armazenando o resultado
                    resultado1[[length(resultado1) + 1]] <- res1
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

# Verificando o tamanho da lista resultado
cat("Número de resultados acumulados:", length(resultado1), "\n")

# Unificando todos os resultados em um único data.frame
resultado_teste1 <- do.call(rbind, resultado1)

resultado_teste1 |> 
  filter(regiao_saude == "Jundiaí") |> 
  ggplot(aes(x = rr)) + geom_histogram() +
  facet_wrap(~nivel, scales = "free")


resultado_teste1 |> 
  filter(regiao_saude == "Alto Acre") |> 
  ggplot(aes(x = rr)) + geom_histogram() + 
  facet_wrap(~nivel, scales = "free") + xlim(c(0,1))



