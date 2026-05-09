options(scipen = 999)

# 1. FUNÇÃO DE DOWNLOAD (Otimizada para API) ----

download_caged <- function(ref = NULL,
                           destino = "dados_caged_raw",
                           temp = FALSE) {
  
  suppressPackageStartupMessages({
    library(googledrive)
    library(dplyr)
    library(stringr)
    library(cli)
    library(purrr)
  })
  
  drive_deauth()
  root_id <- "1F89h6odTPGIGMb9eDiJKCute9W89QmqN"
  
  # =========================
  # 📁 DESTINO
  # =========================
  if(temp){
    # Garante que o arquivo vá para a raiz do tempdir para facilitar a busca depois
    destino <- tempdir()
    cli::cli_alert_info(paste("Modo temporário ativado. Destino:", destino))
  } else {
    dir.create(destino, showWarnings = FALSE)
  }
  
  cli::cli_h1("📥 Download CAGED (RAW)")
  
  # =========================
  # 🔎 BUSCA INTELIGENTE NA API
  # =========================
  anos <- drive_ls(as_id(root_id)) %>% filter(!str_detect(name, "\\."))
  
  # Determina qual ano investigar para economizar chamadas na API do Drive
  if(!is.null(ref)) {
    if(ref %in% c("last", "latest")) {
      anos_alvo <- anos %>% arrange(desc(name)) %>% slice(1)
    } else if(str_detect(ref, "^\\d{6}$") || str_detect(ref, "^\\d{4}$")) {
      ano_ref <- substr(ref, 1, 4)
      anos_alvo <- anos %>% filter(name == ano_ref)
    } else {
      stop("Use: YYYY, YYYYMM ou 'last'")
    }
  } else {
    anos_alvo <- anos
  }
  
  if(nrow(anos_alvo) == 0){
    cli::cli_alert_warning("Ano não encontrado no Drive.")
    return(invisible(NULL))
  }
  
  # Mapeia apenas as pastas do ano(s) alvo
  estrutura <- purrr::map_df(seq_len(nrow(anos_alvo)), function(i){
    meses <- drive_ls(anos_alvo$id[i]) %>% filter(!str_detect(name, "\\."))
    tibble(ym = meses$name, id = meses$id)
  }) %>% arrange(ym)
  
  # Refina o filtro para o mês específico (se aplicável)
  if(!is.null(ref)) {
    if(ref %in% c("last", "latest")) {
      estrutura <- estrutura %>% slice_tail(n = 1)
      cli::cli_alert_info(paste("Último período identificado:", estrutura$ym))
    } else if(str_detect(ref, "^\\d{6}$")) {
      estrutura <- estrutura %>% filter(ym == ref)
    }
  }
  
  if(nrow(estrutura) == 0){
    cli::cli_alert_warning("Período específico não encontrado.")
    return(invisible(NULL))
  }
  
  pb <- cli::cli_progress_bar(
    total  = nrow(estrutura),
    format = "Download [{pb_bar}] {pb_percent} | {pb_current}/{pb_total}"
  )
  
  # =========================
  # 📥 DOWNLOAD ROBUSTO
  # =========================
  baixar_mes <- function(mes_id, ym){
    arq_dest <- file.path(destino, paste0("CAGED_", ym, ".xlsx"))
    
    if(!temp && file.exists(arq_dest)){
      cli::cli_inform(paste("Arquivo já existe no cache local:", ym))
      return(arq_dest)
    }
    
    arquivos <- tryCatch(drive_ls(mes_id), error = function(e) NULL)
    if(is.null(arquivos)) return(NULL)
    
    arquivo <- arquivos %>% filter(str_detect(name, "\\.xlsx")) %>% slice(1)
    if(nrow(arquivo) == 0) return(NULL)
    
    for(i in 1:3){
      ok <- tryCatch({
        drive_download(arquivo$id, path = arq_dest, overwrite = TRUE)
        TRUE
      }, error = function(e) FALSE)
      
      if(ok) {
        cli::cli_alert_success(paste("Baixado com sucesso:", ym))
        return(arq_dest)
      }
      Sys.sleep(1)
    }
    cli::cli_alert_danger(paste("Falha após 3 tentativas:", ym))
    return(NULL)
  }
  
  resultados <- vector("list", nrow(estrutura))
  for(i in seq_len(nrow(estrutura))){
    resultados[[i]] <- baixar_mes(estrutura$id[i], estrutura$ym[i])
    cli::cli_progress_update(id = pb, inc = 1)
  }
  
  cli::cli_progress_done(id = pb)
  validos <- sum(!sapply(resultados, is.null))
  cli::cli_alert_success(paste("Arquivos válidos baixados:", validos))
  
  return(invisible(unlist(resultados)))
}


# 2. FUNÇÃO DE PROCESSAMENTO (Código DRY) ----

processar_caged <- function(usar_temporario = FALSE, 
                            parquet_individual = FALSE, 
                            origem = "dados_caged_raw", 
                            destino = "dados_caged_parquet") {
  
  suppressPackageStartupMessages({
    library(readxl)
    library(dplyr)
    library(stringr)
    library(tidyr)
    library(purrr)
    library(arrow)
    library(cli)
  })
  
  if (usar_temporario) {
    pasta_busca <- tempdir()
    cli::cli_alert_info("Modo Temp Automático: Buscando na memória do sistema...")
  } else {
    pasta_busca <- origem
  }
  
  arquivos <- list.files(pasta_busca, full.names = TRUE, pattern = "xlsx$", recursive = TRUE)
  
  if (length(arquivos) == 0) {
    cli::cli_alert_danger("Nenhum arquivo .xlsx encontrado em: {pasta_busca}")
    return(invisible(NULL))
  }
  
  info_arquivos <- file.info(arquivos)
  caminho_arquivo <- rownames(info_arquivos)[which.max(info_arquivos$mtime)]
  cli::cli_h1("⚙️ Processando Base: {basename(caminho_arquivo)}")
  
  # --- SUBFUNÇÃO PARA EVITAR REPETIÇÃO ---
  padronizar_cabecalho <- function(df_local, colunas_chave) {
    indice_corte <- which(str_detect(str_trim(df_local[[1]]), "(?i)^Não identificado"))[1]
    if(!is.na(indice_corte)) df_limpo <- df_local %>% slice(1:indice_corte) else df_limpo <- df_local
    
    nomes_periodo <- names(df_limpo)
    nomes_periodo <- ifelse(grepl("^\\.\\.\\.", nomes_periodo), NA, nomes_periodo)
    for(i in 2:length(nomes_periodo)) if(is.na(nomes_periodo[i])) nomes_periodo[i] <- nomes_periodo[i-1]
    
    nomes_variaveis <- as.character(df_limpo[1, ])
    nomes_finais <- paste(nomes_periodo, nomes_variaveis, sep = "___")
    
    # Substitui os nomes das chaves (CNAE, UF, Municipio, etc)
    nomes_finais[1:length(colunas_chave)] <- colunas_chave
    names(df_limpo) <- nomes_finais
    df_limpo <- df_limpo[-1, ]
    
    df_limpo %>% 
      pivot_longer(cols = -all_of(colunas_chave), names_to = c("Periodo", "Metrica"), names_sep = "___", values_to = "Valor") %>%
      mutate(Valor = suppressWarnings(as.numeric(Valor)), 
             Periodo = str_remove_all(Periodo, " - sem ajustes?| - com ajustes?|\\*\\*.*") %>% str_trim())
  }
  # ----------------------------------------
  
  limpar_caged_dinamico <- function(df, nome_aba) {
    df <- df %>% mutate(across(everything(), as.character))
    nome_aba <- str_trim(nome_aba)
    
    if (str_detect(nome_aba, "^Tabela 1|^Tabela 6|^Tabela 10")) {
      return(padronizar_cabecalho(df, c("Grupamento_CNAE")))
      
    } else if (str_detect(nome_aba, "^Tabela 2|^Tabela 7|^Tabela 11")) {
      return(padronizar_cabecalho(df, c("Regiao_UF")))
      
    } else if (str_detect(nome_aba, "^Tabela 3|^Tabela 8")) {
      return(padronizar_cabecalho(df, c("UF", "Codigo_Municipio", "Municipio")))
      
    } else if (str_detect(nome_aba, "^Tabela 4")) {
      return(padronizar_cabecalho(df, c("Categoria")))
      
    } else if (str_detect(nome_aba, "^Tabela 5")) {
      indice_corte <- which(str_detect(df[[1]], "(?i)^Fonte|^Nota"))[1]
      if (!is.na(indice_corte)) df_limpo <- df %>% slice(1:(indice_corte - 1)) else df_limpo <- df
      nomes_finais <- as.character(df_limpo[1, ])
      nomes_finais[1] <- "Periodo"
      names(df_limpo) <- nomes_finais
      return(df_limpo[-1, ] %>% pivot_longer(cols = -Periodo, names_to = "Metrica", values_to = "Valor") %>%
               mutate(Valor = suppressWarnings(as.numeric(Valor)), Periodo = str_remove_all(Periodo, "\\*\\*.*|\\*") %>% str_trim()))
      
    } else if (str_detect(nome_aba, "^Tabela 9")) {
      indice_corte <- which(str_detect(df[[1]], "(?i)^Fonte|^Nota"))[1]
      if (!is.na(indice_corte)) df_limpo <- df %>% slice(1:(indice_corte - 1)) else df_limpo <- df
      nomes_finais <- as.character(df_limpo[1, ])
      nomes_finais[1] <- "Periodo"
      names(df_limpo) <- nomes_finais
      return(df_limpo[-1, ] %>% pivot_longer(cols = -Periodo, names_to = "Metrica", values_to = "Valor") %>%
               mutate(Valor = suppressWarnings(ifelse(str_detect(Valor, "R\\$"), as.numeric(str_replace(str_remove_all(Valor, "R\\$\\s*|\\."), ",", ".")), as.numeric(Valor))),
                      Periodo = str_remove_all(Periodo, "\\*\\*.*|\\*") %>% str_trim()))
    }
    return(NULL)
  }
  
  abas <- excel_sheets(caminho_arquivo)
  
  lista_tabelas <- purrr::map(abas, function(aba) {
    df_bruto <- tryCatch(suppressMessages(readxl::read_excel(caminho_arquivo, sheet = aba, skip = 4)), error = function(e) NULL)
    if (is.null(df_bruto) || nrow(df_bruto) == 0) return(NULL)
    df_limpo <- limpar_caged_dinamico(df_bruto, aba)
    if (!is.null(df_limpo)) return(df_limpo %>% mutate(Tabela_Origem = aba)) else return(NULL)
  })
  
  names(lista_tabelas) <- abas
  lista_tabelas <- lista_tabelas[!sapply(lista_tabelas, is.null)]
  
  dir.create(destino, showWarnings = FALSE)
  ym <- str_extract(basename(caminho_arquivo), "\\d{4,6}")
  if(is.na(ym)) ym <- "ATUAL"
  
  if (parquet_individual) {
    purrr::iwalk(lista_tabelas, ~{
      nome_safe <- str_replace_all(.y, "[^a-zA-Z0-9]", "_")
      arrow::write_parquet(.x, file.path(destino, paste0("CAGED_", nome_safe, "_", ym, ".parquet")))
    })
    cli::cli_alert_success("Concluído! Tabelas individuais em: {destino}")
  } else {
    df_consolidado <- dplyr::bind_rows(lista_tabelas)
    caminho_final <- file.path(destino, paste0("CAGED_CONSOLIDADO_", ym, ".parquet"))
    arrow::write_parquet(df_consolidado, caminho_final)
    cli::cli_alert_success("Concluído! Base unificada em: {basename(caminho_final)}")
  }
  
  return(invisible(lista_tabelas))
}


# 3. FUNÇÃO ORQUESTRADORA (Conexão das Pontas) ----

CAGED <- function(ref = "last", 
                  parquet_individual = FALSE, 
                  arquivo_alvo = NULL) {
  
  suppressPackageStartupMessages({
    library(cli)
    library(arrow)
  })
  
  cli::cli_h1("🚀 Pipeline CAGED (PRO)")
  
  # O download roda avisando que é temporário
  download_caged(ref = ref, temp = TRUE) 
  
  # O processamento usa a pasta temporária automaticamente
  processar_caged(usar_temporario = TRUE, 
                  parquet_individual = parquet_individual)
  
  cli::cli_h2("📊 Abrindo dataset via Arrow")
  
  if (parquet_individual) {
    if (is.null(arquivo_alvo)) {
      cli::cli_alert_danger("Erro: Defina o caminho exato no argumento 'arquivo_alvo'.")
      return(invisible(NULL)) 
    }
    base <- arrow::open_dataset(arquivo_alvo)
    cli::cli_alert_success(paste("Tabela individual carregada:", basename(arquivo_alvo)))
    
  } else {
    arquivos_consolidados <- list.files("dados_caged_parquet", pattern = "CONSOLIDADO", full.names = TRUE)
    if (length(arquivos_consolidados) == 0) {
      cli::cli_alert_danger("Nenhum arquivo consolidado encontrado na pasta.")
      return(invisible(NULL))
    }
    caminho_padrao <- tail(sort(arquivos_consolidados), 1)
    base <- arrow::open_dataset(caminho_padrao)
    cli::cli_alert_success(paste("Dataset consolidado carregado:", basename(caminho_padrao)))
  }
  
  return(base)
}


# --- EXEMPLOS DE USO ----
# download_caged("last")
# processar_caged(usar_temporario = TRUE)
# base_fiscal <- CAGED() 
# 
# 
# 
# Tab7 <- base_fiscal %>% 
#   filter(Tabela_Origem == "Tabela 7") %>% 
#   collect()
# 
# 
# base_municipios <- CAGED(
#   parquet_individual = TRUE, 
#   arquivo_alvo = "dados_caged_parquet/CAGED_Tabela_3_ATUAL.parquet"
# )
