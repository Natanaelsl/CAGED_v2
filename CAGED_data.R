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
    if(missing(destino) || is.null(destino)){
      destino <- tempfile("CAGED_", tmpdir = tempdir())
    }
    dir.create(destino, showWarnings = FALSE)
    cli::cli_alert_info(paste("Modo temporário:", destino))
  } else {
    dir.create(destino, showWarnings = FALSE)
  }
  
  cli::cli_h1("📥 Download CAGED (RAW)")
  
  # =========================
  # 🔎 LISTAR DRIVE
  # =========================
  
  anos <- drive_ls(as_id(root_id)) %>%
    filter(!str_detect(name, "\\."))
  
  estrutura <- purrr::map_df(seq_len(nrow(anos)), function(i){
    
    meses <- drive_ls(anos$id[i]) %>%
      filter(!str_detect(name, "\\."))
    
    tibble(
      ym = meses$name,
      id = meses$id
    )
    
  }) %>% 
    arrange(ym)
  
  # =========================
  # 🎯 FILTRO
  # =========================
  
  if(!is.null(ref)){
    
    if(ref %in% c("last", "latest")){
      
      estrutura <- estrutura %>% slice_tail(n = 1)
      cli::cli_alert_info(paste("Último período:", estrutura$ym))
      
    } else if(str_detect(ref, "^\\d{6}$")){
      
      estrutura <- estrutura %>% filter(ym == ref)
      
    } else if(str_detect(ref, "^\\d{4}$")){
      
      estrutura <- estrutura %>% filter(substr(ym,1,4) == ref)
      
    } else {
      stop("Use: YYYY, YYYYMM ou 'last'")
    }
  }
  
  if(nrow(estrutura) == 0){
    cli::cli_alert_warning("Nenhum período encontrado")
    return(invisible(NULL))
  }
  
  # =========================
  # 📊 PROGRESS BAR
  # =========================
  
  pb <- cli::cli_progress_bar(
    total  = nrow(estrutura),
    format = "Download [{pb_bar}] {pb_percent} | {pb_current}/{pb_total}"
  )
  
  # =========================
  # 📥 DOWNLOAD
  # =========================
  
  baixar_mes <- function(mes_id, ym){
    
    arq_dest <- file.path(destino, paste0("CAGED_", ym, ".xlsx"))
    
    # Cache (só fora do temp)
    if(!temp && file.exists(arq_dest)){
      cli::cli_inform(paste("Já existe:", ym))
      return(arq_dest)
    }
    
    arquivos <- tryCatch(
      drive_ls(mes_id),
      error = function(e) NULL
    )
    
    if(is.null(arquivos)){
      cli::cli_alert_danger(paste("Erro ao listar:", ym))
      return(NULL)
    }
    
    arquivo <- arquivos %>%
      filter(str_detect(name, "\\.xlsx")) %>%
      slice(1)
    
    if(nrow(arquivo) == 0){
      cli::cli_alert_warning(paste("Sem XLSX:", ym))
      return(NULL)
    }
    
    # Retry
    for(i in 1:3){
      
      ok <- tryCatch({
        drive_download(arquivo$id, path = arq_dest, overwrite = TRUE)
        TRUE
      }, error = function(e) FALSE)
      
      if(ok){
        cli::cli_alert_success(paste("Baixado:", ym))
        return(arq_dest)
      }
      
      Sys.sleep(1)
    }
    
    cli::cli_alert_danger(paste("Falha:", ym))
    return(NULL)
  }
  
  # =========================
  # 🚀 EXECUÇÃO
  # =========================
  
  resultados <- vector("list", nrow(estrutura))
  
  for(i in seq_len(nrow(estrutura))){
    
    ym <- estrutura$ym[i]
    
    resultados[[i]] <- baixar_mes(estrutura$id[i], ym)
    
    cli::cli_progress_update(id = pb, inc = 1)
  }
  
  cli::cli_progress_done(id = pb)
  
  # =========================
  # 📦 RESUMO
  # =========================
  
  validos <- sum(!is.na(resultados))
  
  cli::cli_h2("📦 Resumo")
  cli::cli_alert_success(paste("Arquivos válidos:", validos))
  
  invisible(unlist(resultados))
}

######################################################

processar_caged <- function(origem = "dados_caged_raw",
                            destino = "dados_caged_parquet",
                            sobrescrever = FALSE) {
  
  suppressPackageStartupMessages({
    library(readxl)
    library(dplyr)
    library(stringr)
    library(tidyr)
    library(purrr)
    library(arrow)
    library(cli)
  })
  
  dir.create(destino, showWarnings = FALSE)
  
  cli::cli_h1("⚙️ Processamento CAGED")
  
  # =========================
  # 🔧 TRATAMENTO BASE
  # =========================
  
  tratar_tabela <- function(df, tabela, ym){
    
    if(is.null(df) || nrow(df) < 10) return(NULL)
    
    padrao_uf <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO",
                   "MA","MT","MS","MG","PA","PB","PR","PE","PI",
                   "RJ","RN","RS","RO","RR","SC","SP","SE","TO")
    
    linha_header <- which(
      apply(df, 1, function(x){
        any(x %in% padrao_uf, na.rm = TRUE)
      })
    )[1]
    
    if(is.na(linha_header)) return(NULL)
    
    nomes <- as.character(unlist(df[linha_header, ]))
    nomes[1] <- "categoria"
    nomes[is.na(nomes)] <- paste0("col_", seq_along(nomes))[is.na(nomes)]
    
    df2 <- df[(linha_header + 1):nrow(df), ]
    colnames(df2) <- nomes
    
    df2 <- df2 %>%
      filter(!if_all(everything(), is.na)) %>%
      filter(!str_detect(as.character(categoria),
                         "Total|Fonte|Elaboração"))
    
    df2 %>%
      pivot_longer(-categoria,
                   names_to = "estado",
                   values_to = "valor") %>%
      mutate(
        valor = suppressWarnings(as.numeric(valor)),
        tabela = tabela,
        ano = substr(ym,1,4),
        mes = substr(ym,5,6)
      ) %>%
      filter(!is.na(valor))
  }
  
  arquivos <- list.files(origem, full.names = TRUE, pattern = "xlsx$")
  
  pb <- cli::cli_progress_bar(
    total = length(arquivos),
    format = "Processamento [{bar}] {percent}"
  )
  
  for(path in arquivos){
    
    ym <- str_extract(path, "\\d{6}")
    out <- file.path(destino, paste0("CAGED_", ym, ".parquet"))
    
    if(file.exists(out) && !sobrescrever){
      cli::cli_inform(paste("Já processado:", ym))
      cli::cli_progress_update(pb, inc = 1)
      next
    }
    
    abas <- excel_sheets(path)
    
    dados <- map_df(abas, function(aba){
      
      df <- tryCatch(
        read_excel(path, sheet = aba, col_names = FALSE),
        error = function(e) NULL
      )
      
      tryCatch(tratar_tabela(df, aba, ym), error = function(e) NULL)
    })
    
    if(nrow(dados) > 0){
      write_parquet(dados, out)
      cli::cli_alert_success(paste("OK:", ym))
    }
    
    cli::cli_progress_update(pb, inc = 1)
  }
  
  cli::cli_progress_update(pb, set = length(arquivos))
  cli::cli_progress_done(pb)
  
  invisible(TRUE)
}

######################################################

CAGED <- function(ref = NULL,
                  consolidar = TRUE) {
  
  suppressPackageStartupMessages({
    library(cli)
    library(arrow)
  })
  
  cli::cli_h1("🚀 Pipeline CAGED (PRO)")
  
  download_caged(ref)
  processar_caged()
  
  if(consolidar){
    cli::cli_h2("📊 Consolidando dataset")
    base <- arrow::open_dataset("dados_caged_parquet")
    cli::cli_alert_success("Dataset pronto!")
    return(base)
  }
  
  invisible(NULL)
}

######################################################


# 1. Defina o caminho do seu arquivo
arquivo_data <- download_caged("last", temp = FALSE)

download_caged("202601", destino = "meus_dados/caged")

###################################################################
# ⚠️ Limitação atual (sutil, mas importante)
#
# Hoje você não consegue fazer isso:
#  
#  download_caged("202601", temp = TRUE, destino = "minha_pasta")
#
# 👉 Porque temp = TRUE sempre sobrescreve destino
####################################################################


# 2. Capture os nomes das abas automaticamente
planilhas <- readxl::excel_sheets(arquivo_data)

# Função para ler uma planilha
ler_planilha <- function(planilha, arquivo) {
  readxl::read_excel(arquivo, sheet = planilha, skip = 4)
}

# 1. Criação do Cluster (Deixando 1 núcleo livre)
n_cores <- max(1, parallel::detectCores() - 1)
cluster <- parallel::makeCluster(n_cores)

# 2. Execução Paralela COM Barra de Progresso
# Usamos pbapply::pblapply no lugar de parallel::parLapply.
# O argumento 'cl = cluster' avisa a função para usar o modo paralelo.
cli::cli_h2("Lendo planilhas em paralelo...")

data_frames <- pbapply::pblapply(
  cl = cluster,          # Passamos o cluster criado
  X = planilhas,         # Nossa lista de planilhas
  FUN = ler_planilha,    # A função
  arquivo = arquivo_data # O argumento extra da função
)

# 3. Fechar Cluster e Renomear
parallel::stopCluster(cluster)
names(data_frames) <- planilhas

cli::cli_alert_success("Leitura concluída!")

####################################################






library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ==========================================
# 🛠️ FUNÇÃO INTELIGENTE (ROTEADOR DE ABAS)
# ==========================================

limpar_caged_dinamico <- function(df, nome_aba) {
  
  # ----------------------------------------
  # 🟢 REGRA: TABELA 2 (Região e UF)
  # ----------------------------------------
  if (nome_aba == "Tabela 2") {
    
    # 1. Corte dinâmico do rodapé
    indice_corte <- which(df[[1]] == "Não identificado")[1]
    df_limpo <- df %>% slice(1:indice_corte)
    
    # 2. Tratamento dos cabeçalhos mesclados
    nomes_periodo <- names(df_limpo)
    nomes_periodo <- ifelse(grepl("^\\.\\.\\.", nomes_periodo), NA, nomes_periodo)
    for(i in 2:length(nomes_periodo)) if(is.na(nomes_periodo[i])) nomes_periodo[i] <- nomes_periodo[i-1]
    
    # 3. Subir variáveis da linha 1
    nomes_variaveis <- as.character(df_limpo[1, ])
    nomes_finais <- paste(nomes_periodo, nomes_variaveis, sep = "___")
    nomes_finais[1] <- "Regiao_UF" 
    
    names(df_limpo) <- nomes_finais
    df_limpo <- df_limpo[-1, ] 
    
    # 4. Transformar em Long e limpar textos
    df_final <- df_limpo %>%
      pivot_longer(-Regiao_UF, names_to = c("Periodo", "Metrica"), names_sep = "___", values_to = "Valor") %>%
      mutate(
        Valor = as.numeric(Valor),
        Periodo = str_remove_all(Periodo, " - sem ajuste| - com ajuste|\\*\\*.*") %>% str_trim()
      )
    
    return(df_final)
    
    # ----------------------------------------
    # 🔵 REGRA: TABELA 4 (Grupamentos/Categorias)
    # ----------------------------------------
  } else if (nome_aba == "Tabela 4") {
    
    # 1. Corte dinâmico do rodapé
    indice_corte <- which(df[[1]] == "Não identificado")[1]
    df_limpo <- df %>% slice(1:indice_corte)
    
    # 2. Tratamento dos cabeçalhos mesclados
    nomes_periodo <- names(df_limpo)
    nomes_periodo <- ifelse(grepl("^\\.\\.\\.", nomes_periodo), NA, nomes_periodo)
    for(i in 2:length(nomes_periodo)) if(is.na(nomes_periodo[i])) nomes_periodo[i] <- nomes_periodo[i-1]
    
    # 3. Subir variáveis da linha 1
    nomes_variaveis <- as.character(df_limpo[1, ])
    nomes_finais <- paste(nomes_periodo, nomes_variaveis, sep = "___")
    nomes_finais[1] <- "Categoria" # Nome genérico para a primeira coluna da Tab 4
    
    names(df_limpo) <- nomes_finais
    df_limpo <- df_limpo[-1, ] 
    
    # 4. Transformar em Long e limpar textos
    df_final <- df_limpo %>%
      pivot_longer(-Categoria, names_to = c("Periodo", "Metrica"), names_sep = "___", values_to = "Valor") %>%
      mutate(
        Valor = as.numeric(Valor),
        Periodo = str_remove_all(Periodo, " - sem ajuste| - com ajuste|\\*\\*.*") %>% str_trim()
      )
    
    return(df_final)
    
    # ----------------------------------------
    # 🟡 REGRA: TABELA 7 (e 7.1)
    # ----------------------------------------
  } else if (str_detect(nome_aba, "Tabela 7")) { 
    # Usamos str_detect para aplicar a mesma regra na 7 e 7.1 (se forem iguais)
    
    # 1. Corte dinâmico do rodapé
    indice_corte <- which(df[[1]] == "Não identificado")[1]
    df_limpo <- df %>% slice(1:indice_corte)
    
    # 2. Tratamento dos cabeçalhos mesclados
    nomes_periodo <- names(df_limpo)
    nomes_periodo <- ifelse(grepl("^\\.\\.\\.", nomes_periodo), NA, nomes_periodo)
    for(i in 2:length(nomes_periodo)) if(is.na(nomes_periodo[i])) nomes_periodo[i] <- nomes_periodo[i-1]
    
    # 3. Subir variáveis da linha 1
    nomes_variaveis <- as.character(df_limpo[1, ])
    nomes_finais <- paste(nomes_periodo, nomes_variaveis, sep = "___")
    nomes_finais[1] <- "Regiao_UF" 
    
    names(df_limpo) <- nomes_finais
    df_limpo <- df_limpo[-1, ] 
    
    # 4. Transformar em Long e limpar textos
    df_final <- df_limpo %>%
      pivot_longer(-Regiao_UF, names_to = c("Periodo", "Metrica"), names_sep = "___", values_to = "Valor") %>%
      mutate(
        Valor = as.numeric(Valor),
        Periodo = str_remove_all(Periodo, " - sem ajuste| - com ajuste|\\*\\*.*") %>% str_trim()
      )
    
    return(df_final)
    
    # ----------------------------------------
    # 🔴 REGRA: ABA DESCONHECIDA
    # ----------------------------------------
  } else {
    cli::cli_alert_warning(paste("Nenhuma regra de limpeza definida para:", nome_aba))
    return(df)
  }
}

# ==========================================
# 🚀 APLICANDO COM IMAP
# ==========================================

cli::cli_h2("⚙️ Estruturando Tabelas por Regras...")

# O 'imap' é o segredo aqui: ele passa o 'df' (.x) e o 'nome_aba' (.y) para a função
data_frames_limpos <- purrr::imap(
  .x = data_frames, 
  .f = limpar_caged_dinamico,
  .progress = "Tratando tabelas"
)

cli::cli_alert_success("Limpeza concluída com roteamento específico!")

# =========================================
# UTILIZANDO A FUNÇÃO DE TRANSFORMAÇÃO
# =========================================

# Processa a lista com a nossa função roteadora
data_frames_limpos <- purrr::imap(
  .x = data_frames, 
  .f = limpar_caged_dinamico,
  .progress = "Tratando tabelas"
)


tab7_limpa <- limpar_caged_dinamico(
  df = data_frames[["Tabela 7"]], 
  nome_aba = "Tabela 7"
)





####################################################

# dados    <- processar_arquivo(arquivos)






#excel_sheets("dados_caged/CAGED_202407.xlsx")
# # Só download
# download_caged("202601")
# 
# # Download + parquet automático
# download_caged("202601", parquet = TRUE)
# 
# # Último mês + parquet
# download_caged("last", parquet = TRUE)
# 
# # Pipeline temporário (sem cache)
# download_caged("last", temp = TRUE, parquet = TRUE)
# download_caged("202601", destino = "meus_dados/caged")

# processar_caged() # camada SILVER
# CAGED()           # orquestrador
