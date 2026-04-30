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


# Certifique-se de ter os pacotes instalados: install.packages(c("readxl", "dplyr", "stringr", "tidyr", "purrr", "arrow", "cli"))

options(scipen = 999)

processar_caged <- function(origem = "dados_caged_raw", 
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
  
  # 1. Identifica o arquivo acumulado mais recente
  arquivos <- list.files(origem, full.names = TRUE, pattern = "xlsx$")
  if (length(arquivos) == 0) {
    cli::cli_alert_danger("Nenhum arquivo .xlsx encontrado em {origem}")
    return(NULL)
  }
  caminho_arquivo <- tail(sort(arquivos), 1)
  cli::cli_h1("⚙️ Processando Base Acumulada (parquet): {basename(caminho_arquivo)}")
  
  # ==========================================
  # 🛠️ MOTOR DE LIMPEZA (REGRAS SILENCIOSAS)
  # ==========================================
  limpar_caged_dinamico <- function(df, nome_aba) {
    df <- df %>% mutate(across(everything(), as.character))
    nome_aba <- str_trim(nome_aba)
    
    # REGRA: CNAE (1, 6, 6.1, 10)
    if (str_detect(nome_aba, "^Tabela 1|^Tabela 6|^Tabela 10")) {
      indice_corte <- which(str_detect(str_trim(df[[1]]), "(?i)^Não identificado"))[1]
      if(!is.na(indice_corte)) df_limpo <- df %>% slice(1:indice_corte) else df_limpo <- df
      nomes_periodo <- names(df_limpo)
      nomes_periodo <- ifelse(grepl("^\\.\\.\\.", nomes_periodo), NA, nomes_periodo)
      for(i in 2:length(nomes_periodo)) if(is.na(nomes_periodo[i])) nomes_periodo[i] <- nomes_periodo[i-1]
      nomes_variaveis <- as.character(df_limpo[1, ])
      nomes_finais <- paste(nomes_periodo, nomes_variaveis, sep = "___")
      nomes_finais[1] <- "Grupamento_CNAE" 
      names(df_limpo) <- nomes_finais
      df_limpo <- df_limpo[-1, ] 
      return(df_limpo %>% pivot_longer(-Grupamento_CNAE, names_to = c("Periodo", "Metrica"), names_sep = "___", values_to = "Valor") %>%
               mutate(Valor = suppressWarnings(as.numeric(Valor)), 
                      Periodo = str_remove_all(Periodo, " - sem ajustes?| - com ajustes?|\\*\\*.*") %>% str_trim()))
      
      # REGRA: UF/REGIAO (2, 7, 7.1, 11)
    } else if (str_detect(nome_aba, "^Tabela 2|^Tabela 7|^Tabela 11")) {
      indice_corte <- which(str_detect(str_trim(df[[1]]), "(?i)^Não identificado"))[1]
      if(!is.na(indice_corte)) df_limpo <- df %>% slice(1:indice_corte) else df_limpo <- df
      nomes_periodo <- names(df_limpo)
      nomes_periodo <- ifelse(grepl("^\\.\\.\\.", nomes_periodo), NA, nomes_periodo)
      for(i in 2:length(nomes_periodo)) if(is.na(nomes_periodo[i])) nomes_periodo[i] <- nomes_periodo[i-1]
      nomes_variaveis <- as.character(df_limpo[1, ])
      nomes_finais <- paste(nomes_periodo, nomes_variaveis, sep = "___")
      nomes_finais[1] <- "Regiao_UF" 
      names(df_limpo) <- nomes_finais
      df_limpo <- df_limpo[-1, ] 
      return(df_limpo %>% pivot_longer(-Regiao_UF, names_to = c("Periodo", "Metrica"), names_sep = "___", values_to = "Valor") %>%
               mutate(Valor = suppressWarnings(as.numeric(Valor)), 
                      Periodo = str_remove_all(Periodo, " - sem ajustes?| - com ajustes?|\\*\\*.*") %>% str_trim()))
      
      # REGRA: MUNICIPIOS (3, 8, 8.1)
    } else if (str_detect(nome_aba, "^Tabela 3|^Tabela 8")) {
      indice_corte <- which(str_detect(str_trim(df[[1]]), "(?i)^Não identificado"))[1]
      if(!is.na(indice_corte)) df_limpo <- df %>% slice(1:indice_corte) else df_limpo <- df
      nomes_periodo <- names(df_limpo)
      nomes_periodo <- ifelse(grepl("^\\.\\.\\.", nomes_periodo), NA, nomes_periodo)
      for(i in 2:length(nomes_periodo)) if(is.na(nomes_periodo[i])) nomes_periodo[i] <- nomes_periodo[i-1]
      nomes_variaveis <- as.character(df_limpo[1, ])
      nomes_finais <- paste(nomes_periodo, nomes_variaveis, sep = "___")
      nomes_finais[1] <- "UF" ; nomes_finais[2] <- "Codigo_Municipio" ; nomes_finais[3] <- "Municipio" 
      names(df_limpo) <- nomes_finais
      df_limpo <- df_limpo[-1, ] 
      return(df_limpo %>% pivot_longer(cols = -c(UF, Codigo_Municipio, Municipio), names_to = c("Periodo", "Metrica"), names_sep = "___", values_to = "Valor") %>%
               mutate(Valor = suppressWarnings(as.numeric(Valor)), 
                      Periodo = str_remove_all(Periodo, " - sem ajustes?| - com ajustes?|\\*\\*.*") %>% str_trim()))
      
      # REGRA: CATEGORIAS (4)
    } else if (str_detect(nome_aba, "^Tabela 4")) {
      indice_corte <- which(str_detect(str_trim(df[[1]]), "(?i)^Não identificado"))[1]
      if(!is.na(indice_corte)) df_limpo <- df %>% slice(1:indice_corte) else df_limpo <- df
      nomes_periodo <- names(df_limpo)
      nomes_periodo <- ifelse(grepl("^\\.\\.\\.", nomes_periodo), NA, nomes_periodo)
      for(i in 2:length(nomes_periodo)) if(is.na(nomes_periodo[i])) nomes_periodo[i] <- nomes_periodo[i-1]
      nomes_variaveis <- as.character(df_limpo[1, ])
      nomes_finais <- paste(nomes_periodo, nomes_variaveis, sep = "___")
      nomes_finais[1] <- "Categoria" 
      names(df_limpo) <- nomes_finais
      df_limpo <- df_limpo[-1, ] 
      return(df_limpo %>% pivot_longer(-Categoria, names_to = c("Periodo", "Metrica"), names_sep = "___", values_to = "Valor") %>%
               mutate(Valor = suppressWarnings(as.numeric(Valor)), 
                      Periodo = str_remove_all(Periodo, " - sem ajustes?| - com ajustes?|\\*\\*.*") %>% str_trim()))
      
      # REGRA: SERIES HISTORICAS (5, 5.1)
    } else if (str_detect(nome_aba, "^Tabela 5")) {
      indice_corte <- which(str_detect(df[[1]], "(?i)^Fonte|^Nota"))[1]
      if (!is.na(indice_corte)) df_limpo <- df %>% slice(1:(indice_corte - 1)) else df_limpo <- df
      nomes_finais <- as.character(df_limpo[1, ])
      nomes_finais[1] <- "Periodo"
      names(df_limpo) <- nomes_finais
      df_limpo <- df_limpo[-1, ] 
      return(df_limpo %>% pivot_longer(cols = -Periodo, names_to = "Metrica", values_to = "Valor") %>%
               mutate(Valor = suppressWarnings(as.numeric(Valor)), 
                      Periodo = str_remove_all(Periodo, "\\*\\*.*|\\*") %>% str_trim()))
      
      # REGRA: SALARIOS (9)
    } else if (str_detect(nome_aba, "^Tabela 9")) {
      indice_corte <- which(str_detect(df[[1]], "(?i)^Fonte|^Nota"))[1]
      if (!is.na(indice_corte)) df_limpo <- df %>% slice(1:(indice_corte - 1)) else df_limpo <- df
      nomes_finais <- as.character(df_limpo[1, ])
      nomes_finais[1] <- "Periodo"
      names(df_limpo) <- nomes_finais
      df_limpo <- df_limpo[-1, ] 
      return(df_limpo %>% pivot_longer(cols = -Periodo, names_to = "Metrica", values_to = "Valor") %>%
               mutate(Valor = suppressWarnings(ifelse(str_detect(Valor, "R\\$"), 
                                                      as.numeric(str_replace(str_remove_all(Valor, "R\\$\\s*|\\."), ",", ".")), 
                                                      as.numeric(Valor))),
                      Periodo = str_remove_all(Periodo, "\\*\\*.*|\\*") %>% str_trim()))
    }
    return(NULL)
  }
  
  # ==========================================
  # 🔄 PROCESSAMENTO E UNIFICAÇÃO
  # ==========================================
  abas <- excel_sheets(caminho_arquivo)
  
  # Mapeia as abas, limpa e adiciona a identificação da origem
  df_unificado <- purrr::map_dfr(abas, function(aba) {
    df_bruto <- tryCatch({
      suppressMessages(readxl::read_excel(caminho_arquivo, sheet = aba, skip = 4))
    }, error = function(e) NULL)
    
    if (is.null(df_bruto)) return(NULL)
    
    df_limpo <- limpar_caged_dinamico(df_bruto, aba)
    
    if (!is.null(df_limpo)) {
      return(df_limpo %>% mutate(Tabela_Origem = aba))
    } else {
      return(NULL)
    }
  })
  
  # 💾 SALVAMENTO EM ARQUIVO ÚNICO
  dir.create(destino, showWarnings = FALSE)
  ym <- str_extract(basename(caminho_arquivo), "\\d{4,6}")
  if(is.na(ym)) ym <- "BASE_ATUAL"
  
  caminho_final <- file.path(destino, paste0("CAGED_CONSOLIDADO_", ym, ".parquet"))
  arrow::write_parquet(df_unificado, caminho_final)
  
  cli::cli_alert_success("Processamento concluído! Todas as abas unificadas em: {basename(caminho_final)}")
  
  # Retorna a lista dividida por aba para manter sua estrutura de análise
  return(split(df_unificado, df_unificado$Tabela_Origem))
} # <---- ADD FUNÇÃO DE CONSOLIDAR OU NÃO PARQUET!!



# --- COMO USAR: ---
# 1. Crie uma pasta chamada "dados_caged_raw" onde o seu script do R está salvo.
# 2. Jogue todos os seus Excels (.xlsx) baixados do CAGED lá dentro.
# 3. Rode o comando abaixo:
# data_frames_limpos <- processar_caged(origem = "dados_caged_raw", destino = "dados_caged_parquet")


######################################################
######################################################
######################################################
######################################################

# ==========================================
# 🚀 FUNÇÃO ORQUESTRADORA: CAGED
# ==========================================
CAGED <- function(ref = NULL, consolidar = TRUE) {
  
  suppressPackageStartupMessages({
    library(cli)
    library(arrow)
    library(dplyr)
    library(purrr)
  })
  
  cli::cli_h1("🚀 Pipeline CAGED (PRO)")
  
  # 1. Download (Supõe-se que a função download_caged já existe)
  # download_caged(ref) 
  
  # 2. Processamento (Usando a nossa função acumulada que gera os Parquets)
  # Aqui chamamos a função que processa o último arquivo disponível
  lista_tabelas <- processar_caged()
  
  if(consolidar){
    cli::cli_h2("📊 Consolidando dataset")
    
    # Abre o diretório onde os parquets foram salvos pela função de processamento
    base <- arrow::open_dataset("dados_caged_parquet")
    
    cli::cli_alert_success("Dataset pronto para consulta via Arrow!")
    return(base)
  }
  
  return(lista_tabelas)
}


# Baixa, processa e já te entrega o link para o dataset consolidado
download_caged("last")
base_fiscal <- CAGED() 


# Exemplo de uso ultra rápido com a base consolidada:
Tab7 <- base_fiscal %>% 
  filter(Tabela_Origem == "Tabela 7") %>% 
  collect()




######################################################


# 1. Defina o caminho do seu arquivo
arquivo_data <- download_caged("last", temp = TRUE)

# download_caged("202601", destino = "meus_dados/caged")

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





options(scipen = 999)

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)

# ==========================================
# 🛠️ FUNÇÃO INTELIGENTE (ROTEADOR DE ABAS)
# ==========================================

limpar_caged_dinamico <- function(df, nome_aba) {
  
  # --- 🛡️ CAMADAS DE SEGURANÇA GLOBAIS ---
  # 1. Força todas as colunas a serem texto puro para evitar erro no pivot_longer
  df <- df %>% mutate(across(everything(), as.character))
  
  # 2. Limpa espaços extras no nome da aba
  nome_aba <- str_trim(nome_aba)
  
  # ----------------------------------------
  # 🟣 REGRA: TABELAS 1, 6, 6.1 e 10 (Grupamento CNAE)
  # ----------------------------------------
  if (str_detect(nome_aba, "^Tabela 1|^Tabela 6|^Tabela 10")) { 
    
    indice_corte <- which(str_detect(str_trim(df[[1]]), "(?i)^Não identificado"))[1]
    if(!is.na(indice_corte)) df_limpo <- df %>% slice(1:indice_corte) else df_limpo <- df
    
    nomes_periodo <- names(df_limpo)
    nomes_periodo <- ifelse(grepl("^\\.\\.\\.", nomes_periodo), NA, nomes_periodo)
    for(i in 2:length(nomes_periodo)) if(is.na(nomes_periodo[i])) nomes_periodo[i] <- nomes_periodo[i-1]
    
    nomes_variaveis <- as.character(df_limpo[1, ])
    nomes_finais <- paste(nomes_periodo, nomes_variaveis, sep = "___")
    nomes_finais[1] <- "Grupamento_CNAE" 
    
    names(df_limpo) <- nomes_finais
    df_limpo <- df_limpo[-1, ] 
    
    df_final <- df_limpo %>%
      pivot_longer(-Grupamento_CNAE, names_to = c("Periodo", "Metrica"), names_sep = "___", values_to = "Valor") %>%
      mutate(
        Valor = as.numeric(Valor),
        Periodo = str_remove_all(Periodo, " - sem ajustes?| - com ajustes?|\\*\\*.*") %>% str_trim()
      )
    
    return(df_final)
    
    # ----------------------------------------
    # 🟢 REGRA: TABELAS 2, 7, 7.1 e 11 (Região e UF)
    # ----------------------------------------
  } else if (str_detect(nome_aba, "^Tabela 2|^Tabela 7|^Tabela 11")) { # <-- Tabela 11 adicionada aqui!
    
    indice_corte <- which(str_detect(str_trim(df[[1]]), "(?i)^Não identificado"))[1]
    if(!is.na(indice_corte)) df_limpo <- df %>% slice(1:indice_corte) else df_limpo <- df
    
    nomes_periodo <- names(df_limpo)
    nomes_periodo <- ifelse(grepl("^\\.\\.\\.", nomes_periodo), NA, nomes_periodo)
    for(i in 2:length(nomes_periodo)) if(is.na(nomes_periodo[i])) nomes_periodo[i] <- nomes_periodo[i-1]
    
    nomes_variaveis <- as.character(df_limpo[1, ])
    nomes_finais <- paste(nomes_periodo, nomes_variaveis, sep = "___")
    nomes_finais[1] <- "Regiao_UF" 
    
    names(df_limpo) <- nomes_finais
    df_limpo <- df_limpo[-1, ] 
    
    df_final <- df_limpo %>%
      pivot_longer(-Regiao_UF, names_to = c("Periodo", "Metrica"), names_sep = "___", values_to = "Valor") %>%
      mutate(
        Valor = as.numeric(Valor),
        Periodo = str_remove_all(Periodo, " - sem ajustes?| - com ajustes?|\\*\\*.*") %>% str_trim()
      )
    
    return(df_final)
    
    # ----------------------------------------
    # 🔵 REGRA: TABELAS 3, 8 e 8.1 (Municípios)
    # ----------------------------------------
  } else if (str_detect(nome_aba, "^Tabela 3|^Tabela 8")) { 
    
    indice_corte <- which(str_detect(str_trim(df[[1]]), "(?i)^Não identificado"))[1]
    if(!is.na(indice_corte)) df_limpo <- df %>% slice(1:indice_corte) else df_limpo <- df
    
    nomes_periodo <- names(df_limpo)
    nomes_periodo <- ifelse(grepl("^\\.\\.\\.", nomes_periodo), NA, nomes_periodo)
    for(i in 2:length(nomes_periodo)) if(is.na(nomes_periodo[i])) nomes_periodo[i] <- nomes_periodo[i-1]
    
    nomes_variaveis <- as.character(df_limpo[1, ])
    nomes_finais <- paste(nomes_periodo, nomes_variaveis, sep = "___")
    
    nomes_finais[1] <- "UF" 
    nomes_finais[2] <- "Codigo_Municipio" 
    nomes_finais[3] <- "Municipio" 
    
    names(df_limpo) <- nomes_finais
    df_limpo <- df_limpo[-1, ] 
    
    df_final <- df_limpo %>%
      pivot_longer(
        cols = -c(UF, Codigo_Municipio, Municipio), 
        names_to = c("Periodo", "Metrica"), 
        names_sep = "___", 
        values_to = "Valor" 
      ) %>%
      mutate(
        Valor = as.numeric(Valor),
        Periodo = str_remove_all(Periodo, " - sem ajustes?| - com ajustes?|\\*\\*.*") %>% str_trim()
      )
    
    return(df_final)
    
    # ----------------------------------------
    # 🟠 REGRA: TABELA 4 (Grupamentos/Categorias)
    # ----------------------------------------
  } else if (str_detect(nome_aba, "^Tabela 4")) {
    
    indice_corte <- which(str_detect(str_trim(df[[1]]), "(?i)^Não identificado"))[1]
    if(!is.na(indice_corte)) df_limpo <- df %>% slice(1:indice_corte) else df_limpo <- df
    
    nomes_periodo <- names(df_limpo)
    nomes_periodo <- ifelse(grepl("^\\.\\.\\.", nomes_periodo), NA, nomes_periodo)
    for(i in 2:length(nomes_periodo)) if(is.na(nomes_periodo[i])) nomes_periodo[i] <- nomes_periodo[i-1]
    
    nomes_variaveis <- as.character(df_limpo[1, ])
    nomes_finais <- paste(nomes_periodo, nomes_variaveis, sep = "___")
    nomes_finais[1] <- "Categoria" 
    
    names(df_limpo) <- nomes_finais
    df_limpo <- df_limpo[-1, ] 
    
    df_final <- df_limpo %>%
      pivot_longer(-Categoria, names_to = c("Periodo", "Metrica"), names_sep = "___", values_to = "Valor") %>%
      mutate(
        Valor = as.numeric(Valor),
        Periodo = str_remove_all(Periodo, " - sem ajustes?| - com ajustes?|\\*\\*.*") %>% str_trim()
      )
    
    return(df_final)
    
    # ----------------------------------------
    # 🟤 REGRA: TABELAS 5 e 5.1 (Séries Históricas Padrão)
    # ----------------------------------------
  } else if (str_detect(nome_aba, "^Tabela 5")) {
    
    indice_corte <- which(str_detect(df[[1]], "(?i)^Fonte|^Nota"))[1]
    
    if (!is.na(indice_corte)) {
      df_limpo <- df %>% slice(1:(indice_corte - 1))
    } else {
      df_limpo <- df
    }
    
    nomes_finais <- as.character(df_limpo[1, ])
    nomes_finais[1] <- "Periodo"
    
    names(df_limpo) <- nomes_finais
    df_limpo <- df_limpo[-1, ] 
    
    df_final <- df_limpo %>%
      pivot_longer(
        cols = -Periodo, 
        names_to = "Metrica", 
        values_to = "Valor"
      ) %>%
      mutate(
        Valor = as.numeric(Valor),
        Periodo = str_remove_all(Periodo, "\\*\\*.*|\\*") %>% str_trim()
      )
    
    return(df_final)
    
    # ----------------------------------------
    # 🟡 REGRA: TABELAS 9 e 9.1 (Séries Históricas de Salários)
    # ----------------------------------------
  } else if (str_detect(nome_aba, "^Tabela 9")) {
    
    indice_corte <- which(str_detect(df[[1]], "(?i)^Fonte|^Nota"))[1]
    if (!is.na(indice_corte)) df_limpo <- df %>% slice(1:(indice_corte - 1)) else df_limpo <- df
    
    nomes_finais <- as.character(df_limpo[1, ])
    nomes_finais[1] <- "Periodo"
    
    names(df_limpo) <- nomes_finais
    df_limpo <- df_limpo[-1, ] 
    
    df_final <- df_limpo %>%
      pivot_longer(
        cols = -Periodo, 
        names_to = "Metrica", 
        values_to = "Valor"
      ) %>%
      mutate(
        Valor = ifelse(
          str_detect(Valor, "R\\$"),
          as.numeric(str_replace(str_remove_all(Valor, "R\\$\\s*|\\."), ",", ".")),
          as.numeric(Valor)
        ),
        Periodo = str_remove_all(Periodo, "\\*\\*.*|\\*") %>% str_trim()
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








#############################

tab8_limpa <- limpar_caged_dinamico(
  df = data_frames[["Tabela 8"]], 
  nome_aba = "Tabela 8"
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
