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





# COMO USAR AS FUNÇÕES ###################################################

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
