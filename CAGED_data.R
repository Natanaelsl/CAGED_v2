download_caged <- function(ref = NULL,
                           destino = "dados_caged") {
  
  suppressPackageStartupMessages({
    library(googledrive)
    library(dplyr)
    library(stringr)
    library(cli)
    library(purrr)
  })
  
  drive_deauth()
  
  root_id <- "1F89h6odTPGIGMb9eDiJKCute9W89QmqN"
  
  dir.create(destino, showWarnings = FALSE)
  
  cli::cli_h1("📥 Download CAGED (RAW)")
  
  # =========================
  # 🔎 LISTAR ESTRUTURA DRIVE
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
    
  }) %>% arrange(ym)
  
  # =========================
  # 🎯 FILTRO (YYYY ou YYYYMM)
  # =========================
  
  if(!is.null(ref)){
    
    if(str_detect(ref, "^\\d{6}$")){
      estrutura <- estrutura %>% filter(ym == ref)
      
    } else if(str_detect(ref, "^\\d{4}$")){
      estrutura <- estrutura %>% filter(substr(ym,1,4) == ref)
      
    } else {
      stop("Use formato YYYY ou YYYYMM")
    }
  }
  
  if(nrow(estrutura) == 0){
    cli::cli_alert_warning("Nenhum período encontrado.")
    return(invisible(NULL))
  }
  
  # =========================
  # 📊 PROGRESS BAR (CORRETO)
  # =========================
  
  pb <- cli::cli_progress_bar(
    total = nrow(estrutura),
    format = "Download [{bar}] {percent} | {current}/{total} | ETA: {eta}"
  )
  
  # =========================
  # 📥 FUNÇÃO DOWNLOAD
  # =========================
  
  baixar_mes <- function(mes_id, ym){
    
    arq_dest <- file.path(destino, paste0("CAGED_", ym, ".xlsx"))
    
    # 🔁 Skip se já existir
    if(file.exists(arq_dest)){
      cli::cli_alert_info(paste("Já existe:", ym))
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
    
    # 📥 Download com retry simples
    tentativa <- 1
    max_tentativas <- 3
    
    while(tentativa <= max_tentativas){
      
      ok <- tryCatch({
        drive_download(arquivo$id, path = arq_dest, overwrite = TRUE)
        TRUE
      }, error = function(e) FALSE)
      
      if(ok){
        cli::cli_alert_success(paste("Baixado:", ym))
        return(arq_dest)
      }
      
      tentativa <- tentativa + 1
      Sys.sleep(1)
    }
    
    cli::cli_alert_danger(paste("Falha após tentativas:", ym))
    return(NULL)
  }
  
  # =========================
  # 🚀 EXECUÇÃO
  # =========================
  
  resultados <- vector("list", nrow(estrutura))
  
  for(i in seq_len(nrow(estrutura))){
    
    ym <- estrutura$ym[i]
    
    resultados[[i]] <- baixar_mes(estrutura$id[i], ym)
    
    cli::cli_progress_update(pb, inc = 1)
  }
  
  cli::cli_progress_done(pb)
  
  # =========================
  # 📦 RETORNO
  # =========================
  
  baixados <- unlist(resultados)
  baixados <- baixados[!is.na(baixados)]
  
  cli::cli_h2("📦 Resumo")
  cli::cli_alert_success(paste("Arquivos válidos:", length(baixados)))
  
  return(invisible(baixados))
}

processar_caged <- function(origem = "dados_caged_raw",
                            destino = "dados_caged_parquet",
                            sobrescrever = FALSE) {
  
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(arrow)
  library(cli)
  
  dir.create(destino, showWarnings = FALSE)
  
  cli_h1("⚙️ Processamento CAGED")
  
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
  
  # =========================
  # 📊 PROCESSAMENTO POR ARQUIVO
  # =========================
  
  arquivos <- list.files(origem, full.names = TRUE, pattern = "xlsx$")
  
  for(path in arquivos){
    
    ym <- stringr::str_extract(path, "\\d{6}")
    out <- file.path(destino, paste0("CAGED_", ym, ".parquet"))
    
    if(file.exists(out) && !sobrescrever){
      cli_alert_info(paste("Já processado:", ym))
      next
    }
    
    cli_alert_info(paste("Processando:", ym))
    
    abas <- excel_sheets(path)
    
    dados <- map_df(abas, function(aba){
      
      df <- tryCatch(
        read_excel(path, sheet = aba, col_names = FALSE),
        error = function(e) NULL
      )
      
      tryCatch(
        tratar_tabela(df, aba, ym),
        error = function(e) NULL
      )
    })
    
    if(nrow(dados) > 0){
      write_parquet(dados, out)
      cli_alert_success(paste("OK:", ym))
    } else {
      cli_alert_warning(paste("Sem dados:", ym))
    }
  }
  
  invisible(TRUE)
}

CAGED <- function(ref = NULL,
                  consolidar = TRUE) {
  
  library(cli)
  library(arrow)
  
  cli_h1("🚀 Pipeline CAGED (PRO)")
  
  download_caged(ref)
  processar_caged()
  
  if(consolidar){
    cli_h2("📊 Consolidando dataset")
    return(open_dataset("dados_caged_parquet"))
  }
  
  invisible(NULL)
}

download_caged("202407")

#excel_sheets("dados_caged/CAGED_202407.xlsx")
# download_caged()   # camada RAW (bronze)
# processar_caged() # camada SILVER
# CAGED()           # orquestrador
