# 1. Carrega as funções que criamos
source("caminho/para/suas_funcoes_caged.R")

# 2. Configura um log de registro (para você saber se rodou de madrugada)
arquivo_log <- paste0("log_caged_", format(Sys.Date(), "%Y_%m"), ".txt")
sink(arquivo_log, append = TRUE)

cat("========================================\n")
cat("Iniciando rotina de atualização CAGED -", as.character(Sys.time()), "\n")

# 3. Executa o Orquestrador (Baixa, Processa e Unifica)
# 'last' garante que ele sempre vá atrás do último mês disponível
tryCatch({
  CAGED(ref = "last", parquet_individual = FALSE)
  cat("Rotina concluída com sucesso!\n")
}, error = function(e) {
  cat("ERRO NA ROTINA:\n", e$message, "\n")
})

cat("========================================\n")
sink()