# Arquivo: tests/testthat/test-CAGED.R

library(testthat)

# ==========================================
# 1. TESTES: download_caged()
# ==========================================
test_that("download_caged valida o argumento 'ref' corretamente", {
  
  # Bloqueia formatos inválidos (deve estourar o erro exato)
  expect_error(
    download_caged(ref = "2026_MARCO"), 
    "Use: YYYY, YYYYMM ou 'last'"
  )
  
  expect_error(
    download_caged(ref = "MAR/2026"), 
    "Use: YYYY, YYYYMM ou 'last'"
  )
})

# ==========================================
# 2. TESTES: processar_caged() - Borda e Erros
# ==========================================
test_that("processar_caged reage corretamente a uma pasta vazia", {
  
  # Cria um diretório temporário vazio simulando falha no download
  dir_vazio <- tempfile("teste_vazio_")
  dir.create(dir_vazio)
  
  # A função deve silenciar o output e retornar NULL para não quebrar o pipeline
  resultado <- expect_invisible(
    processar_caged(usar_temporario = FALSE, origem = dir_vazio)
  )
  
  expect_null(resultado)
  
  # Limpa o ambiente
  unlink(dir_vazio, recursive = TRUE)
})

# ==========================================
# 3. TESTES: processar_caged() - Motor Analítico com Mock
# ==========================================
test_that("processar_caged executa o ETL matematicamente no arquivo miniatura", {
  
  # Aponta para a pasta extdata dentro do seu pacote (onde está o mock)
  pasta_mock <- system.file("extdata", package = "NCAGEDdataR")
  
  # Trava de segurança: Pula o teste se esquecer de colocar o mock na pasta
  skip_if(pasta_mock == "", message = "Pasta extdata não encontrada. Verifique se o caged_miniatura.xlsx está lá.")
  
  # Cria uma pasta temporária isolada apenas para os parquets do teste
  pasta_destino_teste <- tempfile("parquet_teste_")
  
  # Executa a limpeza e unificação
  resultado <- expect_invisible(
    processar_caged(
      usar_temporario = FALSE, 
      origem = pasta_mock,
      destino = pasta_destino_teste,
      parquet_individual = FALSE
    )
  )
  
  # VALIDAÇÕES DE INTEGRIDADE:
  # 1. O retorno tem que ser uma lista estruturada
  expect_type(resultado, "list")
  
  # 2. A lista tem que conter tabelas (não pode estar vazia)
  expect_true(length(resultado) > 0)
  
  # 3. Verifica se o R salvou o arquivo .parquet consolidado fisicamente no disco
  arquivos_gerados <- list.files(pasta_destino_teste, pattern = "parquet$")
  expect_true(length(arquivos_gerados) == 1)
  expect_true(grepl("CONSOLIDADO", arquivos_gerados[1]))
  
  # Limpa a lixeira do teste
  unlink(pasta_destino_teste, recursive = TRUE)
})

# ==========================================
# 4. TESTES: CAGED() - Orquestrador
# ==========================================
test_that("CAGED bloqueia tentativa de ler arquivo individual sem passar o alvo", {
  
  # O orquestrador deve gerar um alerta visual e retornar NULL
  resultado <- expect_invisible(
    CAGED(parquet_individual = TRUE, arquivo_alvo = NULL)
  )
  
  expect_null(resultado)
})