<!-- README.md is generated from README.Rmd. Please edit that file -->

# NCAGEDdataR

<img align="right" src="man/figures/NCAGEDdataR_logo1.png" alt="logo" width="180"><img align="right" src="man/figures/NCAGEDdataR_logo2.png" alt="logo" width="180">

O pacote **`NCAGEDdataR`** foi desenvolvido para oferecer um pipeline de alta performance na extração e tratamento dos dados do **Novo CAGED** (Cadastro Geral de Empregados e Desempregados). 

Projetado originalmente para suprir as demandas rigorosas de análise econômica e inteligência fiscal, o pacote automatiza o ciclo de vida do dado: desde a captura via API do Google Drive até a persistência otimizada em formato **Apache Parquet**.

<br />

<!-- badges: start -->
![GitHub R package version (subdirectory of monorepo)](https://img.shields.io/github/r-package/v/Natanaelsl/NCAGEDdataR)
![GitHub Repo stars](https://img.shields.io/github/stars/Natanaelsl/NCAGEDdataR?color=orange)
![GitHub](https://img.shields.io/github/license/Natanaelsl/NCAGEDdataR)
<!-- badges: end -->

<br />

---

## 🚀 Vantagens e Melhorias na Arquitetura

A consolidação das rotinas de extração no ecossistema do pacote **`NCAGEDdataR`** traz avanços significativos para a engenharia de dados:

* **Eficiência de API (Zero Desperdício):** A lógica de download investiga apenas o ano e a competência de interesse, reduzindo drasticamente o tempo de conexão e o risco de bloqueios por excesso de requisições na API do Google Drive.
* **Arquitetura DRY (Don't Repeat Yourself):** O motor de limpeza processa de forma padronizada as variações estruturais das planilhas. Regras de cabeçalho complexas utilizam funções internas, garantindo que alterações no layout oficial do Ministério do Trabalho sejam replicadas e tratadas automaticamente em todas as 11 tabelas.
* **Gestão Inteligente de Memória:** Implementação nativa do ecossistema Apache Arrow. O pacote não apenas limpa os dados, mas os entrega em formato colunar, permitindo que o analista consulte milhões de linhas (como a série histórica completa de municípios) sem sobrecarregar a memória RAM.
* **Processamento Silencioso (Produção):** Otimização de todos os outputs do console. Foram suprimidos avisos irrelevantes de coerção de dados (`NAs introduced by coercion`), focando a atenção do usuário apenas nos alertas críticos de sucesso, progresso ou falha do pipeline via pacote `cli`.

---

## 💻 Instalação

Você pode instalar a versão de desenvolvimento do `NCAGEDdataR` direto do [GitHub](https://github.com/) com:

```r
# install.packages("devtools")
devtools::install_github("Natanaelsl/NCAGEDdataR")
```

---

## 📊 Exemplo de Uso Rápido

Este é um exemplo básico demonstrando a solução automatizada do pacote:

```r
## Carregando os pacotes necessários
library(NCAGEDdataR)
library(dplyr)

## 1. Fluxo Automático: Baixa, processa e abre a base consolidada mais recente
base_fiscal <- CAGED()

## 2. Exemplo de uso via Apache Arrow (Lazy Evaluation)
# Filtrando instantaneamente apenas a tabela de Municípios de Goiás
dados_goias <- base_fiscal %>%
  filter(Tabela_Origem == "Tabela 8", UF == "GO") %>%
  collect() # O processamento pesado ocorre apenas no collect()!
```

---

## 📖 Guia de Uso Detalhado

Este guia explica as diferentes formas de interagir com o pacote, desde o modo totalmente automatizado até o controle granular de cada etapa de ETL.

### A Solução "One-Click": `CAGED()`

A função `CAGED()` é a orquestradora principal. Ela foi desenhada para que o analista tenha a informação mais recente em mãos com apenas um comando.

**Como funciona:**
Ao executar `base <- CAGED()`, o pacote:
1. Localiza a última competência publicada no Google Drive.
2. Realiza o download para uma área temporária (preservando o armazenamento local do computador).
3. Processa e padroniza todas as abas (CNAE, UF, Municípios, Salários) em um único dataset consolidado.
4. Persiste a versão final em `.parquet` na pasta especificada.
5. Retorna uma conexão de alta performance (Dataset Arrow) para consulta imediata no R.

### Uso Individual das Funções (Controle Granular)

Para fluxos de trabalho que exigem customização (como atualização de Data Lakes específicos ou integração com ferramentas de BI), o pacote expõe suas funções internas:

#### `download_caged(ref, temp, destino)`
Ideal para ingestão de dados brutos e auditoria externa.
* **Argumento `ref`:** Aceita `"last"` para a última competência, `"2026"` para buscar dados de um ano inteiro, ou `"202603"` para um mês/ano específico.
* **Argumento `temp`:** Se `TRUE`, o arquivo `.xlsx` é baixado para a memória volátil (`tempdir()`) e descartado após o encerramento da sessão, sendo a configuração ideal para servidores hospedando Dashboards Shiny.

#### `processar_caged(usar_temporario, parquet_individual)`
O motor analítico do pacote. Transforma planilhas estáticas em infraestrutura de dados.
* **Modo Consolidado (Padrão):** Unifica as 11 tabelas em um único arquivo `.parquet`, alinhando as colunas. Formato otimizado para filtros rápidos utilizando `dplyr`.
* **Modo Individual (`parquet_individual = TRUE`):** Salva cada aba do Excel como um arquivo Parquet independente. Essencial se o destino final dos dados for uma ferramenta de visualização externa (Power BI, Tableau) que exige modelagem em esquema estrela (Star Schema).

---

## 🧠 Considerações Estratégicas e Técnicas

### A Importância do Formato Parquet na Análise Econômica
Lidar com a granularidade municipal do CAGED em formato Excel ou CSV torna-se rapidamente inviável. O formato colunar Parquet permite que o `NCAGEDdataR` comprima os dados de forma extrema. Em análises de saldo de empregos, o Arrow lerá do disco estritamente as colunas solicitadas, reduzindo o tempo de leitura a milissegundos.

### Automação e Integração com Shiny
O `NCAGEDdataR` foi desenhado com o princípio de "Desacoplamento". Para aplicativos Shiny, a interface web jamais deve acionar o processamento das planilhas originais. Recomenda-se configurar um *cron job* (Linux/Servidor) ou *Task Scheduler* (Windows) para executar o pipeline de extração em horários ociosos (ex: de madrugada), garantindo que o usuário final acesse instantaneamente a versão mais recente dos dados através da conexão Arrow.

### Dica de Alta Performance: Avaliação Preguiçosa (*Lazy Evaluation*)
Ao manipular o objeto gerado pelo pacote, utilize todas as funções do Tidyverse (`filter`, `select`, `mutate`, `group_by`, `summarise`) de forma encadeada. O processamento não sobrecarregará o R. A execução real só ocorrerá quando você chamar a função `collect()` no final do pipeline. Isso assegura que o trabalho pesado seja executado pela engine C++ do Apache Arrow, retornando apenas o resumo executivo para a memória da sua sessão.

---

## ℹ️ Informações e Histórico

### O Novo Caged

Desde janeiro de 2020, o uso do Sistema do Cadastro Geral de Empregados e Desempregados (Caged) foi substituído pelo Sistema de Escrituração Digital das Obrigações Fiscais, Previdenciárias e Trabalhistas (eSocial) para parte das empresas, conforme estabelecido pela Portaria SEPRT nº 1.127, de 14/10/2019. Permanece a obrigatoriedade de envio das informações por meio do Caged apenas para órgãos públicos e organizações internacionais que contratam celetistas.

O **Novo Caged** é a geração das estatísticas do emprego formal por meio de informações captadas dos sistemas eSocial, Caged e Empregador Web.

> **_NOTA:_** O referido pacote encontra-se em constante desenvolvimento ativo. O formato dos dados públicos pode sofrer alterações pontuais pelo MTE, e a arquitetura do pacote é sistematicamente atualizada para refletir o layout oficial e manter a melhor performance analítica possível.