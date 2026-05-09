# NCAGEDdataR 2.0.0 (Major Refactoring & Performance Update)

Esta versão traz uma reformulação profunda na arquitetura interna do `NCAGEDdataR`. O foco deste *release* foi otimizar o tempo de extração, implementar práticas rigorosas de Engenharia de Dados (DRY) e preparar o pacote para ser o motor invisível de Dashboards Shiny e rotinas autônomas em servidores de produção.

### 🚀 Melhorias de Arquitetura e Performance

* **Otimização Extrema de API:** A função `download_caged()` foi reescrita. A busca no Google Drive agora é direcionada (filtrando previamente o ano e a competência alvo), o que elimina dezenas de requisições desnecessárias, acelera o download e evita bloqueios (*Rate Limits*).
* **Refatoração DRY (Don't Repeat Yourself):** O motor de limpeza da função `processar_caged()` passou por um grande refatoramento. O tratamento dos cabeçalhos das planilhas do MTE foi consolidado na função interna genérica `padronizar_cabecalho()`, reduzindo o tamanho do código, facilitando a manutenção e garantindo padronização absoluta entre as 11 tabelas.
* **Console "Production-Ready" (Modo Silencioso):** O pipeline agora roda de forma limpa e assíncrona. Avisos irrelevantes de coerção de dados (`NAs introduced by coercion`) e *outputs* nativos das funções de leitura foram suprimidos. A comunicação com o usuário agora é gerida 100% pelo pacote `cli`, oferecendo *feedback* visual elegante e focado em resultados.
* **Upgrade no Fluxo de Memória (Shiny Support):** A integração com as pastas temporárias (`tempdir()`) foi automatizada entre as funções. Agora, `CAGED()` baixa os brutos e converte para Parquet de forma fluida sem deixar rastros no disco rígido do servidor, comportamento ideal para hospedagem em nuvem ou instâncias locais.