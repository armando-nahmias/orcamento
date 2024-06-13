importar.orcamento <- function() {
  arquivo <- 'rds/orcamento.rds'
  
  origem.dados <- 'dados/execucao.geral.txt'
  colunas <- c("AG", "Ação Governo", "PO", "Plano Orçamentário", "GD", "Grupo Despesa", "ND", "Natureza Despesa Detalhada", "Nota de Empenho", "PI", "Plano Interno", "Favorecido", "Processo", "Despesas Empenhadas", "Despesas a Liquidar", "Despesas Pagas")
  
  consultado <- readr::read_csv2(origem.dados, skip = 7, col_names = colunas, col_types = readr::cols(.default = 'c'), locale = readr::locale(decimal_mark = ',', grouping_mark = '.'))
  
  atualizado <- format(file.info(origem.dados)$mtime - 1, format = '%d/%m/%Y')
  
  dados <- list(atualizado = atualizado, consultado = consultado, comunicado = '')
  
  readr::write_rds(dados, arquivo)
}

