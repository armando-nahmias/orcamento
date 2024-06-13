formatar.excel.orcamento <- function() {
  arquivo <- 'rds/orcamento.rds'
  dados <- readr::read_rds(arquivo)
  
  atualizado <- dados |> purrr::pluck('atualizado')
  orcamento <- dados |> purrr::pluck('organizado')
  
  colunas <- c('Ação Governo', 'Plano Orçamentário', 'Grupo Despesa', 
               'Natureza Despesa Detalhada', 'Nota de Empenho', 'Plano Interno', 
               'Favorecido', 'Processo', 'Despesas Empenhadas', 'Despesas a Liquidar', 
               'Despesas Pagas')
  titulo <- 'Execução Orçamentária e Financeira Detalhada'
  data.relatorio <- as.character(epoxy::epoxy('Atualizado até {atualizado}.'))
  wb <- openxlsx::createWorkbook()
  aba <- 'Execução Orçamentária'
  
  # Função para mesclar e centralizar células repetidas
  mesclar.centralizar <- function(coluna, inicio, valores) {
    estilo <- openxlsx::createStyle(halign = 'center', valign = 'center')
    for (linha in 6:(length(valores) + 4)) {
      if (valores[linha - 4] != valores[linha - 5]) {
        if (linha - inicio > 1) {
          openxlsx::mergeCells(wb, sheet = aba, cols = coluna, rows = inicio:(linha - 1))
          openxlsx::addStyle(wb, sheet = aba, style = estilo, cols = coluna, rows = inicio:(linha - 1), gridExpand = TRUE)
        }
        inicio <- linha
      }
    }
    if (length(valores) + 5 - inicio > 1) {
      openxlsx::mergeCells(wb, sheet = aba, cols = coluna, rows = inicio:(length(valores) + 4))
      openxlsx::addStyle(wb, sheet = aba, style = estilo, rows = inicio:(length(valores) + 4), cols = coluna, gridExpand = TRUE)
    }
  }
  
  # Função para ajustar largura das colunas
  ajustar.largura.colunas <- function(colunas, limite.largura) {
    for (coluna in seq_along(colunas)) {
      if (startsWith(colunas[coluna], 'Despesas')) {
        largura <- limite.largura / 2
      } else {
        max.largura <- max(nchar(as.character(orcamento[[coluna]])), na.rm = TRUE)
        largura <- min(max.largura + 2, limite.largura)
      }
      openxlsx::setColWidths(wb, sheet = aba, cols = coluna, widths = largura)
    }
  }
  
  # Função para aplicar estilos às colunas
  aplicar.estilos.colunas <- function(colunas, estilo.texto, estilo.moeda) {
    for (coluna in seq_along(colunas)) {
      if (startsWith(colunas[coluna], 'Despesas')) {
        openxlsx::addStyle(wb, sheet = aba, style = estilo.moeda, rows = 5:(nrow(orcamento) + 4), cols = coluna, gridExpand = TRUE)
      } else {
        openxlsx::addStyle(wb, sheet = aba, style = estilo.texto, rows = 5:(nrow(orcamento) + 4), cols = coluna, gridExpand = TRUE)
      }
    }
  }
  
  openxlsx::addWorksheet(wb, sheetName = aba)
  openxlsx::writeData(wb, sheet = aba, x = titulo, startRow = 1, startCol = 1)
  openxlsx::writeData(wb, sheet = aba, x = data.relatorio, startRow = 3, startCol = 1)
  openxlsx::writeData(wb, sheet = aba, x = as.data.frame(t(colunas)), startRow = 4, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet = aba, x = orcamento, startRow = 5, startCol = 1, colNames = FALSE)
  
  for (coluna in 1:3) {
    valores <- orcamento[[coluna]]
    mesclar.centralizar(coluna, 5, valores)
  }
  
  openxlsx::mergeCells(wb, sheet = aba, cols = 1:8, rows = (nrow(orcamento) + 4))
  openxlsx::writeData(wb, sheet = aba, x = 'Totais', startRow = (nrow(orcamento) + 4), startCol = 1)
  
  limite.largura <- 50
  ajustar.largura.colunas(colunas, limite.largura)
  
  estilo.cabecalho <- openxlsx::createStyle(fgFill = 'gray', halign = 'center', textDecoration = 'bold', valign = 'center')
  openxlsx::addStyle(wb, sheet = aba, style = estilo.cabecalho, rows = 4, cols = 1:length(colunas), gridExpand = TRUE)
  
  estilo.texto <- openxlsx::createStyle(fgFill = 'blue', fontColour = 'white', valign = 'center')
  estilo.moeda <- openxlsx::createStyle(fgFill = 'lightblue', halign = 'right', numFmt = 'R$ #,##0.00', valign = 'center')
  aplicar.estilos.colunas(colunas, estilo.texto, estilo.moeda)
  
  openxlsx::setRowHeights(wb, sheet = aba, rows = c(1, 3, 4, (nrow(orcamento) + 4)), heights = 50)
  openxlsx::setRowHeights(wb, sheet = aba, rows = 5:(nrow(orcamento) + 3), heights = 20)
  
  vertical <- openxlsx::createStyle(valign = 'center')
  openxlsx::addStyle(wb, sheet = aba, style = vertical, rows = 1:(nrow(orcamento) + 4), cols = 1:length(colunas), gridExpand = TRUE, stack = TRUE)
  
  openxlsx::saveWorkbook(wb, 'saida/Execucao_Orcamentaria.xlsx', overwrite = TRUE)
  
  return()
}
