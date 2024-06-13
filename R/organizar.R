organizar.orcamento <- function() {
  arquivo <- 'rds/orcamento.rds'
  dados <- readr::read_rds(arquivo)
  
  atualizado <- dados |> purrr::pluck('atualizado')
  consultado <- dados |> purrr::pluck('consultado')
  
  organizado <- consultado |> 
    tidyr::unite('Ação Governo', AG, `Ação Governo`, sep = ' - ', remove = TRUE) |> 
    tidyr::unite('Plano Orçamentário', PO, `Plano Orçamentário`, sep = ' - ', remove = TRUE) |> 
    tidyr::unite('Grupo Despesa', GD, `Grupo Despesa`, sep = ' - ', remove = TRUE) |> 
    tidyr::unite('Natureza Despesa Detalhada', ND, `Natureza Despesa Detalhada`, sep = ' - ', remove = TRUE) |> 
    tidyr::unite('Plano Interno', PI, `Plano Interno`, sep = ' - ', remove = TRUE) |> 
    dplyr::filter(`Ação Governo` != 'Total - NA') |> 
    dplyr::mutate(
      dplyr::across(dplyr::starts_with('Despesas'), ~ as.numeric(gsub(',', '.', gsub('\\.', '', .)))),
      `Despesas Pagas` = tidyr::replace_na(`Despesas Pagas`, 0),
      `Nota de Empenho` = stringr::str_match(`Nota de Empenho`, '.*(\\d{4}NE\\d{6})')[, 2]
    )
  
  totais <- organizado |> 
    dplyr::summarise(dplyr::across(dplyr::starts_with('Despesas'), \(x) sum(x, na.rm = TRUE)))
  linha.totais <- data.frame(c(rep('Total', 8), totais))
  colnames(linha.totais) <- colnames(organizado)
  organizado <- organizado |> dplyr::add_row(linha.totais)
  
  dados$organizado <- organizado
  
  readr::write_rds(dados, arquivo)
  
  return(organizado)
  
}
