#' Uni a base de dados do coletetor com o cadastro florestal
#'
#' Esta função unir a bade dados do coletor com a o cadastro florestal referente utilizando os campos descritos no parametro **by** \cr
#'
#'
#' @param dc dataframe contendo as informacoes padronizadas do coletor - output da funcao \code{\link{read_dc()}}
#' @param cad dataframe contendo as informacoes padronizadas do coletor - output da funcao \code{\link{read_cad()}}
#' @param by vetor de variaveis usadas para para unir as bases de dados
#'
#'
#' @return um dataframe de mesma estrutura com novas variaveis unidas
#'
#' @examples
#'
#'  dc <- cad_join(dc, cad)
#'
#' @export

#dplyr::distinct
#dplyr::across
#dplyr::all_of
#dplyr::left_join

cad_join <- function(dc, cad, by = c("rf", "talhao", "ciclo", "rotacao"), ...){

  #n linhas do cad1
  nlinhas3 <- nrow(cad)

  #remove dados duplicados no cadastro
  cad <- cad %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(by)), .keep_all = T)

  #n linhas do cad2
  nlinhas4 <- nrow(cad)

  if (nlinhas3 != nlinhas4){
    message("PROBLEMA com o cadastro. VERIFIQUE duplicidade de talhoes no cadastro florestal")
  }

  #n linhas do dc
  nlinhas1 <- nrow(dc)

  #junta dados de campo com o cadastro
  dt <- dc %>%
    dplyr::inner_join(cad, by = by, ...)

  nlinhas2 <- nrow(dt)

  if (nlinhas1 != nlinhas2){
    message("PROBLEMA com o cadastro. VERIFIQUE ausencia de talhoes no cadastro florestal")
  }

  return(dt)
}
