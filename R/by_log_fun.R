#' Gera a base final no nivel tora
#'
#' @description
#' Esta função ira organizar e gerar a base nivel tora \cr
#'
#' @param db dataframe contendo as informacoes padronizadas do coletor - output da funcao \code{\link{poly2vol()}}
#' @param im vetor com campos identificadores de medicao.
#' @param cores n° de nucleos de processamento. \strong{Padrão sao 4}
#' \cr
#'
#' @details
#' \strong{Registros que nao possuirem correspondencia serao removidos da base de dados} \cr
#'
#'
#' @return um dataframe de mesma estrutura de \strong{bd}
#'
#' @examples
#'
#'  db <- by_log(db, im, 4)
#'
#' @export
#'


by_log <-  function(bd,
                    im = c("centro", "rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"),
                    ie = c(),
                    cores = 4) {


  #organizando DF
  bd <- bd %>%
    dplyr::select(tidyselect::all_of(c("atividade",
                                       union(c("centro", "rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"),
                                       im),
                                       "linha",
                                       "arvore",
                                       "fuste",
                                       "tipo",
                                       "forma",
                                       "lado1",
                                       "lado2",
                                       "inc1",
                                       "inc2",
                                       "area_parc",
                                       union(c("especie", "matgen",  "regime" , "dt_plt", "ano_plt", "idade", "classe_idade"),
                                       ie),
                                       "cod1",
                                       "cod2",
                                       "codQ",
                                       "dap",
                                       "h")))


}
