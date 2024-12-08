#' Gera a base final no nivel tora
#'
#' @description
#' Esta função ira organizar e gerar a base nivel tora \cr
#'
#' @param db dataframe contendo as informacoes padronizadas do coletor - output da funcao \code{\link{poly2vol()}}
#' @param im vetor com campos identificadores de medicao.
#' @param ie vetor com campos identificadores de estrato.
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

names(bd_log)
by_log <-  function(bd,
                    im = c("centro", "rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"),
                    ie = c("especie", "matgen", "regime", "classe_idade")) {


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
                                       "coordX",
                                       "coordY",
                                       union(c("especie", "matgen",  "regime" , "dt_plt", "ano_plt", "idade", "classe_idade"),
                                       ie),
                                       "cod1",
                                       "cod2",
                                       "codQ",
                                       "dap",
                                       "dcom",
                                       "g",
                                       "dg",
                                       "ab",
                                       "ddom",
                                       "hdom",
                                       "h",
                                       "hobs",
                                       "hest",
                                       "hip_sel",
                                       "qbr",
                                       "hqbr",
                                       "dwg",
                                       "hdwg",
                                       "hcom",
                                       "htot",
                                       "htoco",
                                       "name",
                                       "log",
                                       "li",
                                       "h0",
                                       "hi",
                                       "d0",
                                       "di",
                                       "vi",
                                       "vcom",
                                       "vtot",
                                       "vpont")),
                                     dplyr::matches("^b[[:digit:]]{1}$"))


  #arredondamentos
  bd <- bd %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::matches("^h|^d|li|g|ab") & dplyr::where(is.numeric),
                                .fns = ~ round(., 2))) %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::matches("vi|vcom|vtot|vpont") & dplyr::where(is.numeric),
                                .fns = ~ round(., 4)))

  return(bd)

}
