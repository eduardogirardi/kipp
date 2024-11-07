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
                    cores = 4) {

  #sumariza sortimento
  bd <- bd %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im, "linha", "arvore", "fuste")))) %>%
    dplyr::mutate(dcom = min(di),
                  hcom = max(hi),
                  htot = dplyr::case_when(cod1 == "Q" | cod2 == "Q" ~ hqbr,
                                          TRUE ~ h),
                  vcom = round(sum(vi , na.rm = T),4)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(hcom = tidyr::replace_na(hcom, 0),
                  dcom = tidyr::replace_na(dcom, 0),
                  htot = tidyr::replace_na(htot, 0))


  #calculo dos volumes
  future::plan(multisession, workers = cores)
  bd <- bd %>%
    dplyr::mutate(vtot = case_when(suprimir ~ 0,
                                    TRUE ~ round(furrr::future_pmap_dbl(list(hb = htoco, hf = htot, h = h, dap = dap, b0 = b0, b1 = b1, b2 = b2, b3 = b3, b4 = b4, b5 = b5), kipp::poly2vol),4)),
                  vpont = vtot - vcom) %>%
    dplyr::mutate(vtot = tidyr::replace_na(vtot, 0),
                  vpont = tidyr::replace_na(vpont, 0))


}
