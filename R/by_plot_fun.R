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

by_log <-  function(bd,
                    im = c("centro", "rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"),
                    ie = c("especie", "matgen", "regime", "classe_idade")) {

  cna <- c("linha", "arvore", "fuste", "cap", "alt", "cod1", "cod2", "codQ", "dap", "g", )

  #gera arquivo ve volumes por sortimento nivel parcela
  unique_values <- unique(unlist(lapply(df$nested_df, function(x) x[[1]])))

  #pivot
  #pivotenado
  temp_sort <- bd %>%
    dplyr::filter(!is.na(name)) %>%
    tidyr::pivot_wider(id_cols = im,
                       names_from = name,
                       values_from = vi,
                       values_fn = function(x) sum(x, na.rm = TRUE))

  #codigos out
  cs <- c("F", "M", "N", "CA", "CR", "Y", "Z", "ZA")

  bd <- bd %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(ip,
                                                       "dt_plt",
                                                       "ano_plt",
                                                       "tipo",
                                                       "area_parc",
                                                       "area_plt",
                                                       "especie",
                                                       "matgen",
                                                       "regime",
                                                       "classe_idade",
                                                       "idade")))) %>%
    dplyr::summarise(dmin = round(min(dap[!cod1 %in% cs &
                                            !cod2 %in% cs &
                                            dap>0]),2),
                     dmed = round(mean(dap[!cod1 %in% cs &
                                             !cod2 %in% cs &
                                             dap>0]),2),
                     dg = round(dplyr::first(dg),2),
                     ddom = round(dplyr::first(ddom),2),
                     dmax = round(max(dap[!cod1 %in% cs &
                                            !cod2 %in% cs &
                                            dap>0]),2),
                     sd = round(sd(dap[!cod1 %in% cs &
                                         !cod2 %in% cs &
                                         dap>0]),2),
                     hmin = round(min(h[!cod1 %in% cs &
                                          !cod2 %in% cs &
                                          dap>0]),2),
                     hmed = round(mean(h[!cod1 %in% cs &
                                           !cod2 %in% cs &
                                           dap>0]),2),
                     hdom = round(first(hdom),2),
                     hmax = round(max(h[!cod1 %in% cs &
                                          !cod2 %in% cs &
                                          dap>0]),2),
                     sh = round(sd(h[!cod1 %in% cs &
                                       !cod2 %in% cs &
                                       dap>0]),2),
                     covas = dplyr::first(covas),
                     arvores = dplyr::first(arvores),
                     fustes = dplyr::first(fustes),
                     area_basal = round(dplyr::first(ab),2),
                     vmi = round(mean(vcom[!cod1 %in% cs &
                                             !cod2 %in% cs]),4),
                     svmi = round(sd(vcom[!cod1 %in% cs &
                                            !cod2 %in% cs]),4),
                     dplyr::across(tidyselect::all_of(c(assortments$name, "vponteira", "vcom", "vtot", "vcom_sc", "vtot_sc")), ~ sum(.[!cod1 %in% cs &
                                                                                                                                         !cod2 %in% cs], na.rm = T)),
                     padrao = dplyr::first(padrao),
                     dplyr::across(tidyselect::matches("c_[A-Z]{1,2}"), ~ round(dplyr::first(.),2)))%>%
    dplyr::ungroup()


  bd_plot <- bd_plot %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("vponteira", assortments$name, "vcom", "vtot", "vcom_sc", "vtot_sc")), ~ round((.x*10000/area_parc),2))) %>%
    dplyr::rename_with(~ stringr::str_replace(.,"^c_",""), tidyselect::matches("c_[A-Z]{1,2}"))


}

