#' Calculo da intensidade amostral
#'
#' Esta função ira calcular a intensidade amostral \cr
#'
#'
#' @param x dataframe contendo as informacoes padronizadas do coletor - output da funcao \code{\link{cad_join()}}
#' @param im vetor com campos identificadores de medicao
#' @param ie vetor com campos identificadores de estrato
#' @param it vetor com campos identificadores de talhao
#'
#' @return um dataframe com as informações de intensidade amostral de cada estrato
#'
#' @examples
#'
#'  ia <- samp_int(x = bd,
#'                im = c("rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"),
#'                ie = c("especie", "matgen", "regime", "classe_idade"),
#'                it = c("rf", "talhao", "ciclo", "rotacao"))
#'
#' @export
#'

#dplyr::distinct
#dplyr::group_by
#dplyr::summarise
#dplyr::ungroup
#dplyr::mutate
#dplyr::across
#tidyselect::all_of

samp_int <- function(x,
                     cad = NULL,
                     im = c("rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"),
                     ie = c("especie", "matgen", "regime", "classe_idade"),
                     it = c("rf", "talhao", "ciclo", "rotacao")){


    #verifica a precenca das variaveis estratificadoras
  if(all(c(ie %in% names(x),im %in% names(x), it %in% names(x)))){



    n_plot <- x %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(ie))) %>%
      dplyr::summarise(n_plot = dplyr::n_distinct(dplyr::across(tidyselect::all_of(im)))) %>%
      dplyr::ungroup()

    area <- x %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(c(it, ie, "area_plt")))) %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(ie))) %>%
      dplyr::summarise(area_plt = sum(area_plt)) %>%
      dplyr::ungroup()

    int_sam <- n_plot %>%
      dplyr::left_join(area) %>%
      dplyr::mutate(n_plot = tidyr::replace_na(n_plot, 0),
                    int_amostral = area_plt/n_plot)


    return(int_sam)

  } else {
    message("A base nao possui as variaveis descritas no estrato ou do agrupamento da populacao")
  }
}

