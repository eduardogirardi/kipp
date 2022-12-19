#' Calculo da intensidade amostral
#'
#' Esta função ira calcular a intensidade amostral \cr
#'
#' O agrupamento da populacao sera definido pelas variaveis c("rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med")\cr
#'
#' @param x dataframe contendo as informacoes padronizadas do coletor - output da funcao \code{\link{cad_join()}}
#' @param cad  dataframe contendo as informacoes padronizadas do coletor \code{\link{read_cad()}}
#' @param var_est vetor contendo as variaveis que compoem o estrato
#'
#' @return um dataframe com as informações de intensidade amostral de cada estrato
#'
#' @examples
#'
#'  ia <- samp_int(x = bd, cad = NULL , var_est = c("especie", "matgen", "regime", "classe_idade"))
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
#
samp_int <- function(x, cad = NULL, var_est = c("especie", "matgen", "regime", "classe_idade")){

  #definindo variaveis de agrupamento da populacao
  im <- c("rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med")

  #verifica a presenca do parametro CAD
  if (is.null(cad)) {
    #verifica a precenca das variaveis estratificadoras
    if(all(c(var_est %in% names(x),im %in% names(x)))){

      int_sam <- x %>%
        dplyr::distinct(dplyr::across(tidyselect::all_of(c(var_est, im, "area_plt")))) %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(var_est))) %>%
        dplyr::summarise(n_plot = dplyr::n_distinct(dplyr::across(tidyselect::all_of(im))),
                         area_plt = sum(area_plt)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(int_amostral = area_plt/n_plot)

      return(int_sam)

    } else {
      message("A base nao possui as variaveis descritas no estrato ou do agrupamento da populacao")
    }

    #sera realizado o calculo apartir de um cadastro
  }else{

    if(all(c(var_est %in% names(x), im %in% names(x), var_est %in% names(cad)))){

      nplot <- x %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(var_est))) %>%
        dplyr::summarise(n_plot = dplyr::n_distinct(dplyr::across(tidyselect::all_of(im)))) %>%
        dplyr::ungroup()

      #calcula a area de cada estrato "var_est"
      int_sam <- cad %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(var_est))) %>%
        dplyr::summarise(area_plt = sum(area_plt)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(nplot) %>%
        dplyr::mutate(n_plot = tidyr::replace_na(n_plot, 0),
               int_amostral = area_plt/n_plot)

      return(int_sam)

    } else{
      message("A base ou cadastro nao possui as variaveis descritas no estrato ou do agrupamento da populacao")
    }
  }


}
