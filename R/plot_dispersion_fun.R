#' @importFrom ggplot2 '%+replace%'
NULL
#' Plotagem de dispersao
#'
#' Esta funcao gera os graficos do residuo % do modelo em função de variaveis externas contidas no dataframe
#'
#' @title plot_dispersion
#'
#' @param x modelo
#' @param data dataframe contendo as variaveis a serem plotadas em função do residuo
#' @param delog quando em TRUE, reverte a escala logaritimica das variaveis preditoras
#' @param exp_variables vetor com as variavris que serao utilizadas para construção dos graficos
#'
#' @return uma lista de graficos
#' @examples
#'
#' mol <- lm(mpg ~ hp + cyl, data = mtcars)
#' pd <- plot_dispersion(mol, data = mtcars, exp_variables = "hp")
#' pd
#'
#' @export
plot_dispersion <- function(x, ...){
  UseMethod("plot_dispersion")
}

#' @rdname plot_dispersion
#' @method plot_dispersion lm
#' @export
plot_dispersion.lm <- function(x, data = NULL, exp_variables = NULL, delog = F, ncol = NULL, nrow = NULL, title = NULL) {

  #create a df
  if( !(is.null(data)) ){
    if(nrow(data) == nrow(broom::augment(x)) ){
      dfplot <-  broom::augment_columns(x, data)
    } else {
      dfplot <- broom::augment(x)
    }
  } else {
    dfplot <- broom::augment(x)
  }


  if(delog){
    dfplot <- dfplot %>%
      dplyr::mutate(.observed = exp(broom::augment(x)[[1]]),
                    .fitted = exp(.fitted),
                    .resid = .observed - .fitted,
                    .residP = (.resid/.observed)*100)
  }else{
    dfplot <- dfplot %>%
      dplyr::mutate(.observed = .[[1]],
                    .residP = (.resid/.observed)*100)
  }

  #list variables
  if (!is.null(exp_variables) & all( exp_variables %in% names(dfplot))) {
    lstVI <- as.list(exp_variables)
  } else {
    lstVI <- as.list(names(dfplot[,(!grepl("^\\.", names(dfplot))) &
                                    (names(dfplot) != toString(x$call[[2]][2])) ]))
  }



  #plot_fun
  plot_data_column2 = function (dfcol, data, x) {

    cl <- c("observed" = "grey", "fitted" = "black")

    ggplot2::ggplot(data = data) +
      ggplot2::geom_point(ggplot2::aes(x = !!rlang::sym(dfcol), y = .observed, color = "observed")) +
      ggplot2::geom_point(ggplot2::aes(x = !!rlang::sym(dfcol), y = .fitted, color = "fitted")) +
      ggplot2::labs(y = all.vars(x$call)[1], x = dfcol)+
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_minimal() %+replace% ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank(), legend.key.size = ggplot2::unit(3, "mm"))

  }

  #apply fun
  lstdisp <- lapply(lstVI, plot_data_column2, data = dfplot, x = x)
  names(lstdisp) <- lstVI

  #resume plot
  resume <- patchwork::wrap_plots(lstdisp, ncol = ncol, nrow = ncol) +
    patchwork::plot_annotation(subtitle = title,
                               caption = paste0("Formula: ",deparse(x$call[[2]])))
  lstdisp[["resume"]] <- resume

  return(lstdisp)
}

