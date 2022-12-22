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

  #create a dtplot
  if( !(is.null(data)) ){
    if( nrow(data) == nrow(ggplot2::fortify(x)) ){
      dfplot <- cbind(ggplot2::fortify(x), data[,(names(data) %in% names(ggplot2::fortify(x))) == F])
    } else {
      dfplot <- ggplot2::fortify(x)
    }
  } else {
    dfplot <- ggplot2::fortify(x)
  }

  dfplot$.residP <- (dfplot$.resid*100)/ dfplot[[toString(x$call[[2]][2])]]

  #list variables
  if (!is.null(exp_variables) & all( exp_variables %in% names(dfplot))) {
    lstVI <- as.list(exp_variables)
  } else {
    lstVI <- as.list(names(dfplot[,(!grepl("^\\.", names(dfplot))) &
                                    (names(dfplot) != toString(x$call[[2]][2])) ]))
  }
  #lstVI <- as.list(names(data))


  #plot_fun
  plot_data_column2 = function (data, dfcol, delog, x) {
    cl <- c("observed" = "grey", "fitted" = "black")
    if(delog == T){
      ggplot2::ggplot(data = data) +
        ggplot2::geom_point(ggplot2::aes(x = !!rlang::sym(dfcol), y = exp(ggplot2::fortify(x)[,1]), color = "observed")) +
        ggplot2::geom_point(ggplot2::aes(x = !!rlang::sym(dfcol), y = exp(.fitted), color = "fitted")) +
        ggplot2::labs(y = all.vars(x$call)[1], x = dfcol)+
        ggplot2::scale_color_manual(values = cl) +
        ggplot2::theme_minimal() %+replace% ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank(), legend.key.size = ggplot2::unit(3, "mm"))
    } else{
      ggplot2::ggplot(data = data) +
        ggplot2::geom_point(ggplot2::aes(x = !!rlang::sym(dfcol), y = (ggplot2::fortify(x)[,1]), color = "observed")) +
        ggplot2::geom_point(ggplot2::aes(x = !!rlang::sym(dfcol), y = (.fitted), color = "fitted")) +
        ggplot2::labs(y = toString(x$call[[2]][2]), x = dfcol)+
        ggplot2::scale_color_manual(values = cl) +
        ggplot2::theme_minimal() %+replace% ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank(), legend.key.size = ggplot2::unit(3, "mm"))
    }
  }

  #apply fun
  lstdisp <- lapply(lstVI, plot_data_column2, data = dfplot, delog = delog, x = x)
  names(lstdisp) <- lstVI

  #resume plot
  resume <- patchwork::wrap_plots(lstdisp, ncol = ncol, nrow = ncol) +
    patchwork::plot_annotation(subtitle = title,
                               caption = paste0("Formula: ",deparse(x$call[[2]])))
  lstdisp[["resume"]] <- resume

  return(lstdisp)
}

