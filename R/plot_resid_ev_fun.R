#' Plotagem de residuos
#'
#' Esta funcao gera os graficos do residuo % do modelo em função de variaveis externas contidas no dataframe
#'
#' @title plot_resid_ev
#'
#' @param x modelo
#' @param data dataframe contendo as variaveis a serem plotadas em função do residuo
#' @param exp_variables vetor com as variavris que serao utilizadas para construção dos graficos
#'
#' @return uma lista de graficos
#' @examples
#'
#' mol <- lm(mpg ~ hp + cyl, data = mtcars)
#' pn <- plot_resid_ev(mol, data = mtcars, exp_variables = "gear")
#' pn
#'
#' @export
plot_resid_ev <- function(x, ...){
  UseMethod("plot_resid_ev")
}

#' @rdname plot_resid_ev
#' @method plot_resid_ev lm
#' @export
plot_resid_ev.lm <- function(x, data = NULL, exp_variables = NULL, ncol = NULL, nrow = NULL, title = NULL) {


  #create a df
  if( !(is.null(data)) ){
    if(nrow(data) == nrow(ggplot2::fortify(x)) ){
      dfplot <- cbind(ggplot2::fortify(x), data[,(names(data) %in% names(ggplot2::fortify(x))) == F])
    } else {
      dfplot <- ggplot2::fortify(x)
    }
  } else {
    dfplot <- ggplot2::fortify(x)
  }

  dv <- toString(x$call[[2]][2])

  if(grepl("^log", dv)){
    dfplot$.residP <- ((exp(dfplot[[dv]]) - exp(dfplot$.fitted)) *100) / exp(dfplot[[dv]])
  }else{
    dfplot$.residP <- (dfplot$.resid*100)/ dfplot[[dv]]
  }




  #list variables
  if (!is.null(exp_variables) & all( exp_variables %in% names(dfplot))) {
    lstVI <- as.list(exp_variables)
  } else {
    lstVI <- as.list(names(dfplot[,(!grepl("^\\.", names(dfplot))) &
                                    (names(dfplot) != toString(x$call[[2]][2])) ]))
  }

  #create residue of each gaph

  #plot_fun
  #funcao para plotagem baseada no fortify
  plot_data_column = function (dfcol, data) {
    ggplot2::ggplot(data,
                    ggplot2::aes_string(x = dfcol, y = ".residP")) +
      ggplot2::geom_point(alpha =0.3) +
      ggplot2::geom_hline(yintercept = 0, size = 0.7) +
      ggplot2::labs(x = dfcol, y = "resid%") +
      ggplot2::ylim(-1*max(abs(data$.residP)),
                    max(abs(data$.residP)))+
      ggplot2::theme_minimal()
  }

  #apply fun
  lstResidI <- lapply(lstVI, plot_data_column, data = dfplot)
  names(lstResidI) <- lstVI

  #resume plot
  resume <- patchwork::wrap_plots(lstResidI, ncol = ncol, nrow = nrow) +
    patchwork::plot_annotation(subtitle = title,
                               caption = paste0("Formula: ",deparse(x$call[[2]])))
  lstResidI[["resume"]] <- resume

  return(lstResidI)
}
