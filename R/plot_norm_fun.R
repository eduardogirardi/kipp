#' @importFrom ggplot2 '%+replace%'
NULL
#' Plotagem da normalidade
#'
#' Esta funcao gera graficos de normalidade dos resisuos do seu modelo
#'
#' @title plot_norm
#'
#' @param x modelo
#'
#' @return uma lista de graficos
#' @examples
#'
#' mol <- lm(mpg ~ hp + cyl, data = mtcars)
#' pn <- plot_norm(mol)
#' pn
#'
#' @export
plot_norm <- function(x, ...){
  UseMethod("plot_norm")
}

#' @rdname plot_norm
#' @method plot_norm lm
#' @export
plot_norm.lm <- function(x, ncol = NULL, nrow = NULL, title = NULL) {

  #gera lista
  lstNormP <- list()

  #quantil-quantil plot
  Pqqplot <- ggplot2::ggplot(x) +
    ggplot2::stat_qq(ggplot2::aes(sample = .stdresid)) +
    ggplot2::geom_abline() +
    ggplot2::theme_minimal()

  lstNormP[["qqplot"]] <- Pqqplot

  #normal curve plot
  cl <- c("normal" = "#3E5496", "sample" = "black")

  Pdens <- ggplot2::ggplot(x, ggplot2::aes(.resid)) +
    ggplot2::geom_density(ggplot2::aes(y=..density.., color="sample"), size =0.71) +
    ggplot2::stat_function(fun = dnorm,
                  args = list(mean = mean(ggplot2::fortify(x)$.resid), sd = sd(ggplot2::fortify(x)$.resid)),
                  ggplot2::aes(color = "normal"), size =0.71) +
    ggplot2::scale_color_manual(values = cl) +
    ggplot2::labs(color = "", x = "resid") +
    ggplot2::theme_minimal() %+replace% ggplot2::theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.key.size = ggplot2::unit(3, "mm"))

  lstNormP[["density"]] <- Pdens

  #resume
  Presume <- patchwork::wrap_plots(lstNormP, ncol = ncol, nrow = nrow) +
    patchwork::plot_annotation(subtitle = title,
                               caption = paste0("Formula: ",deparse(x$call[[2]])))

  lstNormP[["resume"]] <- Presume

  return(lstNormP)
}
