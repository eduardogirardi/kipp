#' Plotagem de residuos
#'
#' Esta funcao gera 3 grupos de graficos: residuos %'s do modelo em funcao do ajustado, observado; do residuo padronizado em funcao do ajustado; observado em funcao do ajustado
#'
#' @title plot_resid
#'
#' @param x modelo
#' @param delog quando em TRUE, reverte a escala logaritimica das variaveis preditoras
#'
#' @return uma lista de graficos
#' @examples
#'
#' mol <- lm(mpg ~ hp + cyl, data = mtcars)
#' pr <- plot_resid(mol, delog = F)
#' pr
#'
#' @export
#'
#'
#'
plot_resid <- function(x, ...){
  UseMethod("plot_resid")
}

#' @rdname plot_resid
#' @method plot_resid lm
#' @export
#'
#'
#'
plot_resid.lm <- function(x, delog = F, ncol = NULL, nrow = NULL, title = NULL) {

  lstResidP <- list()

  if (delog == T){

    dp <- broom::augment(x)
    dp <- dp %>%
      dplyr::mutate(.observed = exp(.[[1]]),
                    .fitted = exp(.fitted),
                    .resid = .observed - .fitted,
                    .residP = (.resid/.observed)*100)

  } else {

    dp <- broom::augment(x)
    dp <- dp %>%
      dplyr::mutate(.observed = .[[1]],
                    .residP = (.resid/.observed)*100)

  }

  #resid x fitted
  limits <- ifelse(ceiling(max(abs(dp$.residP))) > 100, 100, ceiling(max(abs(dp$.residP))))
  Presid_fit <- ggplot2::ggplot(dp, ggplot2::aes(x = .fitted, y = .residP)) +
    ggplot2::geom_point(alpha = 0.2) +
    ggplot2::geom_hline(yintercept = 0, size = 0.7) +
    ggplot2::labs(x = "fitted", y = "resid%") +
    ggplot2::ylim(-1*limits,
                  limits) +
    ggplot2::theme_minimal()
  lstResidP[["Presid_fit"]] <- Presid_fit

  #resid x observed
  Presid_obs <- ggplot2::ggplot(dp, ggplot2::aes(x = .observed, y = .residP)) +
    ggplot2::geom_point(alpha = 0.2) +
    ggplot2::geom_hline(yintercept = 0, size = 0.7) +
    ggplot2::labs(x = "observed", y = "resid%") +
    ggplot2::ylim(-1*limits,
                  limits) +
    ggplot2::theme_minimal()

  lstResidP[["Presid_obs"]] <- Presid_obs

  #stard resid
  limits <- max(abs(dp$.std.resid))
  Sresid_fit <- ggplot2::ggplot(dp, ggplot2::aes(x = .observed, y = .std.resid)) +
    ggplot2::geom_point(alpha = 0.2) +
    ggplot2::geom_hline(yintercept = 0, size = 0.7) +
    ggplot2::geom_hline(yintercept = c(-2,2), size = 0.7, linetype = "dotdash") +
    ggplot2::labs(x = "fitted", y = "std resid") +
    ggplot2::ylim(-1*limits,
                  limits) +
    ggplot2::theme_minimal()
  lstResidP[["Sresid_fit"]] <- Sresid_fit

  #obs x fitted
  limits_min <- min(dp$.observed, dp$.fitted)
  limits_max <- max(dp$.observed, dp$.fitted)
  obs_fit <- ggplot2::ggplot(dp, ggplot2::aes(x = .fitted, y = .observed)) +
    ggplot2::geom_point(alpha = 0.2) +
    ggplot2::geom_abline(intercept = 0, slope = 1, size = 0.7) +
    ggplot2::labs(x = "fitted", y = "observed") +
    ggplot2::lims(x = c(limits_min,
                        limits_max),
                  y = c(limits_min,
                        limits_max)) +
    ggplot2::theme_minimal()

  lstResidP[["obs_fit"]] <- obs_fit


  Presume <- patchwork::wrap_plots(lstResidP,ncol = ncol, nrow = nrow) +
    patchwork::plot_annotation(subtitle = title,
                               caption = paste0("Formula: ",deparse(x$call[[2]])))
  lstResidP[["resume"]] <- Presume

  return(lstResidP)
}
