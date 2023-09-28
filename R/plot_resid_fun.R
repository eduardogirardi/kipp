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

  if (delog == F){
    #resid x fitted
    Presid_fit <- ggplot2::ggplot(x, ggplot2::aes(x = .fitted, y = ((.resid*100)/ggplot2::fortify(x)[,1]))) +
      ggplot2::geom_point(alpha = 0.2) +
      ggplot2::geom_hline(yintercept = 0, size = 0.7) +
      ggplot2::labs(x = "fitted", y = "resid%") +
      ggplot2::ylim(-1*max(abs((ggplot2::fortify(x)[,".resid"]*100)/ggplot2::fortify(x)[,1])),
                    max(abs((ggplot2::fortify(x)[,".resid"]*100)/ggplot2::fortify(x)[,1])))+
      ggplot2::theme_minimal()
    lstResidP[["Presid_fit"]] <- Presid_fit

    #resid x observed
    Presid_obs <- ggplot2::ggplot(x, ggplot2::aes(x = ggplot2::fortify(x)[,1], y = ((.resid*100)/ggplot2::fortify(x)[,1]))) +
      ggplot2::geom_point(alpha = 0.2) +
      ggplot2::geom_hline(yintercept = 0, size = 0.7) +
      ggplot2::labs(x = "observed", y = "resid%") +
      ggplot2::ylim(-1*max(abs((ggplot2::fortify(x)[,".resid"]*100)/ggplot2::fortify(x)[,1])),
                    max(abs((ggplot2::fortify(x)[,".resid"]*100)/ggplot2::fortify(x)[,1]))) +
      ggplot2::theme_minimal()

    lstResidP[["Presid_obs"]] <- Presid_obs

    #stard resid
    Sresid_fit <- ggplot2::ggplot(x, ggplot2::aes(x = .fitted, y = .stdresid)) +
      ggplot2::geom_point(alpha = 0.2) +
      ggplot2::geom_hline(yintercept = 0, size = 0.7) +
      ggplot2::geom_hline(yintercept = c(-2,2), size = 0.7, linetype = "dotdash") +
      ggplot2::labs(x = "fitted", y = "std resid") +
      ggplot2::ylim(-1*max(abs(ggplot2::fortify(x)[,".stdresid"])),
                    max(abs(ggplot2::fortify(x)[,".stdresid"]))) +
      ggplot2::theme_minimal()

    lstResidP[["Sresid_fit"]] <- Sresid_fit

    #obs x fitted
    obs_fit <- ggplot2::ggplot(x, ggplot2::aes(x = .fitted, y = ggplot2::fortify(x)[,1])) +
      ggplot2::geom_point(alpha = 0.2) +
      ggplot2::geom_abline(intercept = 0, slope = 1, size = 0.7) +
      ggplot2::labs(x = "fitted", y = "observed") +
      ggplot2::lims(x = c(min(ggplot2::fortify(x)[,1],ggplot2::fortify(x)[,".fitted"]),
                          max(ggplot2::fortify(x)[,1],ggplot2::fortify(x)[,".fitted"])),
                    y = c(min(ggplot2::fortify(x)[,1],ggplot2::fortify(x)[,".fitted"]),
                          max(ggplot2::fortify(x)[,1],ggplot2::fortify(x)[,".fitted"]))) +
      ggplot2::theme_minimal()

    lstResidP[["obs_fit"]] <- obs_fit

  } else {
    #resid x fitted
    Presid_fit <- ggplot2::ggplot(x, ggplot2::aes(x = exp(.fitted), y = ((.resid*100)/ggplot2::fortify(x)[,1]))) +
      ggplot2::geom_point(alpha = 0.2) +
      ggplot2::geom_hline(yintercept = 0, size = 0.7) +
      ggplot2::labs(x = "fitted", y = "resid%") +
      ggplot2::ylim(-1*max(abs((ggplot2::fortify(x)[,".resid"]*100)/ggplot2::fortify(x)[,1])),
                    max(abs((ggplot2::fortify(x)[,".resid"]*100)/ggplot2::fortify(x)[,1])))+
      ggplot2::theme_minimal()
    lstResidP[["Presid_fit"]] <- Presid_fit

    #resid x observed
    Presid_obs <- ggplot2::ggplot(x, ggplot2::aes(x = exp(ggplot2::fortify(x)[,1]), y = ((.resid*100)/ggplot2::fortify(x)[,1]))) +
      ggplot2::geom_point(alpha = 0.2) +
      ggplot2::geom_hline(yintercept = 0, size = 0.7) +
      ggplot2::labs(x = "observed", y = "resid%") +
      ggplot2::ylim(-1*max(abs((ggplot2::fortify(x)[,".resid"]*100)/ggplot2::fortify(x)[,1])),
                    max(abs((ggplot2::fortify(x)[,".resid"]*100)/ggplot2::fortify(x)[,1]))) +
      ggplot2::theme_minimal()

    lstResidP[["Presid_obs"]] <- Presid_obs

    #stard resid
    Sresid_fit <- ggplot2::ggplot(x, ggplot2::aes(x = exp(.fitted), y = .stdresid)) +
      ggplot2::geom_point(alpha = 0.2) +
      ggplot2::geom_hline(yintercept = 0, size = 0.7) +
      ggplot2::geom_hline(yintercept = c(-2,2), size = 0.7, linetype = "dotdash") +
      ggplot2::labs(x = "fitted", y = "std resid") +
      ggplot2::ylim(-1*max(abs(ggplot2::fortify(x)[,".stdresid"])),
                    max(abs(ggplot2::fortify(x)[,".stdresid"]))) +
      ggplot2::theme_minimal()
    lstResidP[["Sresid_fit"]] <- Sresid_fit

    #obs x fitted
    obs_fit <- ggplot2::ggplot(x, ggplot2::aes(x = exp(.fitted), y = exp(ggplot2::fortify(x)[,1]))) +
      ggplot2::geom_point(alpha = 0.2) +
      ggplot2::geom_abline(intercept = 0, slope = 1, size = 0.7) +
      ggplot2::labs(x = "fitted", y = "observed") +
      ggplot2::lims(x = c(min(exp(ggplot2::fortify(x)[,1]),exp(ggplot2::fortify(x)[,".fitted"])),
                          max(exp(ggplot2::fortify(x)[,1]),exp(ggplot2::fortify(x)[,".fitted"]))),
                    y = c(min(exp(ggplot2::fortify(x)[,1]),exp(ggplot2::fortify(x)[,".fitted"])),
                          max(exp(ggplot2::fortify(x)[,1]),exp(ggplot2::fortify(x)[,".fitted"])))) +
      ggplot2::theme_minimal()
    lstResidP[["obs_fit"]] <- obs_fit

  }

  Presume <- patchwork::wrap_plots(lstResidP,ncol = ncol, nrow = nrow) +
    patchwork::plot_annotation(subtitle = title,
                               caption = paste0("Formula: ",deparse(x$call[[2]])))
  lstResidP[["resume"]] <- Presume

  return(lstResidP)
}
