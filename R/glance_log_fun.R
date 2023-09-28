#' glance log
#'
#' Esta função ainda esta em construcao, por hora acredito que funcione para lm e nlm \cr
#'
#'
#' @param x modelo (lm e nlm)
#'
#'
#' @return um dataframe
#'
#' @examples
#'
#' glance_log(mol)
#'
#' @export
#'
glance_log <- function(x){
  temp <- broom::augment(x) %>%
    mutate(obs = exp(.resid + .fitted),
           fit = exp(.fitted),
           resid = obs - fit,
           residP = (obs - fit)/obs)


  syx <- sqrt(sum(temp$resid**2)/(dim(temp)[[1]]-(1+length(x$coefficients)-1)))
  syxP <- syx/mean(temp$obs)


  df <- broom::glance(x) %>%
    dplyr::mutate(sigma = syx,
                  sigmaP = syxP,
                  .after = sigma)
  return(df)
}
