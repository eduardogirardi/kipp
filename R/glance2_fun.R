#' glance 2
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
#' glance2(mol)
#'
#' @export
#'
glance2 <- function(x){
  x <- broom::glance(x)
  x <- x %>%
    dplyr::mutate(sigmaP = sigma / mean(x$residuals + x$fitted.values),
           .after = sigma)
  return(x)
}
