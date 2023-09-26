#' Estima a altura dado uma diametro de referencia utilizando a polinomial do 5° grau
#'
#' Dada a altura e o DAP esta função ira estimar a altura a um diametro de referencia indicado \cr
#'
#'
#'
#' @param di diametro que a altura sera estimada - cm
#' @param h altura total da arvore - m
#' @param dap diametro a 1,3m - cm
#' @param b0 parametro b0 da equação de afilamento - polinomio do 5°
#' @param b1 parametro b1 da equação de afilamento - polinomio do 5°
#' @param b2 parametro b2 da equação de afilamento - polinomio do 5°
#' @param b3 parametro b3 da equação de afilamento - polinomio do 5°
#' @param b4 parametro b4 da equação de afilamento - polinomio do 5°
#' @param b5 parametro b5 da equação de afilamento - polinomio do 5°
#'
#' @return um valor numerico que indiaca o diametro a determinada altura
#'
#' @examples
#'
#'  dc <- poly2hi(di = 10,
#'                h = 36.6,
#'                dap = 21.58,
#'                b0 = 1.180918512,
#'                b1 = -4.024085656,
#'                b2 = 19.64764832,
#'                b3 = -47.01626045,
#'                b4 = 48.7462987,
#'                b5 = -18.53635173)
#'
#' @export
#'

poly2hi <- function(di,
                    h,
                    dap,
                    b0 = b0,
                    b1 = b1,
                    b2 = b2,
                    b3= b3,
                    b4 = b4,
                    b5 = b5){

  if (dap <= 0 | h <= 0){
    return(0)
  }else{
    fun_opt_hi <- function(hi){
      ((b0+b1*(hi/h)+b2*(hi/h)^2+b3*(hi/h)^3+b4*(hi/h)^4+b5*(hi/h)^5)-(di/dap))^2
    }

    x <- stats::optimise(fun_opt_hi,lower=0, upper=h, tol=0.001)$minimum
    return(x)
  }
}
