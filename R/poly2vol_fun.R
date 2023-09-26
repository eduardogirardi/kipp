#' Estima o volume total ou de uma seccao da arvore utilizando a polinomial do 5° grau
#'
#' Dada a altura e o DAP esta função ira estimar o volume total ou de uma seccao dada referencia de uma altura base e uma altura final\cr
#'
#'
#'
#' @param hb altura da seccao inferior, quando omitida sera considerada 0 - m
#' @param hf altura da seccao inferior, quando omitida sera considerada altura total - m
#' @param h altura total da arvore - m
#' @param dap diametro a 1,3m - cm
#' @param b0 parametro b0 da equação de afilamento - polinomio do 5°
#' @param b1 parametro b1 da equação de afilamento - polinomio do 5°
#' @param b2 parametro b2 da equação de afilamento - polinomio do 5°
#' @param b3 parametro b3 da equação de afilamento - polinomio do 5°
#' @param b4 parametro b4 da equação de afilamento - polinomio do 5°
#' @param b5 parametro b5 da equação de afilamento - polinomio do 5°
#'
#' @return um valor numerico que indiaca o volume parcial ou total da arvore
#'
#' @examples
#'
#'  dc <- poly2hi(hb = 0.1,
#'                hf = 25.6,
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

poly2vol <- function(hb,
                     hf,
                     h,
                     dap,
                     b0 = b0,
                     b1 = b1,
                     b2 = b2,
                     b3 = b3,
                     b4 = b4,
                     b5 = b5){

  if(missing(hf)){
    hf <- h
  }

  if(missing(hb)){
    vol_b <- 0
  } else {
    vol_b <- (b0^2*hb + b0*b1*hb^2/h + (b1^2+2*b0*b2)*hb^3/(3*h^2) + (b0*b3+b1*b2)*hb^4/(2*h^3) +
                (2*b0*b4+2*b1*b3+b2^2)*hb^5/(5*h^4) + (b0*b5+b1*b4+b2*b3)*hb^6/(3*h^5) +
                (2*b1*b5+2*b2*b4+b3^2)*hb^7/(7*h^6) + (b2*b5+b3*b4)*hb^8/(4*h^7) +
                (2*b3*b5+b4^2)*hb^9/(9*h^8) + b4*b5*hb^10/(5*h^9) + b5^2*hb^11/(11*h^10)) *
      pi*dap^2/40000

  }

  vol_f <- (b0^2*hf + b0*b1*hf^2/h + (b1^2+2*b0*b2)*hf^3/(3*h^2) + (b0*b3+b1*b2)*hf^4/(2*h^3) +
              (2*b0*b4+2*b1*b3+b2^2)*hf^5/(5*h^4) + (b0*b5+b1*b4+b2*b3)*hf^6/(3*h^5) +
              (2*b1*b5+2*b2*b4+b3^2)*hf^7/(7*h^6) + (b2*b5+b3*b4)*hf^8/(4*h^7) +
              (2*b3*b5+b4^2)*hf^9/(9*h^8) + b4*b5*hf^10/(5*h^9) + b5^2*hf^11/(11*h^10)) *
    pi*dap^2/40000

  vol <- vol_f - vol_b

  return(vol)
}
