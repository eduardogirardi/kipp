% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poly2hi_fun.R
\name{poly2hi}
\alias{poly2hi}
\title{Estima a altura dado uma diametro de referencia utilizando a polinomial do 5° grau}
\usage{
poly2hi(di, h, dap, b0 = b0, b1 = b1, b2 = b2, b3 = b3, b4 = b4, b5 = b5)
}
\arguments{
\item{di}{diametro que a altura sera estimada - cm}

\item{h}{altura total da arvore - m}

\item{dap}{diametro a 1,3m - cm}

\item{b0}{parametro b0 da equação de afilamento - polinomio do 5°}

\item{b1}{parametro b1 da equação de afilamento - polinomio do 5°}

\item{b2}{parametro b2 da equação de afilamento - polinomio do 5°}

\item{b3}{parametro b3 da equação de afilamento - polinomio do 5°}

\item{b4}{parametro b4 da equação de afilamento - polinomio do 5°}

\item{b5}{parametro b5 da equação de afilamento - polinomio do 5°}
}
\value{
um valor numerico que indiaca o diametro a determinada altura
}
\description{
Dada a altura e o DAP esta função ira estimar a altura a um diametro de referencia indicado \cr
}
\examples{

 dc <- poly2hi(di = 10,
               h = 36.6,
               dap = 21.58,
               b0 = 1.180918512,
               b1 = -4.024085656,
               b2 = 19.64764832,
               b3 = -47.01626045,
               b4 = 48.7462987,
               b5 = -18.53635173)

}
