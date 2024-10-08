% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_resid_ev_fun.R
\name{plot_resid_ev}
\alias{plot_resid_ev}
\alias{plot_resid_ev.lm}
\title{plot_resid_ev}
\usage{
plot_resid_ev(x, ...)

\method{plot_resid_ev}{lm}(
  x,
  data = NULL,
  exp_variables = NULL,
  delog = F,
  ncol = NULL,
  nrow = NULL,
  title = NULL
)
}
\arguments{
\item{x}{modelo}

\item{data}{dataframe contendo as variaveis a serem plotadas em função do residuo}

\item{exp_variables}{vetor com as variavris que serao utilizadas para construção dos graficos}
}
\value{
uma lista de graficos
}
\description{
Plotagem de residuos
}
\details{
Esta funcao gera os graficos do residuo % do modelo em função de variaveis externas contidas no dataframe
}
\examples{

mol <- lm(mpg ~ hp + cyl, data = mtcars)
pn <- plot_resid_ev(mol, data = mtcars, exp_variables = "gear")
pn

}
