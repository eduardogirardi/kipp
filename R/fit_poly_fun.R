#' Cubagem e afilamento
#'
#' Esta função ira realizar a cubagem dos dados e o ajustye da curva de afiliamento \cr
#'
#'
#'
#' @param cub dataframe contendo as informacoes padronizadas - output da funcao \code{\link{read_cuba()}}
#' @param path_outputs diretorio onde sera escrito os outputs da funcao
#' @param dcom diametro comercial na ponta fina em centimetros, quando NULL é mantido os valores da base original, quando definido o valor este é simulado por interpolação linear
#' @param at altura de toco em metros, quando NULL é mantido os valores da base original, quando definido o valor este é simulado por interpolação linear
#' @param qc limites classes de diametro para ajuste particionado da polinomial 
#'
#' @return sera emitido o relatorio em html no caminho indicado.
#'
#' @examples
#'
#'
#'  fit_poly(bd, path_outputs, dcom = 0, at = .1)
#'
#' @export
#'

#rmarkdown::render

fit_poly <- function(cub,
                     path_outputs = NULL,
                     dcom = NULL,
                     at = NULL,
                     qc = c(0, 500)){
  
  # run rmd -----------------------------------------------------------------
  
  
  if (is.null(path_outputs)){
    path_outputs <- getwd()
  }
  
  #create proj folder
  dir.create(path_outputs, recursive = T)
  
  #read and run rmd
  rmarkdown::render(input = system.file("rmd", "fit_poly.Rmd", package = "kipp"),
                    output_file = paste0(path_outputs, "\\fit_poly.html"))
  
}