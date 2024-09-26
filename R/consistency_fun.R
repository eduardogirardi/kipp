#' Relatorio de cosistencia
#'
#' Esta função ira gerar o relatorio de consistencia \cr
#'
#'
#'
#' @param bd dataframe contendo as informacoes padronizadas - output da funcao \code{\link{cal_var()}}
#' @param path_outputs diretorio onde sera escrito os outputs da funcao
#' @param im vetor com campos identificadores de medicao
#' @param ie vetor com campos identificadores de estrato
#' @param it vetor com campos identificadores de talhao
#'
#' @return sera emitido o relatorio em html no caminho indicado.
#'
#' @examples
#'
#' im <- c("centro", "rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med")
#' ie <- c("especie", "matgen", "regime", "classe_idade")
#' it <- c("centro", "rf", "talhao", "ciclo", "rotacao")
#'
#'  consistency(bd, path_outputs, im, ie)
#'
#' @export
#'

#rmarkdown::render

consistency <- function(bd,
                        path_outputs,
                        im = c("centro", "rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"),
                        ie = c("especie", "matgen", "regime", "classe_idade"),
                        it = c("centro", "rf", "talhao", "ciclo", "rotacao")){

  # run rmd -----------------------------------------------------------------

  #create proj folder
  dir.create(path_outputs, recursive = T)

  #read and run rmd
  rmarkdown::render(input = system.file("rmd", "consistency.Rmd", package = "kipp"),
                    output_file = paste0(path_outputs, "\\consistencia.html"))

}
