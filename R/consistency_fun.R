#' Relatorio de cosistencia
#'
#' Esta função ira gerar o relatorio de consistencia \cr
#'
#'
#'
#' @param bd dataframe contendo as informacoes padronizadas - output da funcao \code{\link{cal_var()}}
#' @param path_outputs diretorio onde sera escrito os outputs da funcao
#' @param im vetor com variaveis de medicao
#' @param ie vetor com variaveis de estrato
#'
#' @return sera emitido o relatorio em html no caminho indicado.
#'
#' @examples
#'
#' im <- c("rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med")
#' ie <- c("especie", "matgen", "regime", "classe_idade")
#'
#'  consistency(bd, path_outputs, im, ie)
#'
#' @export
#'

#rmarkdown::render

consistency <- function(bd,
                        path_outputs,
                        im = c("rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"),
                        ie = c("especie", "matgen", "regime", "classe_idade")){

  # run rmd -----------------------------------------------------------------

  #create proj folder
  dir.create(path_outputs, recursive = T)

  #read and run rmd
  rmarkdown::render(input = system.file("rmd", "consistency.Rmd", package = "kipp"),
                    output_file = paste0(path_outputs, "\\consistencia.html"))

}
