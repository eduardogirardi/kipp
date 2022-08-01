#' @import dplyr
#' @import tidyr
#' @import readr
NULL
#' Escreve o csv de input do SAS
#'
#' Padroniza e escreve o csv formatado para input na rotina de processamento SAS \cr
#'
#'
#' @param bd dataframe com a base dados a ser aplicada os modelos. Output da funcao \code{\link{apply_hip()}}
#' @param path_outputs caminho onde sera escrito o csv
#'
#'
#' @return Escreve arquivo csv
#'
#' @examples
#'
#' write_dc(bd, path_outputs)
#'
#' @export
#'

write_dc <- function(bd, path_outputs){

  #defini o nome das principais variaveis
  dc_names <- c("atividade","rf", "talhao", "ciclo", "rotacao", "dt_med", "lider", "auxiliar", "parcela", "tipo",
                "forma", "hr_ini", "hr_fim", "coordX", "coordY", "lado1", "lado2", "inc1", "inc2", "linha", "arvore",
                "cap", "alt", "cod1", "cod2", "codQ")

  bd <- bd %>%
    select(all_of(dc_names))

  write_csv2(bd, file.path(path_outputs, "dados_campo_hest.csv"), na = ".")

}
