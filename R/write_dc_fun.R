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

  bd <- bd %>%
    dplyr::mutate(alt = dplyr::case_when(!is.na(h_quebra) ~ round(h_quebra*10),
                                         TRUE ~ round(h*10)))

  #defini o nome das principais variaveis
  dc_names <- c("atividade",
                "rf",
                "talhao",
                "ciclo",
                "rotacao",
                "dt_med",
                "lider",
                "auxiliar",
                "parcela",
                "tipo",
                "forma",
                "hr_ini",
                "hr_fim",
                "coordX",
                "coordY",
                "lado1",
                "lado2",
                "inc1",
                "inc2",
                "linha",
                "arvore",
                "cap",
                "alt",
                "cod1",
                "cod2",
                "codQ")

  bd <- bd %>%
    dplyr::select(tidyselect::all_of(dc_names)) %>%
    dplyr::rename("tal" = "talhao",
                  "hora_ini" = "hr_ini",
                  "hora_fim" = "hr_fim")

  readr::write_csv2(bd, path_outputs, na = ".")

}
