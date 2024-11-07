#' Leitura dos coeficientes da curva de afilamento
#'
#' Esta função le os coeficientes do ajuste da curva de afilamento (Schöepfer, 1996). Os dados devem ser salvos em um arquivo formatdo csv e ter como delimitador o \strong{";"} para para separacao decimal deve-se usar \strong{","} . \cr
#' Sao 17 campos obrigatorios e devem estar ordenados conforme a sequencia abaixo: \cr
#' \cr
#' \strong{especie} - codificação de especie \cr
#' \strong{matgen} - codificacao de material genetico \cr
#' \strong{regime} - codificação do regime de manejo \cr
#' \strong{dap_min} - D.A.P. minimo - cm \cr
#' \strong{dap_max} - D.A.P. maximo - cm \cr
#' \strong{b0} - b0 do polinomio (Schöepfer, 1996) - ajuste com casca \cr
#' \strong{b1} - b1 do polinomio (Schöepfer, 1996) - ajuste com casca \cr
#' \strong{b2} - b2 do polinomio (Schöepfer, 1996) - ajuste com casca \cr
#' \strong{b3} - b3 do polinomio (Schöepfer, 1996) - ajuste com casca \cr
#' \strong{b4} - b4 do polinomio (Schöepfer, 1996) - ajuste com casca \cr
#' \strong{b5} - b5 do polinomio (Schöepfer, 1996) - ajuste com casca \cr
#' \strong{b0_sc} - b0 do polinomio (Schöepfer, 1996) - ajuste sem casca \cr
#' \strong{b1_sc} - b1 do polinomio (Schöepfer, 1996) - ajuste sem casca \cr
#' \strong{b2_sc} - b2 do polinomio (Schöepfer, 1996) - ajuste sem casca \cr
#' \strong{b3_sc} - b3 do polinomio (Schöepfer, 1996) - ajuste sem casca \cr
#' \strong{b4_sc} - b4 do polinomio (Schöepfer, 1996) - ajuste sem casca \cr
#' \strong{b5_sc} - b5 do polinomio (Schöepfer, 1996) - ajuste sem casca \cr
#'
#' Não importa a nomenclatura dos campos no arquivo csv, mas sim a sua posicao. \cr
#' O encording pre definido é o ISO-8859-1 - Latin alphabet \cr
#'
#' @param file caminho do csv2 contendo os dados cadastrais
#' @param guess_max estimativa do numero de observacoes da base de dados
#'
#' @return um dataframe similar ao csv2 de input com suas variaveis padroniadas
#'
#' @examples
#'
#'  dc <- read_poly("C:/Users/klem00125926/descricao_sortimento.csv")
#'
#' @export
#'
#'

read_poly <- function(file, guess_max = 30000, ...){

  coltype <- readr::cols(
    readr::col_character(),
    readr::col_character(),
    readr::col_character(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double())


  coef <- readr::read_csv2(file,
                             locale = readr::locale(encoding = 'ISO-8859-1',
                                                    date_format = "%d/%m/%Y"),
                             na = c(".", "NA", "NaN", ""),
                             col_types = coltype,
                             ...)

  #defini as variaveis essenciais
  coef_names <- c("especie",	"matgen","regime",	"dap_1",	"dap_2",
                  "b0",	"b1",	"b2",	"b3", "b4", "b5",
                  "b0_sc",	"b1_sc",	"b2_sc",	"b3_sc", "b4_sc", "b5_sc")

  #ajustando os nomes da variaveis conforme o numero de variais do input
  if (length(names(coef)) == length(coef_names)) {
    names(coef) <- coef_names
  } else if (length(names(coef)) > length(coef_names)){
    coef_names <- c(coef_names, names(coef)[(length(coef_names)+1):length(names(coef))])
    names(coef) <- coef_names
  } else {
    stop("Falta variaveis no arquivo de cadastro. Consultar documentação.")
  }


  return(coef)
}
