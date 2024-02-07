#' Leitura de cubagem
#'
#' Esta função le os dados de campo. Os dados devem ser salvos em um arquivo formatdo csv e ter como delimitador o \strong{";"} e \strong{","} para separacao decimal. \cr
#' Sao 20 campos obrigatorios e devem estar ordenados conforme a sequencia abaixo: \cr
#' \cr
#' \strong{rf} - identificacao de regiao florestal \cr
#' \strong{talhao} - identificacao do talhao \cr
#' \strong{ciclo} - identificacao do ciclo de plantio \cr
#' \strong{rotacao} - identificacao da rotacao de plantio \cr
#' \strong{arvore} - identificacao da arvore \cr
#' \strong{dt_med} - data da medicao - dd/mm/yyyy \cr
#' \strong{idade} - idade da arvore -anos \cr
#' \strong{especie} - descricao da especie \cr
#' \strong{matgen} - descricao do material genetico \cr
#' \strong{regime} - regime de manejo \cr
#' \strong{toco} - altura de toco - m \cr
#' \strong{dap} - diametro a 1,3m - cm \cr
#' \strong{h} - altura total da arvore - m \cr
#' \strong{hi} - altura da seccao - m \cr
#' \strong{dcc1} - 1° diametro da seccao com casca - cm \cr
#' \strong{dcc2} - 2° diametro da seccao com casca - cm \cr
#' \strong{dsc1} - 1° diametro da seccao sem casca - cm \cr
#' \strong{dsc2} - 2° diametro da seccao sem casca - cm \cr
#' \strong{coleta} - realizador da coleta \cr
#' \strong{obs} - observacao \cr
#' \cr
#' Não importa a nomenclatura dos campos no arquivo csv, mas sim a sua posicao. \cr
#' O encording pre definido é o ISO-8859-1 - Latin alphabet \cr
#'
#' @param file caminho do csv2 contendo os dados da cubagem
#' @param ... argumentos da \link[=readr]{read_csv2}
#'
#' @return um dataframe similar ao csv2 de input com suas variaveis padroniadas
#'
#' @examples
#'
#'  dc <- read_cuba("dados_campo.csv")
#'
#' @export


#readr::read_csv2
#readr::locale
#readr::cols
#readr::col_character
#readr::col_double
#readr::col_date
#readr::col_time
#readr::type_convert


#leitura do dados de campo
#esta função le os dados de campo. Os dados devem ser salvos em um arquivo formatdo csv e ter como delimitador o ";" e "," para separacao decimal.
#sao 20 campos obrigatorios e devem estar ordenados conforme a variavel "dc_names" da funcao. Não imposta a nomenclatura dos campos no arquivo csv, mas sim a posição.
#os campos de data estao definidos para serem utilizados como dd/mm/aaaa
#o encording pre definido é o ISO-8859-1 - Latin alphabet

read_cuba <- function(file, guess_max = 100000, ...){
  
  coltype <- readr::cols(
    readr::col_character(),
    readr::col_character(),
    readr::col_double(),
    readr::col_double(),
    readr::col_character(),
    readr::col_date(format = ""),
    readr::col_double(),
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
    readr::col_character(),
    readr::col_character())
  
  dc <- readr::read_csv2(file,
                         locale = readr::locale(encoding = 'ISO-8859-1',
                                                date_format = "%d/%m/%Y"),
                         na = c(".", "NA", "NaN", "", "#N/D"),
                         col_types = coltype,
                         ...)
  
  
  #defini o nome das principais variaveis
  dc_names <- c("rf", "talhao", "ciclo", 	"rotacao",	"arvore",	"dt_med",	"idade",	"especie",	"matgen",	"regime",	"htoco",
                "dap",	"h",	"hi",	"dcc1",	"dcc2",	"dsc1",	"dsc2",	"coleta",	"obs")
  
  #ajustando os nomes da variaveis conforme o numero de variais do input
  if (length(names(dc)) == length(dc_names)) {
    names(dc) <- dc_names
  } else if (length(names(dc)) > length(dc_names)){
    dc_names <- c(dc_names, names(dc)[(length(dc_names)+1):length(names(dc))])
    names(dc) <- dc_names
  } else {
    stop("Falta variaveis no arquivo de campo. Consultar documentação.")
  }
  
  return(dc)
}
