#' Leitura do dados cadastrais
#'
#' Esta função le os dados cadastrais. Os dados devem ser salvos em um arquivo formatdo csv e ter como delimitador o **";"** para para separacao decimal deve-se usar **","** . \cr
#' Sao 26 campos obrigatorios e devem estar ordenados conforme a sequencia abaixo: \cr
#' \cr
#' **id** - indexador do talhao \cr
#' **rf** - identificacao de regiao florestal \cr
#' **talhao** - identificacao do talhao \cr
#' **ciclo** - identificacao do ciclo de plantio \cr
#' **rotacao** - identificacao da rotacao de plantio \cr
#' **especie** - descricao da especie \cr
#' **matgen** - descricao do material genetico \cr
#' **espacamento** - espacamento de plantio \cr
#' **regime** - regime de manejo \cr
#' **dt_plt** - data de plantio - dd/mm/yyyy \cr
#' **dt_int** - data da ultima intervencao - dd/mm/yyyy \cr
#' **area_plt** - area plantada do talhao - ha \cr
#' \cr
#' Não importa a nomenclatura dos campos no arquivo csv, mas sim a sua posicao. \cr
#' O encording pre definido é o ISO-8859-1 - Latin alphabet \cr
#' campos adicionais podem ser adicionados apos a ultima coluna
#'
#' @param file caminho do csv2 contendo os dados cadastrais
#' @param guess_max estimativa do numero de observacoes da base de dados
#'
#' @return um dataframe similar ao csv2 de input com suas variaveis padroniadas
#'
#' @examples
#'
#'  dc <- read_cad("C:/Users/klem00125926/dados_coletor.csv")
#'
#' @export
#'
#'

#readr::read_csv2
#readr::locale
#readr::cols
#readr::col_character
#readr::col_double
#readr::col_date
#readr::col_time
#readr::type_convert

#leitura do dados de campo
#esta função le os dados de cadastrais dos talhoes. Os dados devem ser salvos em um arquivo formatdo csv e ter como separador o ";" e "," para separacao decimal.
#sao 12 campos obrigatorios e devem estar ordenados conforme descrito acima. Não imposta a nomenclatura dos campos no arquivo csv, mas sim a posição.
#os campos de data estao definidos para serem utilizados como dd/mm/aaaa
#o encording pre definido é o ISO-8859-1 - Latin alphabet
#demais campos podem ser adicionados apos a ultima coluna e podem ser usados para estratificação por exemplo - evite caracteres especiais e espacos em nome de variaveis

read_cad <- function(file, guess_max = 30000, ...){

  coltype <- readr::cols(
    readr::col_character(),
    readr::col_character(),
    readr::col_character(),
    readr::col_double(),
    readr::col_double(),
    readr::col_character(),
    readr::col_character(),
    readr::col_character(),
    readr::col_character(),
    readr::col_date(format = ""),
    readr::col_date(format = ""),
    readr::col_double())


  cad <- readr::read_csv2(file,
                          locale = readr::locale(encoding = 'ISO-8859-1',
                                                 date_format = "%d/%m/%Y"),
                          na = c(".", "NA", "NaN", ""),
                          col_types = coltype,
                          ...)

  #defini as variaveis essenciais
  cad_names <- c("id_talhao",	"rf",	"talhao",	"ciclo",	"rotacao",	"especie",	"matgen",
                 "espacamento" ,"regime",	"dt_plt",	"dt_int",	"area_plt")

  #ajustando os nomes da variaveis conforme o numero de variais do input
  if (length(names(cad)) == length(cad_names)) {
    names(cad) <- cad_names
  } else if (length(names(cad)) > length(cad_names)){
    cad_names <- c(cad_names, names(cad)[(length(cad_names)+1):length(names(cad))])
    names(cad) <- cad_names
  } else {
    stop("Falta variaveis no arquivo de cadastro. Consultar documentação.")
  }


  return(cad)
}
