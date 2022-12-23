#' Leitura do dados de campo
#'
#' Esta função le os dados de campo. Os dados devem ser salvos em um arquivo formatdo csv e ter como delimitador o **";"** e **","** para separacao decimal. \cr
#' Sao 26 campos obrigatorios e devem estar ordenados conforme a sequencia abaixo: \cr
#' \cr
#' **atividade** - descricao da atividade \cr
#' **rf** - identificacao de regiao florestal \cr
#' **talhao** - identificacao do talhao \cr
#' **ciclo** - identificacao do ciclo de plantio \cr
#' **rotacao** - identificacao da rotacao de plantio \cr
#' **dt_med** - data da medicao - dd/mm/yyyy \cr
#' **lider** - nome do lider \cr
#' **auxiliar** - nome do auxiliar \cr
#' **parcela** - dentificacao da parcela \cr
#' **tipo** - tipo de medicao \cr
#' **forma** - forma da parcela \cr
#' **hr_ini** - horario de inicio da medicao - HH:MM:SS \cr
#' **hr_fim** - horario de finalizacao da medicao - HH:MM:SS \cr
#' **coordX** - Coordenada geografica  \cr
#' **coordY** - Coordenada geografica \cr
#' **lado1** - Comprimento do lado da parcela -dm \cr
#' **lado2** - Comprimento do lado da parcela -dm \cr
#' **inc1** - Inclinacao da parcela - ° \cr
#' **inc2** - Inclinacao da parcela - ° \cr
#' **linha** - numero da linha \cr
#' **arvore** - numero da arvore na linha \cr
#' **cap** - circunferencia a 1,3m - mm \cr
#' **alt** - altura total da arvore - dm \cr
#' **cod1** - codigo qualitativo 1 \cr
#' **cod2** - codigo qualitativo 2 \cr
#' **codQ** - codigo qualitativo de tora \cr
#' \cr
#' Não importa a nomenclatura dos campos no arquivo csv, mas sim a sua posicao. \cr
#' O encording pre definido é o ISO-8859-1 - Latin alphabet \cr
#'
#' @param file caminho do csv2 contendo os dados de campo do coletor
#' @param ... argumentos da readr::read_csv2
#'
#' @return um dataframe similar ao csv2 de input com suas variaveis padroniadas
#'
#' @examples
#'
#'  dc <- read_dc("dados_coletor.csv")
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
#sao 27 campos obrigatorios e devem estar ordenados conforme a variavel "dc_names" da funcao. Não imposta a nomenclatura dos campos no arquivo csv, mas sim a posição.
#os campos de data estao definidos para serem utilizados como dd/mm/aaaa
#o encording pre definido é o ISO-8859-1 - Latin alphabet

read_dc <- function(file, guess_max = 100000, ...){

  coltype <- readr::cols(
    readr::col_character(),
    readr::col_character(),
    readr::col_character(),
    readr::col_double(),
    readr::col_double(),
    readr::col_date(format = ""),
    readr::col_character(),
    readr::col_character(),
    readr::col_double(),
    readr::col_character(),
    readr::col_character(),
    readr::col_time(format = ""),
    readr::col_time(format = ""),
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
    readr::col_character(),
    readr::col_character(),
    readr::col_character())

  dc <- readr::read_csv2(file,
                         locale = readr::locale(encoding = 'ISO-8859-1',
                                                date_format = "%d/%m/%Y"),
                         na = c(".", "NA", "NaN", ""),
                         col_types = coltype,
                         ...)


  #defini o nome das principais variaveis
  dc_names <- c("atividade","rf", "talhao", "ciclo", "rotacao", "dt_med", "lider", "auxiliar", "parcela", "tipo",
                "forma", "hr_ini", "hr_fim", "coordX", "coordY", "lado1", "lado2", "inc1", "inc2", "linha", "arvore",
                "cap", "alt", "cod1", "cod2", "codQ")

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
