#' @import readr
NULL
#' Leitura do dados cadastrais
#'
#' Esta função le os dados cadastrais. Os dados devem ser salvos em um arquivo formatdo csv e ter como delimitador o **";"** e **","** para separacao decimal. \cr
#' Sao 26 campos obrigatorios e devem estar ordenados conforme a sequencia abaixo: \cr
#' \cr
#' **id** - idexador do talhao \cr
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
#leitura do dados de campo
#esta função le os dados de cadastrais dos talhoes. Os dados devem ser salvos em um arquivo formatdo csv e ter como separador o ";" e "," para separacao decimal.
#sao 12 campos obrigatorios e devem estar ordenados conforme descrito acima. Não imposta a nomenclatura dos campos no arquivo csv, mas sim a posição.
#os campos de data estao definidos para serem utilizados como dd/mm/aaaa
#o encording pre definido é o ISO-8859-1 - Latin alphabet
#demais campos podem ser adicionados apos a ultima coluna e podem ser usados para estratificação por exemplo - evite caracteres especiais e espacos em nome de variaveis

read_cad <- function(file, guess_max = 30000){
  cad <- read_csv2(file,
                   locale = locale(encoding = 'ISO-8859-1',
                                   date_format = "%d/%m/%Y"),
                   na = c(".", "NA", "NaN", ""),
                   guess_max = guess_max)

  #defini as variaveis essenciais
  cad_names <- c("id_talhao",	"rf",	"talhao",	"ciclo",	"rotacao",	"especie",	"matgen", "espacamento" ,"regime",	"dt_plt",	"dt_int",	"area_plt")

  #ajustando os nomes da variaveis conforme o numero de variais do input
  if (length(names(cad)) == length(cad_names)) {
    names(cad) <- cad_names
  } else if (length(names(cad)) > length(cad_names)){
    cad_names <- c(cad_names, names(cad)[13:length(names(cad))])
    names(cad) <- cad_names
  } else {
    stop("Falta variaveis no arquivo de cadastro. Consultar documentação.")
  }

  coltype <- cols(
    id_talhao = col_character(),
    rf = col_character(),
    talhao = col_character(),
    ciclo = col_double(),
    rotacao = col_double(),
    especie = col_character(),
    matgen = col_character(),
    espacamento = col_character(),
    regime = col_character(),
    dt_plt = col_date(format = ""),
    dt_int = col_date(format = ""),
    area_plt = col_double())

  cad <- type_convert(cad,
                      col_types = coltype)

  return(cad)
}
