#' @import readr
NULL
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
#' @param guess_max estimativa do numero de observacoes da base de dados
#'
#' @return um dataframe similar ao csv2 de input com suas variaveis padroniadas
#'
#' @examples
#'
#'  dc <- read_dc("dados_coletor.csv")
#'
#' @export



#leitura do dados de campo
#esta função le os dados de campo. Os dados devem ser salvos em um arquivo formatdo csv e ter como delimitador o ";" e "," para separacao decimal.
#sao 27 campos obrigatorios e devem estar ordenados conforme a variavel "dc_names" da funcao. Não imposta a nomenclatura dos campos no arquivo csv, mas sim a posição.
#os campos de data estao definidos para serem utilizados como dd/mm/aaaa
#o encording pre definido é o ISO-8859-1 - Latin alphabet

read_dc <- function(file, guess_max = 100000){
  dc <- read_csv2(file,
                         locale = locale(encoding = 'ISO-8859-1',
                                         date_format = "%d/%m/%Y"),
                         na = c(".", "NA", "NaN", ""),
                         guess_max = guess_max)

  #defini o nome das principais variaveis
  dc_names <- c("atividade","rf", "talhao", "ciclo", "rotacao", "dt_med", "lider", "auxiliar", "parcela", "tipo",
                "forma", "hr_ini", "hr_fim", "coordX", "coordY", "lado1", "lado2", "inc1", "inc2", "linha", "arvore",
                "cap", "alt", "cod1", "cod2", "codQ")

  #ajustando os nomes da variaveis conforme o numero de variais do input
  if (length(names(dc)) == length(dc_names)) {
    names(dc) <- dc_names
  } else if (length(names(dc)) > length(dc_names)){
    dc_names <- c(dc_names, names(dc)[28:length(names(dc))])
    names(dc) <- dc_names
  } else {
    stop("Falta variaveis no arquivo de campo. Consultar documentação.")
  }

  coltype <- cols(
    atividade = col_character(),
    rf = col_character(),
    talhao = col_character(),
    ciclo = col_double(),
    rotacao = col_double(),
    dt_med = col_date(format = ""),
    equipe = col_character(),
    auxiliar = col_character(),
    parcela = col_double(),
    tipo = col_character(),
    forma = col_character(),
    hr_ini = col_time(format = ""),
    hr_fim = col_time(format = ""),
    coordX = col_double(),
    coordY = col_double(),
    lado1 = col_double(),
    lado2 = col_double(),
    inc1 = col_double(),
    inc2 = col_double(),
    linha = col_double(),
    arvore = col_double(),
    cap = col_double(),
    alt = col_double(),
    cod1 = col_character(),
    cod2 = col_character(),
    codQ = col_character())

  dc <- type_convert(dc,
                     col_types = coltype)

  return(dc)
}
