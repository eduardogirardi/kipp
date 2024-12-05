#' Leitura dos padrões de sortimento
#'
#' @description
#' Esta função le os padores de sortimento. Os dados devem ser salvos em um arquivo formatdo csv e ter como delimitador o \strong{";"}. Como separador decimal deve-se usar \strong{","} . \cr
#'
#' @details
#'
#' Sao 7 campos obrigatorios e devem estar ordenados conforme descrito \cr
#' \strong{nome} - indexador do sortimento \cr
#' \strong{dpf} - diametro limite da ponta fina - cm. O valor sera arredondado para 2 casas decimais \cr
#' \strong{mincomp} - comprimento de tora minimo - m. O valor sera arredondado para 3 casas decimais \cr
#' \strong{maxcomp} - comprimento de tora maximo - m. O valor sera arredondado para 3 casas decimais. O menor comprimento possiver sera de 1 centimetro. \cr
#' \strong{padrao} - padrao de sortimento - esse campo chave deve corresponder ao valor atribuido na base de dados no campo "padrao" - campo numerico \cr
#' \strong{perda} - perda pela serra - cm \cr
#' \strong{molsap} - descricao do campo de atribuição na carga de modelo do SAP. Sendo as unicas classes disponiveis \strong{"c_vacel2", "c_vaclasse0", "c_vaclasse1", "c_vaclasse2", "c_vaclasse3", "p_vacel1", "p_vacel2", "p_vaclasse0", "p_vaclasse1"} \cr
#' \cr
#' Não importa a nomenclatura dos campos no arquivo csv, mas sim a sua posicao. \cr
#' O encording pre definido é o ISO-8859-1 - Latin alphabet \cr
#'
#' @param file caminho do csv2 contendo os dados cadastrais
#' @param guess_max estimativa do numero de observacoes da base de dados
#'
#' @return
#' um dataframe similar ao csv2 de input com suas variaveis padroniadas
#'
#' @examples
#'
#'  dc <- read_sort("descricao_sortimento.csv")
#'
#' @export
#'
#'

read_sort <- function(file, guess_max = 30000, ...){

  coltype <- readr::cols(
    readr::col_character(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_double(),
    readr::col_character())


  assort <- readr::read_csv2(file,
                          locale = readr::locale(encoding = 'ISO-8859-1',
                                                 date_format = "%d/%m/%Y"),
                          na = c(".", "NA", "NaN", ""),
                          col_types = coltype,
                          ...)

  #defini as variaveis essenciais
  sort_names <- c("name",	"dpf",	"mincomp",	"maxcomp",	"loss",	"padrao",	"molsap")

  #ajustando os nomes da variaveis conforme o numero de variais do input
  if (length(names(assort)) == length(sort_names)) {
    names(assort) <- sort_names
  } else if (length(names(assort)) > length(sort_names)){
    sort_names <- c(sort_names, names(assort)[(length(sort_names)+1):length(names(assort))])
    names(assort) <- sort_names
  } else {
    stop("Falta variaveis no arquivo de cadastro. Consultar documentação.")
  }

  #arredondamentos
  assort <- assort %>%
    dplyr::mutate(dpf = round(dpf, 2),
                  mincomp = dplyr::case_when(mincomp > 0 & mincomp < 0.001 ~ 0.001,
                                             TRUE ~ mincomp),
                  maxcomp = dplyr::case_when(maxcomp > 0 & maxcomp < 0.001 ~ 0.001,
                                             TRUE ~ maxcomp),
                  mincomp = round(mincomp, 3),
                  maxcomp = round(maxcomp, 3))

  #remove dpf < 0 e ou comprimento maximo menor que 1 cm
  assort <- assort %>%
    dplyr::filter(dpf >= 0) %>%
    dplyr::filter(maxcomp >= 0.01)

  #corrige comprimento zerado e compmin maior que compmac
  assort <- assort %>%
    dplyr::mutate(mincomp = dplyr::case_when(mincomp <= 0 ~ 0.001,
                                             TRUE ~ mincomp)) %>%
    dplyr::mutate(mincomp = dplyr::case_when(mincomp > maxcomp ~  maxcomp,
                                             TRUE ~ mincomp))

  #remove duplicacao de dpf
  assort <- assort %>%
    dplyr::group_by(padrao, dpf) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  #sort
  assort <- assort %>%
    dplyr::arrange(padrao, dplyr::desc(dpf))

  #ajusta names
  assort <- assort %>%
    dplyr::mutate(name = janitor::make_clean_names(assort$name, allow_dupes =T)) %>%
    dplyr::group_by(padrao, name) %>%
    dplyr::mutate(n = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(name = dplyr::case_when(n >= 2 ~ paste0(name,"_", n),
                                           TRUE ~ name)) %>%
    dplyr::select(-n)



  #ajusta molsap
  molvars <- c("c_vacel2",
               "c_vaclasse0",
               "c_vaclasse1",
               "c_vaclasse2",
               "c_vaclasse3",
               "p_vacel1",
               "p_vacel2",
               "p_vaclasse0",
               "p_vaclasse1")

  assort <- assort %>%
    dplyr::mutate(molsap = dplyr::case_when(!molsap %in% molvars ~ NA_character_,
                                            TRUE ~ molsap))

  #aninhar
  assort <- assort %>%
    dplyr::group_by(padrao) %>%
    tidyr::nest(.key = "assort")

  #função de concatenacao do df de sortimentos
  concat_df <- function(df, separator = "|") {
    # Converter cada linha em uma string concatenada
    concatenated_rows <- apply(df, 1, function(row) paste(row, collapse = "_"))

    # Concatenar todas as linhas em uma única string
    result <- paste(concatenated_rows, collapse = separator)

    return(result)
  }

  assort <- assort %>%
    dplyr::mutate(desc_sort = purrr::map(assort, concat_df))


  return(assort)
}
