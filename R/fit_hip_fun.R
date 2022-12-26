#' Ajuste dos modelos
#'
#' Esta função ira ajustar os modelos hipsometricos \cr
#'
#'
#' Esta funcao ira ajustar 4 modelos, divididos em 2 grupos. Para cada grupo sera usado uma estratificacao especifica definida nos parametros\cr
#' É necessario o minimo de 8 observacoes para o ajuste.
#'
#'
#' @param bd dataframe contendo as informacoes padronizadas - output da funcao \code{\link{cal_var()}}
#' @param proj_name objeto tipo texto com o nome do projeto
#' @param path_outputs diretorio onde sera escrito os outputs da funcao
#' @param estrato deve ser uma vetor contendo as variaveis que compoem os estratos dos dois grupos de modelos. O primeiro grupo se refere aos modelos de Scolforoe e o segundo grupo aos modelos de Pettersen e Curtis
#'
#' @return sera emitido o relatorio em html e arquivos de apoio no caminho indicado. Tambem sera retornado um objeto contendo os coeficientes ajustados utilizado para aplicacao dos modelos
#'
#' @examples
#'
#' estrato <- list(c("especie", "matgen", "regime"),
#'                 c("especie", "matgen", "regime", "classe_idade"))
#'
#'  hip <- fit_hip(bd, proj_name = "fit", path_outputs, estrato)
#'
#' @export
#'

fit_hip <- function(bd, proj_name, path_outputs, estrato){

  # filtra dados para hipsometrica ------------------------------------------

  #filtra aluras zeradas
  base <- bd %>%
    dplyr::filter(alt != 0) %>%
    dplyr::filter(dap != 0)


  #filtra codigos problematicos
  rmcod <- c("E", "H", "J", "A", "B")
  base <- base %>%
    dplyr::filter(is.na(cod1) | cod1 %in% rmcod) %>%
    dplyr::filter(is.na(cod2) | cod2 %in% rmcod) %>%
    dplyr::filter(is.na(codQ) | codQ %in% rmcod)



  # run rmd -----------------------------------------------------------------

  #create proj folder
  dir.create(path_outputs, recursive = T)

  #read and run rmd
  rmarkdown::render(input = system.file("rmd", "fit_hip.Rmd", package = "kipp"), output_file = paste0(path_outputs, "\\", proj_name, ".html"))


  #cria lista com os coeficientes ajustados de cada modelo
  lst_coefs <- list()

  if(exists("maf_coefs")){

    lst_coefs$sf <- maf_coefs %>%
      dplyr::select(tidyselect::all_of(estrato[[1]]), tidyselect::matches("b[[:digit:]]{1}")) %>%
      dplyr::rename_if(stringr::str_detect(names(.), "b[[:digit:]]{1}"), ~paste0(. , "_sf"))

  }
  if (exists("mafsim_coefs")){

    lst_coefs$ss <-  mafsim_coefs %>%
      dplyr::select(tidyselect::all_of(estrato[[1]]), tidyselect::matches("b[[:digit:]]{1}")) %>%
      dplyr::rename_if(stringr::str_detect(names(.), "b[[:digit:]]{1}"), ~paste0(. , "_ss"))

  }
  if (exists("pettersen_coefs")){

    lst_coefs$pt <- pettersen_coefs %>%
      dplyr::select(tidyselect::all_of(estrato[[2]]), tidyselect::matches("b[[:digit:]]{1}")) %>%
      dplyr::rename_if(stringr::str_detect(names(.), "b[[:digit:]]{1}"), ~paste0(. , "_pt"))

  }
  if (exists("curtis_coefs")){

    lst_coefs$ct <- curtis_coefs %>%
      dplyr::select(tidyselect::all_of(estrato[[2]]), tidyselect::matches("b[[:digit:]]{1}")) %>%
      dplyr::rename_if(stringr::str_detect(names(.), "b[[:digit:]]{1}"), ~paste0(. , "_ct"))

  }


  return(lst_coefs)
}
