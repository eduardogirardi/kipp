#' Leitura dos coeficientes
#'
#' Esta função ira ler os coeficientes do arquivo csv e criar o objeto necessario para aplicação do modelo \cr
#'
#'
#' @param folder_path diretorio contendo os arquivos csvs dos coeficientes. Os arquivos csvs de input fazem parte do output da funcao \code{\link{fit_hip()}} e devem seguir o mesmo padrao de nomenclatura e variaveis.
#'
#' @return Um objeto tipo lista sera criado com os coeficientes presentes nos arquivos csvs
#'
#' @examples
#'
#' hip <- coef_hip(folder_path)
#'
#' @export
#'

coef_hip <- function(folder_path){

  #defini as variaveis a serem removidas do dataframe
  rmvars <- c("r.squared",
              "adj.r.squared",
              "sigma",
              "sigmaP",
              "statistic",
              "p.value",
              "df",
              "logLik",
              "AIC",
              "BIC",
              "deviance",
              "df.residual",
              "nobs")


  #gera lista com um dataframe para cada modelo existente, removendo variaveis do rmvars e adicionando sufixos aos coefs
  lst_coefs <- list()

  if(file.exists(file.path(folder_path, "coefs_scolforo.csv"))){

    lst_coefs$sf <- readr::read_csv2(file.path(folder_path, "coefs_scolforo.csv")) %>%
      dplyr::select(-1, -tidyselect::all_of(rmvars)) %>%
      dplyr::rename_if(stringr::str_detect(names(.), "b[[:digit:]]{1}"), ~paste0(. , "_sf"))

  }
  if (file.exists(file.path(folder_path, "coefs_scolforo_simp.csv"))){

    lst_coefs$ss <-  readr::read_csv2(file.path(folder_path, "coefs_scolforo_simp.csv")) %>%
      dplyr::select(-1, -tidyselect::all_of(rmvars)) %>%
      dplyr::rename_if(stringr::str_detect(names(.), "b[[:digit:]]{1}"), ~paste0(. , "_ss"))

  }
  if (file.exists(file.path(folder_path, "coefs_pettersen.csv"))){

    lst_coefs$pt <- readr::read_csv2(file.path(folder_path, "coefs_pettersen.csv")) %>%
      dplyr::select(-1, -tidyselect::all_of(rmvars)) %>%
      dplyr::rename_if(stringr::str_detect(names(.), "b[[:digit:]]{1}"), ~paste0(. , "_pt"))

  }
  if (file.exists(file.path(folder_path, "coefs_curtis.csv"))){

    lst_coefs$ct <- readr::read_csv2(file.path(folder_path, "coefs_curtis.csv")) %>%
      dplyr::select(-1, -tidyselect::all_of(rmvars)) %>%
      dplyr::rename_if(stringr::str_detect(names(.), "b[[:digit:]]{1}"), ~paste0(. , "_ct"))

  }


  return(lst_coefs)

}
