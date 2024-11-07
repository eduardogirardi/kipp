#' Uni a base de dados com os coeficientes da função de afilamento
#'
#' Esta função unir a bade dados com os coeficientes da função de afilamento utilizando os campos \strong{especie, matgen, regime e dap}
#' A condição utilizada é dap >= dap_1 & dap < dap_2
#'
#'
#' @param db dataframe contendo as informacoes padronizadas do coletor - output da funcao \code{\link{cal_var()}}
#' @param coefs dataframe contendo as informacoes padronizadas dos coeficientes da curva de afilamento - output da funcao \code{\link{read_poly()}}
#'
#' @details
#' \strong{Registros que nao possuirem correspondencia serao removidos da base de dados} \cr
#'
#'
#' @return um dataframe de mesma estrutura com novas variaveis unidas
#'
#' @examples
#'
#'  db <- poly_join(dc, coefs)
#'
#' @export


poly_join <- function(db, coefs, ...){


  #n linhas do dc
  nlinhas1 <- nrow(db)

  est1 <- db %>%
    dplyr::select(especie, matgen, regime) %>%
    dplyr::distinct_all()

  by <- dplyr::join_by(especie, matgen, regime, dplyr::between(dap, dap_1, dap_2, bounds ="[)"))

  db <- db %>%
    dplyr::inner_join(coefs, by) %>%
    dplyr::select(-dap_1, -dap_2)


  nlinhas2 <- nrow(db)

  est2 <- db %>%
    dplyr::select(especie, matgen, regime) %>%
    dplyr::distinct_all()

  if (nlinhas1 != nlinhas2){
    miss <- est1 %>%
      dplyr::anti_join(est2)

    message("PROBLEMA com a base de coeficientes. VERIFIQUE ausencia estratos abaixo na base de coeficientes")
    print(miss)

  }

  return(db)
}
