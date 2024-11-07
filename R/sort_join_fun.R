#' Uni a base de dados com os padroes de sortimento
#'
#' @description
#' Esta função ira unir a bade dados com os padroes de sortimento pelo campo chave  \strong{padrao} \cr
#'
#'
#' @param db dataframe contendo as informacoes padronizadas do coletor - output da funcao \code{\link{cal_var()}} \strong{adicionado da variavel padrao}
#' @param sort dataframe contendo as informacoes padronizadas dos coeficientes da curva de afilamento - output da funcao \code{\link{read_sort()}}
#' \cr
#'
#' @details
#' \strong{Registros que nao possuirem correspondencia serao removidos da base de dados} \cr
#'
#'
#' @return um dataframe de mesma estrutura com novas variaveis unidas
#'
#' @examples
#'
#'  db <- sort_join(db, sort)
#'
#' @export


sort_join <- function(db, sort){


  #n linhas do dc
  nlinhas1 <- nrow(db)

  est1 <- db %>%
    dplyr::select(padrao) %>%
    dplyr::distinct_all()

  db <- db %>%
    dplyr::inner_join(sort, by = join_by(padrao))


  nlinhas2 <- nrow(db)

  est2 <- db %>%
    dplyr::select(padrao) %>%
    dplyr::distinct_all()

  if (nlinhas1 != nlinhas2){
    miss <- est1 %>%
      dplyr::anti_join(est2)

    message("PROBLEMA com os padroes de traçamento. VERIFIQUE ausencia dos padores abaixo ou duplicidade da base de descrição dos sortimentos")
    print(miss)

  }

  return(db)
}
