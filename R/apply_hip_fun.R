#' Aplicacao dos modelos hipsometricos
#'
#' Aplicação dos modelos hipsometricos previamente ajustados \cr
#'
#'
#' @param bd dataframe com a base dados a ser aplicada os modelos. Output da funcao \code{\link{cal_var()}}
#' @param coefs lista dos coeficientes ajustados. Output da funcao \code{\link{fit_hip()}} ou \code{\link{coef_hip()}}
#' @param priority vetor com a prioridade de modelo a ser utilizado na altura predita. c("scolforo_simp", "scolforo", "pettersen", "curtis")
#' @param by.hip_sel se TRUE sera utilizado o modelo pre-definido na variavel \strong{hip_sel} presente no dataframe \strong{bd}. c("scolforo_simp", "scolforo", "pettersen", "curtis")
#'
#' @return Dataframe contendo as variaveis preditas com os modelos inputados
#'
#' @examples
#'
#' #OP1
#' priority <- c("scolforo_simp",
#'               "pettersen",
#'               "scolforo",
#'               "curtis")
#'
#' bd <- apply_hip(bd, coefs = hip, priority = priority)
#'
#' #OP2
#' bd$hip_sel <- "scolforo"
#'
#' bd <- apply_hip(bd, coefs = hip, priority = NULL, by.hip_sel = T)
#'
#' @export
#'


apply_hip <- function(bd, coefs, priority = NULL, by.hip_sel = F){

  #converte a classe das variaveis do dataframe
  coefs_aj <- purrr::map(coefs, readr::type_convert)

  bd <- purrr::reduce(append(coefs_aj, list(bd = bd), after = 0), dplyr::left_join)

  #defini as variaveis necessarias no dataframe
  coefs_vars <- c("b0_sf", "b1_sf", "b2_sf", "b3_sf", "b4_sf",
                  "b0_ss", "b1_ss", "b2_ss", "b3_ss",
                  "b0_pt", "b1_pt", "b2_pt",
                  "b0_ct", "b1_ct")

  #garante a presenca das variaveis necessarias no dataframe
  coefs_vars <- setdiff(coefs_vars, names(bd))

  for(i in coefs_vars){
    bd <- bd %>%
      dplyr::mutate(!!rlang::sym(i) := NA_real_)
  }

  #estima a altura utilizando os coeficientes para cada modelo
  bd <- bd %>%
    dplyr::mutate(h_sf = dplyr::case_when(dap == 0 & !is.na(b0_sf) ~ 0,
                                          TRUE ~ round(exp(b0_sf + (b1_sf*log(hdom)) + (b2_sf*log((dg/dap))) + (b3_sf*(1/dap)) + (b4_sf*(1/(dap*idade)))), 4)),
                  h_ss = dplyr::case_when(dap == 0 & !is.na(b0_ss)~ 0,
                                          TRUE ~ round(exp(b0_ss + (b1_ss*log(hdom)) + (b2_ss*log((dg/dap))) + (b3_ss*(1/dap))), 4)),
                  h_pt = dplyr::case_when(dap == 0 & !is.na(b0_pt) ~ 0,
                                          TRUE ~ round(exp(b0_pt + (b1_pt*log(dap)) + (b2_pt*(log(dap)^2))), 4)),
                  h_ct = dplyr::case_when(dap == 0 & !is.na(b0_ct) ~ 0,
                                          TRUE ~ round(exp(b0_ct + (b1_ct*log(1/dap))), 4)))

  #remove alturas estimadas < 1.6
  bd <- bd %>%
    dplyr::mutate(dplyr::across(tidyselect::matches("h_[[:lower:]]{2}"),
                                ~ dplyr::case_when(.x < 1.6 ~ 0,
                                                   TRUE ~ .x)))

  #metodos de selecao do modelo a ser utiliado
  if(!is.null(priority)){

    priority <- priority %>%
      stringr::str_replace("scolforo_simp", "ss") %>%
      stringr::str_replace("scolforo", "sf") %>%
      stringr::str_replace("pettersen", "pt") %>%
      stringr::str_replace("curtis", "ct")

    h1 <- rlang::sym(paste0("h_", priority[[1]]))
    h2 <- rlang::sym(paste0("h_", priority[[2]]))
    h3 <- rlang::sym(paste0("h_", priority[[3]]))
    h4 <- rlang::sym(paste0("h_", priority[[4]]))

    bd <- bd %>%
      dplyr::mutate(hest = dplyr::case_when(!is.na(!!h1) ~ !!h1,
                                             !is.na(!!h2) ~ !!h2,
                                             !is.na(!!h3) ~ !!h3,
                                             !is.na(!!h4) ~ !!h4))

    bd <- bd %>%
      dplyr::mutate(hip_sel = dplyr::case_when(!is.na(!!h1) ~ priority[[1]],
                                               !is.na(!!h2) ~ priority[[2]],
                                               !is.na(!!h3) ~ priority[[3]],
                                               !is.na(!!h4) ~ priority[[4]]))



  } else if(by.hip_sel & "hip_sel" %in% names(bd)){

    bd <- bd %>%
      dplyr::mutate(hest = dplyr::case_when(hip_sel == "scolforo" ~ h_sf,
                                             hip_sel == "scolforo_simp" ~ h_ss,
                                             hip_sel == "pettersen" ~ h_pt,
                                             hip_sel == "curtis" ~ h_ct))

  } else {

    stop("nenhum metodo de selecao de modelo aplicado")

  }

  #salva os valores de altura observados em uma nova variavel
  bd <- bd %>%
    dplyr::mutate(hobs = h)

  #salva a altura de quebra
  bd <- bd %>%
    dplyr::mutate(hqbr = dplyr::case_when(cod1 == "Q" | cod2 == "Q" ~ round(hobs, 4),
                                              TRUE ~ NA_real_))

  #gera o campo h com as alturas totais (inclusive das quebradas) ordenando observadas e estimadas
  bd <- bd %>%
    dplyr::mutate(h = dplyr::case_when(hobs > 0 &
                                         (cod1 != "Q" | is.na(cod1)) &
                                         (cod2 != "Q" | is.na(cod2))  ~ round(hobs, 4),
                                       TRUE ~ hest)) %>%
    dplyr::relocate(h, .after = hqbr)

  bd <- bd %>%
    dplyr::select(-tidyselect::matches("^b[0-9]_*"))

  return(bd)

}




