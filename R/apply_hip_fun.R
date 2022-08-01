#' @import dplyr
#' @import tidyr
#' @import stringr
NULL
#' Aplicacao dos modelos hipsometricos
#'
#' Aplicação dos modelos hipsometricos previamente ajustados \cr
#'
#'
#' @param bd dataframe com a base dados a ser aplicada os modelos. Output da funcao \code{\link{cal_var()}}
#' @param coefs lista dos coeficientes ajustados. Output da funcao \code{\link{fit_hip()}} ou \code{\link{coef_hip()}}
#' @param priority vetor com a prioridade de modelo a ser utilizado na altura predita. c("scolforo_simp", "scolforo", "pettersen", "curtis")
#' @param by.model_sel se TRUE sera utilizado o modelo pre-definido na na variavel  **model_sel** presente no dataframe **bd**. c("scolforo_simp", "scolforo", "pettersen", "curtis")
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
#' bd$model_sel <- "scolforo"
#'
#' bd <- apply_hip(bd, coefs = hip, priority = NULL, by.model_sel = T)
#'
#' @export
#'


apply_hip <- function(bd, coefs, priority = NULL, by.model_sel = F){

  #converte a classe das variaveis do dataframe
  coefs_aj <- map(coefs, type_convert)

  bd <- reduce(append(coefs_aj, list(bd = bd), after = 0), left_join)

  #defini as variaveis necessarias no dataframe
  coefs_vars <- c("b0_sf", "b1_sf", "b2_sf", "b3_sf", "b4_sf",
                  "b0_ss", "b1_ss", "b2_ss", "b3_ss",
                  "b0_pt", "b1_pt", "b2_pt",
                  "b0_ct", "b1_ct")

  #garante a presenca das variaveis necessarias no dataframe
  coefs_vars <- setdiff(coefs_vars, names(bd))

  for(i in coefs_vars){
    bd <- bd %>%
      mutate(!!sym(i) := NA_real_)
  }

  #estima a altura utilizando os coeficientes para cada modelo
  bd <- bd %>%
    mutate(h_sf = case_when(dap == 0 & !is.na(b0_sf) ~ 0,
                            TRUE ~ exp(b0_sf + (b1_sf*log(hdom)) + (b2_sf*log((dg/dap))) + (b3_sf*(1/dap)) + (b4_sf*(1/(dap*idade))))),
           h_ss = case_when(dap == 0 & !is.na(b0_ss)~ 0,
                            TRUE ~ exp(b0_ss + (b1_ss*log(hdom)) + (b2_ss*log((dg/dap))) + (b3_ss*(1/dap)))),
           h_pt = case_when(dap == 0 & !is.na(b0_pt) ~ 0,
                            TRUE ~ exp(b0_pt + (b1_pt*log(dap)) + (b2_pt*(log(dap)^2)))),
           h_ct = case_when(dap == 0 & !is.na(b0_ct) ~ 0,
                            TRUE ~ exp(b0_ct + (b1_ct*log(1/dap)))))

  #remove alturas estimadas < 1.6
  bd <- bd %>%
    mutate(across(matches("h_[[:lower:]]{2}"),
                  ~ case_when(.x < 1.6 ~ 0,
                              TRUE ~ .x)))

  #metodos de selecao do modelo a ser utiliado
  if(!is.null(priority)){

    priority <- priority %>%
      str_replace("scolforo_simp", "ss") %>%
      str_replace("scolforo", "sf") %>%
      str_replace("pettersen", "pt") %>%
      str_replace("curtis", "ct")

    h1 <- sym(paste0("h_", priority[[1]]))
    h2 <- sym(paste0("h_", priority[[2]]))
    h3 <- sym(paste0("h_", priority[[3]]))
    h4 <- sym(paste0("h_", priority[[4]]))

    bd <- bd %>%
      mutate(h_est = case_when(!is.na(!!h1) ~ !!h1,
                               !is.na(!!h2) ~ !!h2,
                               !is.na(!!h3) ~ !!h3,
                               !is.na(!!h4) ~ !!h4))

  } else if(by.model_sel & "model_sel" %in% names(bd)){

    bd <- bd %>%
      mutate(h_est = case_when(model_sel == "scolforo" ~ h_sf,
                               model_sel == "scolforo_simp" ~ h_ss,
                               model_sel == "pettersen" ~ h_pt,
                               model_sel == "curtis" ~ h_ct))

  } else {

    stop("nenhum metodo de selecao de modelo aplicado")

  }

  #salva os valores de altura observados em uma nova variavel
  bd <- bd %>%
    mutate(alt_obs = alt)

  #calula a variavel alt com os valores estimados na ausencia do observado
  bd <- bd %>%
    mutate(alt = case_when(alt_obs > 0 ~ round(alt_obs),
                           TRUE ~ round(h_est*10)))

}




