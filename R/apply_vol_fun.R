#' Caculos de volumes
#'
#' @description
#' Realiza os caculos de volumes totais e comerciais abertos por sortimentos \cr
#'
#' @param bd base de dados para aplicação do volume
#' @param cores n° de nucleos de processamento. \strong{Padrão sao 4}
#' @param im vetor com campos identificadores de medicao
#' @param h altura total da arvore - m
#' @param dap diametro a 1,3m - m
#' @param sortimento um data frame contendo a descrição dos sortimento. Para maiores informações checar a documentação da \code{\link{read_sort()}}.
#' @param h_toco altura de corte da arvore em metros. O Padrão é 0.1m
#' @param dwg se TRUE, sera considerado apenas o ultimo produto dos sortimentos ate a h_dwg. O padrão é FALSE
#' @param h_dwg altura em metros (caso dwg for TRUE) ate a qual sera realizado a extração de toras seguindo os produtos descritos no sortimento, apos essa altura sera apenas utilizado o ultimo sortimento (menor D.P.F.)
#' @param qbr se TRUE, sera simulada a extração de toras apenas ate a h_qbr. O padrao é falso FALSE.
#' @param h_qbr altura em metros (caso qbr for TRUE) ate a qual sera realizado a extração de toras. Caso qbr for TRUE e o valor de h_qbr for NA ou 0, esse sera calculado utilizando 70% da altura toral.
#' @param suprimir se TRUE, nao sera simulada a extração de nenhuma tora. Os valores de volume serao zerados. O padrão é FALSE. Aplica-se a DAPs e altura total zeradas, assim como falta de coeficientes da curva de afilamento.
#' @param b0 b0 do polinomio (Schöepfer, 1996)
#' @param b1 b1 do polinomio (Schöepfer, 1996)
#' @param b2 b2 do polinomio (Schöepfer, 1996)
#' @param b3 b3 do polinomio (Schöepfer, 1996)
#' @param b4 b4 do polinomio (Schöepfer, 1996)
#' @param b5 b5 do polinomio (Schöepfer, 1996)
#'
#' @return um data frame contendo campos calculados com os volumes correspondentes
#'
#'
#' @examples
#'
#'
#' apply_vol(bd,
#'           cores = 4,
#'           im = c("centro", "rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"),
#'           h,
#'           dap,
#'           sortimento,
#'           h_toco,
#'           dwg,
#'           h_dwg,
#'           qbr,
#'           h_qbr,
#'           suprimir,
#'           b0,
#'           b1,
#'           b2,
#'           b3,
#'           b4,
#'           b5)
#'
#' @export
#'
#'
#'

apply_vol <- function(bd,
                      im = c("centro", "rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"),
                      cores = 4,
                      h,
                      dap,
                      sortimento,
                      h_toco,
                      dwg,
                      h_dwg,
                      qbr,
                      h_qbr,
                      suprimir,
                      b0,
                      b1,
                      b2,
                      b3,
                      b4,
                      b5){


  h_quo <- rlang::enquo(h)
  dap_quo <- rlang::enquo(dap)
  sortimento_quo <- rlang::enquo(sortimento)
  h_toco_quo <- rlang::enquo(h_toco)
  dwg_quo <- rlang::enquo(dwg)
  h_dwg_quo <- rlang::enquo(h_dwg)
  qbr_quo <- rlang::enquo(qbr)
  h_qbr_quo <- rlang::enquo(h_qbr)
  suprimir_quo <- rlang::enquo(suprimir)
  b0_quo <- rlang::enquo(b0)
  b1_quo <- rlang::enquo(b1)
  b2_quo <- rlang::enquo(b2)
  b3_quo <- rlang::enquo(b3)
  b4_quo <- rlang::enquo(b4)
  b5_quo <- rlang::enquo(b5)


  coluns <- c(rlang::quo_name(h_quo),
              rlang::quo_name(dap_quo),
              rlang::quo_name(sortimento_quo),
              rlang::quo_name(h_toco_quo),
              rlang::quo_name(dwg_quo),
              rlang::quo_name(h_dwg_quo),
              rlang::quo_name(qbr_quo),
              rlang::quo_name(h_qbr_quo),
              rlang::quo_name(suprimir_quo),
              rlang::quo_name(b0_quo),
              rlang::quo_name(b1_quo),
              rlang::quo_name(b2_quo),
              rlang::quo_name(b3_quo),
              rlang::quo_name(b4_quo),
              rlang::quo_name(b5_quo))


  # Verificar se a coluna fornecida existe no data.frame
  if (!all(coluns %in% colnames(bd))) {
    stop("A coluna especificada não existe no data.frame.")
  }

  #prepara processamento paralelo
  future::plan(multisession, workers = cores)
  bd <- bd %>%
    dplyr::mutate(vol = furrr::future_pmap(list(h = !!h_quo,
                                                dap = !!dap_quo,
                                                sortimento = !!sortimento_quo,
                                                h_toco = !!h_toco_quo,
                                                dwg = !!dwg_quo,
                                                h_dwg = !!h_dwg_quo,
                                                qbr = !!qbr_quo,
                                                h_qbr = !!h_qbr_quo,
                                                suprimir = !!suprimir_quo,
                                                b0 = !!b0_quo,
                                                b1 = !!b1_quo,
                                                b2 = !!b2_quo,
                                                b3 = !!b3_quo,
                                                b4 = !!b4_quo,
                                                b5 = !!b5_quo), kipp::poly2log)) %>%
    tidyr::unnest(vol)

  #sumariza sortimento
  bd <- bd %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im, "linha", "arvore", "fuste")))) %>%
    dplyr::mutate(dcom = min(di),
                  hcom = max(hi),
                  htot = dplyr::case_when(cod1 == "Q" | cod2 == "Q" ~ !!h_qbr_quo,
                                          TRUE ~ !!h_quo),
                  vcom = round(sum(vi , na.rm = T),4)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(hcom = tidyr::replace_na(hcom, 0),
                  dcom = tidyr::replace_na(dcom, 0),
                  htot = tidyr::replace_na(htot, 0))


  #calculo dos volumes
  future::plan(multisession, workers = cores)
  bd <- bd %>%
    dplyr::mutate(vtot = case_when(suprimir ~ 0,
                                   TRUE ~ round(furrr::future_pmap_dbl(list(hb = !!h_toco_quo,
                                                                            hf = htot,
                                                                            h = !!h_quo,
                                                                            dap = !!dap_quo,
                                                                            b0 = !!b0_quo,
                                                                            b1 = !!b1_quo,
                                                                            b2 = !!b2_quo,
                                                                            b3 = !!b3_quo,
                                                                            b4 = !!b4_quo,
                                                                            b5 = !!b5_quo), kipp::poly2vol),4)),
                  vpont = vtot - vcom) %>%
    dplyr::mutate(vtot = tidyr::replace_na(vtot, 0),
                  vpont = tidyr::replace_na(vpont, 0))

  return(bd)
}

