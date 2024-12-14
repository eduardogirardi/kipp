#' Simula a estração de toras de um fuste utilizando a equação de afilamento (Schöepfer, 1996)
#'
#' @description
#' Essa função ira simular a estração de toras da arvore utilizando uma função de afilamento (Schöepfer, 1996) em funcao de um determinado diametro, altura total, altura de toco e descricao do sortimento apontada. \cr
#'
#' @param h altura total da arvore - m.
#' @param dap diametro a 1,3m - m.
#' @param sortimento um data frame contendo a descrição dos sortimento. Para maiores informações checar a documentação da \code{\link{read_sort()}}.
#' @param h_toco altura de corte da arvore em metros. O Padrão é 0.1m
#' @param dwg se TRUE, sera considerado apenas o ultimo produto dos sortimentos ate a h_dwg. O padrão é FALSE.
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
#' @return um data frame contendo a relação de toras extraidas na simulação
#'
#'
#' @examples
#'
#'
#' poly5_logs(dap, h, coef_poli, sortimento)
#'
#' @export
#'
#'
#'



# tr <- 1
# dap <- bd_tree$dap[tr]
# h <- bd_tree$h[tr]
# b0 <- bd_tree$b0[tr]
# b1 <- bd_tree$b1[tr]
# b2 <- bd_tree$b2[tr]
# b3 <- bd_tree$b3[tr]
# b4 <- bd_tree$b4[tr]
# b5 <- bd_tree$b5[tr]
# sortimento <- bd_tree$assort[[tr]]
# h_toco <- bd_tree$h_toco[tr]
# h_toco <- 0.1
# dwg <- bd_tree$dwg[tr]
# qbr <- bd_tree$qbr[tr]
# h_dwg <- bd_tree$h_dwg[tr]
# h_dwg <- 0
# h_qbr <- bd_tree$h_quebra[tr]
# h_qbr <- 0
# suprimir <- bd_tree$suprimir[tr]


poly2log <-  function(h,
                      dap,
                      sortimento,
                      h_toco = .1,
                      dwg = F,
                      h_dwg,
                      qbr = F,
                      h_qbr,
                      suprimir = F,
                      b0 = b0,
                      b1 = b1,
                      b2 = b2,
                      b3 = b3,
                      b4 = b4,
                      b5 = b5) {

  #ajusta falta de argumentos

  if (missing(h_toco) | is.na(h_toco)) {
    h_toco <- .1
  }
  if (missing(dwg) | is.na(dwg)) {
    dwg <- FALSE
  }
  if (missing(suprimir) | is.na(suprimir)) {
    suprimir <- FALSE
  }
  if (missing(qbr) | is.na(qbr)) {
    qbr <- FALSE
  }

  #ajusta falta das alturas de apoio
  if(dwg & (missing(h_dwg) | is.na(h_dwg))){
    h_dwg <- 0
  }

  if(qbr & (missing(h_qbr) | is.na(h_qbr) | h_qbr == 0)){
    h_qbr <- h * 0.7
  }

  if(qbr & dwg){
    h_dwg <- 0
  }

  if(!qbr & !dwg){
    h_dwg <- h
    h_qbr <- h
  }

  h0 <- h_toco

  #rename
  colnames(sortimento) <- c("name", "dpf", "mincomp", "maxcomp", "loss", "molsap")

  #ordena
  sortimento <- sortimento[order(sortimento$dpf, decreasing = T),]

  logs <- tibble::tibble(
    name = character(),
    log = numeric(),
    li = numeric(),
    h0 = numeric(),
    hi = numeric(),
    d0 = numeric(),
    di = numeric(),
    vi = numeric())

  if (any(c(is.na(dap), is.na(h), suprimir))) {
    logs[1, ] <- NA
  } else {

    logs[1, ] <- NA
    nlogs <- 0
    vsort <- 0

    for (i in seq_along(sortimento$name)){

      namesort <- sortimento[[i, 1]]
      dsort <- sortimento[[i, 2]]
      cminsort <- sortimento[[i, 3]]
      cmaxsort <- sortimento[[i, 4]]
      psort <- sortimento[[i, 5]]/100
      harv_dsort <- round(kipp::poly2hi(di = dsort, h = h, dap = dap, b0, b1, b2, b3, b4, b5),3)

      if ((dwg & !qbr & i < nrow(sortimento)) & harv_dsort > h_dwg) {
        harv_dsort <- h_dwg
      }

      if(qbr & !dwg & harv_dsort > h_qbr){
        harv_dsort <- h_qbr
      }

      if(qbr & dwg){
        if(i < nrow(sortimento)){
          harv_dsort <- h_dwg
        }
        else{
          harv_dsort <- h_qbr
        }
      }

      while (h0 <= harv_dsort - cminsort) {

        #name & log
        nlogs <- nlogs + 1

        logs[nlogs, 1] <- namesort
        logs[nlogs, 2] <- nlogs

        #comprimento tora
        csort <- ifelse(h0 <= harv_dsort - cmaxsort, cmaxsort, harv_dsort-h0)
        logs[nlogs, 3] <- csort

        #h0
        logs[nlogs, 4] <- h0

        #hi
        h0 <- h0 + csort + psort
        logs[nlogs, 5] <- h0

        #v
        vsort <- kipp::poly2vol(hb = h0 - (psort + csort), hf = h0 - psort, h = h, dap = dap, b0, b1, b2, b3, b4, b5)
        logs[nlogs, 8] <- vsort
      }
    }
  }

  logs$d0 <- round(kipp::poly2di(logs$h0, h = h, dap = dap, b0, b1, b2, b3, b4, b5),1)
  logs$di <- round(kipp::poly2di(logs$hi, h = h, dap = dap, b0, b1, b2, b3, b4, b5),1)


  return(logs)
}


