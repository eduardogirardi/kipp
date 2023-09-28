#' Calculo de variaveis
#'
#' Esta função ira calcular novas variaveis a nivel de arvore e de populacao \cr
#'
#' Serão calculadas as variaveis a nivel de arvore:\cr
#' \strong{dap} - diametro a 1,3m - cm\cr
#' \strong{b} - area transversal - m²\cr
#' \strong{h} - altura total - m\cr
#' \cr
#'
#'  Serão calculadas as variaveis a nivel de populacao:\cr
#' \strong{area_parc} - area da parcela - m²\cr
#' \strong{dg} - diametro medio quadratico - cm\cr
#' \strong{ab} - area basal - m²\cr
#' \strong{hdom} - altura dominante - m\cr
#' \strong{ddom} - diametro dominante - cm\cr
#' \strong{idade} - idade - anos\cr
#' \strong{covas} - numero de covas - covas/ha\cr
#' \strong{arvores} - numero de arvores - arvores/ha\cr
#' \strong{fustes} - numero de fustes - fustes/ha\cr
#'
#'
#'
#' @param x dataframe contendo as informacoes padronizadas do coletor - output da funcao \code{\link{cad_join()}}
#' @param by.assmann o parametro by.assmann = FALSE calcula o hdom e ddom pelo metodo hibrido priorizando o codigo H quando presente cod1 e cod2. Caso nao tenha nenhum codigo H ele tenta realizar o calculo nas parcelas restantes utilizando o principio de Assmann(1970) a partir dos cap e alturas medidos. Quando este TRUE ocalculo ignora o codigo H e realiza o calculo apenas utilizando o principio de Assmann(1970)
#' @param cor_area quando o valor TRUE, indica que deve ser realizada a correção de area. E utilizado o valor maximo da inclinação para realização da correção. O campo **forma** deve conter a informacao `C` para indicar parcelas circulares, neste caso apenas a informacao do **lado1** é utiliada como raio da parcela.
#' @param im vetor com campos identificadores de medicao.
#'
#' @return um dataframe de mesma estrutura com novas variaveis calculadas
#'
#' @examples
#'
#'  dc <- cal_var(bd, by.assmann = F, cor_area = F, im = c("rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"))
#'
#' @export
#'
#esta função ira padronizar, calcular novas variaveis necessarias para ajuste e aplicação de hipsometrica,
#e variaveis necessarias no resumo de parcelas
#campos necessarios:
#
#1 - ajusta unidades
#conversao do cap: mm -> cm
#conversao dos lados: dm -> m
#conversao da alt de dm -> m
#
#2 - calculo do dap e g
# dap calculado em cm
#
#3 - calculo da area
# o codigo C em forma indica parcela circular
# parametro "cor_area = FALSE" indica se deve ou nao ser realizada a correção de area
# e utilizado o valor maximo da inclinação para realização da correção
#
#4 - calculo hdom, ddom
# o parametro by.assmann = FALSE calcula o hdom e ddom pelo metodo hibrido priorizando o codigo H  quando presente cod1 e cod2
# caso nao tenha nenhum codigo H ele tenta realizar o calculo nas parcelas restantes utilizando o principio
# de assmann apartir dos cap e alturas medidos. o parametro by.assmann = TRUE ignora o codigo H e realiza o calculo apenas
# pelo principio de assmann apartir dos caps e alturas medidos
#
#5 - calculo dg, ab
#6 - calculo da idade
#7 - adiciona numero de fuste
#8 - calcula densidade de plantio

#dplyr::mutate
#dplyr::case_when
#pracma::deg2rad
#dplyr::filter
#dplyr::group_by
#dplyr::group_modify
#dplyr::across
#tidyselect::all_of
#dplyr::summarise
#dplyr::ungroup
#dplyr::left_join
#dplyr::slice_max
#dplyr::first
#dplyr::row_number


cal_var <- function(x, by.assmann = FALSE, cor_area = FALSE, im = c("rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med")){


  # calulo dap, g, h, area_parc ---------------------------------------------

  x <- x %>%
    dplyr::mutate(dap =  cap/(pi*10),
                  g = pi*(dap/200)^2,
                  h = alt/10)


  # calcula da area da parcela ----------------------------------------------

  #funcao de conversao de graus para radiano
  deg2rad <- function(deg) {(deg * pi) / (180)}

  if (cor_area){
    x <- x %>%
      dplyr::mutate(area_parc = dplyr::case_when(forma == "C" ~ (( (lado1/10) ^2) * pi) * cos( pracma::deg2rad( pmax(inc1, inc2, na.rm =T))),
                                                 TRUE ~ ((lado1/10) * (lado2/10)) * cos( pracma::deg2rad( pmax(inc1, inc2, na.rm =T)))))
  } else {
    x <- x %>%
      dplyr::mutate(area_parc = dplyr::case_when(forma == "C" ~ (((lado1/10)^2) * pi),
                                                 TRUE ~ ((lado1/10) * (lado2/10))))
  }


  # Calculando hdom, ddom ---------------------------------------------------

  if(by.assmann){

    ## calculo pelo metodo assmann
    #codigos removidos
    cs <- c("Q", "U", "F", "M", "N", "CA", "CR", "Y", "Z", "ZA")

    hdomtemp <- x %>%
      dplyr::mutate(n_assmann  = round(area_parc/100)) %>%
      dplyr::filter(h > 0 &
                      !cod1 %in% cs &
                      !cod2 %in% cs) %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(im))) %>%
      dplyr::group_modify(~ .x %>%
                            dplyr::slice_max(cap, n = dplyr::first(.x$n_assmann))) %>%
      dplyr::summarise(n_assmann = max(n_assmann, na.rm = T),
                       n_tree = dplyr::n(),
                       hdom = mean(h, na.rm = T),
                       ddom = mean(dap, na.rm = T)) %>%
      dplyr::ungroup()

    x <- x %>%
      dplyr::left_join(hdomtemp)


  }else{

    ## calculo pelo metodo hibrido, prioriza melo codigo H, na falta deste tenta pelo metodo de assmann
    #codigos removidos
    cs <- c("F", "M", "N", "CA", "CR", "Y", "Z", "ZA")

    x <- x %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(im))) %>%
      dplyr::mutate(hdom = mean(h[(cod1 == "H" | cod2 == "H") &
                                    h != 0 &
                                    !cod1 %in% cs &
                                    !cod2 %in% cs] , na.rm = T)) %>%
      dplyr::mutate(ddom = mean(dap[(cod1 == "H" | cod2 == "H") &
                                      h != 0 &
                                      !cod1 %in% cs &
                                      !cod2 %in% cs] , na.rm = T)) %>%
      dplyr::ungroup()


    #é necessario verificar se existe parcelas a serem calculada pelo metodos assmann

    n_obs <- nrow(x %>%
                    dplyr::filter(hdom == 0 | is.na(hdom)) %>%
                    dplyr::filter(!cod1 %in% cs &
                                    !cod2 %in% cs))

    if(n_obs > 0){
      hdomtemp <- x %>%
        dplyr::filter(hdom == 0 | is.na(hdom)) %>%
        dplyr::mutate(n_assmann  = round(area_parc/100)) %>%
        dplyr::filter(!cod1 %in% cs &
                        !cod2 %in% cs) %>%
        dplyr::group_by(across(tidyselect::all_of(im))) %>%
        dplyr::group_modify(~ .x %>%
                              dplyr::slice_max(cap, n = dplyr::first(.x$n_assmann))) %>%
        dplyr::filter(h > 0) %>%
        dplyr::summarise(n_assmann = max(n_assmann, na.rm = T),
                         n_tree = dplyr::n(),
                         hdom_ass = mean(h, na.rm = T),
                         ddom_ass = mean(dap, na.rm = T)) %>%
        dplyr::ungroup()


      x <- x %>%
        dplyr::left_join(hdomtemp) %>%
        dplyr::mutate(hdom = dplyr::case_when(hdom == 0 | is.na(hdom) ~ hdom_ass,
                                              TRUE ~ hdom),
                      ddom = dplyr::case_when(ddom == 0 | is.na(ddom) ~ ddom_ass,
                                              TRUE ~ ddom)) %>%
        dplyr::select(-n_assmann, -n_tree, -hdom_ass, -ddom_ass)
    }
  }

  # calculo do dg e ab ------------------------------------------------------

  #codigos removidos
  cs <- c("F", "M", "N", "CA", "CR", "Y", "Z", "ZA")
  x <- x %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(im))) %>%
    dplyr::mutate(dg = sqrt(mean(dap[dap > 0 &
                                       !cod1 %in% cs &
                                       !cod2 %in% cs]^2 , na.rm = T)),
                  ab = sum(g[!cod1 %in% cs &
                               !cod2 %in% cs])) %>%
    dplyr::ungroup()


  # calculo da idade --------------------------------------------------------

  x <- x %>%
    dplyr::mutate(idade = dplyr::case_when(rotacao > 1 ~ as.numeric(difftime(dt_med, dt_int,  units = "days"))/365.25,
                                           TRUE ~ as.numeric(difftime(dt_med, dt_plt,  units = "days"))/365.25),
                  classe_idade = round(idade))


  # calculo fuste -----------------------------------------------------------

  ia <- c(im, "linha", "arvore")

  x <- x %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(ia))) %>%
    dplyr::mutate(fuste = dplyr::row_number(), .after = arvore) %>%
    dplyr::ungroup()


  # calculo densidade -------------------------------------------------------

  cod_cova <- c("Y", "Z", "ZA")
  cod_fuste <- c("F", "M", "N", "CA", "CR", "Y", "Z", "ZA")


  x <- x %>%
    dplyr::mutate(id_temp = paste(linha, arvore, fuste, sep = "_")) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im)))) %>%
    dplyr::mutate(covas_parc = n_distinct(id_temp[!cod1 %in% cod_cova &
                                                    !cod2 %in% cod_cova &
                                                    fuste == 1]),
                  arvores_parc = n_distinct(id_temp[!cod1 %in% cod_fuste &
                                                      !cod2 %in% cod_fuste &
                                                      fuste == 1]),
                  fustes_parc = n_distinct(id_temp[!cod1 %in% cod_fuste &
                                                     !cod2 %in% cod_fuste]),
                  covas = round((10000*covas_parc)/max(area_parc),0),
                  arvores = round((10000*arvores_parc)/max(area_parc),0),
                  fustes = round((10000*fustes_parc)/max(area_parc),0)) %>%
    dplyr::select(-covas_parc, -arvores_parc, -fustes_parc) %>%
    dplyr::ungroup()

  return(x)
}
