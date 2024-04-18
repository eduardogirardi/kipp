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


cal_var <- function(x,
                    by.assmann = FALSE,
                    cor_area = FALSE,
                    im = c("rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med")){


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
                  ab = (sum(g[!cod1 %in% cs &
                               !cod2 %in% cs])*10000)/area_parc) %>%
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
  #para calculo percentual de caidas
  cod_percC <- c("F", "M", "Y", "Z", "ZA")


  x <- x %>%
    dplyr::mutate(id_temp = paste(linha, arvore, fuste, sep = "_")) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im)))) %>%
    dplyr::mutate(covas_parc = dplyr::n_distinct(id_temp[!cod1 %in% cod_cova &
                                                    !cod2 %in% cod_cova &
                                                    fuste == 1]),
                  arvores_parc = dplyr::n_distinct(id_temp[!cod1 %in% cod_fuste &
                                                      !cod2 %in% cod_fuste &
                                                      fuste == 1]),
                  fustes_parc = dplyr::n_distinct(id_temp[!cod1 %in% cod_fuste &
                                                     !cod2 %in% cod_fuste]),
                  percC_parc = dplyr::n_distinct(id_temp[!cod1 %in% cod_percC &
                                                           !cod2 %in% cod_percC &
                                                           fuste == 1]),
                  covas = round((10000*covas_parc)/max(area_parc),0),
                  arvores = round((10000*arvores_parc)/max(area_parc),0),
                  fustes = round((10000*fustes_parc)/max(area_parc),0)) %>%
    #dplyr::select(-covas_parc, -arvores_parc, -fustes_parc) %>%
    dplyr::ungroup()



  # codigos qualitativos ----------------------------------------------------

  #n codigos 1
  cods <- x %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im, "cod1")))) %>%
    dplyr::tally() %>%
    dplyr::rename(cod = cod1, n1 = n) %>%
    dplyr::mutate(cod = tidyr::replace_na(cod, "SC"))

  #n codigos 2
  temp_cods <- x %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im, "cod2")))) %>%
    dplyr::tally() %>%
    dplyr::filter(!is.na(cod2)) %>%
    dplyr::rename(cod = cod2, n2 = n)

  #n de codigos em cod1 e cod2
  cods <- dplyr::full_join(cods, temp_cods) %>%
    dplyr::mutate(n1 = tidyr::replace_na(n1, 0),
                  n2 = tidyr::replace_na(n2, 0),)

  #correcao do codigo B - conta quantos codigos B estao repetidos em cada arvore
  temp_codb <- x %>%
    dplyr::filter(fuste > 1) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im)))) %>%
    dplyr::tally(name = "temp_codb") %>%
    dplyr::mutate(cod = "B")

  #n total de codigos
  cods <- dplyr::left_join(cods, temp_codb) %>%
    dplyr::mutate(temp_codb = tidyr::replace_na(temp_codb, 0)) %>%
    dplyr::mutate(n = (n1 + n2) - temp_codb) %>%
    dplyr::select(-n1, -n2, -temp_codb )

  #trasendo o n de covas, fuste e arvores para a base de codigos
  cods <- dplyr::left_join(cods,
                           dplyr::distinct_all( dplyr::select(x,
                                                              tidyselect::all_of(c(im, "covas_parc", "arvores_parc", "fustes_parc", "percC_parc")))))



  #cosistir:
  #codigo B e codigo em todos os fustes
  #nao pode haver codigo J e K na mesmo fuste - sem tem os 2 ele é K
  #nao pode haver codigo N com J ou K na mesmo fuste - sem tem N ele é N
  # nao pode haver o codigo M ou N em conjunto com C

  #calculo da proporcao de cada codigo
  cods <- cods %>%
    dplyr::mutate(prop_cod = dplyr::case_when(cod == "SC" ~ (n / covas_parc)*100, #sem codigo
                                              cod == "A" ~ (n / fustes_parc)*100, #bifurcada acima
                                              cod == "B" ~ (n / arvores_parc)*100, #bifurcada abaixo
                                              cod == "CA" ~ (n / percC_parc)*100, #caida deitada
                                              cod == "CR" ~ (n / percC_parc)*100, #caida raiz
                                              cod == "D" ~ (n / fustes_parc)*100, #dominada
                                              cod == "E" ~ (n / arvores_parc)*100, #marcada para sair
                                              cod == "F" ~ (n / covas_parc)*100, #falha
                                              cod == "G" ~ (n / fustes_parc)*100, #geada
                                              cod == "H" ~ (n / fustes_parc)*100, #dominante
                                              cod == "I" ~ (n / fustes_parc)*100, #praga doenca
                                              cod == "J" ~ (n / fustes_parc)*100, #macaco_com_rec
                                              cod == "K" ~ (n / fustes_parc)*100, #macaco_sem_rec
                                              cod == "L" ~ (n / fustes_parc)*100, #atacada por vespa
                                              cod == "M" ~ (n / covas_parc)*100, #morta
                                              cod == "N" ~ (n / covas_parc)*100, #morta macaco
                                              cod == "P" ~ (n / fustes_parc)*100, #pontera seca
                                              cod == "Q" ~ (n / fustes_parc)*100, #quebrada
                                              cod == "R" ~ (n / fustes_parc)*100, #rebrota
                                              cod == "S" ~ (n / fustes_parc)*100, #formiga
                                              cod == "T" ~ (n / fustes_parc)*100, #torta
                                              cod == "U" ~ (n / fustes_parc)*100, #fox_tail
                                              cod == "V" ~ (n / fustes_parc)*100, #inclinada vento
                                              cod == "W" ~ (n / fustes_parc)*100, #deitada vento
                                              cod == "X" ~ (n / fustes_parc)*100)) #verida na base

  cd <- c("c_SC", #sem codigo
          "c_A", #bifurcada acima
          "c_B",  #bifurcada abaixo
          "c_CA", #caida deitada
          "c_CR", #caida raiz
          "c_D", #dominada
          "c_E", #marcada para sair
          "c_F",  #falha
          "c_G", #geada
          "c_H", #dominante
          "c_I", #praga doenca
          "c_J", #macaco_com_rec
          "c_K", #macaco_sem_rec
          "c_L", #atacada por vespa
          "c_M", #morta
          "c_N",  #morta macaco
          "c_P", #pontera seca
          "c_Q", #quebrada
          "c_R", #rebrota
          "c_S", #formiga
          "c_T", #torta
          "c_U", #fox_tail
          "c_V", #inclinada vento
          "c_W", #deitada vento
          "c_X") #verida na base


  cods <- cods %>%
    dplyr::select(-covas_parc, -arvores_parc, -fustes_parc, -percC_parc,  -n) %>%
    dplyr::mutate(cod = paste0("c_", cod)) %>%
    dplyr::mutate(cod = factor(cod, levels = cd)) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im)))) %>%
    tidyr::complete(cod, fill = list(n = 0, prop_cod = 0)) %>%
    tidyr::pivot_wider(names_from = cod, values_from = prop_cod) %>%
    dplyr::ungroup()


  #join cods
  x <- x %>%
    dplyr::left_join(cods)

  #remove campos temporarios
  x <- x %>%
    dplyr::select(-id_temp, -covas_parc, -arvores_parc, -fustes_parc, -percC_parc)

  return(x)
}
