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
#'  dc <- cal_var(bd, by.assmann = F, cor_area = F, im = c("centro", "rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"))
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


cal_var <- function(x,
                    by.assmann = FALSE,
                    cor_area = FALSE,
                    im = c("centro", "rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med")){




  # calulo dap, g, h, area_parc ---------------------------------------------

  x <- x %>%
    dplyr::mutate(dap =  round(cap/(pi*10), 4),
                  dap = tidyr::replace_na(dap, 0),
                  g = round(pi*(dap/200)^2, 4),
                  h = round(alt/10, 4),
                  h = tidyr::replace_na(h, 0))


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

  x <- x %>%
    dplyr::mutate(area_parc = round(area_parc, 4))


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

    ## calculo pelo metodo hibrido, prioriza pelo codigo H, na falta deste tenta pelo metodo de assmann
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

  x <- x %>%
    dplyr::mutate(hdom = round(hdom, 4),
                  ddom = round(ddom, 4))

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

  x <- x %>%
    dplyr::mutate(dg = round(dg, 4),
                  ab = round(ab, 4))



  # data e ano de plt -------------------------------------------------------

  x <- x %>%
    dplyr::mutate(dt_plt = dplyr::case_when(rotacao > 1 ~ dt_int,
                                            TRUE ~ dt_plt),
                  ano_plt = format(dt_plt, "%Y"), .after = dt_plt)


  # calculo da idade --------------------------------------------------------

  x <- x %>%
    dplyr::mutate(idade = round(as.numeric(difftime(dt_med, dt_plt,  units = "days"))/365.25, 4),
                  classe_idade = round(idade))


  # calculo fuste -----------------------------------------------------------

  ia <- c(im, "linha", "arvore")

  x <- x %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(ia))) %>%
    dplyr::mutate(temp_fuste = dplyr::case_when(any(cod1 == "B" | cod2 == "B") ~ "b",
                                                TRUE ~ NA_character_),
                  fuste = dplyr::case_when(temp_fuste == "b"  ~ dplyr::row_number(),
                                           TRUE ~ 1), .after = arvore) %>%
    dplyr::ungroup() %>%
    dplyr::select(-temp_fuste)

  # calculo densidade -------------------------------------------------------

  cod_cova <- c("Y", "Z")
  cod_fuste <- c("F", "M", "N", "CA", "CR", "Y", "Z")
  #para calculo percentual de caidas
  cod_percC <- c("F", "M", "Y", "Z")


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

  # ajusta codigo VV
  x <- x %>%
    dplyr::mutate(cod1 = dplyr::case_when(cod1 == "VV" ~ "W",
                                          TRUE ~ cod1)) %>%
    dplyr::mutate(cod2 = dplyr::case_when(cod2 == "VV" ~ "W",
                                          TRUE ~ cod2)) %>%
    dplyr::mutate(cod1 = dplyr::case_when(cod1 == "ZA" ~ "Z",
                                          TRUE ~ cod1)) %>%
    dplyr::mutate(cod2 = dplyr::case_when(cod2 == "ZA" ~ "Z",
                                          TRUE ~ cod2))


  # numero codigos 1
  cods <- x %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im, "cod1")))) %>%
    dplyr::tally() %>%
    dplyr::rename(cod = cod1, n1 = n) %>%
    dplyr::mutate(cod = tidyr::replace_na(cod, "SC"))

  # numero codigos 2
  temp_cods <- x %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im, "cod2")))) %>%
    dplyr::tally() %>%
    dplyr::filter(!is.na(cod2)) %>%
    dplyr::rename(cod = cod2, n2 = n)

  # numero de codigos em cod1 e cod2
  cods <- dplyr::full_join(cods, temp_cods) %>%
    dplyr::mutate(n1 = tidyr::replace_na(n1, 0),
                  n2 = tidyr::replace_na(n2, 0))

  # correcao do codigo B - conta quantos codigos B estao repetidos em cada arvore
  temp_codb <- x %>%
    dplyr::filter(fuste > 1) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im)))) %>%
    dplyr::tally(name = "temp_codb") %>%
    dplyr::mutate(cod = "B")

  # numero total de codigos
  cods <- dplyr::left_join(cods, temp_codb) %>%
    dplyr::mutate(temp_codb = tidyr::replace_na(temp_codb, 0)) %>%
    dplyr::mutate(n = (n1 + n2) - temp_codb) %>%
    dplyr::mutate(n = dplyr::case_when(n < 0 ~ 0,
                                       T ~ n)) %>%
    dplyr::select(-n1, -n2, -temp_codb )

  # trasendo o n de covas, fuste e arvores para a base de codigos
  cods <- dplyr::left_join(cods,
                           dplyr::distinct_all( dplyr::select(x,
                                                              tidyselect::all_of(c(im, "covas_parc", "arvores_parc", "fustes_parc", "percC_parc")))))



  #cosistir:
  # codigo B e codigo em todos os fustes
  # nao pode haver codigo J e K na mesmo fuste - sem tem os 2 ele é K
  # nao pode haver codigo N com J ou K na mesmo fuste - sem tem N ele é N
  # nao pode haver o codigo M ou N em conjunto com C

  #calculo da proporcao de cada codigo
  cods <- cods %>%
    dplyr::mutate(prop_cod = dplyr::case_when(cod == "SC" ~ round((n / covas_parc)*100, 2), #sem codigo
                                              cod == "A" ~ round((n / fustes_parc)*100, 2), #bifurcada acima
                                              cod == "B" ~ round((n / arvores_parc)*100, 2), #bifurcada abaixo
                                              cod == "CA" ~ round((n / percC_parc)*100, 2), #caida deitada
                                              cod == "CR" ~ round((n / percC_parc)*100, 2), #caida raiz
                                              cod == "D" ~ round((n / fustes_parc)*100, 2), #dominada
                                              cod == "E" ~ round((n / arvores_parc)*100, 2), #marcada para sair
                                              cod == "F" ~ round((n / covas_parc)*100, 2), #falha
                                              cod == "G" ~ round((n / fustes_parc)*100, 2), #geada
                                              cod == "H" ~ round((n / fustes_parc)*100, 2), #dominante
                                              cod == "I" ~ round((n / fustes_parc)*100, 2), #praga doenca
                                              cod == "J" ~ round((n / fustes_parc)*100, 2),#macaco_com_rec
                                              cod == "K" ~ round((n / fustes_parc)*100, 2),#macaco_sem_rec
                                              cod == "L" ~ round((n / fustes_parc)*100, 2),#atacada por vespa
                                              cod == "M" ~ round((n / covas_parc)*100, 2), #morta
                                              cod == "N" ~ round((n / covas_parc)*100, 2), #morta macaco
                                              cod == "O" ~ round((n / covas_parc)*100, 2), #
                                              cod == "P" ~ round((n / fustes_parc)*100, 2), #pontera seca
                                              cod == "Q" ~ round((n / fustes_parc)*100, 2), #quebrada
                                              cod == "R" ~ round((n / fustes_parc)*100, 2), #rebrota
                                              cod == "S" ~ round((n / fustes_parc)*100, 2), #formiga
                                              cod == "T" ~ round((n / fustes_parc)*100, 2), #torta
                                              cod == "U" ~ round((n / fustes_parc)*100, 2), #fox_tail
                                              cod == "V" ~ round((n / fustes_parc)*100, 2), #inclinada vento
                                              cod == "W" ~ round((n / fustes_parc)*100, 2), #deitada vento
                                              cod == "X" ~ round((n / fustes_parc)*100, 2), #verida na base
                                              cod == "Y" ~ round((n / covas_parc)*100, 2), #
                                              cod == "Z" ~ round((n / fustes_parc)*100, 2))) #

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
          "c_N", #morta macaco
          "c_O", #
          "c_P", #pontera seca
          "c_Q", #quebrada
          "c_R", #rebrota
          "c_S", #formiga
          "c_T", #torta
          "c_U", #fox_tail
          "c_V", #inclinada vento
          "c_W", #deitada vento
          "c_X", #verida na base
          "c_Y", #
          "c_Z") #


  cods <- cods %>%
    dplyr::select(-covas_parc, -arvores_parc, -fustes_parc, -percC_parc,  -n) %>%
    dplyr::mutate(cod = paste0("c_", cod)) %>%
    dplyr::filter(cod %in% cd) %>%
    dplyr::mutate(cod = factor(cod, levels = cd)) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im)))) %>%
    tidyr::complete(cod, fill = list(n = 0, prop_cod = 0)) %>%
    tidyr::pivot_wider(names_from = cod, values_from = prop_cod) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("c_"), ~ dplyr::case_when(is.infinite(.) ~ 0,
                                                                                  TRUE ~ .)))


  #join cods
  x <- x %>%
    dplyr::left_join(cods)

  #remove campos temporarios
  x <- x %>%
    dplyr::select(-id_temp, -covas_parc, -arvores_parc, -fustes_parc, -percC_parc)

  return(x)
}
