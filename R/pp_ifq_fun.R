#' Processamento IFQ
#'
#' Esta função ira processar o IFQ \cr
#'
#'
#'
#'
#' @param x dataframe contendo as informacoes padronizadas do coletor - output da funcao \code{\link{cad_join()}}
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
#
#3 - calculo da area
# o codigo C em forma indica parcela circular
# parametro "cor_area = FALSE" indica se deve ou nao ser realizada a correção de area
# e utilizado o valor maximo da inclinação para realização da correção
#
#
#5 - calculo dg, ab
#6 - calculo da idade
#7 - adiciona numero de fuste
#8 - calcula densidade de plantio


pp_ifq <- function(x, cor_area = FALSE, im = c("rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med")){


  # calulo dap, g, h, area_parc ---------------------------------------------

  x <- x %>%
    dplyr::mutate(h = alt/10)


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



  # calculo fuste -----------------------------------------------------------

  ia <- c(im, "linha", "arvore")

  x <- x %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(ia))) %>%
    dplyr::mutate(fuste = dplyr::row_number(), .after = arvore) %>%
    dplyr::ungroup()


  # calculo da idade --------------------------------------------------------

  x <- x %>%
    dplyr::mutate(idade = as.numeric(difftime(dt_med, dt_plt,  units = "days"))/365.25,
                  classe_idade = round(idade))


  # calculo densidades ------------------------------------------------------

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
    #dplyr::select(-covas_parc, -arvores_parc, -fustes_parc) %>%
    dplyr::ungroup()


  # calculo ph50 -------------------------------------------------------------


  ph50 <- function(x) {

    mortas <- x[is.na(x)] #deixa os valores como 0 caso esteja NA
    metade <- floor(length(x)/2) #quantidade de observacaoes que representa 50% (percentil 50) do total de observacoes
    soma_todas <- sum(x, na.rm = TRUE) #faz a soma de todas as alturas das arvores da parcela
    soma_acumulada <- c(mortas, cumsum(sort(x))) #faz a soma acumulada de todas as observacoes
    soma_metade <- mean(soma_acumulada[metade], na.rm = TRUE) #da soma acumulada seleciona ate a posição que representa 50% das obserrvacoes
    z <- (soma_metade / soma_todas) #relacao entre soma acumulada total e soma acumulado do percentil 50

    return(z)
  }

  x <- x %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im)))) %>%
    dplyr::mutate(ph50 = ph50(h),
                  H = mean(h)) %>%
    dplyr::ungroup()


  # codigos qualitativos ----------------------------------------------------

  #n codigos 1
  cods <- x %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im, "cod1")))) %>%
    dplyr::tally() %>%
    dplyr::rename(cod = cod1, n1 = n) %>%
    dplyr::mutate(cod = tidyr::replace_na(cod, "ND"))

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
  cods <- dplyr::left_join(cods,  dplyr::distinct_all( dplyr::select(x,
                                                                     tidyselect::all_of(c(im, "covas_parc", "arvores_parc", "fustes_parc")))))

  #calculo da proporcao de cada codigo
  cods <- cods %>%
    dplyr::mutate(prop_cod = case_when(cod == "ND" ~ (n / covas_parc)*100, #normais
                                       cod == "A" ~ (n / arvores_parc)*100, #bifurcada acima
                                       cod == "B" ~ (n / covas_parc)*100, #bifurcada abaixo
                                       cod == "CA" ~ (n / arvores_parc)*100, #caida deitada
                                       cod == "CR" ~ (n / arvores_parc)*100, #caida raiz
                                       cod == "D" ~ (n / fustes_parc)*100, #dominada
                                       cod == "E" ~ (n / fustes_parc)*100, #marcada para sair
                                       cod == "F" ~ (n / covas_parc)*100, #falha
                                       cod == "G" ~ (n / fustes_parc)*100, #geada
                                       cod == "H" ~ (n / fustes_parc)*100, #dominante
                                       cod == "I" ~ (n / fustes_parc)*100, #praga doenca
                                       cod == "J" ~ (n / fustes_parc)*100, #macaco_com_rec
                                       cod == "K" ~ (n / fustes_parc)*100, #macaco_sem_rec
                                       cod == "L" ~ (n / fustes_parc)*100, #atacada por vespa
                                       cod == "M" ~ (n / fustes_parc)*100, #morta
                                       cod == "N" ~ (n / fustes_parc)*100, #morta macaco
                                       cod == "P" ~ (n / fustes_parc)*100, #pontera seca
                                       cod == "Q" ~ (n / fustes_parc)*100, #quebrada
                                       cod == "R" ~ (n / fustes_parc)*100, #rebrota
                                       cod == "S" ~ (n / fustes_parc)*100, #formiga
                                       cod == "T" ~ (n / fustes_parc)*100, #torta
                                       cod == "U" ~ (n / fustes_parc)*100, #fox_tail
                                       cod == "V" ~ (n / fustes_parc)*100, #inclinada vento
                                       cod == "W" ~ (n / fustes_parc)*100, #deitada vento
                                       cod == "X" ~ (n / fustes_parc)*100)) #verida na base

  cd <- c("ND", #normais
          "A", #bifurcada acima
          "B",  #bifurcada abaixo
          "CA", #caida deitada
          "CR", #caida raiz
          "D", #dominada
          "E", #marcada para sair
          "F",  #falha
          "G", #geada
          "H", #dominante
          "I", #praga doenca
          "J", #macaco_com_rec
          "K", #macaco_sem_rec
          "L", #atacada por vespa
          "M", #morta
          "N", #morta macaco
          "P", #pontera seca
          "Q", #quebrada
          "R", #rebrota
          "S", #formiga
          "T", #torta
          "U", #fox_tail
          "V", #inclinada vento
          "W", #deitada vento
          "X") #verida na base

  cods <- cods %>%
    dplyr::select(-covas_parc, -arvores_parc, -fustes_parc, -n) %>%
    dplyr::mutate(cod = factor(cod, levels = cd)) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(im)))) %>%
    tidyr::complete(cod, fill = list(n = 0, prop_cod = 0)) %>%
    tidyr::pivot_wider(names_from = cod, values_from = prop_cod) %>%
    dplyr::ungroup()


  return(x)
}

