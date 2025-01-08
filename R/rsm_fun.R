#' Gera a bases resumo
#'
#' @description
#' Esta função ira organizar e gerar as bases para output \cr
#'
#' @param db dataframe contendo as informacoes padronizadas do coletor - output da funcao \code{\link{poly2vol()}}
#' @param grp grupo de processameto. Os resumos do processamento pode ser realizado por estrato ou por talhao \strong{Padrão sera estrato}
#' @param sort dataframe contendo as informacoes padronizadas dos coeficientes da curva de afilamento - output da funcao \code{\link{read_sort()}}
#' @param im vetor com campos identificadores de medicao.\strong{Padrão sera c("centro", "rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med")}
#' @param ie vetor com campos identificadores de estrato.\strong{Padrão sera c("especie", "matgen", "regime", "classe_idade")}
#' @param it vetor com campos identificadores de talhao.\strong{Padrão sera c("centro", "rf", "talhao", "ciclo", "rotacao")}
#' \cr
#'
#'
#'
#' @return uma lista de dataframe
#'
#' @examples
#'
#'  db <- rsm(db, im, ie)
#'
#' @export
#'

rsm <-  function(bd,
                 grp = "talhao",
                 sort,
                 im = c("centro", "rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"),
                 ie = c("especie", "matgen", "regime", "classe_idade"),
                 it = c("centro", "rf", "talhao", "ciclo", "rotacao")) {


  #gera uma lista para output
  rsm <- list()

  #Gera as variaveis necessarias a partir da tabela de sortimentos
  ds <- sort %>%
    tidyr::unnest(cols = c(assort))

  assort_names <- unique(ds$name)
  assort_mols <- unique(ds$molsap)
  ja <- ds %>%
    dplyr::select(name, padrao, molsap)


# nivel tora --------------------------------------------------------------


  rsm[["log"]] <- bd %>%
    dplyr::select(tidyselect::all_of(c("atividade",
                                       base::union(c("centro", "rf", "talhao", "ciclo", "rotacao", "parcela", "dt_med"),
                                                   im),
                                       "linha",
                                       "arvore",
                                       "fuste",
                                       "tipo",
                                       "forma",
                                       "lado1",
                                       "lado2",
                                       "inc1",
                                       "inc2",
                                       "cod1",
                                       "cod2",
                                       "codQ",
                                       "coordX",
                                       "coordY",
                                       "area_parc",
                                       "covas",
                                       "arvores",
                                       "fustes",
                                       base::union(c("especie", "matgen",  "regime" , "dt_plt", "ano_plt", "idade", "classe_idade"),
                                                   ie),
                                       "area_plt",
                                       "dap",
                                       "dcom",
                                       "g",
                                       "dg",
                                       "ab",
                                       "ddom",
                                       "hdom",
                                       "h",
                                       "hobs",
                                       "hest",
                                       "hip_sel",
                                       "qbr",
                                       "hqbr",
                                       "dwg",
                                       "hdwg",
                                       "hcom",
                                       "htot",
                                       "htoco",
                                       "name",
                                       "log",
                                       "li",
                                       "h0",
                                       "hi",
                                       "d0",
                                       "di",
                                       "vi",
                                       "vpont",
                                       "vcom",
                                       "vtot",
                                       "vcom_sc",
                                       "vtot_sc",
                                       "padrao",
                                       "desc_sort")),
                  tidyselect::matches("^b[[:digit:]]{1}$|^b[[:digit:]]{1}_sc$"),
                  tidyselect::matches("^c_[A-Z]{1,2}$"))






  # nivel arvore ------------------------------------------------------------


  id_cols <- base::setdiff(names(rsm[["log"]]), c("name",
                                                  "log",
                                                  "li",
                                                  "h0",
                                                  "hi",
                                                  "d0",
                                                  "di",
                                                  "vi"))

  rsm[["tree"]] <- rsm[["log"]] %>%
    tidyr::pivot_wider(id_cols = tidyselect::all_of(id_cols),
                       names_from = name,
                       values_from = vi,
                       values_fill = 0,
                       values_fn = function(x) sum(x, na.rm = TRUE))

  if ('NA' %in% names(rsm[["tree"]])){
    rsm[["tree"]] <- rsm[["tree"]] %>%
      select(-`NA`)
  }



  #gera campos para todos sortimentos
  missing_columns <- base::setdiff(assort_names, names(rsm[["tree"]]))
  rsm[["tree"]][missing_columns] <- 0

  #ordena colunas
  rsm[["tree"]] <- rsm[["tree"]] %>%
    dplyr::relocate(tidyselect::any_of(assort_names), .before = vpont)


# nivel parcela -----------------------------------------------------------

  #codigos out
  cs <- c("F", "M", "N", "CA", "CR", "Y", "Z")


  rsm[["plot"]] <- rsm[["tree"]] %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c("atividade",
                                                       im,
                                                       "tipo",
                                                       "coordX",
                                                       "coordY",
                                                       "area_parc",
                                                       union(c("especie", "matgen",  "regime" , "dt_plt", "ano_plt", "idade", "classe_idade"),
                                                             ie),
                                                       "area_plt")))) %>%
    dplyr::summarise(dmin = round(min(dap[!cod1 %in% cs &
                                            !cod2 %in% cs &
                                            dap>0]),2),
                     dmed = round(mean(dap[!cod1 %in% cs &
                                             !cod2 %in% cs &
                                             dap>0]),2),
                     dg = round(dplyr::first(dg),2),
                     ddom = round(dplyr::first(ddom),2),
                     dmax = round(max(dap[!cod1 %in% cs &
                                            !cod2 %in% cs &
                                            dap>0]),2),
                     sd = round(sd(dap[!cod1 %in% cs &
                                         !cod2 %in% cs &
                                         dap>0]),2),
                     hmin = round(min(h[!cod1 %in% cs &
                                          !cod2 %in% cs &
                                          dap>0]),2),
                     hmed = round(mean(h[!cod1 %in% cs &
                                           !cod2 %in% cs &
                                           dap>0]),2),
                     hdom = round(first(hdom),2),
                     hmax = round(max(h[!cod1 %in% cs &
                                          !cod2 %in% cs &
                                          dap>0]),2),
                     sh = round(sd(h[!cod1 %in% cs &
                                       !cod2 %in% cs &
                                       dap>0]),2),
                     covas = dplyr::first(covas),
                     arvores = dplyr::first(arvores),
                     fustes = dplyr::first(fustes),
                     area_basal = round(dplyr::first(ab),2),
                     vmi = round(mean(vcom[!cod1 %in% cs &
                                             !cod2 %in% cs]),4),
                     svmi = round(sd(vcom[!cod1 %in% cs &
                                            !cod2 %in% cs]),4),
                     dplyr::across(tidyselect::all_of(c(assort_names, "vpont", "vcom", "vtot", "vcom_sc", "vtot_sc")), ~ sum(.[!cod1 %in% cs &
                                                                                                                                 !cod2 %in% cs], na.rm = T)),
                     dplyr::across(tidyselect::matches("padrao|desc_sort|^c_[A-Z]{1,2}$"), ~ dplyr::first(.))) %>%
    dplyr::ungroup()




  rsm[["plot"]] <- rsm[["plot"]] %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c(assort_names, "vpont", "vcom", "vtot", "vcom_sc", "vtot_sc")), ~ round((.x*10000/area_parc),2)))


# nivel estrato -----------------------------------------------------------

  rsm[["stratum"]] <- rsm[["plot"]] %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(ie))) %>%
    dplyr::summarise(dt_med = mean(dt_med),
                     idade = mean(idade),
                     dmin = round(min(dmin),2),
                     dmed = round(mean(dmed),2),
                     dg = round(mean(dg),2),
                     ddom = round(mean(ddom),2),
                     dmax = round(max(dmax),2),
                     sd = round(mean(sd),2),
                     hmin = round(min(hmin),2),
                     hmed = round(mean(hmed),2),
                     hdom = round(mean(hdom),2),
                     hmax = round(max(hmax),2),
                     sh = round(mean(sh),2),
                     covas = round(mean(covas)),
                     arvores = round(mean(arvores)),
                     fustes = round(mean(fustes)),
                     area_basal = round(mean(area_basal), 2),
                     vmi = round(weighted.mean(mean(svmi), fustes), 4),
                     svmi = round(weighted.mean(mean(svmi), fustes), 4),
                     svcom = round(sd(vcom),2),
                     svtot = round(sd(vtot),2),
                     dplyr::across(tidyselect::all_of(c(assort_names, "vpont", "vcom", "vtot", "vcom_sc", "vtot_sc")), ~ round(mean(., na.rm =T),2)),
                     padrao = dplyr::first(padrao),
                     desc_sort = dplyr::first(desc_sort),
                     dplyr::across(tidyselect::matches("^c_[A-Z]{1,2}$"), ~ round(mean(., na.rm =T),2)),
                     n_plot = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vmi = round(vcom/fustes,4))

  #gera a ara plantada do estrato a partir da base nivel parcela
  inf_dd <- rsm[["plot"]] %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(it))) %>%
    dplyr::select(tidyselect::all_of(c(it, ie, "area_plt"))) %>%
    dplyr::distinct_all() %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(ie))) %>%
    dplyr::summarise(area_plt = sum(area_plt, na.rm = T)) %>%
    dplyr::ungroup()

  rsm[["stratum"]] <- rsm[["stratum"]] %>%
    dplyr::left_join(inf_dd)

  rsm[["stratum"]] <- rsm[["stratum"]] %>%
    dplyr::mutate(erro_a = round(qt(.95, n_plot-1) * svcom / sqrt(n_plot),2),
                  erro_r = round((erro_a*100)/vcom,2),
                  intensidade_amostral = round(area_plt/n_plot, 2))


# nivel talhao ------------------------------------------------------------

  if(grp == "talhao"){

    rsm[["stand"]] <- rsm[["plot"]] %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(it,
                                                         union(c("especie", "matgen",  "regime" , "dt_plt", "ano_plt"),
                                                                   ie))))) %>%
      dplyr::summarise(dt_med = mean(dt_med),
                       idade = mean(idade),
                       dmin = round(min(dmin),2),
                       dmed = round(mean(dmed),2),
                       dg = round(mean(dg),2),
                       ddom = round(mean(ddom),2),
                       dmax = round(max(dmax),2),
                       sd = round(mean(sd),2),
                       hmin = round(min(hmin),2),
                       hmed = round(mean(hmed),2),
                       hdom = round(mean(hdom),2),
                       hmax = round(max(hmax),2),
                       sh = round(mean(sh),2),
                       covas = round(mean(covas)),
                       arvores = round(mean(arvores)),
                       fustes = round(mean(fustes)),
                       area_basal = round(mean(area_basal),2),
                       vmi = round(weighted.mean(mean(vmi), fustes), 4),
                       svmi = round(weighted.mean(mean(svmi), fustes), 4),
                       svcom = round(sd(vcom),2),
                       svtot = round(sd(vtot),2),
                       dplyr::across(tidyselect::all_of(c(assort_names, "vpont", "vcom", "vtot", "vcom_sc", "vtot_sc")), ~ round(mean(., na.rm =T),2)),
                       padrao = dplyr::first(padrao),
                       desc_sort = dplyr::first(desc_sort),
                       dplyr::across(tidyselect::matches("^c_[A-Z]{1,2}$"), ~ round(mean(., na.rm =T),2)),
                       n_plot = dplyr::n(),
                       area_plt = dplyr::first(area_plt))  %>%
      dplyr::ungroup()


    rsm[["stand"]] <- rsm[["stand"]] %>%
      dplyr::mutate(erro_a = round(qt(.95, n_plot-1) * svcom / sqrt(n_plot),2),
                    erro_r = round((erro_a*100)/vcom,2),
                    intensidade_amostral = round(area_plt/n_plot, 2))

  } else if(grp == "estrato"){

    #gerando um df com os talhoes presentes nas parcelas
    rsm[["stand"]] <- rsm[["plot"]] %>%
      dplyr::select(tidyselect::all_of(c(it,
                                         union(c("especie", "matgen",  "regime" , "dt_plt", "ano_plt"),
                                               ie)))) %>%
      dplyr::distinct_all()

    rsm[["stand"]] <- rsm[["stand"]] %>%
      dplyr::left_join(rsm[["stratum"]])

  } else {
    stop("REVER parametro 'grp'!")
  }



# modelos sap -------------------------------------------------------------

  molvars <- c("c_vacel2",
               "c_vaclasse0",
               "c_vaclasse1",
               "c_vaclasse2",
               "c_vaclasse3",
               "p_vacel1",
               "p_vacel2",
               "p_vaclasse0",
               "p_vaclasse1")


  if(!all(assort_mols %in% molvars)){
    erro("REVER o parametro molsap da descricao dos sortimentos. Os unicos valore spossiveis são:
             'c_vacel2'
             'c_vaclasse0'
             'c_vaclasse1'
             'c_vaclasse2'
             'c_vaclasse3'
             'p_vacel1'
             'p_vacel2'
             'p_vaclasse0'
             'p_vaclasse1'")
  }



  # calcula os sortimentos absolutos de cada classe definida na descricao de sortimento
  rsm[["sap"]] <- rsm[["stand"]] %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(assort_names),
                        names_to = "name",
                        values_to = "volume") %>%
    dplyr::mutate(volume = round(volume)) %>%
    dplyr::left_join(ja) %>%
    dplyr::select(-name) %>%
    dplyr::filter(!is.na(molsap)) %>%
    tidyr::pivot_wider(names_from = molsap,
                       values_from = volume,
                       values_fn = sum)

  #adiciona todas as classes caso nao esteja na descrição dos sortimentos
  cols <- c(c_vacel2 = 0,
            c_vaclasse0 = 0,
            c_vaclasse1 = 0,
            c_vaclasse2 = 0,
            c_vaclasse3 = 0,
            p_vacel1 = 0,
            p_vacel2 = 0,
            p_vaclasse0 = 0,
            p_vaclasse1 = 0,
            c_vabci2 = 0,
            c_vpbci2 = 0,
            c_vabci3 = 0,
            c_vpbci3 = 0)


  rsm[["sap"]] <- rsm[["sap"]] %>%
    tibble::add_column(!!!cols[!names(cols) %in% names(.)])


  # calcula os sortimentos relativos a comercio e processo
  rsm[["sap"]] <- rsm[["sap"]] %>%
    dplyr::mutate(c_vpcel2 = round((c_vacel2 / (c_vacel2 + c_vaclasse0 + c_vaclasse1 + c_vaclasse2 + c_vaclasse3))*100,2),
                  c_vpclasse0 = round((c_vaclasse0 / (c_vacel2 + c_vaclasse0 + c_vaclasse1 + c_vaclasse2 + c_vaclasse3))*100,2),
                  c_vpclasse1 = round((c_vaclasse1 / (c_vacel2 + c_vaclasse0 + c_vaclasse1 + c_vaclasse2 + c_vaclasse3))*100,2),
                  c_vpclasse2 = round((c_vaclasse2 / (c_vacel2 + c_vaclasse0 + c_vaclasse1 + c_vaclasse2 + c_vaclasse3))*100,2),
                  c_vpclasse3 = round((c_vaclasse3 / (c_vacel2 + c_vaclasse0 + c_vaclasse1 + c_vaclasse2 + c_vaclasse3))*100,2),
                  p_vpcel1 = round((p_vacel1 / (p_vacel1 + p_vacel2 + p_vaclasse0 + p_vaclasse1))*100,2),
                  p_vpcel2 = round((p_vacel2 / (p_vacel1 + p_vacel2 + p_vaclasse0 + p_vaclasse1))*100,2),
                  p_vpclasse0 = round((p_vaclasse0 / (p_vacel1 + p_vacel2 + p_vaclasse0 + p_vaclasse1))*100,2),
                  p_vpclasse1 = round((p_vaclasse1 / (p_vacel1 + p_vacel2 + p_vaclasse0 + p_vaclasse1))*100,2)) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^(c_|p_)"), ~ tidyr::replace_na(.x, 0)))

  # ajuste para fechamento em 100 na classe1 para comercio
  rsm[["sap"]] <- rsm[["sap"]] %>%
    dplyr::mutate(c_vpclasse1 = dplyr::case_when((c_vpclasse0 + c_vpclasse1 + c_vpclasse2 + c_vpclasse3) > 100 ~ (c_vpclasse0 + c_vpclasse2 + c_vpclasse3 - 100),
                                                 (c_vpclasse0 + c_vpclasse1 + c_vpclasse2 + c_vpclasse3) < 100 ~ (100 - c_vpclasse0 - c_vpclasse2 - c_vpclasse3),
                                                 TRUE ~ c_vpclasse1))

  # ajuste para fechamento em 100 na p_vacel1 para processo
  rsm[["sap"]] <- rsm[["sap"]] %>%
    dplyr::mutate(p_vpcel1 = dplyr::case_when((p_vpcel1 + p_vpcel2 + p_vpclasse0 + p_vpclasse1) > 100 ~ (p_vpcel2 + p_vpclasse0 + p_vpclasse1 - 100),
                                              (p_vpcel1 + p_vpcel2 + p_vpclasse0 + p_vpclasse1) < 100 ~ (100 - c_vpclasse0 - c_vpclasse2 - c_vpclasse3),
                                              TRUE ~ p_vpcel1))


  #calcula variaveis do layout
  rsm[["sap"]] <- rsm[["sap"]] %>%
    dplyr::mutate(tp_inventa = "IPC",
                  centro,
                  rf,
                  objeto_locacao = paste0(rf, talhao),
                  dt_inicio = lubridate::today(),
                  dt_fim = case_when(idade >= 20 ~ dt_inicio + lubridate::years(2),
                                     TRUE ~ dt_inicio + lubridate::years(1)),
                  grp_cod = "FL-INVENT",
                  cod = "FIPC",
                  mes_med = lubridate::month(dt_med),
                  ano_med = lubridate::year(dt_med),
                  vmi_sc = vcom_sc/round(fustes),
                  densidade = round(0, 3),
                  observacao = NA_character_,
                  indice_sitio = round(0,2))


  #arredondamentos
  rsm[["sap"]] <- rsm[["sap"]] %>%
    dplyr::mutate(across(c(idade:sh, area_basal, erro_r), ~ round(.x, 1)),
                  across(c(covas, arvores, fustes, n_plot, vcom, vcom_sc), ~ round(.x, 0)),
                  across(c(vmi, svmi, vmi_sc), ~ round(.x, 4)))


  rsm[["sap"]] <- rsm[["sap"]] %>%
    dplyr::select(tp_inventa,
                  centro,
                  rf,
                  objeto_locacao,
                  ciclo,
                  rotacao,
                  padrao,
                  dt_inicio,
                  dt_fim,
                  grp_cod,
                  cod,
                  mes_med,
                  ano_med,
                  dmin,
                  dmax,
                  idade,
                  dmed,
                  dg,
                  hmed,
                  hdom,
                  fustes,
                  area_basal,
                  vcom,
                  vmi,
                  vcom_sc,
                  vmi_sc,
                  densidade,
                  c_vacel2,
                  c_vpcel2,
                  c_vaclasse0,
                  c_vpclasse0,
                  c_vaclasse1,
                  c_vpclasse1,
                  c_vaclasse2,
                  c_vpclasse2,
                  c_vaclasse3,
                  c_vpclasse3,
                  p_vacel1,
                  p_vpcel1,
                  p_vacel2,
                  p_vpcel2,
                  p_vaclasse0,
                  p_vpclasse0,
                  p_vaclasse1,
                  p_vpclasse1,
                  c_vabci2,
                  c_vpbci2,
                  c_vabci3,
                  c_vpbci3,
                  observacao,
                  n_plot,
                  erro_r,
                  indice_sitio)

  # arredondamento ----------------------------------------------------------

  #arredondamentos
  rsm[["log"]] <- rsm[["log"]] %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::matches("^h|^d|li|g|ab|c_[A-Z]{1,2}") & tidyselect::where(is.numeric),
                                .fns = ~ round(., 2))) %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::matches("vi|vcom|vtot|vpont") & tidyselect::where(is.numeric),
                                .fns = ~ round(., 4))) %>%
    dplyr::rename_with(~ stringr::str_replace(.,"^c_",""), tidyselect::matches("^c_[A-Z]{1,2}$"))

  rsm[["tree"]] <- rsm[["tree"]] %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::matches("^h|^d|g|ab|c_[A-Z]{1,2}") & tidyselect::where(is.numeric),
                                .fns = ~ round(., 2))) %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::matches("vi|vcom|vtot|vpont") & tidyselect::where(is.numeric),
                                .fns = ~ round(., 4))) %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::any_of(assort_names) & tidyselect::where(is.numeric),
                                .fns = ~ round(., 4))) %>%
    dplyr::rename_with(~ stringr::str_replace(.,"^c_",""), tidyselect::matches("^c_[A-Z]{1,2}$"))

  rsm[["plot"]] <- rsm[["plot"]] %>%
    dplyr::rename_with(~ stringr::str_replace(.,"^c_",""), tidyselect::matches("^c_[A-Z]{1,2}$"))

  rsm[["stratum"]] <- rsm[["stratum"]] %>%
    dplyr::rename_with(~ stringr::str_replace(.,"^c_",""), tidyselect::matches("^c_[A-Z]{1,2}$"))

  rsm[["stand"]] <- rsm[["stand"]] %>%
    dplyr::rename_with(~ stringr::str_replace(.,"^c_",""), tidyselect::matches("^c_[A-Z]{1,2}$"))


  return(rsm)

}
