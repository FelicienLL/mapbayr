#' Title
#'
#' @param adapation a list, output of adaptation_tice
#' @param n_cycle a numeric input
#'
#' @return a character string
#' @export
#'
write_result_tice <- function(adapation, n_cycle){

  a <- paste0("Clairance observee du carboplatine du Cycle ", n_cycle, " J1 = ", round(adapation$CL, 2), " mL/min")

  b <- paste0("AUC observee du C", n_cycle, " J1 = ", round(adapation$AUC_D1, 2), " mg/mL.min")

  paste0(a, "\n", b)

}


#' Title
#'
#' @param adapation a list, output of adaptation_tice
#' @param n_cycle a numeric input
#' @param auc_cible a numeric input
#' @param date_adm a date input
#'
#' @importFrom lubridate days
#' @return a character string
#' @export
#'
write_comment_tice <- function(adapation, n_cycle, auc_cible, date_adm){

  a <- paste0(
    "Interpretation biologique :"," \n",
    "L'analyse des donnees a ete realisee grace a une base de donnees pharmacocinetiques de 188 patients. ", " \n",
    "AUC cible journaliere de votre protocole = ",
    auc_cible / 3 ,
    " min.mg/mL")

  b <- paste0(
    "La dose a administrer a C",
    n_cycle,
    " J3 (c'est-a-dire le ",
    format.Date((date_adm + days(2)), "%d/%m/%Y"),
    ") est de ",
    round(adapation$DOSE_D3 / 10) * 10,
    " mg (ajustement necessaire pour obtenir une AUC de ",
    auc_cible,
    " sur les 3 jours).")

  c <- paste0(
    "Pour le prochain cycle et a condition que la creatininemie de votre patient n'ait pas varie de plus de 30%, ",
    "la dose estimee journaliere se calculera de la maniere suivante :\n",
    "Dose (mg) = AUC cible (mg/mL.min) x CLobservee (ml/min) = ",
    auc_cible / 3 ,
    " x ",
    round(adapation$CL, 2),
    " = ",
    round(round(adapation$CL, 2) * (auc_cible / 3) * 0.1) * 10 ,
    " mg, a moduler en fonction de la tolerance observee durant l'intercycle.")

  txt <- ifelse(n_cycle == 3,
                paste0(a, "\n", b),
                paste0(a, "\n", b, "\n\n", c))

  txt

}
