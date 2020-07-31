#' Title
#'
#' @return a list of models (list of characteristics)
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_c
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#'
interpretation_tki <- function(){
  l <- list(
    Ibrutinib = list (
      drug                 = "Ibrutinib",
      concentration_unit   = "ng/mL",
      concentration_target = NULL,
      target_ref           = "Gallais, Clin PK, 2020",
      sim_dose_regimens    = data.frame(
        amt    = c(140, 280, 420, 560),
        ii     = 24) %>%
        mutate(choice = str_c(.data$amt, "mg/", .data$ii, "h"))
    ),
    Nilotinib = list (
      drug                 = "Nilotinib",
      concentration_unit   = "ng/mL",
      concentration_target = 469,
      target_ref           = "Giles, Eur J Clin Pharmacol, 2013",
      sim_dose_regimens    = data.frame(
        amt    = c(100, 200, 300, 400, 500),
        ii     = 12) %>%
        mutate(choice = str_c(.data$amt, "mg/", .data$ii, "h"))
    ),
    Cabozantinib = list(
      drug                 = "Cabozantinib",
      concentration_unit   = "mg/L",
      concentration_target = 1.1,
      target_ref           = 'Lacy et al, Cancer Chemother Pharmacol, 2018',
      sim_dose_regimens    = data.frame(
        amt = c(20, 40, 60, 20, 40, 60),
        ii =  c(24, 24, 24, 48, 48, 48)) %>%
        mutate(choice = str_c(.data$amt, "mg/", .data$ii,"h"))
    ),
    Pazopanib = list(
      drug                 = "Pazopanib",
      concentration_unit   = "mg/L",
      concentration_target = 20.5,
      target_ref           = 'Suttle et al, Br J Cancer, 2014',
      sim_dose_regimens    = data.frame(
        amt = c(400, 600, 800, 400, 600, 800),
        ii  = c( 24,  24,  24,  12,  12,  12)) %>%
        mutate(
          choice = str_c(.data$amt, "mg/", .data$ii,"h")
        )
    ),
    Imatinib = list(
      drug                 = "Imatinib",
      concentration_unit   = "ng/mL",
      concentration_target = 1000,
      target_ref           = "Larson et al, Blood, 2008",
      sim_dose_regimens   = data.frame(
        amt = c(100, 300, 400, 600, 400),
        ii  = c(24, 24, 24, 24, 12)) %>%
        mutate(choice = str_c(.data$amt, "mg/", .data$ii, "h"))
    )
  )

  l

}


