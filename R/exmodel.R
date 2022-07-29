#' Example model and data
#'
#' @name exmodel_exdata
#' @description A collection of example models and corresponding data to test and explore `mapbayr`.
#' @param num model number (see details)
#' @param add_exdata should data be automatically loaded with the model
#' @param cache read the model with `mrgsolve::mread_cache()`
#' @param quiet don't print messages when compiling
#' @param ... passed to `mrgsolve::mread()` or `mrgsolve::mread_cache()`
#' @param ID individual number to include in the data (from 1 to 8)
#' @param clean_data remove useless columns and rows from the original data
#' @details
#' Available models are:
#'
#' - 1: Base model. A simple monocompartmental PK model with inter-individual variability on absorption constant (KA), volume of distribution (VC) and clearance (CL). The residual error model is proportional.
#' - 6: Complex absorption model. Dual 0- and 1st orders absorption phenomenons.
#' - 301: Time-varying covariates. A continuous covariate (body weight "BW") and a categorical one (sex "SEX") influence the clearance parameter. In the corresponding dataset, the values randomly changes from one record to another within a single individual.
#' - 401: Metabolite. The PK model of both a parent drug and its metabolite.
#'
#' An example dataset of eight (simulated) individuals is available for each model. Individuals differ in terms of sampling times (sparse or rich) and dosing regimen (single or multiple dosing).
#'
#' Model code and data files are stored at the location given by `system.file("exmodel", package = "mapbayr")`.
#'
#' These models and data were created for the validation study of `mapbayr` published in \href{https://pubmed.ncbi.nlm.nih.gov/34342170/}{CPT:Pharmacometrics & System Pharmacology}. More models and full datasets can be accessed \href{https://github.com/FelicienLL/mapbayr-CPTPSP-2021}{in a dedicated repository}
#'
#' @return `exmodel()` reads and compiles code, and returns a (`mrgmod`) model object. `exdata()` returns a data.frame.
#' @export
#' @examples
#' # Models can be loaded with data (the default), ready for parameter estimation
#' est <- mapbayest(exmodel())
#'
#' # Number of subjects in dataset can be chosen up to 8 individuals
#' exdata(301, ID = c(5,8))
#' @source \url{https://github.com/FelicienLL/mapbayr-CPTPSP-2021}
#'
#' @rdname exmodel_exdata
#' @export
exmodel <- function(num = 1, add_exdata = TRUE, cache = TRUE, quiet = getOption("mrgsolve_mread_quiet", TRUE), ..., ID = 1, clean_data = TRUE){
  num <- as.double(num)
  check_num(num = num)
  model_name <- make_model_name(x = num, cache = cache)

  if(cache){
    model <- mrgsolve::mread_cache(model_name,
                                   project = system.file("exmodel", package = "mapbayr"),
                                   quiet = quiet,
                                   ...)
  } else {
    model <- mrgsolve::mread(model_name,
                             quiet = quiet,
                             ...)
  }

  if(add_exdata){
    dat <- exdata(num = num, ID = ID, clean_data = clean_data)
    model <- data_set(x = model, data = dat)
  }
  model
}

#' @rdname exmodel_exdata
#' @export
exdata <- function(num = 1, ID = 1, clean_data = TRUE){
  num <- as.double(num)
  check_num(num = num)

  dat <- utils::read.csv(make_data_name(num), na = ".")
  dat <- dat[dat$ID%in%ID,]
  if(clean_data){
    dat <- clean_exdata(dat)
  }
  dat
}

make_model_name <- function(x, cache = TRUE){
  num <- str_pad(x, 3, pad = 0)
  nam <- paste0("mrg_", num)
  if(cache) return(nam)

  system.file("exmodel", paste0(nam, ".cpp"), package = "mapbayr")
}

make_data_name <- function(x){
  num <- str_pad(x, 3, pad = 0)
  nam <- paste0("data_to_fit", num, ".csv")
  system.file("exmodel", nam, package = "mapbayr")
}

clean_exdata <- function(x){
  x <- x[!(x$s2_sampling%in%1:2 & x$time == 72 & x$evid == 1),]
  x$s2_sampling <- NULL
  x
}

check_num <- function(num){
  if(length(num) > 1) stop("num must be of length 1.")
  available_models <- c(1, 6, 301, 401)
  if(!num %in% available_models) stop("num must be selected from: ", paste(available_models, collapse = ", "), ".", call. = FALSE)
}
