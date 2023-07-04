#' Data helpers: functions to build the dataset
#'
#' @name data_helpers
#' @description Use [adm_rows()], [obs_rows()] and [add_covariates()] to create or modify a dataset from scratch, from a pre-existing dataset, or from a dataset stored into a 'mrgsolve' model.
#' @details
#' Instead of importing a '.csv' file, or painfully build a data set with a call to `data.frame()` and mind how to format the data, you can pass information about:
#'
#' * administrations with [adm_rows()],
#' * observations with [obs_rows()],
#' * covariates with [add_covariates()],
#'
#' all being called jointly with a pipe (`%>%` or `|>`).
#' These functions can be used to create or modify a dataset as a proper data.frame, or to create or modify a dataset within a 'mrgsolve' model (`@args$data` slot).
#' The latter is particularly useful in order to:
#' * automatically use default administration and observation compartments,
#' * automatically duplicate rows if there are several depot compartments,
#' * automatically set `rate = -2` if model has zero-order absorption pathways,
#' * automatically duplicate rows if concentrations of Parent drug and Metabolite are observed together,
#' * automatically add "Amount Of Last Administration" and "Time Of Last Administration" variables if these are covariates,
#' * subsequently call `mrgsim()` or `mapbayest()`.
#'
#' @examples
#' library(magrittr)
#' # First option: work with a data.frame
#'
#' adm_rows(amt = 1000, cmt = 1, addl = 4, ii = 12) %>%
#'   obs_rows(time = c(12.3, 45.6), DV = c(.111, .222), cmt = 2) %>%
#'   obs_rows(time = 48, cmt = 2) %>%
#'   add_covariates(BW = 90, SEX = 0, TOLA = TRUE)
#'
#' # You can even inform "time" using date and hours:
#' adm_rows(amt = 1000, cmt = 1, addl = 4, ii = 12, .datehour = "2022-01-01 11:11:11") %>%
#'   obs_rows(.datehour = "2022-01-02 22:22:22", DV = 0.111, cmt = 2)
#'
#' # Second option: work with a dataset within a 'mrgsolve' model
#' \donttest{
#' mod <- exmodel(add_exdata = FALSE)
#' # call `mrgsolve::see(mod)` to see how default compartment were coded
#' adm_cmt(mod)
#' obs_cmt(mod)
#'
#' mod %>%
#'   adm_rows(amt = 10000) %>%
#'   obs_rows(time = c(1.5, 4.4, 7.5, 24.6), DV = c(91.2904, 110.826, 79.384, 20.6671)) %>%
#'   # get_data() # for curiosity, you can extract the data set at this step
#'   mapbayest()
#' }
#'
NULL
#> NULL

#' Add administration lines to a dataset
#'
#' @description The `adm_rows()` function adds an one or several administration lines to a dataset provided as a proper data.frame or within a 'mrgsolve' model. Used in combination with [obs_rows()] and [add_covariates()], it helps the creation of datasets in the proper format for simulations with 'mrgsolve' or parameter estimation with 'mapbayr', as explained in [data_helpers].
#'
#' @param x either a data.frame or a 'mrgsolve' model object
#' @param ID subject ID (default is 1)
#' @param time event time. Default is 0 if no previous events. Mind consistency with `.datehour`.
#' @param evid event identification (default is 1 for administration, 0 for observation)
#' @param cmt compartment (no default, except if `[ADM]` was tagged in the `$CMT` block in model code. See `examples`.)
#' @param amt dose amount (for administration records only)
#' @param mdv missing dependent value (default is 1 for administration records)
#' @param addl additional dose (optional)
#' @param ss steady-state (optional, is this dose the last of an infinity of administration? Yes, 1, or no, 0)
#' @param ii inter-dose interval (optional, use it with `ss` and `addl`)
#' @param rate rate of administration (optional, set to -2 if you model zero-order infusion. See `examples`.)
#' @param .datehour a object of class POSIXct, a number or a character vector that can be passed to [parse_datehour()]. Using `.datehour` will update the value of `time` in the dataset, with units in hours. Mind consistency with the `time` argument.
#' @param ... additional columns or arguments for [mrgsolve::ev()]
#'
#' @return a data.frame, or a 'mrgsolve' model with a dataset in the `@args$data` slot (accessible with [get_data()]).
#' @export
#'
#' @examples
#' # Create a dataset from scratch
#' adm_rows(amt = 100, cmt = 1)
#'
#' # Pipe-friendly addition of administration record to a pre-existing dataset
#' library(magrittr)
#' adm_rows(amt = 100, cmt = 1) %>%
#'   adm_rows(time = 3, amt = 200, cmt = 1, addl = 3, ii = 1)
#'
#' # Inform times using the `.datehour` argument:
#' adm_rows(.datehour = "2020-01-01 11:11", amt = 100, cmt = 1) %>%
#'   adm_rows(.datehour = "2020-01-02 22:22", amt = 200, cmt = 1) %>%
#'   adm_rows(time = 48, amt = 300, cmt = 1)
#'
#' # Start from a 'mrgsolve' model
#' library(mrgsolve)
#' house() %>%
#'   adm_rows(amt = 100, cmt = 1) %>%
#'   adm_rows(time = 3, amt = 200, cmt = 1, addl = 3, ii = 1) %>%
#'   mrgsim(delta = 1)
#'
#' # Default administration compartments
#' # Set default administration compartments in the code with `[ADM]`
#' model <- mcode("model", "
#' $CMT @annotated
#' DEPOT : Depot [ADM]
#' CENTR : Central
#' ", compile = FALSE)
#' adm_cmt(model)
#'
#' # Thus, no need to manually specify `cmt = 1` anymore.
#' model %>%
#'   adm_rows(amt = 100) %>%
#'   adm_rows(time = 3, amt = 200, addl = 3, ii = 1) %>%
#'   get_data()
#'
#' # Automatic lines duplication if multiple depot compartments
#' # Automatic `rate = -2` increment if model with 0-order absorption
#' model <- mcode("model", "
#' $PARAM DUR = 1.0
#' $CMT @annotated
#' DEPOT : Depot [ADM]
#' CENTR : Central [ADM]
#' $MAIN
#' D_CENTR = DUR ;
#' ", compile = FALSE)
#' adm_cmt(model)
#'
#' model %>%
#'   adm_rows(amt = 100) %>%
#'   adm_rows(time = 3, amt = 200, addl = 3, ii = 1) %>%
#'   get_data()
#' @seealso [data_helpers]
adm_rows <- function(x, ...) {
  if (missing(x)) {
    adm_rows.missing(...)
  } else {
    UseMethod("adm_rows")
  }
}

#' @rdname adm_rows
#' @method adm_rows data.frame
#' @export
adm_rows.data.frame <- function(x,
                                 ID = NULL,
                                 time = NULL,
                                 evid = 1L,
                                 cmt,
                                 amt,
                                 mdv = 1L,
                                 addl = NULL,
                                 ss = NULL,
                                 ii = NULL,
                                 rate = NULL,
                                 .datehour = NULL,
                                 ...) {
  old_data <- x

  # ID
  if (is.null(ID)) {
    cur_ID <- utils::tail(old_data[["ID"]], 1)
    if (is.null(cur_ID)) {
      ID <- 1L
    } else {
      ID <- cur_ID
    }
  }

  # Redefine time and deal with `.datehour`
  datehour_answers <- datehour_manager(
    old_data = old_data,
    time = time,
    .datehour = .datehour
  )
  dh0 <- datehour_answers$dh0
  time <- datehour_answers$time
  .datehour <- datehour_answers$.datehour
  old_data <- datehour_answers$old_data

  # MATCH time & amt, and CROSS WITH cmt and rate
  if (length(time) == length(amt)) {
    nadm <- length(time)
  } else {
    amttime <- expand.grid(amt, time)
    nadm <- nrow(amttime)
    amt <- amttime[, 1]
    time <- amttime[, 2]
  }
  amt <- rep(amt, each = length(cmt))
  time <- rep(time, each = length(cmt))
  cmt <- rep(cmt, nadm)
  rate <- rep(rate, nadm)

  # Call `mrgsolve::ev()`
  ev_args <- list(ID = ID, time = time, evid = evid, cmt = cmt, amt = amt, mdv = mdv, addl = addl, ss = ss, ii = ii, rate = rate, ... = ...)
  ev_args <- NULL_remove(ev_args)

  new_lines <- as.data.frame(do.call(ev, ev_args))
  new_data <- bind_rows(old_data, new_lines)
  rearrange_nmdata(new_data, dh0 = dh0)
}

#' @rdname adm_rows
#' @method adm_rows missing
#' @export
adm_rows.missing <- function(...) {
  x <- as_tibble(data.frame())
  adm_rows.data.frame(x = x, ... = ...)
}

#' @rdname adm_rows
#' @method adm_rows mrgmod
#' @export
adm_rows.mrgmod <- function(x, cmt = adm_cmt(x), rate = NULL, ...) {
  # x = a model object
  old_data <- get_data.mrgmod(x) # if no data: an empty 0x0 tibble, not "NULL"!

  if (is.null(rate)) {
    zero_order_cmt <- adm_0_cmt(x)
    if (!is.null(zero_order_cmt) && any(cmt == zero_order_cmt)) {
      rate <- rep(0, length(cmt))
      rate[cmt == zero_order_cmt] <- -2
    }
  }

  args <- list(x = old_data, cmt = cmt, rate = rate, ... = ...)
  args <- NULL_remove(args)

  new_data <- do.call(adm_rows.data.frame, args)

  data_set(x, new_data)
}

#' Add observation lines to a dataset
#'
#' @description The `obs_rows()` function adds an one or several observation lines to a dataset provided as a proper data.frame or within a 'mrgsolve' model. Used in combination with [adm_rows()] and [add_covariates()], it helps the creation of datasets in the proper format for simulations with 'mrgsolve' or parameter estimation with 'mapbayr', as explained in [data_helpers].
#'
#' @param x either a data.frame or a 'mrgsolve' model object
#' @param ID subject ID (default is 1)
#' @param time event time. Default is 0 if no previous events. Mind consistency with `.datehour`.
#' @param evid event identification (default is 1 for administration, 0 for observation)
#' @param cmt compartment (no default, except if `[OBS]` was tagged in the `$CMT` block in model code. See `examples`.)
#' @param DV dependent value, i.e. observed concentration.
#' @param mdv missing dependent value (default is 0 a non-missing concentration value to take into account for parameter estimation, 1 otherwise)
#' @param ... additional columns
#' @param .datehour a object of class POSIXct, a number or a character vector that can be passed to [parse_datehour()]. Using `.datehour` will update the value of `time` in the dataset, with units in hours. Mind consistency with the `time` argument.
#' @param DVmet second observation at the same time (e.g. a metabolite, "DVmet") observed jointly with parent drug ("DV"). Works only if `x` is a 'mrgsolve' model where two `[OBS]` compartments were defined (see `examples`)
#'
#' @return a data.frame, or a 'mrgsolve' model with a dataset in the `@args$data` slot (accessible with [get_data()]).
#' @export
#'
#' @examples
#' # Create a dataset from scratch
#' obs_rows(time = 12, DV = 0.12, cmt = 2)
#'
#' # Pipe-friendly addition of observation record to a pre-existing dataset
#' library(magrittr)
#' obs_rows(time = 12, DV = 0.12, cmt = 2) %>%
#'   obs_rows(time = c(24, 36, 48), DV = c(0.34, 0.56, 0.78), mdv = c(0, 1, 0), cmt = 2)
#'
#' # Inform times using the `.datehour` argument:
#' obs_rows(.datehour = "2020-01-01 11:11", DV = 0.12, cmt = 1) %>%
#'   obs_rows(.datehour = "2020-01-02 22:22", DV = 0.34, cmt = 1) %>%
#'   obs_rows(time = 48, DV = 0.56, cmt = 1)
#'
#' # Start from a 'mrgsolve' model
#' library(mrgsolve)
#' house() %>%
#'   obs_rows(time = 12, DV = 0.12, cmt = 2) %>%
#'   obs_rows(time = c(24, 36, 48), DV = c(0.34, 0.56, 0.78), mdv = c(0, 1, 0), cmt = 2) %>%
#'   mrgsim()
#'
#' # Default observation compartments
#' # Set default observation compartments in the code with `[OBS]`
#' model <- mcode("model", "
#' $CMT @annotated
#' DEPOT : Depot
#' CENTR : Central [OBS]
#' ", compile = FALSE)
#' obs_cmt(model)
#'
#' # Thus, no need to manually specify `cmt = 2` anymore.
#' model %>%
#'   obs_rows(time = 12, DV = 0.12) %>%
#'   obs_rows(time = c(24, 36, 48), DV = c(0.34, 0.56, 0.78), mdv = c(0, 1, 0)) %>%
#'   get_data()
#'
#' # Automatic lines duplication if parent + metabolite defined in the model
#' model <- mcode("model", "
#' $CMT @annotated
#' DEPOT : Depot
#' CENTR : Central [OBS]
#' PERIPH : Periph
#' METABO : Metabo [OBS]
#' ", compile = FALSE)
#' obs_cmt(model)
#'
#' model %>%
#'   obs_rows(time = 12, DV = 0.12, DVmet = 120) %>%
#'   obs_rows(
#'     time = c(24, 36, 48), DV = c(0.34, 0.56, 0.78),
#'     mdv = c(0, 1, 0), DVmet = c(340, 560, 780)
#'   ) %>%
#'   get_data()
#' @seealso [data_helpers]
obs_rows <- function(x, ...) {
  if (missing(x)) {
    obs_rows.missing(...)
  } else {
    UseMethod("obs_rows")
  }
}

#' @method obs_rows data.frame
#' @rdname obs_rows
#' @export
obs_rows.data.frame <- function(x,
                                 ID = NULL,
                                 time = NULL,
                                 evid = 0L,
                                 cmt,
                                 DV = NA_real_,
                                 mdv = NULL,
                                 .datehour = NULL,
                                 ...) {
  old_data <- x

  # ID
  if (is.null(ID)) {
    cur_ID <- utils::tail(old_data[["ID"]], 1)
    if (is.null(cur_ID)) {
      ID <- 1L
    } else {
      ID <- cur_ID
    }
  }

  # Redefine time and deal with .datehour
  datehour_answers <- datehour_manager(
    old_data = old_data,
    time = time,
    .datehour = .datehour
  )
  dh0 <- datehour_answers$dh0
  time <- datehour_answers$time
  .datehour <- datehour_answers$.datehour
  old_data <- datehour_answers$old_data

  # Match time and cmt to the length of DV (especially if DV is parent + metab)
  cmttime <- expand.grid(cmt, time)
  # nobs <- nrow(cmttime)
  cmt <- cmttime[, 1]
  time <- cmttime[, 2]

  # MDV
  if (is.null(mdv)) {
    mdv <- as.integer(is.na(DV))
  } else {
    if (length(mdv) < length(DV)) {
      mdv <- rep(mdv, each = length(unique(cmt)))
    }
  }

  new_lines <- data.frame(ID = ID, time = time, evid = evid, cmt = cmt, DV = DV, mdv = mdv, ... = ...)
  if (any(names(new_lines) == "DVmet")) {
    warning("`DVmet` column added to the data. If you expected metabolite concentrations set in `DV`, `obs_rows()` must be used with a 'mrgsolve' model.")
  }

  new_data <- bind_rows(old_data, new_lines)
  rearrange_nmdata(new_data, dh0 = dh0)
}

#' @method obs_rows missing
#' @rdname obs_rows
#' @export
obs_rows.missing <- function(...) {
  x <- as_tibble(data.frame())
  obs_rows.data.frame(x = x, ... = ...)
}

#' @method obs_rows mrgmod
#' @rdname obs_rows
#' @export
obs_rows.mrgmod <- function(x, cmt = NULL, DV = NA_real_, DVmet = NULL, ...) {
  # x = a model object
  old_data <- get_data.mrgmod(x) # if no data: an empty 0x0 tibble, not "NULL"!

  # CMT
  model_obs_cmt <- obs_cmt(x)
  if (is.null(cmt)) {
    if (is.null(DVmet)) {
      cmt <- model_obs_cmt[1]
    } else {
      cmt <- model_obs_cmt
    }
  }

  # DVmet to DV
  if (!is.null(DVmet)) {
    if (length(cmt) != 2) stop("Define 2 observation compartments with the [OBS] tags in model code")
    nobs <- length(DV)
    if (nobs != length(DVmet)) stop("`DVmet` must be the same length as `DV`")
    DV <- as.double(rbind(DV, DVmet)) # = pivot longer: DV[1] DVmet[1] DV[2] DVmet[2] etc
    DVmet <- NULL
  }

  args <- list(x = old_data, cmt = cmt, DV = DV, ... = ...)
  args <- NULL_remove(args)

  new_data <- do.call(obs_rows.data.frame, args)

  data_set(x, new_data)
}

#' Add covariate columns to a dataset
#'
#' @description The `add_covariates()` function adds an one or several covariate columns to a dataset provided as a proper data.frame or within a 'mrgsolve' model. Used in combination with [adm_rows()] and [obs_rows()], it helps the creation of datasets in the proper format for simulations with 'mrgsolve' or parameter estimation with 'mapbayr', as explained in [data_helpers].
#'
#' @param x either a data.frame or a 'mrgsolve' model object
#' @param ... covariates values to add to the data. For each variable, supply a vector of length 1 or with the same number of rows. Ignored if `covariates` argument is used.
#' @param covariates Covariates passed as a single list of variables. Overrides `...`.
#' @param AOLA,TOLA a logical. Should the "Amount Of Last Administration" and "Time Of Last Administration" variables be added into the dataset? Default if FALSE if `x` is a dataset, TRUE if `x` is a 'mrgsolve' model where `AOLA` and `TOLA` are defined as covariates
#'
#' @return a data.frame, or a 'mrgsolve' model with a dataset in the `@args$data` slot (accessible with [get_data()]).
#' @export
#'
#' @examples
#' # Cannot start from scratch
#' \dontrun{
#' add_covariates(BW = 90, SEX = 0)
#' }
#'
#' library(magrittr)
#' adm_rows(time = c(0, 24, 48), cmt = 1, amt = c(100, 200, 300)) %>%
#'   add_covariates(BW = c(90, 85, 80), SEX = 0)
#'
#' # If covariates are stored in a list, use `covariates = `
#' adm_rows(time = c(0, 24, 48), cmt = 1, amt = c(100, 200, 300)) %>%
#'   add_covariates(covariates = list(BW = c(90, 85, 80), SEX = 0))
#'
#' # Missing values are filled with the "next observation carried backward" rule
#' adm_rows(time = c(0, 24, 48), cmt = 1, amt = c(100, 200, 300)) %>%
#'   add_covariates(BW = c(90, 85, 80), SEX = 0) %>%
#'   obs_rows(time = 36, DV = .0123, cmt = 2)
#' # Always verify the output in case of time-varying covariates
#'
#' # Possibility to add Time and Amount of last administration as covariates
#' adm_rows(time = c(0, 24, 48), amt = c(100, 200, 300), cmt = 1) %>%
#'   obs_rows(time = c(8, 16, 32, 40), cmt = 2, DV = runif(4)) %>%
#'   add_covariates(TOLA = TRUE, AOLA = TRUE) %>%
#'   obs_rows(time = 72, cmt = 2, DV = .123) # AOLA/TOLA re-updated afterwards
#'
#' # Automatic inclusion of `TOLA`/`AOLA` if they are covariates of the model
#' library(mrgsolve)
#' model <- mcode("model", "
#' $PARAM @annotated @covariates
#' TOLA : 0 : Time Last Adm
#' AOLA : 0 : Amount Last Adm
#' ", compile = FALSE)
#'
#' model %>%
#'   adm_rows(time = c(0, 24, 48), amt = c(100, 200, 300), cmt = 1) %>%
#'   add_covariates() %>%
#'   get_data()
#' @seealso [data_helpers]
add_covariates <- function(x, ...) {
  if (missing(x)) {
    stop("Initial dataset not found. Cannot add columns to a dataset that do not exists.")
  } else {
    UseMethod("add_covariates")
  }
}

#' @method add_covariates data.frame
#' @rdname add_covariates
#' @export
add_covariates.data.frame <- function(x, ..., covariates = list(), AOLA = FALSE, TOLA = FALSE) {
  old_data <- x

  if (length(covariates) != 0) {
    forbidden_covariate(x = covariates, cov = ".datehour")
    new_data <- bind_cols(old_data, covariates)
  } else {
    dots <- list(...)
    forbidden_covariate(x = dots, cov = ".datehour")
    if (length(dots) != 0) {
      if ((is.null(names(dots[1])) || names(dots[1]) == "") & is.list(dots[[1]]) & !is.null(names(dots[[1]]))) {
        warning("A list was passed as first argument to `add_covariates()`, thus will be interpretated as a list of covariates. This behaviour will be deprecated. Please modify and use the argument add_covariates(covariates = ) explicitely.")
        new_data <- bind_cols(old_data, dots[[1]])
      } else {
        if (any(is.null(names(dots)))) {
          stop("Arguments must be named (with covariates names)")
        }
        new_data <- bind_cols(old_data, dots)
      }
    } else {
      new_data <- old_data
    }
  }
  if (AOLA) {
    new_data$AOLA <- 1
  }
  if (TOLA) {
    new_data$TOLA <- 1
  }
  rearrange_nmdata(new_data)
}

#' @method add_covariates mrgmod
#' @rdname add_covariates
#' @export
add_covariates.mrgmod <- function(x, ..., covariates = list(), AOLA = NULL, TOLA = NULL) {
  old_data <- get_data.mrgmod(x)

  # AOLA /TOLA
  if (is.null(AOLA) | is.null(TOLA)) {
    model_covariates <- mbr_cov_names(x)
  }

  if (is.null(AOLA)) {
    AOLA <- "AOLA" %in% model_covariates
  }

  if (is.null(TOLA)) {
    TOLA <- "TOLA" %in% model_covariates
  }

  new_data <- add_covariates.data.frame(x = old_data, ... = ..., covariates = covariates, AOLA = AOLA, TOLA = TOLA)
  data_set(x, new_data)
}

AOLA <- function(x) {
  x %>%
    arrange(.data$ID, .data$time, desc(.data$evid), .data$cmt) %>%
    group_by(.data$ID) %>%
    mutate(AOLA = ifelse(.data$evid %in% c(1, 4), .data$amt, NA_real_)) %>%
    fill("AOLA") %>%
    ungroup()
}

TOLA <- function(x) {
  x %>%
    realize_addl() %>%
    arrange(.data$ID, .data$time, desc(.data$evid), .data$cmt) %>%
    group_by(.data$ID) %>%
    mutate(TOLA = ifelse(.data$evid %in% c(1, 4), .data$time, NA_real_)) %>%
    fill("TOLA") %>%
    ungroup()
}

NULL_remove <- function(x) {
  x[!sapply(x, is.null)]
}

rearrange_nmdata <- function(x, dh0 = NULL) {
  # Arrange. ADM (evid1) before OBS (evid0) if same time = recsort 3/4
  if (!any(is.null(x[["ID"]]), is.null(x[["time"]]), is.null(x[["evid"]]), is.null(x[["cmt"]]))) {
    x <- arrange(x, .data$ID, .data$time, desc(.data$evid), .data$cmt)
  }

  # Fill AOLA/TOLA if exists
  if (!is.null(x[["AOLA"]])) x <- AOLA(x)
  if (!is.null(x[["TOLA"]])) x <- TOLA(x)

  # Fill .datehour if exists or requested
  if (any(!is.null(x[[".datehour"]]), !is.null(dh0))) {
    if (is.null(dh0)) {
      dh0 <- cur_dh0(x, na.rm = TRUE)
    }
    x[[".datehour"]] <- dh0 + x$time * 60 * 60
  }

  nmtran <- c("ID", "time", "evid", "cmt", "amt", "DV", "mdv", "ss", "addl", "ii", "rate")
  # Relocate
  x <- relocate(x, any_of(nmtran))

  # Fill covariates (NOCB)
  non_nmtran_variables <- names(x)[!names(x) %in% c(nmtran, "AOLA", "TOLA", ".datehour")]
  x <- x %>%
    group_by(.data$ID) %>%
    fill(any_of(non_nmtran_variables), .direction = "updown") %>%
    ungroup()

  # Type
  if (!is.null(x[["ID"]])) x$ID <- as.integer(x$ID)
  if (!is.null(x[["evid"]])) x$evid <- as.integer(x$evid)
  if (!is.null(x[["cmt"]])) x$cmt <- as.integer(x$cmt)
  if (!is.null(x[["mdv"]])) x$mdv <- as.integer(x$mdv)
  if (!is.null(x[["ss"]])) x$ss <- as.integer(x$ss)
  if (!is.null(x[["addl"]])) x$addl <- as.integer(x$addl)

  if (!is.null(x[["DV"]])) x$DV <- as.double(x$DV)
  if (!is.null(x[["amt"]])) x$amt <- as.double(x$amt)
  if (!is.null(x[["time"]])) x$time <- as.double(x$time)
  if (!is.null(x[["ii"]])) x$ii <- as.double(x$ii)
  if (!is.null(x[["rate"]])) x$rate <- as.double(x$rate)


  # If no pre-existing AMT, RATE, SS, II or ADDL in former data, lines will be filled with NA -> fill with 0 instead
  if (!is.null(x[["amt"]])) x$amt[is.na(x$amt)] <- 0
  if (!is.null(x[["addl"]])) x$addl[is.na(x$addl)] <- 0
  if (!is.null(x[["ii"]])) x$ii[is.na(x$ii)] <- 0
  if (!is.null(x[["rate"]])) x$rate[is.na(x$rate)] <- 0
  if (!is.null(x[["ss"]])) x$ss[is.na(x$ss)] <- 0
  x
}

#' Parse value to "POSIXct"
#'
#' @description A wrapper around functions of `lubridate`, mainly in order to transform characters into a date-time ("POSIXct") format.
#'
#' @param x a numeric or a character.
#' @param orders format specification for x, passed to [lubridate::parse_date_time()]
#'
#' @return a POSIXct
#' @export
#'
#' @examples
#' # POSITct are returned as is.
#' parse_datehour(x = as.POSIXct("2022-02-02 22:22:22", tz = "UTC"))
#'
#' # Numerics are passed to `lubridate::as_datetime()`.
#' parse_datehour(1643840542)
#'
#' # Characters are passed to `lubridate::parse_date_time()`.
#' # The format used will be the one defined in `orders`
#' parse_datehour(x = "2022-02-02 22:22:22", orders = "Ymd HMS")
#' parse_datehour(x = "02-02-2022 22:22", orders = "dmY HM")
#'
#' # By default, the following formats will be subsequently tried:
#' # "Ymd HMS", "Ymd HM", "dmY HMS", "dmY HM"
#'
#' # Alternatively, set a format through `options(mapbayr.datehour)`.
#' # Convenient for the use `.datehour` in  `adm_rows()` and `obs_rows()`.
#'
#' # Following format will return NA:
#' adm_rows(.datehour = "22:22 02-02-2022", amt = 100, cmt = 1)
#'
#' options(mapbayr.datehour = "HM dmY")
#' adm_rows(.datehour = "22:22 02-02-2022", amt = 100, cmt = 1)
#' options(mapbayr.datehour = NULL)
#'
parse_datehour <- function(x, orders = getOption("mapbayr.datehour", default = c("Ymd HMS", "Ymd HM", "dmY HMS", "dmY HM"))) {
  if (inherits(x, "POSIXct")) {
    return(x)
  }

  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop(
      "Package \"lubridate\" must be installed to use `.datehour`",
      call. = FALSE
    )
  }

  if (is.double(x)) {
    return(lubridate::as_datetime(x))
  }

  if (is.character(x)) {
    return(lubridate::parse_date_time(x = x, orders = orders, quiet = TRUE))
  }
}

cur_dh0 <- function(x, na.rm = FALSE) {
  if (is.null(x[[".datehour"]])) {
    return(NULL)
  }
  which_min_datehour <- which.min(x$.datehour)
  if (length(which_min_datehour) == 0) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  suppressWarnings(min(x$.datehour, na.rm = na.rm)) - x$time[which_min_datehour] * 60 * 60
}

datehour_manager <- function(old_data, time, .datehour) {
  dh0 <- NULL
  if (is.null(.datehour)) {
    if (is.null(time)) {
      if (all(old_data[["time"]] == 0) | nrow(old_data) == 0) {
        time <- 0
      } # else error later, a time must be provided
    } # else : time is time and dh0 does not change if any
  } else { # -> .datehour is non NULL
    .datehour <- parse_datehour(.datehour)
    if (is.null(time)) {
      if (nrow(old_data) == 0) {
        dh0 <- suppressWarnings(min(.datehour, na.rm = TRUE))
        time <- as.double.difftime(.datehour - dh0, units = "hours")
      } else { # old dataset exists
        if (is.null(old_data[[".datehour"]])) {
          if (all(old_data[["time"]] == 0)) {
            dh0 <- suppressWarnings(min(.datehour, na.rm = TRUE))
            time <- as.double.difftime(.datehour - dh0, units = "hours")
          } else { # cannot default to 0 -> stop.
            stop("Cannot assign when `.datehour` is in the timeline already defined by `time`.")
          }
        } else {
          old_dh0 <- cur_dh0(old_data, na.rm = TRUE)
          dh0 <- suppressWarnings(min(.datehour, old_dh0, na.rm = TRUE))
          delta_dh <- as.double.difftime(old_dh0 - dh0, units = "hours")
          old_data$time <- old_data$time + delta_dh
          time <- as.double.difftime(.datehour - dh0, units = "hours")
        }
      }
    } else { # time and .datehour are non-null
      if (length(time) != length(.datehour)) stop("`.time` and `.datehour` are of different length.")
      if (nrow(old_data) == 0 || is.null(old_data[[".datehour"]])) {
        dh0 <- unique(.datehour - time * 60 * 60)
        if (length(dh0) > 1) stop("Difference between values in `.datehour` are not equal to those in `time`. Cannot set a common initial time.")
      } else { # old dataset exists and has .datehour
        old_dh0 <- cur_dh0(old_data)
        new_dh0 <- unique(.datehour - time * 60 * 60)
        if (any(old_dh0 != new_dh0)) stop("`time` and `.datehour` are inconsistent with values already in the initial dataset.")
      }
    }
  }

  list(
    old_data = old_data,
    time = time,
    .datehour = .datehour,
    dh0 = dh0
  )
}

forbidden_covariate <- function(x, cov) {
  if (any(names(x) %in% cov)) stop("Cannot have a covariate named: ", paste(cov, collapse = " "))
}

#' @export
dplyr::filter

#' Filter a dataset within a mrgmod
#'
#' @param .data a mrgmod
#' @param ...,.preserve additional arguments for `dplyr::filter()`
#'
#' @return a mrgmod
#' @method filter mrgmod
#' @export
#' @examples
#' library(magrittr)
#' mod <- mrgsolve::mcode("mod", "$CMT FOO", compile = FALSE)
#' mod %>%
#'   adm_rows(amt = c(100, 200, 300), cmt = 1) %>%
#'   filter(amt != 200) %>%
#'   get_data()
filter.mrgmod <- function(.data, ..., .preserve = FALSE) {
  data <- dplyr::filter(get_data.mrgmod(.data), ... = ..., .preserve = .preserve)
  data_set(.data, data)
}
