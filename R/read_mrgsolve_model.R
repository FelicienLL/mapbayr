#' Get administration compartment numbers from mrgsolve model
#'
#' @param x model object
#'
#' @return vector of integer
#' @export
#'
#' @examples
#' model <- mrgsolve::mread("ex_mbr1", mbrlib())
#' adm_cmt(model)
adm_cmt <- function(x){
  dat <- as.list(x)$details$data

  if(is.null(dat[["block"]])){ # Is it annotated ?
    v <- NULL
  } else { # If it is annotated, find where "ADM" is set
    datcmt <- dat[dat$block %in% c("CMT", "INIT"),]
    admcmtname <- datcmt$name[str_detect(tolower(datcmt$options), "adm")]
    v <- x@Icmt[x@cmtL %in% admcmtname]

    if(length(v)== 0){ # If not found, return NULL
      v <- NULL
    }
  }

  return(v)
}

#' Get observation compartment numbers from mrgsolve model
#'
#' @param x model object
#'
#' @return vector of integer
#' @export
#'
#' @examples
#' model <- mrgsolve::mread("ex_mbr1", mbrlib())
#' obs_cmt(model)
obs_cmt <- function(x){
  dat <- as.list(x)$details$data

  if(is.null(dat[["block"]])){ # Is it annotated ?
    v <- NULL
  } else { # If it is annotated, find where "OBS" is set
    datcmt <- dat[dat$block %in% c("CMT", "INIT"),]
    obscmtname <- datcmt$name[str_detect(tolower(datcmt$options), "obs")]
    v <- x@Icmt[x@cmtL %in% obscmtname]

    if(length(v)== 0){ # If not found, return NULL
      v <- NULL
    }
  }

  return(v)
}

obs_cmt_data <- function(data){
  v <- sort(unique(data[data$mdv==0,]$cmt))
  if(length(v)== 0){
    v <- NULL
  }
  return(v)
}

fit_cmt <- function(x, data){
  cmt_model <- obs_cmt(x)
  if(is.null(cmt_model)){
    return(obs_cmt_data(data))
  } else{
    return(cmt_model)
  }
}


#' Get zero-order infusion compartment from mrgsolve model
#'
#' @param x model object
#'
#' @return vector of integer
#' @export
#'
#' @examples
#' model <- mrgsolve::mread("ex_mbr1", mbrlib())
#' adm_0_cmt(model)
adm_0_cmt <- function(x){
  v <- paste0("D_", x@cmtL) %>%
    map(str_detect, string = x@code) %>%
    map(any) %>%
    as.logical() %>%
    which()

  if(length(v)== 0){
    v <- NULL
  }

  return(v)
}


#' Check if error is log-additive
#'
#' @param x model file
#'
#' @return a logical
#' @noRd
log_transformation <- function(x){
  x@code %>%
    str_subset("EPS") %>%
    str_detect("exp *\\(.*EPS") %>%
    any()
}

# ETA -----------

#' Name of ETA no estimate
#'
#' @param x model file
#'
#' @return a vector of character
#' @noRd
eta_names <- function(x){
  parnames <- names(param(x))
  v <- parnames[grepl("^ETA\\d+$", parnames)]
  if(length(v)== 0){
    v <- NULL
  }
  return(v)
}

#' Number of ETA to estimate
#'
#' @param x model file
#'
#' @return a numeric
#' @noRd
n_eta <- function(x){
  length(eta_names(x))
}

#' Description of ETA to estimate
#'
#' @param x model object
#'
#' @return a vector of character
#' @noRd
eta_descr <- function(x){
  dat <- as.list(x)$details$data

  if(is.null(dat[["block"]])){ # Is it annotated ? if not put ETA1, ETA2 etc...
    v <- eta_names(x)
  } else { # If it is annotated, take names
    datpar <- filter(dat, .data$block=="PARAM")
    v <- datpar$descr[datpar$name %in% eta_names(x)]
  }

  return(v)

}


# COVARIATES -----------

#' Get covariate names from mrgsolve model
#'
#' @param x model object
#'
#' @return a character string vector
#' @noRd
#' @examples
#' model <- mrgsolve::mread("ex_mbr1", mbrlib())
#' mbr_cov_names(model)
mbr_cov_names <- function(x){
  as.list(x)$covariates
}

#' Get covariate reference values from mrgsolve model
#'
#' @param x model object
#'
#' @return a named vector of numerics
#' @noRd
#' @examples
#' model <- mrgsolve::mread("ex_mbr1", mbrlib())
#' mbr_cov_refvalues(model)
mbr_cov_refvalues <- function(x){
  unlist(x@param[mbr_cov_names(x)])
}





#' Get covariate description from mrgsolve model
#'
#' @param x model object
#'
#' @return a character string vector
#' @noRd
#' @examples
#' model <- mrgsolve::mread("ex_mbr1", mbrlib())
#' mbr_cov_descr(model)
mbr_cov_descr <- function(x){
  as.list(x)$details$data %>%
    filter(.data$name %in% mbr_cov_names(x)) %>%
    mutate(covariate_description = paste0(.data$descr, " (", .data$unit, ")")) %>%
    pull(.data$covariate_description)
}



#DEPRECATED FUNCTIONS

# #' Get drug name from mrgsolve model
# #'
# #' @param x model object
# #'
# #' @return name of the drug as a character string
# #' @export
# #' @examples
# #' model <- mrgsolve::mread("ex_mbr1", mbrlib())
# #' mbr_drug(model)
# #'
# mbr_drug <- function(x){
#   pattern <- "drug"
#   x@code %>%
#     str_subset(paste0("\\s*-\\s*", pattern, "\\s*:\\s*")) %>%
#     str_remove(paste0("\\s*-\\s*", pattern, "\\s*:\\s*")) %>%
#     str_squish()
# }

# #' Get model reference from mrgsolve model
# #'
# #' @param x model object
# #'
# #' @return model reference as a character string
# #' @export
# #' @examples
# #' model <- mrgsolve::mread("ex_mbr1", mbrlib())
# #' mbr_model_ref(model)
# #'
# mbr_model_ref <- function(x){
#   pattern <- "model_ref"
#   x@code %>%
#     str_subset(paste0("\\s*-\\s*", pattern, "\\s*:\\s*")) %>%
#     str_remove(paste0("\\s*-\\s*", pattern, "\\s*:\\s*")) %>%
#     str_squish()
# }

# # Get model file from mrgsolve model
# #
# #@param x model object
# #
# # @return model file as a character string
# # @export
# # @examples
# # model <- mrgsolve::mread("ex_mbr1", mbrlib())
# # mbr_model_name(model)
# #
# mbr_model_file <- function(x){
#   x@model %>%
#     str_remove("_mapbay_cpp")
# }

# #' Get model name from mrgsolve model
# #'
# #' @param x model object
# #'
# #' @return model name as a character string
# #' @export
# #' @examples
# #' model <- mrgsolve::mread("ex_mbr1", mbrlib())
# #' mbr_model_name(model)
# #'
# mbr_model_name <- function(x){
#   x@model %>%
#     str_remove("_mapbay_cpp") %>%
#     str_replace("_", " ") %>%
#     str_to_title()
# }

# #' Get concentration units from mrgsolve model
# #'
# #' @param x model object
# #'
# #' @return vector of character
# #' @export
# #'
# #' @examples
# #' model <- mrgsolve::mread("ex_mbr1", mbrlib())
# #' mbr_conc_unit(model)
# mbr_conc_unit <- function(x){
#   as.list(x)$details$data %>%
#     filter(.data$block  %in%c("CMT", "INIT"), .data$unit != '') %>%
#     slice(1) %>%
#     pull(.data$unit)
# }

# #' Get scaling factor from user to model
# #'
# #' @param x model object
# #'
# #' @return an integer
# #' @export
# #'
# #' @examples
# #' #Dose expected in mg, and Volume of distribution in L.
# #' #Possible values for concentration units: mg/L, ng/mL, pg/mL
# #' #Microgramme not supported for compatibility and encoding reasons (greek letter mu)
# #' model <- mrgsolve::mread("ex_mbr1", mbrlib())
# #' mbr_conc_scaling(model)
# mbr_conc_scaling <- function(x){
#   switch(mbr_conc_unit(x),
#          "mg/L"  = 1,
#          "ng/mL" = 1000,
#          "pg/mL" = 1000000)
# }

# #' Get parameter units from mrgsolve model
# #'
# #' @param x model object
# #'
# #' @return a character string vector
# #' @export
# #'
# #' @examples
# #' model <- mrgsolve::mread("ex_mbr1", mbrlib())
# #' mbr_param_units(model)
# mbr_param_units <- function(x){
#   ((as.list(x))$details$data %>%
#      filter(.data$block=="PARAM", str_detect(.data$name, "ETA")))$unit
# }

# #' Get parameter typical values from mrgsolve model
# #'
# #' @param x model object
# #'
# #' @return a numeric vector
# #' @export
# #'
# #' @examples
# #' model <- mrgsolve::mread("ex_mbr1", mbrlib())
# #' mbr_param_tv(model)
# mbr_param_tv <- function(x){
#   tab <- (as.list(x))$details$data %>%
#     filter(.data$block=="PARAM") %>%
#     filter(str_detect(.data$name, "ETA"))
#   x@param@data %>%
#     unlist() %>%
#     enframe() %>%
#     mutate(descr = str_remove(.data$name, "TV"), .keep = "unused") %>%
#     right_join(tab, by = "descr") %>%
#     replace_na(list(value = 1)) %>%
#     mutate(descr = factor(.data$descr, mbr_param_names(x))) %>%
#     arrange(.data$descr) %>%
#     pull(.data$value)
# }
#
#




