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
  v <- as.list(x)$details$data %>%
    filter(.data$block %in%c("CMT", "INIT")) %>%
    select(all_of('options')) %>%
    pull() %>%
    tolower() %>%
    str_which("adm") %>%
    as.integer()

  if(length(v)== 0){
    v <- NULL
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
  v <- as.list(x)$details$data %>%
    filter(.data$block %in%c("CMT", "INIT")) %>%
    select(all_of('options')) %>%
    pull() %>%
    tolower() %>%
    str_which("obs") %>%
    as.integer()

  if(length(v)== 0){
    v <- NULL
  }

  return(v)

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
  v <- str_c("D_", x@cmtL) %>%
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
log.transformation <- function(x){
  x@code %>%
    str_subset("EPS") %>%
    str_detect("exp *\\(.*EPS") %>%
    any()
}

# ETA NAMES -----------


#' Get eta names from mrgsolve model
#'
#' @param x model object
#'
#' @return a character string vector
#' @export
#'
#' @examples
#' model <- mrgsolve::mread("ex_mbr1", mbrlib())
#' mbr_eta_names(model)
mbr_eta_names <- function(x){
  ((as.list(x))$details$data %>%
     filter(.data$block=="PARAM", str_detect(.data$name, "ETA")))$descr
}


# COVARIATES -----------

#' Get covariate names from mrgsolve model
#'
#' @param x model object
#'
#' @return a character string vector
#' @export
#'
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
#' @export
#'
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
#' @export
#'
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




