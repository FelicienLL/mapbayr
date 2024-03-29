#' Read compartment options in a model
#'
#' @name x_cmt
#' @param x model object
#' @return a vector of compartment identified as default "administration" or "observation" compartments.
#'
#' @details
#' In a mrgsolve model, it is possible to specify options in `$CMT`. If `[ADM]` or `[OBS]` are set, mapbayr will interpret these as defaults administration and observation compartments, respectively.
#'
#' @examples
#' #Administration:  Both 1st and 0- order
#' model <- exmodel(6, compile = FALSE)
#' mrgsolve::see(model)
#' adm_cmt(model)
#'
#' #Observation: Both parent drug and metabolite
#' model <- exmodel(401, compile = FALSE)
#' mrgsolve::see(model)
#' obs_cmt(model)


#' @rdname x_cmt
#' @export
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

#' @rdname x_cmt
#' @export
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
  v <- sort(unique(data$cmt[data$mdv==0]))
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
#' @noRd
#' @examples
#' # Both 1st and 0- order administration
#' model <- exmodel(6, compile = FALSE)
#' adm_0_cmt(model)
adm_0_cmt <- function(x){
  v <- which(sapply(paste0("D_", x@cmtL), function(i) any(grepl(i, x@code)), USE.NAMES = FALSE))
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
  eps_patterns <- c(
    grep(pattern = "[^\\.{1}]", x = unlist(x@sigma@labels), value = TRUE),
    "EPS\\(\\d+\\)"
  )
  dv_patterns <- paste0("DV.*exp *\\(.*", eps_patterns)
  any(sapply(dv_patterns, grepl, x = x@code))
}

# ETA -----------


#' Description of ETA to estimate
#'
#' @param x model object
#'
#' @return a vector of character
#' @noRd
eta_descr <- function(x){
  etas <- eta_names(x)
  dat <- as.list(x)$details$data

  if(is.null(dat[["block"]])){ # Is it annotated ? if not put ETA1, ETA2 etc...
    return(etas)
  } else { # If it is annotated, take names
    ans <- character(0)
    for(i in etas){
      idescr <- dat$descr[(dat$name==i)]
      if(length(idescr)==0){
        idescr <- i
      }
      if(is.na(idescr)){
        idescr <- i
      }
      ans <- c(ans, idescr)
    }
    return(ans)
  }
}


# COVARIATES -----------

#' Get covariate names from mrgsolve model
#'
#' @param x model object
#'
#' @return a character string vector
#' @noRd
#' @examples
#' model <- exmodel(301, compile = FALSE)
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
#' model <- exmodel(301, compile = FALSE)
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
#' model <- exmodel(301, compile = FALSE)
#' mbr_cov_descr(model)
mbr_cov_descr <- function(x){
  as.list(x)$details$data %>%
    filter(.data$name %in% mbr_cov_names(x)) %>%
    mutate(covariate_description = paste0(.data$descr, " (", .data$unit, ")")) %>%
    pull(.data$covariate_description)
}
