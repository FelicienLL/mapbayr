get_LL <- function(x, LL = TRUE){
  opt <- x$opt.value
  ans <- matrix(opt$value, dimnames = list(opt$ID, NULL))
  if(LL){
    ans <- exp(-0.5 * ans)
  }
  ans
}

get_AIC <- function(x){
  OFV <- get_LL(x, LL = FALSE)
  k <- sum(grepl("ETA", names(x$opt.value)))
  exp(-0.5 * OFV - k)
}

model_averaging <- function(..., scheme = c("LL", "AIC"), estlist = NULL){
  if(is.null(estlist)){
    estlist <- list(...)
  }

  if(!all(sapply(estlist, inherits, "mapbayests"))){
    add_msg <- NULL
    if(all(sapply(estlist[[1]], inherits, "mapbayests"))){
      add_msg <- "\n Did you forget the argument `estlist = `?"
    }
    stop("All objects passed to `model_averaging() must be `mapbayests` class object", add_msg)
  }

  IDs <- lapply(estlist, function(x){x$opt.value$ID})

  if(length(unique(IDs)) != 1){
    stop("Subject IDs are not the same between estimation objects")
  }

  scheme <- scheme[1]

  scheme_fn <- switch(scheme,
                      LL = get_LL,
                      AIC = get_AIC)

  values <- do.call(
    cbind,
    lapply(estlist, scheme_fn) #no sapply to keep rownames
  )
  colnames(values) <- names(estlist)

  values / rowSums(values)
}
