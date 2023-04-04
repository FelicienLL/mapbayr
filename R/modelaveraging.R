get_LL <- function(x, LL = TRUE, colname = NULL){
  opt <- x$opt.value
  ans <- matrix(opt$value, dimnames = list(opt$ID, NULL))
  if(LL){
    ans <- exp(-0.5 * ans)
  }
  ans
}


model_averaging <- function(..., modlist = NULL){
  if(is.null(modlist)){
    modlist <- list(...)
  }

  if(!all(sapply(modlist, inherits, "mapbayests"))){
    add_msg <- NULL
    if(all(sapply(modlist[[1]], inherits, "mapbayests"))){
      add_msg <- "\n Did you forget the argument `modlist = `?"
    }
    stop("All objects passed to `model_averaging() must be `mapbayests` class object", add_msg)
  }

  values <- do.call(
    cbind,
    lapply(modlist, get_LL) #no sapply to keep rownames
  )
  colnames(values) <- names(modlist)

  values / rowSums(values)
}
