#' Perform mapbayesian estimation from a mrgsolve model and a NM_tran dataset
#'
#' @param x model file
#' @param data NM tran data to optimize
#' @param method "newuoa" or "L-BFGS-B"
#' @param output return a mapbay_tab only
#' @param control a list passed to the optimizer (see source code for default, as function of the optimizer)
#' @param force_initial_eta a numeric vector of starting estimates (exact length of eta to estimate )
#' @param quantile_bound for L-BFGS-B only: a numeric value of the probability expected as extreme value for a ETA
#'
#' @return default: a list with data, model, initial and final eta, mapbay_tab and rough optimization output
#' @export
#'
mbrest <- function(x, data = NULL, method = "newuoa", output = NULL, control = list(), force_initial_eta = NULL, quantile_bound = 0.001){
  arg.optim <- preprocess.optim(method = method, model = x, control = control, force_initial_eta = force_initial_eta, quantile_bound = quantile_bound)

  if(is.null(data)){
    data <- x@args$data
  }

  idata <- preprocess.data(data)

  arg.ofv <- idata %>%
    map(preprocess.ofv, model = x)

  opt.value <- arg.ofv %>%
    map(function(x){
       cat(paste0("\nID ", unique(x$mrgsolve_model@args$data$ID), "..."))
      #cat(paste0("\nID ", names(x), "..."))
      opt <- do.call(quietly(optimx), c(arg.optim, x))$result
      cat(" done.\n")
      return(opt)
    })

  post <- list(
    data = idata,
    opt.value = opt.value,
    arg.ofv = arg.ofv) %>%
    pmap(postprocess,
         model = x,
         arg.optim = arg.optim)

  output_mbr(idata = idata, model = x, arg.optim = arg.optim, arg.ofv = arg.ofv, opt.value = opt.value, post = post, output = output)
}
