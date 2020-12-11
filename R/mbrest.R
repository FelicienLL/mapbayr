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

  data <- data %>%
    rename_with(tolower, any_of(c("TIME", "AMT", "MDV", "CMT", "EVID", "II", "ADDL", "SS", "RATE")))

  if(length(unique(data$ID)) != 1) stop("Only one individual at a time (consider apply or map)")

  arg.ofv <- preprocess.ofv(data = data, model = x)

  opt.value <- do.call(quietly(optimx), c(arg.optim, arg.ofv))$result

  post <- postprocess(data = data, model = x, opt.value = opt.value, arg.optim = arg.optim, arg.ofv = arg.ofv)

  mapbay_output <- post
  if(!is.null(output)){
    if(output == "df") mapbay_output <- post$mapbay_tab
  }

  return(mapbay_output)
}
