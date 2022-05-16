exmodel <- function(num = 1, add_exdata = TRUE, ..., ID = 1, clean_data = TRUE){
  num <- as.double(num)
  check_num(num = num)

  model <- mread(make_model_name(num), ...)

  if(add_exdata){
    dat <- exdata(num = num, ID = ID, clean_data = clean_data)
    model <- data_set(x = model, data = dat)
  }
  model
}

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

make_model_name <- function(x){
  num <- stringr::str_pad(x, 3, pad = 0)
  nam <- paste0("mrg_", num, ".cpp")
  system.file("exmodel", nam, package = "mapbayr")
}

make_data_name <- function(x){
  num <- stringr::str_pad(x, 3, pad = 0)
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
