#' Internal library of mapbay models
#'
#' @param path path to the internal library
#' @param app_list if TRUE, return a list used in ui.R of tki app
#'
#' @return a data.frame
#' @export
#'
#' @importFrom readr read_file
#' @importFrom magrittr %>%
#' @importFrom stringr str_split str_subset str_remove
#' @importFrom dplyr tibble group_by ungroup mutate pull
#' @importFrom purrr map map_chr
#' @importFrom tidyr nest
#' @importFrom rlang .data set_names
#'
mapbay_library <- function(path = system.file('mrg_models', package='mapbayr'), app_list = F){
  path <- system.file('mrg_models', package='mapbayr')
  filenames <- list.files(path)
  refs <- paste0(path, "/", filenames) %>%
    map_chr(~ read_file(.x) %>%
              str_split("\n") %>%
              unlist() %>%
              str_subset("model_ref") %>%
              str_remove(".*model_ref: ")%>%
              str_remove("\r")
    )

  drugs <- paste0(path, "/", filenames) %>%
    map_chr(~ read_file(.x) %>%
              str_split("\n") %>%
              unlist()%>%
              str_subset("drug") %>%
              str_remove(".*drug: ")%>%
              str_remove("\r")
    )


  if(app_list){

    tab <- tibble(
      DRUG = drugs,
      MODEL= filenames %>%
        str_remove("_mapbay.cpp") %>%
        set_names(refs)
    ) %>%
      group_by(.data$DRUG) %>%
      nest(MODEL = .data$MODEL) %>%
      ungroup()

    output <- tab %>%
      mutate(MODEL = map(.data$MODEL, function(x) unlist(x[["MODEL"]]))) %>%
      pull(.data$MODEL) %>%
      set_names(toupper(tab$DRUG))

  } else {
    output <- data.frame(
      DRUG = drugs,
      REF = refs,
      FILE = filenames
    )
  }
  return(output)

}



#' Print code of .cpp file
#'
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @return print code
#' @export
print_code <- function(model){
  cat(model$mrgsolve_model@code, sep = "\n")
}




