#' processing_function
#'
#'Processes downloaded data for given location and given time period.
#'
#' @param filepath Enter filepath where data to be processed is stored
#'
#' @return
#' @export
#'
#' @examples
processing_function <- function(filepath = "C:/Docs/MIRO/radiation_model/download_test"){
  files <- list.files(pattern = ".rdata")
  load(files[1])



}



