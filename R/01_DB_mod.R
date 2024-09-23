#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
blank_REDCapDB <- function(){
  DB <-RosyDB:::blank_DB()
  DB$REDCap <- list()
  AFTER <- which(names(DB)=="dir_path")
  NEW <- which(names(DB)=="REDCap")
  ORDER <- 1:length(DB)
  ORDER <- ORDER[-NEW]
  AFTER <- c(1:AFTER)
  ORDER <- ORDER[-AFTER]
  AFTER
  DB <- DB[c(AFTER,NEW,ORDER)]
  return(DB)
}
#' @title Setup for DB including token
#' @param short_name character name as a shortcut
#' @param dir_path character file path of the directory
#' @param token_name character string of what the token is called when using Sys.setenv and Sys.getenv
#' @param redcap_base character of the base REDCap link, ex. https://redcap.miami.edu
#' @param force logical for force blank load vs last save
#' @param validate logical for validation
#' @param merge_form_name name of merged non-repeating to be used in package
#' @return DB
#' @export
setup_REDCapDB <- function(){

}
