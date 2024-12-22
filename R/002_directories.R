#' @import RosyUtils
set_dir <- function(dir_path){
  dir_path <- clean_dir_path(dir_path)
  if( ! file.exists(dir_path)){
    if(utils::menu(choices = c("Yes","No"),title = "No file path found, create?")==1){
      dir.create(file.path(dir_path))
    }
    if ( ! file.exists(dir_path)) {
      stop("Path not found. Use absolute path or choose one within R project working directory.")
    }
  }
  for(folder in dir_folders){
    if ( ! file.exists(file.path(dir_path,folder))) {
      dir.create(file.path(dir_path,folder),showWarnings = F)
    }
  }
  return(validate_dir(dir_path,silent=F))
}
#' @title get your directory
#' @inheritParams save_DB
#' @export
get_dir <- function(DB){
  dir_path <- DB$dir_path
  stop_mes <- "Did you use `set_dir()`?"
  if ( ! file.exists(dir_path)) {
    warning("Searched for directory --> '",dir_path,"' ...")
    stop(paste0("Does not exist. ", stop_mes))
  }
  return(validate_dir(dir_path,silent=T))
}
#' @title nav_to_dir
#' @return opens browser link
#' @export
nav_to_dir <- function(DB){
  rstudioapi::filesPaneNavigate(DB$dir_path)
  utils::browseURL(DB$dir_path)
}
dir_folders <- c("R_objects","output","scripts","input")
validate_dir <- function(dir_path,silent=T){
  #param check
  dir_path <- clean_dir_path(dir_path)
  if ( ! file.exists(dir_path)) stop("dir_path does not exist")
  if ( ! is.logical(silent)) stop("silent parameter must be T/F")
  stop_mes <- "Did you use `set_dir()`?"
  for(folder in dir_folders){
    if ( ! file.exists(file.path(dir_path,folder))) stop("'",dir_path,"/",folder,"' missing! ",stop_mes)
  }
  # if ( ! file.exists(file.path(dir_path,"ref_tables"))) stop("'",dir_path,"/ref_tables' missing! ",stop_mes)
  if( ! silent) bullet_in_console("Directory is Valid!",url=dir_path,bullet_type = "v")
  dir_path
}
clean_dir_path <- function(dir_path){
  if ( ! is.character(dir_path)) stop("dir must be a character string")
  dir_path <- dir_path %>% trimws(whitespace = "[\\h\\v]") %>% normalizePath( winslash = "/",mustWork = F)
  return(dir_path)
}
