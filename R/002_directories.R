#' @import RosyUtils
#' @noRd
set_dir <- function(dir_path){
  dir_path <- clean_dir_path(dir_path)
  if( ! file.exists(dir_path)){
    if(utils::menu(choices = c("Yes","No"),title = paste0("No file path found for chosen directory, create? (",dir_path,")"))==1){
      dir.create(file.path(dir_path))
    }
    if ( ! file.exists(dir_path)) {
      stop("Path not found. Use absolute path or choose one within R project working directory.")
    }
  }
  for(folder in internal_dir_folders){
    if ( ! file.exists(file.path(dir_path,folder))) {
      dir.create(file.path(dir_path,folder),showWarnings = F)
    }
  }
  return(validate_dir(dir_path,silent=F))
}
#' @title get your directory
#' @inheritParams save_DB
#' @return file path of directory
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
#' @inheritParams save_DB
#' @return opens browser link
#' @export
nav_to_dir <- function(DB){
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    rstudioapi::filesPaneNavigate(DB$dir_path)
  }
  utils::browseURL(DB$dir_path)
}
#' @title List File Paths of RosyREDCap Projects in a Folder
#' @description
#' Searches a specified folder for files related to RosyREDCap projects and returns their file paths.
#' Optionally validates the folder to ensure it was previously set up using `setup_DB()`.
#'
#' @param file_path Character. The path to the folder to search.
#' @param validate Logical. If `TRUE`, the function will only accept valid directories previously set up with `setup_DB()`. Default is `TRUE`.
#'
#' @return
#' A character vector of file paths for valid RosyREDCap project files in the folder. Returns an empty character vector if no valid files are found.
#'
#' @details
#' This function checks a folder (and optionally validates its setup) for `.RData` files that correspond to RosyREDCap projects.
#' It identifies files with the extension `.RData` and names ending in `_RosyREDCap`, filtering out any unrelated files.
#'
#' @seealso
#' \link{setup_DB} for setting up valid directories.
#'
#' @export
check_folder_for_projects <- function(file_path,validate = T){
  check_path <- file_path
  if(validate){
    file_path <- validate_dir(file_path)
    check_path <- file.path(file_path,"R_objects")
  }
  files <- list.files.real(check_path,full.names = T,recursive = T)
  if(length(file)==0)return(character(0))
  file_name <- tools::file_path_sans_ext(basename(files))
  file_ext <- tools::file_ext(files) %>% tolower()
  df <- data.frame(
    file_path = files,
    file_name = file_name,
    file_ext = file_ext
  )
  df <- df[which((df$file_ext == "rdata")&(endsWith(df$file_name,"_RosyREDCap"))),]
  if(nrow(df)==0)return(character(0))
  return(df$file_path)
}
internal_dir_folders <- c("R_objects","output","scripts","input","REDCap")
validate_dir <- function(dir_path,silent=T){
  #param check
  dir_path <- clean_dir_path(dir_path)
  if ( ! file.exists(dir_path)) stop("dir_path does not exist")
  if ( ! is.logical(silent)) stop("silent parameter must be T/F")
  stop_mes <- "Did you use `setup_DB()`?"
  for(folder in internal_dir_folders){
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
