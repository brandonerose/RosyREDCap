#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
blank_RosyREDCap <- function(){
  DB <-RosyDB:::blank_DB()
  DB$REDCap <- list(
    token_name=NULL,
    project_id=NULL,
    project_title= NULL,
    id_col=NULL,
    version=NULL,
    project_info=NULL,
    log=NULL,
    users=NULL,
    current_user=NULL,
    choices=NULL,
    raw_structure_cols = NULL,
    is_longitudinal = NULL,
    has_arms = NULL,
    has_multiple_arms = NULL,
    has_arms_that_matter = NULL,
    has_repeating_instruments_or_events = NULL,
    has_repeating_instruments = NULL,
    has_repeating_events = NULL
  )
  AFTER <- which(names(DB)=="dir_path")
  NEW <- which(names(DB)=="REDCap")
  ORDER <- 1:length(DB)
  ORDER <- ORDER[-NEW]
  AFTER <- c(1:AFTER)
  ORDER <- ORDER[-AFTER]
  AFTER
  DB <- DB[c(AFTER,NEW,ORDER)]
  DB$links$github <- "https://github.com/brandonerose/RosyREDCap"
  new_link_names <- c(
    "REDCap_base",
    "REDCap_URI",
    "REDCap_home",
    "REDCap_record_home",
    "REDCap_record_subpage",
    "REDCap_records_dashboard",
    "REDCap_API",
    "REDCap_API_playground"
  )
  for(new_link_name in new_link_names){
    DB$links[[new_link_name]] <- NA
  }
  return(DB)
}
validate_RosyREDCap <- function(DB,silent = T,warn_only = F){
  DB <- RosyDB:::validate_DB(DB,silent = silent,warn_only = warn_only,allowed_names = names(blank_RosyREDCap()))
  return(DB)
}
load_RosyREDCap <- function(short_name,validate = T){
  projects <- get_projects()
  if(nrow(projects)==0)return(blank_RosyREDCap())
  if(!short_name%in%projects$short_name)return(blank_RosyREDCap())
  DB_path <- file.path(DB$dir_path,"R_objects",paste0(short_name,".rdata"))
  if(!file.exists(DB_path))return(blank_RosyREDCap())
  readRDS(file=DB_path) %>%
    validate_RosyREDCap(silent = F, warn_only = !validate) %>%
    return()
}
#' @title Setup for DB including token
#' @param short_name character name as a shortcut
#' @param dir_path character file path of the directory
#' @param token_name character string of what the token is called when using Sys.setenv and Sys.getenv
#' @param REDCap_base character of the base REDCap link, ex. https://redcap.miami.edu
#' @param force logical for force blank load vs last save
#' @param validate logical for validation
#' @param merge_form_name name of merged non-repeating to be used in package
#' @return DB
#' @export
setup_RosyREDCap <- function (
    short_name, dir_path, token_name, REDCap_base, force = F,
    merge_form_name = "merged", validate = T, use_csv = F
)
{
  if(missing(short_name))stop("`short_name` is required for DBs that haven't been validated")
  missing_dir_path <- missing(dir_path)
  if(missing_dir_path){
    warning("If you don't supply a directory, RosyDB will only run in R session. Package is best with a directory",immediate. = T)
    DB <- blank_RosyREDCap()
  }
  if( ! missing_dir_path){
    DB <- load_RosyREDCap(dir_path,validate = validate)
    DB$dir_path <-RosyDB:::set_dir(dir_path)
  }
  DB$short_name <- validate_env_name(short_name)
  if(validate)DB <- validate_RosyREDCap(DB)
  DB$internals$use_csv <- use_csv
  DB$data <- DB$data %>% all_character_cols_list()
  if (force || is.null(DB$internals$last_metadata_update) ||
      is.null(DB$REDCap$project_info) || is.null(DB$short_name) |
      is.null(DB$REDCap$token_name) || is.null(DB$links$REDCap_URI) |
      is.null(DB$REDCap$project_title) || is.null(DB$REDCap$project_id)) {
    if (missing(short_name)) stop("`short_name` is required for DBs that haven't been validated")
    if (missing(token_name)) stop("`token_name` is required for DBs that haven't been validated")
    if (missing(REDCap_base))  stop("`REDCap_base` is required for DBs that haven't been validated")
    DB$REDCap$token_name <- token_name %>% validate_env_name()
    DB$links$REDCap_base <- validate_web_link(REDCap_base)
    DB$links$REDCap_URI <- DB$links$REDCap_base %>% paste0("api/")
    if (validate)DB <- validate_RosyREDCap(DB)
  } else {
    if (!missing(token_name)) {
      if (validate) {
        if (DB$REDCap$token_name != token_name)
          stop("The `token_name`, ", token_name, ", you provided does not match the one the was loaded ",
               DB$REDCap$token_name)
      } else {
        DB$REDCap$token_name <- token_name %>% validate_env_name()
      }
    }
    if (!missing(REDCap_base)) {
      if (validate) {
        if (DB$links$REDCap_base != REDCap_base)
          stop("The `REDCap_base`, ", REDCap_base, ", you provided does not match the one the was loaded ",
               DB$links$REDCap_base)
      } else {
        DB$links$REDCap_base <- validate_web_link(REDCap_base)
        DB$links$REDCap_URI <- DB$links$REDCap_base %>% paste0("api/")
      }
    }
  }
  if (!missing(merge_form_name)) {
    DB$internals$merge_form_name <- merge_form_name
  }
  DB$internals$use_csv <- use_csv
  DB$data <- DB$data %>% all_character_cols_list()
  return(DB)
}
