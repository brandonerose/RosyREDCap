#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
blank_RosyREDCap <- function(){
  DB <-RosyDB:::blank_DB()
  DB$redcap <- list(
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
    has_repeating_forms_or_events = NULL,
    has_repeating_forms = NULL,
    has_repeating_events = NULL
  )
  AFTER <- which(names(DB)=="dir_path")
  NEW <- which(names(DB)=="redcap")
  ORDER <- 1:length(DB)
  ORDER <- ORDER[-NEW]
  AFTER <- c(1:AFTER)
  ORDER <- ORDER[-AFTER]
  AFTER
  DB <- DB[c(AFTER,NEW,ORDER)]
  DB$links$github <- "https://github.com/brandonerose/RosyREDCap"
  new_link_names <- c(
    "redcap_base",
    "redcap_uri",
    "redcap_home",
    "redcap_record_home",
    "redcap_record_subpage",
    "redcap_records_dashboard",
    "redcap_api",
    "redcap_api_playground"
  )
  for(new_link_name in new_link_names){
    DB$links[[new_link_name]] <- NA
  }
  DB$internals$DB_type <- "redcap"
  return(DB)
}
validate_RosyREDCap <- function(DB,silent = T,warn_only = F){
  DB <- RosyDB:::validate_DB(DB,silent = silent,warn_only = warn_only,allowed_names = names(blank_RosyREDCap()))
  return(DB)
}
#' @title load_RosyREDCap
#' @export
load_RosyREDCap <- function(short_name,dir_path,validate = T){
  projects <- get_projects()
  if(missing(dir_path)){
    if(nrow(projects)==0)return(blank_RosyREDCap())
    if(!short_name%in%projects$short_name)return(blank_RosyREDCap())
    dir_path <- projects$dir_path[which(projects$short_name==short_name)]
  }else{
    if(!file.exists(dir_path))stop("`dir_path` doesn't exist")
    #consider search feature by saving all rdata files with suffix "_RosyREDCap.Rdata"
  }
  DB_path <- file.path(dir_path,"R_objects",paste0(short_name,".rdata"))
  if(!file.exists(DB_path))return(blank_RosyREDCap())
  readRDS(file=DB_path) %>%
    validate_RosyREDCap(silent = F, warn_only = !validate) %>%
    return()
}
#' @title Setup for DB including token
#' @param DB a list object slightly modified version of RosyDB and contains all metadata, data, transformations, and more
#' @param short_name character name as a shortcut
#' @param dir_path character file path of the directory
#' @param token_name character string of what the token is called when using Sys.setenv and Sys.getenv
#' @param redcap_base character of the base REDCap link, ex. https://redcap.miami.edu
#' @param force logical for force blank load vs last save
#' @param validate logical for validation
#' @param merge_form_name name of merged non-repeating to be used in package
#' @return DB
#' @export
setup_RosyREDCap <- function (
    short_name,
    dir_path,
    token_name,
    redcap_base,
    force = F,
    merge_form_name = "merged",
    validate = T,
    use_csv = F
)
{
  if(missing(short_name))stop("`short_name` is required for DBs that haven't been validated")
  missing_dir_path <- missing(dir_path)
  if(missing_dir_path){
    warning("If you don't supply a directory, RosyDB will only run in R session. Package is best with a directory",immediate. = T)
    DB <- blank_RosyREDCap()
  }
  if( ! missing_dir_path){
    if(force){
      # log_save <- tryCatch({
      #   load_RosyREDCap(short_name=short_name)$redcap$log
      # },error = function(e) {NULL})
      DB <- blank_RosyREDCap()
    }else{
      DB <- load_RosyREDCap(short_name,validate = validate)
    }
    DB$dir_path <-RosyDB:::set_dir(dir_path)
  }
  DB$short_name <- validate_env_name(short_name)
  if(validate)DB <- validate_RosyREDCap(DB)
  DB$internals$use_csv <- use_csv
  DB$data <- DB$data %>% all_character_cols_list()
  if (force || is.null(DB$internals$last_metadata_update) ||
      is.null(DB$redcap$project_info) || is.null(DB$short_name) |
      is.null(DB$redcap$token_name) || is.null(DB$links$redcap_uri) |
      is.null(DB$redcap$project_title) || is.null(DB$redcap$project_id)) {
    if (missing(short_name)) stop("`short_name` is required for DBs that haven't been validated")
    if (missing(token_name)) stop("`token_name` is required for DBs that haven't been validated")
    if (missing(redcap_base))  stop("`redcap_base` is required for DBs that haven't been validated")
    DB$redcap$token_name <- token_name %>% validate_env_name()
    DB$links$redcap_base <- validate_web_link(redcap_base)
    DB$links$redcap_uri <- DB$links$redcap_base %>% paste0("api/")
    if (validate)DB <- validate_RosyREDCap(DB)
  } else {
    if (!missing(token_name)) {
      if (validate) {
        if (DB$redcap$token_name != token_name)
          stop("The `token_name`, ", token_name, ", you provided does not match the one the was loaded ",
               DB$redcap$token_name)
      } else {
        DB$redcap$token_name <- token_name %>% validate_env_name()
      }
    }
    if (!missing(redcap_base)) {
      if (validate) {
        if (DB$links$redcap_base != redcap_base)
          stop("The `redcap_base`, ", redcap_base, ", you provided does not match the one the was loaded ",
               DB$links$redcap_base)
      } else {
        DB$links$redcap_base <- validate_web_link(redcap_base)
        DB$links$redcap_uri <- DB$links$redcap_base %>% paste0("api/")
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
redcap_field_types_not_in_data <- c(
  "descriptive", "checkbox"
)
