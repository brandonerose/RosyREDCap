#' @import RosyUtils
#' @import RosyApp
#' @rdname setup-load
#' @title Setup or Load RosyREDCap Project
#' @description
#' Setup or Load the `DB` object with the necessary REDCap API token and other configurations.
#'
#' @details
#' This function sets up the `DB` object by storing the REDCap API token and other configurations required for interacting with the REDCap server.
#' It ensures that the token is valid and ready for use in subsequent API calls.
#' Neither function directly attempts communication with REDCap.
#'
#' `setup_DB` is used the first time you initialize/link a REDCap project. Mainly, it sets your unique `short_name` and your intended directory. Unless you run \code{force = TRUE} the default will first try load_DB. dir_path is technically optional but without it the user cannot save/load/update projects.
#'
#' `load_DB` can be used with just the `short_name` parameter after you have already run `setup_DB` in the past with an established directory. `dir_path` is optional for this function but can be used if you relocated the directory.
#'
#' @param short_name A character string with no spaces or symbols representing the unique short name for the REDCap project.
#' @param dir_path Optional character string representing the directory path where you want the REDCap project data to be stored. If missing, DB object will only be in current R session.
#' @param redcap_base A character string representing the base URL of the REDCap server.
#' @param force Logical (TRUE/FALSE). If TRUE, forces the setup even if the `DB` object already exists. Default is `FALSE`.
#' @param merge_form_name A character string representing the name of the merged form. Default is "merged".
#' @param use_csv Logical (TRUE/FALSE). If TRUE, uses CSV files for data storage. Default is `FALSE`.
#' @param auto_check_token Logical (TRUE/FALSE). If TRUE, automatically checks the validity of the REDCap API token. Default is `TRUE`.
#' @return RosyREDCap `DB` list object.
#' @seealso
#' \code{\link[RosyREDCap]{get_projects}} for retrieving a list of projects from the directory cache.
#' @examplesIf FALSE
#' # Initialize the DB object with the REDCap API token and URL
#' DB <- setup_DB(
#'   short_name = "TEST",
#'   dir_path = "path/to/secure/file/storage",
#'   redcap_base = "https://redcap.yourinstitution.edu/"
#' )
#' DB <- load_DB("TEST")
#' @family DB object
#' @export
setup_DB <- function (
    short_name,
    dir_path,
    redcap_base,
    force = F,
    merge_form_name = "merged",
    use_csv = F,
    auto_check_token = T
)
{
  projects <- get_projects()# add cache check
  if(missing(short_name))stop("`short_name` is required for DBs that haven't been validated")
  short_name <- validate_env_name(short_name)
  token_name <- paste0("RosyREDCap_token_",short_name) %>% validate_env_name()
  missing_dir_path <- missing(dir_path)
  if(missing_dir_path){
    warning("If you don't supply a directory, RosyREDCap will only run in R session. Package is best with a directory.",immediate. = T)
    DB <- blank_DB()
  }
  if( ! missing_dir_path){
    if(force){
      DB <- blank_DB()
    }else{
      DB <- load_DB(short_name)
    }
    DB$dir_path <-set_dir(dir_path)
  }
  DB$short_name <- short_name
  DB$internals$use_csv <- use_csv
  DB$redcap$token_name <- token_name
  DB$links$redcap_base <- validate_web_link(redcap_base)
  DB$links$redcap_uri <- DB$links$redcap_base %>% paste0("api/")
  DB$internals$merge_form_name <- validate_env_name(merge_form_name)
  DB$internals$use_csv <- use_csv
  DB$data <- DB$data %>% all_character_cols_list()
  bullet_in_console(paste0("Token name: '",token_name,"'"))
  if(auto_check_token){
    if(!is_valid_REDCap_token(validate_REDCap_token(DB))){
      set_REDCap_token(DB,ask = F)
    }
  }
  validate_REDCap_token(DB,silent = F)
  return(DB)
}
#' @rdname setup-load
#' @export
load_DB <- function(short_name,dir_path,validate = T){
  projects <- get_projects()
  if(missing(dir_path)){
    if(nrow(projects)==0)return(blank_DB())
    if(!short_name%in%projects$short_name)return(blank_DB())
    dir_path <- projects$dir_path[which(projects$short_name==short_name)]
  }else{
    if(!file.exists(dir_path))stop("`dir_path` doesn't exist")
    #consider search feature by saving all rdata files with suffix "_RosyREDCap.Rdata"
  }
  DB_path <- file.path(dir_path,"R_objects",paste0(short_name,".rdata"))
  if(!file.exists(DB_path))return(blank_DB())
  readRDS(file=DB_path) %>%
    validate_DB(silent = F, warn_only = !validate) %>%
    return()
}
#' @title Saves DB in the directory
#' @return Message
#' @family DB object
#' @export
save_DB <- function(DB){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  DB <- DB %>% validate_DB()
  # DB <- reverse_clean_DB(DB) # # problematic because setting numeric would delete missing codes
  DB %>% saveRDS(file=file.path(DB$dir_path,"R_objects",paste0(DB$short_name,".rdata")))
  add_project(DB)
  # save_xls_wrapper(DB)
  bullet_in_console(paste0("Saved ",DB$short_name," to directory!"),url = DB$dir_path,bullet_type = "v")
}
#' @title Shows DB in the env
#' @inheritParams save_DB
#' @param also_metadata logical for including metadata
#' @param data_choice whether to use 'data' or 'data'
#' @param only_dfs logical for including data.frames
#' @return DB tables
#' @family DB object
#' @export
show_DB <- function(DB,also_metadata=T,only_dfs = T){
  DB <- validate_DB(DB)
  data_list <- list()
  DB$data %>% add_list_to_global(only_dfs = only_dfs)
  if(also_metadata){
    DB$metadata %>% add_list_to_global(only_dfs = only_dfs)
  }
  data_list %>% list2env(envir = .GlobalEnv)
}
#' @title Deletes DB object from directory (solves occasional problems)
#' @inheritParams save_DB
#' @inheritParams setup_DB
#' @param dir_path character file path of the directory
#' @return message
#' @family DB object
#' @export
delete_DB <- function(DB,dir_path){
  if(!missing(DB)){
    DB <- validate_DB(DB)
    DIR <- DB$dir_path
    if(!missing(dir_path))warning("You only need to provide a directory path using a DB object OR dir_path. DB will be used by default.",immediate. = T)
  } else {
    if(missing(dir_path))stop("You must provide a directory path using a DB object or dir_path")
    DIR <- dir_path
  }
  DIR  <- validate_dir(DIR,silent = F)
  delete_this <- file.path(DIR,"R_objects","DB.Rdata")
  if(file.exists(delete_this)){
    unlink(delete_this) %>%
      message("Deleted saved DB")
  }else{
    warning("The DB object you wanted to is not there. Did you delete already? ",delete_this)
  }
}
validate_DB <- function(DB,silent = T,warn_only = F,allowed_names = names(blank_DB())){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  outcome_valid <- T
  messages <- NULL
  if( ! all(allowed_names%in%names(DB))){
    outcome_valid <- F
    messages <- messages %>% append("`DB` does not have the appropriate names. Did you use `load_DB()` or `setup_DB()` to generate it?")
  }
  if(is.null(DB$dir_path)){
    outcome_valid <- F
    messages <- messages %>% append("`DB$dir_path` is NULL!, Did you use `setup_DB()`?")
  }else{
    if( ! DB$dir_path %>% file.exists()) warning("`DB$dir_path`, '",DB$dir_path,"', does not exist!, Did you use `setup_DB()`?\nThis can also happen with shared directories.",immediate. = T)
  }
  if(is.null(DB$short_name)){
    outcome_valid <- F
    messages <- messages %>% append("`DB$short_name` is NULL!, Did you use `setup_DB()`?")
  }else{
    DB$short_name %>% validate_env_name()
  }
  if(!outcome_valid){
    for(m in messages){
      if(warn_only){
        warning(m,immediate. = T)
      }else{
        stop(m)
      }
    }
  }
  if(!silent){
    if((length(DB$data)==0)>0){
      warning("Valid list but no data yet!",immediate. = T)
    }
    if(outcome_valid){
      bullet_in_console(DB$short_name," is valid DB object!",bullet_type = "v")
    }
    bullet_in_console(DB$short_name %>% paste0(" loaded from: "),url = DB$dir_path,bullet_type = "v")
    if(DB$internals$is_transformed){
      bullet_in_console(DB$short_name %>% paste0(" is currently transformed! Can reverse with `untransform_DB(DB)`"),bullet_type = "i")
    }
  }
  DB
}
blank_DB <-  function(){ # can sort this better in version 3.0.0
  list(
    short_name=NULL,
    dir_path=NULL,
    redcap = list(
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
    ),
    metadata = list(# model
      forms=NULL,
      fields=NULL,
      choices=NULL,
      form_key_cols = NULL,
      arms=NULL,
      events=NULL,
      event_mapping = NULL,
      missing_codes=NULL
    ),
    data = NULL, #model
    data_update = NULL,
    quality_checks = NULL,
    transformation = list(
      forms = NULL,
      fields = NULL,
      field_functions = NULL,
      original_forms = NULL,
      original_fields = NULL,
      data_updates = NULL
    ),
    summary = list(
      subsets=NULL
    ),
    internals = list(
      last_test_connection_attempt = NULL,
      last_test_connection_outcome = NULL,
      last_metadata_update=NULL,
      last_metadata_dir_save=NULL,
      last_full_update=NULL,
      last_data_update=NULL,
      last_data_dir_save = NULL,
      last_data_transformation = NULL,
      last_summary = NULL,
      last_quality_check = NULL,
      last_clean = NULL,
      last_directory_save=NULL,
      data_extract_labelled = NULL,
      data_extract_merged = NULL,
      merge_form_name = "merged",
      DB_type = "redcap",
      is_transformed = F,
      is_clean = F,
      use_csv = F
    ),
    links = list(
      redcap_base = NULL,
      redcap_uri = NULL,
      redcap_home = NULL,
      redcap_record_home = NULL,
      redcap_record_subpage = NULL,
      redcap_records_dashboard = NULL,
      redcap_api = NULL,
      redcap_api_playground = NULL,
      pkgdown = "https://brandonerose.github.io/RosyREDCap/",
      github = "https://github.com/brandonerose/RosyREDCap/",
      thecodingdocs = "https://www.thecodingdocs.com/"
    )
  )
}
