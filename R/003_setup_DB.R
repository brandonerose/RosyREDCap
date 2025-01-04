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
#' @param validate Logical (TRUE/FALSE). If TRUE, validates DB object based on current rules. Default is `TRUE`.
#' @param merge_form_name A character string representing the name of the merged form. Default is "merged".
#' @param use_csv Logical (TRUE/FALSE). If TRUE, uses CSV files for data storage. Default is `FALSE`.
#' @param auto_check_token Logical (TRUE/FALSE). If TRUE, automatically checks the validity of the REDCap API token. Default is `TRUE`.
#' @param DB_path A character string representing the file path of the exact `<short_name>_RosyREDCap.rdata` file to be loaded.
#' @param with_data Logical (TRUE/FALSE). If TRUE, loads the test DB object with data as if user ran `update_DB`. Default is `FALSE`.
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
  em <- '`short_name` must be character string of length 1'
  if(!is.character(short_name))stop(em)
  if(length(short_name)!=1)stop(em)
  projects <- get_projects() # add short_name conflict check if id and base url differs
  short_name <- validate_env_name(short_name)
  token_name <- paste0(internal_RosyREDCap_token_prefix,short_name) %>% validate_env_name()
  in_proj_cache <- short_name %in% projects$short_name
  missing_dir_path <- missing(dir_path)
  is_a_test <- is_test_short_name(short_name = short_name)
  if(force){ # load blank if force = T
    DB <- internal_blank_DB
    bullet_in_console(paste0("Setup blank DB object because `force = T`"))
  }
  if(!force){
    if(in_proj_cache){ # if its seen in cache the load from there
      DB <- load_DB(short_name)
    }
    if(!in_proj_cache){ # if it's not in the cache start from blank
      if(is_a_test){
        DB <- load_test_DB(short_name = short_name, with_data = F)
      }else{
        DB <- internal_blank_DB
      }
      bullet_in_console("Setup blank DB object because nothing found in cache.",bullet_type = "!")
    }
  }
  if(missing_dir_path){ # if missing the directory path from setup or load then let user know nothing will be stored
    if(!is_something(DB$dir_path)){ # only show message if load_DB wasn't used internally (that has a directory)
      bullet_in_console("If you don't supply a directory, RosyREDCap will only run in R session. Package is best with a directory.",bullet_type = "!")
    }
  }
  if(!missing_dir_path){
    DB$dir_path <- set_dir(dir_path) # will also ask user if provided dir is new or different (will load from original but start using new dir)
  }
  DB$short_name <- short_name
  DB$internals$use_csv <- use_csv
  DB$redcap$token_name <- token_name
  if(!is_a_test){
    DB$links$redcap_base <- validate_web_link(redcap_base)
    DB$links$redcap_uri <- DB$links$redcap_base %>% paste0("api/")
  }else{
    bullet_in_console("Test objects ignore the `redcap_base` url argument and will not communicate with the REDCap API.")
  }
  DB$internals$merge_form_name <- validate_env_name(merge_form_name)
  DB$internals$use_csv <- use_csv
  DB$internals$is_blank <- F
  DB$data <- DB$data %>% all_character_cols_list()
  bullet_in_console(paste0("Token name: '",token_name,"'"))
  if(auto_check_token){
    if(!is_valid_REDCap_token(validate_REDCap_token(DB))){
      set_REDCap_token(DB,ask = F)
    }
  }
  DB <- validate_DB(DB,silent = F)
  return(DB)
}
#' @rdname setup-load
#' @export
load_DB <- function(short_name,validate = T){
  projects <- get_projects()
  if(nrow(projects)==0)stop("No projects in cache")
  if(!short_name%in%projects$short_name)stop("No project named ",short_name," in cache. Did you use `setup_DB()` and `update_DB()`?")
  dir_path <- projects$dir_path[which(projects$short_name==short_name)]
  if(!file.exists(dir_path))stop("`dir_path` doesn't exist: '",dir_path,"'")
  DB_path <- file.path(dir_path,"R_objects",paste0(short_name,"_RosyREDCap.rdata"))
  load_DB_from_path(
    DB_path = DB_path,
    validate = validate
  ) %>% return()
}
#' @rdname setup-load
#' @export
load_DB_from_path <- function(DB_path,validate = T){
  if(!file.exists(DB_path))stop("No file at path '",DB_path,"'. Did you use `setup_DB()` and `update_DB()`?")
  DB <- readRDS(file=DB_path)
  if(validate){
    DB <- DB %>% validate_DB(silent = F)
  }
  return(DB)
}
#' @rdname setup-load
#' @export
load_test_DB <- function(short_name="TEST_repeating",with_data = F){
  em <- '`short_name` must be character string of length 1 equal to one of the following: ' %>% paste0(as_comma_string(internal_allowed_test_short_names))
  if(!is.character(short_name))stop(em)
  if(length(short_name)!=1)stop(em)
  if(!is_test_short_name(short_name = short_name))stop(em)
  DB <- internal_blank_DB
  DB$short_name <- short_name
  DB$internals$is_test <- T
  if(with_data){
    if(short_name == "TEST_classic"){
    }
    if(short_name == "TEST_repeating"){
    }
    if(short_name == "TEST_longitudinal"){
    }
    if(short_name == "TEST_multiarm"){
    }
  }
  return(DB)
}
#' @noRd
is_test_short_name <- function(short_name){
  return(short_name%in%internal_allowed_test_short_names)
}
#' @noRd
is_test_DB <- function(DB){
  return((DB$short_name %in% internal_allowed_test_short_names)&&DB$internals$is_test)
}
#' @rdname save-deleteDB
#' @title Save or Delete DB file from the directory
#' @param DB A validated `DB` object containing REDCap project data and settings. Generated using \link{load_DB} or \link{setup_DB}
#' @description
#' This will save/delete the "<short_name>_RosyREDCap.rdata" file in the given DB directories R_objects folder. These are optional functions given that `save_DB` is a also handled by a default parameter in `update_DB.`
#'
#' @details delete_DB will not delete any other files from that directory. The user must delete any other files manually.
#' @return Message
#' @family DB object
#' @export
save_DB <- function(DB){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  DB <- DB %>% validate_DB()
  if(!DB$internals$ever_connected){
    bullet_in_console(paste0("Did not save ",DB$short_name," because there has never been a REDCap connection! You must use `setup_DB()` and `update_DB()`"),bullet_type = "x")
    return(invisible())
  }
  # DB <- reverse_clean_DB(DB) # # problematic because setting numeric would delete missing codes
  save_file_path <- file.path(DB$dir_path,"R_objects",paste0(DB$short_name,"_RosyREDCap.rdata"))
  DB %>% saveRDS(file=save_file_path)
  add_project(DB)
  # save_xls_wrapper(DB)
  # nav_to_dir(DB)
  return(invisible())
}
#' @rdname save-deleteDB
#' @export
delete_DB <- function(DB){
  DB <- validate_DB(DB)
  dir_path <- DB$dir_path
  dir_path  <- validate_dir(dir_path,silent = F)
  delete_this <- file.path(dir_path,"R_objects",paste0(DB$short_name,"_RosyREDCap.rdata"))
  if(file.exists(delete_this)){
    unlink(delete_this) %>%
      message("Deleted saved DB")
  }else{
    warning("The DB object you wanted to is not there. Did you delete already? ",delete_this)
  }
}
#' @noRd
validate_DB <- function(DB,silent = T,warn_only = F){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  outcome_valid <- T
  messages <- NULL
  if( ! all(names(internal_blank_DB)%in%names(DB))){
    outcome_valid <- F
    messages <- messages %>% append("`DB` does not have the appropriate names. Did you use `load_DB()` or `setup_DB()` to generate it?")
  }
  if(is.null(DB$short_name)){
    outcome_valid <- F
    messages <- messages %>% append("`DB$short_name` is NULL!, Did you use `setup_DB()`?")
  }else{
    DB$short_name %>% validate_env_name()
  }
  if(!silent){
    if((length(DB$data)==0)>0){
      bullet_in_console("Valid DB object but no data yet!",bullet_type = "!")
    }
    if(is.null(DB$dir_path)){
      bullet_in_console("`DB$dir_path` is NULL!, Did you use `setup_DB()`?",bullet_type = "!")
    }else{
      if( ! DB$dir_path %>% file.exists()) {
        bullet_in_console(paste0("`DB$dir_path`, '",DB$dir_path,"', does not exist!, Did you use `setup_DB()`?\nThis can also happen with shared directories."),bullet_type = "!")
      }else{
        bullet_in_console(DB$short_name %>% paste0(" loaded from: "),url = DB$dir_path,bullet_type = "v")
      }
    }
    if((DB$internals$is_test)){
      bullet_in_console(DB$short_name %>% paste0(" is a test DB object that doesn't actually communicate with any REDCap API!"),bullet_type = "i")
    }
    if(DB$internals$is_transformed){
      bullet_in_console(DB$short_name %>% paste0(" is currently transformed! Can reverse with `untransform_DB(DB)`"),bullet_type = "i")
    }
    bullet_in_console("To get data/updates from REDCap run `DB <- update_DB(DB)`")
  }
  if(outcome_valid){
    bullet_in_console(paste0(DB$short_name," is valid DB object!"),bullet_type = "v")
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
  return(DB)
}
#' @noRd
internal_allowed_test_short_names <- c("TEST_classic","TEST_repeating","TEST_longitudinal","TEST_multiarm")
#' @noRd
internal_blank_DB <- list(
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
    is_blank = T,
    is_test = F,
    ever_connected = F,
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
