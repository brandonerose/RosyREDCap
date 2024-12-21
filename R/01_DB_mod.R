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
#' @title Setup a DB Object for a REDCap Project
#' @description
#' Initializes a RosyREDCap `DB` object from one REDCap project and if a directory is chosen it will structure that directory for later imports and exports.
#'
#' @details
#' This function creates or loads a `DB` object containing all the necessary elements for working with a REDCap project.
#' It supports integration with REDCap servers, handles metadata configuration, and optionally validates the REDCap API token on setup.
#'
#' The function allows customization of project-specific settings such as the base REDCap URL, file paths, and merge form names.
#' If the `force` argument is `TRUE`, the function skips loading a previously saved project and initializes a blank setup, which may be helpful if you are getting errors.
#'
#' @param DB A validated list object that contains all metadata, data, logs, transformations, and more for one REDCap project.
#' @param short_name Character string with no spaces or symbols. A short unique name to use as a shortcut for the project such PSDB.
#' @param dir_path Character. File path of the directory where project data is stored.
#' @param redcap_base Character. The base URL of the REDCap server, e.g., "https://redcap.miami.edu".
#' @param force Logical. If `TRUE`, forces a blank load instead of loading the last saved state. Default is `FALSE`.
#' @param merge_form_name Character. Name of the merged non-repeating form to use in the package. Default is `"merged"`.
#' @param use_csv Logical. If `TRUE`, uses CSV files instead of other storage formats. Default is `FALSE`.
#' @param auto_check_token Logical. If `TRUE`, automatically tests the REDCap API token after setup. Default is `TRUE`.
#' @return A `DB` object configured for the REDCap project.
#' @seealso
#' \code{\link[RosyREDCap]{test_REDCap_token}} for testing the API token.
#' \code{\link[RosyREDCap]{set_REDCap_token}} for setting a new API token.
#' \code{\link[RosyREDCap]{view_REDCap_token}} for viewing the stored API token.
#' @export
setup_RosyREDCap <- function (
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
    DB <- blank_RosyREDCap()
  }
  if( ! missing_dir_path){
    if(force){
      DB <- blank_RosyREDCap()
    }else{
      DB <- load_RosyREDCap(short_name)
    }
    DB$dir_path <-RosyDB:::set_dir(dir_path)
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
#' @title Load RosyREDCap
#' @description
#' Will take your previously chosen `short_name` and load directory-saved DB object by using the cache. `dir_path` is optional if you already used `setup_RosyREDCap()`
#' @inheritParams setup_RosyREDCap
#' @return DB list object
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
redcap_field_types_not_in_data <- c(
  "descriptive", "checkbox"
)
