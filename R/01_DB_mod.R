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
#' @title Setup RosyREDCap
#' @description
#' Initializes the `DB` object with the necessary REDCap API token and other configurations.
#'
#' @details
#' This function sets up the `DB` object by storing the REDCap API token and other configurations required for interacting with the REDCap server.
#' It ensures that the token is valid and ready for use in subsequent API calls.
#'
#' @param short_name A character string representing the short name for the REDCap project.
#' @param dir_path Optional character string representing the directory path where you want the REDCap project data to be stored. If missing, DB object will only be in current R session.
#' @param redcap_base A character string representing the base URL of the REDCap server.
#' @param force Logical (TRUE/FALSE). If TRUE, forces the setup even if the `DB` object already exists. Default is `FALSE`.
#' @param merge_form_name A character string representing the name of the merged form. Default is "merged".
#' @param use_csv Logical (TRUE/FALSE). If TRUE, uses CSV files for data storage. Default is `FALSE`.
#' @param auto_check_token Logical (TRUE/FALSE). If TRUE, automatically checks the validity of the REDCap API token. Default is `TRUE`.
#' @return The `DB` object with the REDCap API token and configurations set.
#' @examples
#' \dontrun{
#' # Initialize the DB object with the REDCap API token and URL
#' DB <- setup_RosyREDCap(
#'   short_name = "ABC",
#'   dir_path = "path/to/secure/file/storage",
#'   redcap_base = "https://redcap.yourinstitution.edu/"
#' )
#' }
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
#' @title Load RosyREDCap Project
#' @description
#' Will take your previously chosen `short_name` and load the directory-saved DB object by using the cache. `dir_path` is optional if you already used `setup_RosyREDCap()`.
#'
#' @details
#' This function loads the `DB` object from the specified directory using the provided `short_name`. If `dir_path` is not provided, it will use the path set during the `setup_RosyREDCap()` call.
#' Optionally, it can validate the loaded `DB` object. It references a cache that is updated whenever you run `setup_RosyREDCap()`.
#'
#' @param short_name A character string representing the short name you previously chose for the REDCap project.
#' @param dir_path A character string representing the directory path where the REDCap project files are stored. Optional if already set by `setup_RosyREDCap()`.
#' @param validate Logical (TRUE/FALSE). If TRUE, validates the loaded `DB` object. Default is `TRUE`.
#' @return The RosyREDCap `DB` list object.
#' @seealso
#' \code{\link[RosyDB]{get_projects}} for retrieving a list of projects from the directory cache.
#' \code{\link[RosyREDCap]{setup_RosyREDCap}} for retrieving a list of projects from the directory cache.
#' @examples
#' \dontrun{
#' # Load the DB object using the short name and directory path
#' DB <- load_RosyREDCap("ABC")
#' }
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
