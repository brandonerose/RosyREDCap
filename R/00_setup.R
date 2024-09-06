#' @import RosyUtils
cache <- NULL
.onLoad <- function(libname, pkgname){
  x <- hoardr::hoard()
  x$cache_path_set(path=.packageName,type="user_cache_dir")
  x$mkdir()
  cache <<-x
}
cache_path <- function(){
  cache$cache_path_get() %>% normalizePath()
}
cache_exists <-  function(){
  cache_path() %>% file.exists()
}
#' @title Clear your cached projects
#' @return message confirmation
#' @export
cache_clear <-  function(){
  cache$delete_all()
  message("Cache cleared!")
}
cache_projects_exists <-  function(){
  if(cache_exists()){
    cache_path() %>% file.path("projects.rds") %>% file.exists()
  }else{
    warning("Cache doesn't exist",immediate. = T)
    FALSE
  }
}
#' @title Get your REDCap projects used by RosyREDCap
#' @description
#' Everytime a setup or update is performed RosyREDCap stores the most basic information
#' about that project to the cache so the user has a running log of everywhere there project information is stored,
#' which can be used to find, move, edit, delete that data.
#' @return data.frame of projects from the cache
#' @export
get_projects <- function(){
  if(cache_projects_exists()){
    projects <- cache_path() %>% file.path("projects.rds") %>% readRDS()
    return(projects)
  }else{
    "You have no projects cached. Try `add_project`" %>% message()
    return(
      blank_projects()
    )
  }
}
blank_project_cols <- function(){
  c(
    "short_name",
    "dir_path",
    "token_name",
    "project_id",
    "version",
    "project_title",
    "id_col",
    "is_longitudinal",
    "has_repeating_instruments_or_events",
    "has_multiple_arms",
    "n_records",
    "last_metadata_update",
    "last_data_update",
    "redcap_base",
    "redcap_home",
    "redcap_API_playground",
    "test_dir",
    "test_DB",
    "test_RC"
  )
}
blank_projects <- function(){
  x <- matrix(data = character(0),ncol = length(blank_project_cols())) %>% as.data.frame()
  colnames(x) <- blank_project_cols()
  x
}
save_projects_to_cache <- function(projects,silent=T){
  projects <- projects[order(projects$short_name),]
  projects$test_dir <- projects$test_dir %>% as.logical()
  projects$test_DB <- projects$test_DB %>% as.logical()
  projects$test_RC <- projects$test_RC %>% as.logical()
  saveRDS(projects, file = cache$cache_path_get() %>% normalizePath() %>% file.path("projects.rds"))
  if(!silent){
    message("RosyREDCap saved ",nrow(projects)," project locations to the cache...\n",paste0("   Name: ",stringr::str_pad(projects$short_name,width = max(nchar(projects$short_name))+2,side = "right"),"   Token: ",projects$token_name,collapse = "\n"))
    message("The cache is stored in directory on your computer. It can be found with `RosyREDCap::cache_path()`, and cleared with `RosyREDCap::cache_clear()`.")
  }
}
extract_project_details <- function(DB){
  OUT <- data.frame(
    short_name = DB$short_name,
    dir_path = DB$dir_path %>% is.null() %>% ifelse(NA,DB$dir_path),
    token_name = DB$token_name,
    project_id = DB$redcap$project_id,
    version = DB$redcap$version,
    project_title = DB$redcap$project_title,
    id_col = DB$redcap$id_col,
    is_longitudinal = DB$redcap$is_longitudinal,
    has_multiple_arms = DB$redcap$has_multiple_arms,
    has_repeating_instruments_or_events = DB$redcap$has_repeating_instruments_or_events,
    n_records = DB$summary$all_records %>% nrow(),
    last_metadata_update = DB$internals$last_metadata_update,
    last_data_update = DB$internals$last_data_update,
    redcap_base = DB$links$redcap_base ,
    redcap_home = DB$links$redcap_home,
    redcap_API_playground =  DB$links$redcap_API_playground
  ) %>% all_character_cols()
  rownames(OUT) <- NULL
  return(OUT)
}
add_project <- function(DB,silent = T){
  projects <- get_projects()
  projects <- projects[which(projects$short_name!=DB$short_name),]
  OUT <- extract_project_details(DB = DB)
  projects <- projects %>% dplyr::bind_rows(OUT)
  save_projects_to_cache(projects,silent = silent)
}
delete_project <- function(short_name){
  projects <- get_projects()
  ROW <- which(projects$short_name==short_name)
  OTHERS <- which(projects$short_name!=short_name)
  if(!is_something(ROW))message("Nothing to delete named: ",short_name) %>% return()
  projects <- projects[OTHERS,]
  message("Deleted: ",short_name)
  save_projects_to_cache(projects)
  return(projects)
}
#' @title project_health_check
#' @description
#' Check directory, DB object, and REDCap token. Optional update.
#' @export
project_health_check <- function(update = F){
  projects <- get_projects()
  DROPS <- NULL
  projects$test_dir <- F
  projects$test_DB <- F
  projects$test_RC <- F
  if(nrow(projects)>0){
    # DROPS<- projects[which(is.na(projects$project_id)),]
    for(i in 1:nrow(projects)){#i <- 1:nrow(projects)%>%  sample1()
      if(file.exists(projects$dir_path[i])){
        projects$test_dir[i] <- T
        DB <- tryCatch({
          load_DB(projects$dir_path[i])
        },error = function(e) {NULL})
        projects$test_DB[i] <- !is.null(DB)
        if(!projects$test_DB[i]){
          DB <- tryCatch({
            setup_DB(
              short_name = projects$short_name[i],
              dir_path = projects$dir_path[i],
              token_name = projects$token_name[i],
              redcap_base = "https://redcap.miami.edu/",
              force = T,
              merge_form_name = "merged"
            )
          },error = function(e) {NULL})
          projects$test_DB[i] <- !is.null(DB)
        }
        if(projects$test_DB[i]){
          projects$test_RC[i] <- redcap_token_works(DB)
          if(projects$test_RC[i]){
            if(update)DB <- update_DB(DB)
          }
        }
        if(projects$test_DB[i]){
          OUT <- extract_project_details(DB = DB)
          OUT$test_dir <- projects$test_dir[i]
          OUT$test_DB <- projects$test_DB[i]
          OUT$test_RC <- projects$test_RC[i]

        }else{
          OUT <- projects[i,]
        }
        projects <- projects[which(projects$short_name!=DB$short_name),]
        projects <- projects %>% dplyr::bind_rows(OUT)
      }
    }
    save_projects_to_cache(projects,silent = F)
  }
}
