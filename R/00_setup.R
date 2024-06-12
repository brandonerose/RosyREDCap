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
    "redcap_version",
    "project_title",
    "last_metadata_update",
    "last_data_update",
    "redcap_home_link",
    "redcap_API_playground_link"
  )
}
blank_projects <- function(){
  x <- matrix(data = character(0),ncol = length(blank_project_cols())) %>% as.data.frame()
  colnames(x) <- blank_project_cols()
  x
}
add_project <- function(DB){
  projects <- get_projects()
  projects <- projects[which(projects$short_name!=DB$short_name),]
  OUT <- data.frame(
    short_name = DB$short_name,
    dir_path = DB$dir_path %>% is.null() %>% ifelse(NA,DB$dir_path),
    token_name = DB$token_name,
    project_id = DB$redcap$project_id,
    redcap_version = DB$redcap$version,
    project_title = DB$redcap$project_title,
    last_metadata_update = DB$internals$last_metadata_update,
    last_data_update = DB$internals$last_data_update,
    redcap_home_link = DB$links$redcap_home,
    redcap_API_playground_link =  DB$links$redcap_API_playground
  ) %>% all_character_cols()
  colnames(OUT) <- blank_project_cols()
  rownames(OUT) <- NULL
  OUT
  projects <- projects %>% dplyr::bind_rows(OUT)
  saveRDS(projects, file = cache$cache_path_get() %>% normalizePath() %>% file.path("projects.rds"))
}
