#' @import RosyUtils
.onLoad <- function(libname, pkgname){
  cache <- get_cache()
  bullet_in_console("RosyREDCap Loaded",bullet_type = "v")
}
#' @title cache_path
#' @return your path
#' @family Cache
#' @export
cache_path <- function(){
  cache <- get_cache()
  path <- normalizePath(cache$cache_path_get())
  return(path)
}
#' @title Clear your cached projects
#' @return message confirmation
#' @family Cache
#' @export
cache_clear <-  function(){
  cache <- get_cache()
  cache$delete_all()
  bullet_in_console(paste0(pkg_name," cache cleared!"),bullet_type = "v")
}
cache_exists <-  function(){
  cache <- get_cache()
  return(file.exists(cache_path()))
}
cache_projects_exists <-  function(){
  if(cache_exists()){
    cache_path() %>% file.path("projects.rds") %>% file.exists() %>% return()
  }else{
    warning("Cache doesn't exist",immediate. = T)
    return(FALSE)
  }
}
get_cache <- function(){
  cache <- hoardr::hoard()
  cache$cache_path_set(path=.packageName,type="user_cache_dir")
  cache$mkdir()
  return(cache)
}
