#' @import RosyUtils
.onLoad <- function(libname, pkgname){
  cache <- get_cache()
  bullet_in_console("RosyREDCap Loaded",bullet_type = "v")
}
#' @title Get your Get Cache Path
#' @description
#' Included for transparency and confirmation/testing. This is where the basic information about your projects is cached when you use the RosyREDCap package.
#' @details
#' This function checks the location of the cache established by [hoardr::hoard()].
#' \emph{No project data is stored here. Tokens are not stored here either.}
#' Key information stored here is `short_name` (primary key for RosyREDCap projects) and other details about project information.
#' See [RosyREDCap:::blank_project_cols()]
#' @return The file path of your RosyREDCap cache
#' @examples
#' \dontrun{
#' path <- cache_path()
#' print(path)
#' }
#' @return your path
#' @family cache
#' @keywords cache, internal
#' @export
cache_path <- function(){
  cache <- get_cache()
  path <- normalizePath(cache$cache_path_get())
  return(path)
}
#' @return message confirmation
#' @family Cache
#' @export
#'
#' @title Clear your cached projects
#' @description
#' Included for transparency and confirmation/testing.
#' @details
#' This function checks the location of the cache established by [hoardr::hoard()].
#' @return The file path of your RosyREDCap cache
#' @examples
#' \dontrun{
#' path <- cache_path()
#' print(path)
#' }
#' @return your path
#' @family cache
#' @keywords cache, internal
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
