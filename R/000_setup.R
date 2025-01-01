#' @noRd
.onLoad <- function(libname, pkgname){
  cache <- get_cache()
}
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("RosyREDCap Loaded! Check available projects with `get_projects()`")
  }
}
#' @title Get your Get Cache Path
#' @description
#' Included for transparency and confirmation/testing. This is where the basic information about your projects is cached when you use the RosyREDCap package.
#' @details
#' This function checks the location of the cache established by \code{\link[hoardr]{hoard}}.
#' \emph{No project data is stored here. Tokens are not stored here either.}
#' Key information stored here is `short_name` (primary key for RosyREDCap projects) and other details about project information.
#' @return The file path of your RosyREDCap cache
#' @seealso
#' For more details, see \code{\link[hoardr]{hoard}}.
#' @examples
#'
#' \dontrun{
#' path <- cache_path()
#' print(path)
#' }
#' @family Project Cache Functions
#' @keywords Project Cache Functions
#' @export
cache_path <- function(){
  cache <- get_cache()
  path <- normalizePath(cache$cache_path_get())
  return(path)
}
#' @title Clear your cached projects
#' @description
#' Included for transparency and confirmation/testing.
#' @details
#' This function checks the location of the cache established by \code{\link[hoardr]{hoard}} and deletes it!
#' This will not delete project data, just the packages "memory" of it.
#' @return The file path of your RosyREDCap cache
#' @examples
#'
#' \dontrun{
#' #warning this will delete the packages cache of project locations
#' cache_clear()
#' }
#' @return messages confirming deleted cache
#' @family Project Cache Functions
#' @keywords Project Cache Functions
#' @export
cache_clear <-  function(){
  cache <- get_cache()
  cache$delete_all()
  bullet_in_console("If you intend to delete any/all files, that must be done manually from the directory/directories.",bullet_type = "!")
  bullet_in_console("RosyREDCap cache cleared!",file = cache$cache_path_get(),bullet_type = "v")
}
#' @noRd
cache_exists <-  function(){
  cache <- get_cache()
  return(file.exists(cache_path()))
}
#' @noRd
cache_projects_exists <-  function(){
  if(cache_exists()){
    cache_path() %>% file.path("projects.rds") %>% file.exists() %>% return()
  }else{
    warning("Cache doesn't exist",immediate. = T)
    return(FALSE)
  }
}
#' @noRd
get_cache <- function(){
  cache <- hoardr::hoard()
  cache$cache_path_set(path="RosyREDCap",type="user_cache_dir")
  cache$mkdir()
  return(cache)
}
