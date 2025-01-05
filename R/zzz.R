.onLoad <- function(libname, pkgname){
  cache <- get_cache()
}
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("RosyREDCap Loaded! Check available projects with `get_projects()`")
  }
}
