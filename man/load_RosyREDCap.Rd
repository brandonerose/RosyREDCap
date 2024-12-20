% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_DB_mod.R
\name{load_RosyREDCap}
\alias{load_RosyREDCap}
\title{Load RosyREDCap}
\usage{
load_RosyREDCap(short_name, dir_path, validate = T)
}
\arguments{
\item{short_name}{character name as a shortcut}

\item{dir_path}{character file path of the directory}
}
\value{
DB list object
}
\description{
Will take your previously chosen `short_name` and load directory-saved DB object by using the cache. `dir_path` is optional if you already used `setup_RosyREDCap()`
}
