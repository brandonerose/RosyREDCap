% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/11_transform.R
\name{generate_default_remap}
\alias{generate_default_remap}
\title{Generate Default Re-Map}
\usage{
generate_default_remap(
  DB,
  save_file = !is.null(DB$dir_path),
  merge_non_repeating = T
)
}
\arguments{
\item{DB}{object generated using `load_DB()` or `setup_DB()`}

\item{save_file}{logical for whether or not to save sample default remap file to input folder of directory}

\item{merge_non_repeating}{logical for whether or not to merge non-repeating instruments}
}
\description{
Generate Default Re-Map
}
