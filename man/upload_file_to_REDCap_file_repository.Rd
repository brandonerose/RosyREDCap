% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/REDCap_files.R
\name{upload_file_to_REDCap_file_repository}
\alias{upload_file_to_REDCap_file_repository}
\title{Uploads a file to REDCap}
\usage{
upload_file_to_REDCap_file_repository(DB, file)
}
\arguments{
\item{DB}{A validated `DB` object containing REDCap project data and settings. Generated using \link{load_DB} or \link{setup_DB}}

\item{file}{file location on your PC}
}
\value{
messages for confirmation
}
\description{
Uploads a file to REDCap
}
