% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/09_upload.R
\name{upload_DB_to_redcap}
\alias{upload_DB_to_redcap}
\title{Upload from your directory to REDCap}
\usage{
upload_DB_to_redcap(
  DB,
  batch_size = 500,
  ask = T,
  view_old = T,
  n_row_view = 20
)
}
\arguments{
\item{batch_size}{numeric of how big the REDCap batch upload is. Default 500.}

\item{ask}{logical for if you want to preview uploads first}
}
\value{
messages
}
\description{
This function is meant to be run after `DB_import <- read_redcap_dir(DB)`.
It compares DB_import to DB and only uploads the changes.
This will only overwrite and new data. It will not directly delete and data.
Because this is the only function that can mess up your data, use it at your own risk.
Remember all changes are saved in the redcap log if there's an issue.
}
