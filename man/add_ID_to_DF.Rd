% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/07_process.R
\name{add_ID_to_DF}
\alias{add_ID_to_DF}
\title{add REDCap ID to any dataframe using a ref_id}
\usage{
add_ID_to_DF(DF, DB, ref_id)
}
\arguments{
\item{DF}{dataframe}

\item{ref_id}{column name that matches a REDCap variable name that could be an ALT id such as MRN}
}
\value{
original dataframe with REDCap id_col added as the first column
}
\description{
add REDCap ID to any dataframe using a ref_id
}
