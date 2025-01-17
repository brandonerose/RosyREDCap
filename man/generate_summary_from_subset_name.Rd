% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DB_summarize.R
\name{generate_summary_from_subset_name}
\alias{generate_summary_from_subset_name}
\title{Generate a Summary from a Subset Name}
\usage{
generate_summary_from_subset_name(
  DB,
  subset_name,
  clean = T,
  drop_blanks = T,
  include_metadata = T,
  annotate_metadata = T,
  include_record_summary = T,
  include_users = T,
  include_log = T
)
}
\arguments{
\item{DB}{A validated `DB` object containing REDCap project data and settings. Generated using \link{load_DB} or \link{setup_DB}}

\item{subset_name}{Character. The name of the subset from which to generate the summary.}

\item{clean}{Logical. If `TRUE`, the data will be cleaned before summarizing. Default is `TRUE`.}

\item{drop_blanks}{Logical. If `TRUE`, records with blank fields will be dropped. Default is `TRUE`.}

\item{include_metadata}{Logical. If `TRUE`, metadata will be included in the summary. Default is `TRUE`.}

\item{annotate_metadata}{Logical. If `TRUE`, metadata will be annotated in the summary. Default is `TRUE`.}

\item{include_record_summary}{Logical. If `TRUE`, a record summary will be included in the generated summary. Default is `TRUE`.}

\item{include_users}{Logical. If `TRUE`, user-related information will be included in the summary. Default is `TRUE`.}

\item{include_log}{Logical. If `TRUE`, the log of changes will be included in the summary. Default is `TRUE`.}
}
\value{
A list containing the generated summary based on the specified options. The list includes filtered and cleaned data, metadata, and other summary details.
}
\description{
Generates a summary from a predefined subset of data within a REDCap project. The summary can be customized based on various options, such as cleaning the data, including metadata, and annotating metadata.
}
\details{
This function allows you to generate a summary of data from a specific subset of records within the REDCap project. The function provides flexible options for cleaning, annotating, and including metadata, as well as controlling whether to include record summaries, user information, and logs.
}
