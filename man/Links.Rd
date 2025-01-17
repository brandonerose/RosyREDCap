% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DB_helpers.R
\name{link_API_token}
\alias{link_API_token}
\alias{link_API_playground}
\alias{link_REDCap_home}
\alias{link_REDCap_project}
\alias{link_REDCap_record}
\title{Open Links to REDCap Pages}
\usage{
link_API_token(DB)

link_API_playground(DB)

link_REDCap_home(DB)

link_REDCap_project(DB)

link_REDCap_record(DB, record, page, instance, text_only = F)
}
\arguments{
\item{DB}{A validated `DB` object containing REDCap project data and settings. Generated using \code{DB <- \link{load_DB}("PROJ")} or \link{setup_DB}()}

\item{record}{REDCap record id or study id etc, any column names that match `DB$redcap$id_col`}

\item{page}{REDCap page for the record. Must be one of `DB$metadata$forms$form_name`}

\item{instance}{REDCap instance if it's a repeating instrument}

\item{text_only}{logical for only returning text}
}
\value{
Nothing will be returned in R. Instead, a browser link
}
\description{
Opens browser page for a given DB object.
}
\details{
Uses [utils::browseURL()] to open the specified REDCap page.
In order for the function to work you must have ran \code{DB <- update_DB(DB)} successfully at least once.
If the link brings you to a page that doesn't work check the url. It's possible your institution may have changed redcap versions, which is part of the URL. In that case run \code{DB <- update_DB(DB)} again.
You may have to be signed into REDCap for it to work properly.
When in doubt, just seek out the page by navigating on your own in REDCap. Report issues if you can.
}
\concept{Link Functions}
