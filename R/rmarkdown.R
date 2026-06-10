#' @title rmarkdown_project
#' @inheritParams REDCap_diagram
#' @param dir_other Character. The directory where the dataset file will be
#' saved. Default is the `output` folder within the database directory.
#' @description
#' `r lifecycle::badge("experimental")`
#' Generate a rmarkdown PDF from a project object
#' @details
#' You will need the `tinytex` and `reticulate` packages. Follow errors during
#' your first use and afterwards it should work. Currently working on mac but
#' troubleshooting windows errors with latex.
#' @export
rmarkdown_project <- function(project, dir_other) {
  dataset <- project$load_dataset("REDCapSync")
  project <- convert_project(project)
  if (missing(dir_other)) {
    if (!file.exists(project$dir_path)) {
      stop("project$dir_path does not exist")
    }
    dir_other <- project$dir_path |> file.path("output")
  }
  if (!file.exists(dir_other)) {
    stop("dir does not exist")
  }
  filename <- paste0(project$project_name,
                     "_full_summary_",
                     gsub("-", "_", Sys.Date()),
                     ".pdf")
  temp_dir <- tempdir()
  final_file <- file.path(dir_other, filename)
  final_file_temp <- file.path(temp_dir, filename)
  project |> REDCap_diagram() |> visNetwork::visSave(file = file.path(temp_dir, "redcap_diagram.html"))
  webshot2::webshot(
    file.path(temp_dir, "redcap_diagram.html"),
    file.path(temp_dir, "redcap_diagram.png")
  )
  log <- project$redcap$log
  last_metadata <- log$timestamp[which(log$action_type == "Metadata Change Major")] |>
    dplyr::first()
  last_data <- log$timestamp[which(log$action_type %in% c("Update", "Delete", "Create"))] |>
    dplyr::first()
  number_records <- dataset$records |> nrow()
  rmarkdown::render(
    input = system.file("rmarkdown", "pdf.Rmd", package = "RosyREDCap"),
    output_format = "pdf_document",
    output_file = final_file_temp,
    output_dir = temp_dir,
    intermediates_dir = temp_dir,
    # change to tempdir
    knit_root_dir = temp_dir,
    quiet = FALSE
  )
  if (!file.exists(final_file_temp)) {
    stop("failed to save")
  }
  file.copy(from = final_file_temp,
            to = final_file,
            overwrite = TRUE)
  # REDCapSync:::cli_alert_wrap("Saved! ", file = REDCapSync:::sanitize_path(final_file))
}
