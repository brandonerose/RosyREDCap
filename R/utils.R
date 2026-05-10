#' @title generate_redcap_ids
#' @description
#' `r lifecycle::badge("experimental")`
#' generate redcap record ids
#' @param project REDCapSync project object
#' @param needed integer length of new IDs needed
#' @param random T/F for random
#' @param prefix character string of ID prefix such as "ID" for "ID001"
#' @param chosen_length integer length of padding for IDs
#' @param start_n integer 0 or 1
#' @return character string of new REDCap IDs not included in current object
#' @export
generate_redcap_ids <- function(project,
                                needed,
                                random = TRUE,
                                prefix = "",
                                chosen_length = 6L,
                                start_n = 1L) {
  project <- convert_project(project)
  checkmate::assert_logical(random)
  checkmate::assert_integerish(chosen_length, lower = 1L)
  checkmate::assert_integerish(needed, lower = 1L)
  checkmate::assert_integerish(start_n, lower = 1L)
  used <- project$record_summary[[project$metadata$id_col]]
  chosen_max <- as.integer(paste(rep(9L, chosen_length), collapse = ""))
  all <- paste0(prefix, sprintf(paste0("%0", chosen_length, "d"), start_n:chosen_max))
  unused <- all[which(!all %in% used)]
  if (random) {
    new_records <- sample(unused, needed, replace = FALSE)
  } else{
    new_records <- unused[1L:needed]
  }
  new_records
}
#' @noRd
convert_project <- function(project) {
  if (test_class(project, classes = c("REDCapSyncProject", "R6"))) {
    project <- project$.internal
  }
  invisible(project)
}
#' @noRd
add_redcap_links_table <- function(DF, project) {
  if (nrow(DF) > 0L) {
    link_vector <- REDCapSync:::add_redcap_links(DF, project)$redcap_link
    DF[[project$metadata$id_col]] <- paste0("<a href='", link_vector, "' target='_blank'>", DF[[project$metadata$id_col]], "</a>")
  }
  DF
}
#' @noRd
clean_RC_col_names <- function(DF, project) {
  colnames(DF) <- colnames(DF) |> lapply(function(COL) {
    x <- project$metadata$fields$field_label[which(project$metadata$fields$field_name ==
                                                     COL)]
    if (length(x) > 1L) {
      x <- x[[1L]]
    }
    ifelse(length(x) > 0L, x, COL)
  }) |>
    unlist()
  DF
}
#' @noRd
clean_RC_df_for_DT <- function(DF, project) {
  DF |>
    add_redcap_links_table(project) |>
    clean_RC_col_names(project)
}
#' @noRd
sidebar_choices <- function(data_list, n_threshold = 1L) {
  choices <- REDCapSync:::annotate_choices(data_list)
  choices <- choices[which(choices$n >= n_threshold), ]
  sbc <- data.frame(
    form_name = choices$form_name,
    field_name = choices$field_name,
    name = choices$name,
    label = paste0(choices$label, " (n = ", clean_num(choices$n), ")")
  )
  sbc
}
#' @noRd
redcap_field_types_not_in_data <- c("descriptive", "checkbox")
#' @noRd
form_names_to_field_names <- function(form_names, project, original_only = FALSE) {
  field_names <- NULL
  if (original_only) {
    fields <- project$metadata$fields
  } else {
    fields <- project$metadata$fields
  }
  for (form_name in form_names) {
    field_names <- field_names |> append(fields$field_name[which(fields$form_name == form_name)])
  }
  unique(field_names)
}
#' @noRd
form_names_to_form_labels_alt <- function(form_names, metadata) {
  metadata$forms$form_label[match(x = form_names, table = metadata$forms$form_name)]
}
#' @noRd
form_labels_to_form_names_alt <- function(form_labels, metadata) {
  metadata$forms$form_name[match(x = form_labels, table = metadata$forms$form_label)]
}
#' @noRd
field_names_to_field_labels_alt <- function(field_names, metadata) {
  metadata$fields$field_label[match(x = field_names, table = metadata$fields$field_name)]
}
#' @noRd
get_field_type_from_data_list <- function(data_list,
                                          field_type_r,
                                          form_name = NULL,
                                          include_no_choice = TRUE) {
  fields <- data_list$metadata$fields
  field_names <- fields$field_name[which(fields$field_type_r %in% field_type_r)]
  field_labels <- field_names |> field_names_to_field_labels_alt(data_list$metadata)
  field_names <- stats::setNames(field_names, field_labels)
  if (!is.null(form_name)) {
    field_names <- field_names |> vec1_in_vec2(colnames(data_list$data[[form_name]]))
  }
  if (include_no_choice) {
    field_names <- c(stats::setNames("no_choice", "None"), field_names)
  }
  field_names
}
#' @noRd
form_list_to_text <- function(form_list,
                              project,
                              drop_nas = TRUE,
                              clean_names = TRUE) {
  output_list <- NULL
  for (i in seq_along(form_list)) {
    form <- form_list[[i]]
    the_raw_name <- names(form_list)[[i]]
    the_name <- the_raw_name
    if (clean_names)
      the_name <- project$metadata$forms$form_label[which(project$metadata$forms$form_name == the_raw_name)]
    df_name <- paste0("----- ", the_name, " Table -----")
    output_list <- output_list |>
      append(paste0("&nbsp;&nbsp;<strong>", df_name, "</strong><br>"))
    key_col_names <- project$metadata$form_key_cols[[the_raw_name]]
    for (j in seq_len(nrow(form))) {
      for (col_name in colnames(form)) {
        entry <- form[j, col_name]
        if (!col_name %in% key_col_names) {
          if (!is.na(entry) || !drop_nas) {
            entry <- gsub("\\n", "<br>", entry)
            col_name_clean <- col_name
            if (clean_names)
              col_name_clean <- project$metadata$fields$field_label[which(project$metadata$fields$field_name == col_name)]
            output_list <- output_list |>
              append(
                paste0(
                  "&nbsp;&nbsp;<strong>",
                  col_name_clean,
                  ":</strong> <br>&nbsp;&nbsp;&nbsp;&nbsp;",
                  entry,
                  "<br>"
                )
              )
          }
        }
      }
    }
    output_list <- c(output_list, "<br>")
  }
  output_list
}
#' @noRd
sanitize_path <- function(path) {
  sanitized <- gsub("\\\\", "/", path)
  sanitized <- normalizePath(sanitized, winslash = "/", mustWork = FALSE)
  sanitized
}
#' @noRd
col_12 <- function(...) {
  column(12, ...)
}
#' @noRd
col_10 <- function(...) {
  column(10, ...)
}
#' @noRd
col_9 <- function(...) {
  column(9, ...)
}
#' @noRd
col_8 <- function(...) {
  column(8, ...)
}
#' @noRd
col_6 <- function(...) {
  column(6, ...)
}
#' @noRd
col_4 <- function(...) {
  column(4, ...)
}
#' @noRd
col_3 <- function(...) {
  column(3, ...)
}
#' @noRd
col_2 <- function(...) {
  column(2, ...)
}
#' @noRd
col_1 <- function(...) {
  column(1, ...)
}
