#' @title make_table1
#' @param DF data.frame
#' @param group character string of no_choice or a factor column name
#' @param variables character vector of variables to include. NULL is all.
#' @param render.missing logical for rendering missing data or not
#' @export
make_table1 <- function(DF,
                        group = "no_choice",
                        variables = NULL,
                        render.missing = FALSE) {
  if (is.null(variables)) {
    variables <- colnames(DF)
  }
  has_group <- group != "no_choice"
  # if(any(!x))warning(paste0(x,collapse = ", ")," <- not in the form you specified")
  if (!is_something(variables))
    return(h3("Nothing to return!")) # stop("Provide variable names of at least length 1!")
  # caption  <- "Basic stats"
  # footnote <- "ᵃ Also known as Breslow thickness"
  BAD <- variables |> vec1_not_in_vec2(colnames(DF))
  if (length(BAD) > 0L) {
    variables <- variables[which(!variables %in% BAD)]
    warning(
      "Following variables dropped (not included in DF): " |> paste0(toString(BAD)),
      immediate. = TRUE
    )
  }
  if (!is_something(variables))
    return(h3("Nothing to return!")) # stop("Provide variable names of at least length 1!")
  DF <- DF[, index_na(DF, invert = TRUE), drop = FALSE]
  BAD <- variables |> vec1_not_in_vec2(colnames(DF))
  if (length(BAD) > 0L) {
    variables <- variables[which(!variables %in% BAD)]
    warning("Following variables dropped (only NAs in DF): " |> paste0(toString(BAD)),
            immediate. = TRUE)
  }
  if (!is_something(variables))
    return(h3("Nothing to return!")) # stop("Provide variable names of at least length 1!")
  if (has_group) {
    if (!group %in% colnames(DF))
      stop("`group` not included in DF colnames")
    if (!is.factor(DF[[group]]))
      DF$group <-  DF[[group]] |> factor()
    DF <- DF[which(!is.na(DF[[group]])), ] |> clone_attr(from = DF)
    variables <- variables[which(variables != group)]
  }
  if (!is_something(variables))
    return(h3("Nothing to  |> !")) # stop("Provide variable names of at least length 1!")
  forumla <- paste(variables, collapse = " + ")
  if (has_group)
    forumla <- paste0(forumla, " | ", group)
  forumla <- stats::as.formula(paste0("~", forumla))
  if (render.missing) {
    table1::table1(forumla, data = DF, big.mark = ",")
  } else {
    table1::table1(
      forumla,
      data = DF,
      big.mark = ",",
      render.missing = NULL
    )
  }
}
#' @noRd
index_na <- function(DF, MARGIN = "col", invert = FALSE) {
  okcols <- c("cols", "col")
  okrows <-  c("row", "rows")
  allowed <- c(okcols, okrows, 1L, 2L)
  if (length(MARGIN) != 1L)
    stop("MARGIN must be length 1")
  if (!tolower(MARGIN) %in% allowed)
    stop("MARGIN must be one of the following ... ", toString(allowed))
  if (tolower(MARGIN) %in% okcols)
    MARGIN <- 2L
  if (tolower(MARGIN) %in% okrows)
    MARGIN <- 1L
  x <- DF |> apply(MARGIN = MARGIN, function(IN) {
    all(is.na(IN))
  })
  if (invert)
    x <- !x
  x <- x |> which() |> unname()
  x
}
#' @title save_table1
#' @param table1 from [make_table1]
#' @param filepath filepath to save
#' @export
save_table1 <- function(table1, filepath) {
  table1 |>
    table1::t1flex() |>
    flextable::bg(bg = "white", part = "all") |>
    flextable::save_as_image(path = filepath)
}
#' @noRd
clone_attr <- function(to, from) {
  units_vec <- from |>
    lapply(function(col) {
      attr(col, "units")
    }) |>
    unlist()
  label_vec <- from |>
    lapply(function(col) {
      attr(col, "label")
    }) |>
    unlist()
  to_cols <- colnames(to)
  if (is_something(units_vec)) {
    for (i in seq_along(units_vec)) {
      x <- units_vec[i]
      col <- names(x)
      if (is_something(x)) {
        if (col %in% to_cols) {
          attr(to[[col]], "units") <- as.character(x)
        }
      }
    }
  }
  if (is_something(label_vec)) {
    for (i in seq_along(label_vec)) {
      x <- label_vec[i]
      col <- names(x)
      if (is_something(x)) {
        if (col %in% to_cols) {
          attr(to[[col]], "label") <- as.character(x)
        }
      }
    }
  }
  to
}
#' @noRd
get_labels <- function(DF) {
  DF |>
    names() |>
    lapply(function(name) {
      out <- attr(DF[[name]], "label")
      if (!is.null(out)) {
        return(out)
      }
      name
    }) |>
    unlist() |>
    as.character()
}
#' @noRd
get_field_names_date <- function(data_list) {
  #assert
  the_rows <- which(data_list$metadata$fields$field_type_r == "date")
  if (length(the_rows) == 0L) {
    return(NULL)
  }
  data_list$metadata$fields$field_name[the_rows]
}
#' @noRd
make_DT_table <- function(DF,
                          editable = FALSE,
                          selection = "single",
                          paging = TRUE,
                          scrollY = FALSE,
                          searching = TRUE) {
  if (!is_something(DF)) {
    return(DT::datatable(
      data.frame(x = " ", stringsAsFactors = FALSE)[0L, , drop = FALSE],
      options = list(
        dom = "t",
        # Simplify the table appearance
        paging = FALSE,
        # Disable pagination
        ordering = FALSE # Disable ordering
      ),
      rownames = FALSE,
      colnames = " "
    ))
  }
  x <- DF |> DT::datatable(
    selection = selection,
    editable = editable,
    rownames = FALSE,
    # fillContainer = TRUE,
    # extensions = "Buttons",
    options = list(
      columnDefs = list(list(
        className = "dt-center", targets = "_all"
      )),
      paging = paging,
      pageLength = 20L,
      fixedColumns = FALSE,
      ordering = TRUE,
      scrollY = scrollY,
      scrollX = TRUE,
      # autoWidth = TRUE,
      searching = searching,
      # dom = "Bfrtip",
      # buttons = c("copy", "csv", "excel", "pdf", "print"),
      scrollCollapse = FALSE,
      stateSave = FALSE
    ),
    class = "cell-border",
    # filter = "top",
    escape = FALSE
  ) |>
    DT::formatStyle(colnames(DF), color = "#000")
  x
}
#' @noRd
make_DT_table_simple <- function(DF) {
  if (!is_something(DF)) {
    return(h3("No data available to display."))
  }
  DF |> DT::datatable() |> DT::formatStyle(colnames(DF), color = "#000")
}
