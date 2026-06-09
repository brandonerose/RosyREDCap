#' @noRd
age <- function(dob,
                age_day = lubridate::today(),
                units = "years",
                floor = TRUE) {
  calc_age <- lubridate::interval(dob, age_day) / lubridate::duration(num = 1L, units = units)
  if (floor) {
    return(as.integer(floor(calc_age)))
  }
  calc_age
}
#' @noRd
process_df_list <- function(list, drop_empty = TRUE) {
  if (is_something(list)) {
    if (!is_df_list(list)) {
      stop("list must be ...... a list :)")
    }
    if (drop_empty) {
      is_a_df_with_rows <- list |>
        lapply(function(x) {
          is_df <- is.data.frame(x)
          out <- FALSE
          if (is_df) {
            out <- nrow(x) > 0L
          }
          out
        }) |>
        unlist()
      keeps <- which(is_a_df_with_rows)
      list <- list[keeps]
    }
    if (length(list) > 0L) {
      if (!is_named_df_list(list)) {
        names(list) <- paste0(seq_along(list))
      }
    }
  }
  list
}
#' @noRd
is_something <- function(thing, row = 0L) {
  out <- FALSE
  if (is.function(thing)) {
    return(TRUE)
  }
  if (!is.null(thing)) {
    if (is.data.frame(thing)) {
      if (nrow(thing) > row) {
        out <- TRUE
      }
    } else {
      if (length(thing) > 0L) {
        if (is.list(thing)) {
          out <- TRUE
        } else {
          if (length(thing) == 1L) {
            if (!is.na(thing)) {
              if (is.character(thing)) {
                if (thing != "") {
                  out <- TRUE
                }
              } else {
                out <- TRUE
              }
            }
          } else {
            out <- TRUE
          }
        }
      }
    }
  }
  out
}
#' @noRd
vec1_in_vec2 <- function(vec1, vec2) {
  vec1[which(vec1 %in% vec2)]
}
#' @noRd
vec1_not_in_vec2 <- function(vec1, vec2) {
  vec1[which(!vec1 %in% vec2)]
}
#' @noRd
drop_nas <- function(x) {
  x[!unlist(lapply(x, is.na))]
}
#' @noRd
is_named_df_list <- function(x, strict = FALSE) {
  is_named_list(x) && is_df_list(x, strict = strict)
}
#' @noRd
is_named_list <- function(x,
                          silent = TRUE,
                          recursive = FALSE) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (is.null(names(x))) {
    return(FALSE)
  }
  named_all <- TRUE
  if (recursive) {
    for (n in names(x)) {
      element <- x[[n]]
      if (is.list(element)) {
        named_all <- named_all && is_named_list(element)
        if (!silent && !named_all)
          message("'", n, "' is not named")
      }
    }
  }
  named_all # Return the result
}
#' @noRd
remove_html_tags <- function(text_vector) {
  gsub(pattern = "<[^>]+>", replacement = "", text_vector)
}
#' @noRd
drop_if <- function(x, drops) {
  x[which(!x %in% drops)]
}
#' @noRd
sample1 <- function(x) {
  sample(x, 1L)
}
#' @noRd
wrap_text <- function(text,
                      max_length = 40L,
                      spacer = "\n") {
  words <- unlist(strsplit(text, " "))
  current_line <- ""
  result <- ""
  for (word in words) {
    if (nchar(current_line) + nchar(word) + 1L > max_length) {
      result <- paste0(result, current_line, spacer)
      current_line <- word
    } else {
      if (nchar(current_line) == 0L) {
        current_line <- word
      } else {
        current_line <- paste0(current_line, " ", word)
      }
    }
  }
  result <- paste0(result, current_line)
  result
}
#' @noRd
is_df_list <- function(x, strict = FALSE) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (length(x) == 0L) {
    return(FALSE)
  }
  if (is_nested_list(x)) {
    return(FALSE)
  }
  out <- unlist(lapply(x, is.data.frame))
  if (strict) {
    return(all(out))
  }
  any(out)
}
#' @noRd
is_nested_list <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (is.data.frame(x)) {
    return(FALSE)
  }
  outcome <- length(x) == 0L
  for (i in seq_along(x)) {
    outcome <- outcome || is_nested_list(x[[i]])
    # print(outcome)
  }
  outcome
}
#' @noRd
clean_num <- function(num) {
  formatC(num, format = "d", big.mark = ",")
}
