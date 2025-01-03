#' @title Select REDCap Records from DB
#' @description
#' This function filters the records in the `DB` object by specified criteria, such as field names, form names, and optional filtering based on a specific field and its values. It returns a modified `DB` object containing only the records that match the filter criteria.
#'
#' @inheritParams save_DB
#' @param field_names A character vector of field names to be included in the filtered data. If missing, all fields are included.
#' @param form_names A character vector of form names to be included in the filtered data. If missing, all forms are included.
#' @param filter_field A character string representing an extra variable name to be filtered by. This field must be present in the data frame.
#' @param filter_choices A character vector of values to filter by for the `filter_field`. Only records with these values in the specified field will be included.
#' @param warn_only A logical flag (`TRUE` or `FALSE`). If `TRUE`, the function will issue a warning instead of stopping if the filtering criteria do not match any records. Defaults to `FALSE`.
#' @param no_duplicate_cols A logical flag (`TRUE` or `FALSE`). If `TRUE`, the function will avoid including duplicate columns in the output. Defaults to `FALSE`.
#'
#' @return A modified `DB` object with filtered records and columns based on the provided criteria.
#'
#' @details
#' This function filters the data in the `DB` object according to the specified form and field names and optional filter criteria. If no field names or form names are provided, it defaults to using all fields and forms in the database.
#' The function uses the helper `filter_DF_list` to apply the filtering logic to the `DB$data` list.
#'
#' @export
filter_DB <- function(DB, filter_field, filter_choices, form_names, field_names, warn_only = F, no_duplicate_cols = F){#, ignore_incomplete=F, ignore_unverified = F
  if(missing(field_names))field_names <- DB %>% get_all_field_names()
  if(is.null(field_names))field_names <- DB %>% get_all_field_names()
  if(missing(form_names))form_names <- names(DB$data)
  if(is.null(form_names))form_names <- names(DB$data)
  return(
    filter_DF_list(
      DF_list = DB$data,
      DB = DB,
      filter_field = filter_field,
      filter_choices = filter_choices,
      form_names = form_names,
      field_names = field_names,
      warn_only = warn_only,
      no_duplicate_cols = no_duplicate_cols
    )
  )
}
#' @title rmarkdown_DB
#' @description
#' Generates an RMarkdown report for the given REDCap database (`DB` object).
#' This function creates an RMarkdown file in the specified directory or default directory,
#' allowing users to create custom reports based on the database content.
#'
#' @details
#' This function checks if a directory is specified, and if not, defaults to the `output` folder
#' within the project's directory. It generates the RMarkdown file that can then be used for further
#' processing or rendering into HTML, PDF, or other formats.
#'
#' @inheritParams save_DB
#' @param dir_other Character string specifying the directory where the RMarkdown report will be saved.
#' If not provided, it defaults to the `output` directory inside the project's main directory.
#' @return A message indicating the creation of the RMarkdown report and the path to the generated file.
#' @seealso
#' \code{\link[RosyREDCap]{save_DB}} for saving the `DB` object.
#' @family db_functions
#' @export
rmarkdown_DB <- function (DB,dir_other){
  if(missing(dir_other)){
    dir <- get_dir(DB) %>% file.path("output")
  }else{
    dir  <- dir_other
  }
  filename <- paste0(DB$short_name,"_full_summary_",gsub("-","_",Sys.Date()),".pdf")
  rmarkdown::render(
    input = system.file("rmarkdown","pdf.Rmd",package = pkg_name),
    output_format = "pdf_document",
    output_file = dir %>% file.path(filename),
    output_dir = dir,
    quiet = F
  )
}
#' @title Clean to Raw REDCap forms
#' @inheritParams save_DB
#' @param FORM data.frame of labelled REDCap to be converted to raw REDCap (for uploads)
#' @return DB object that has been filtered to only include the specified records
#' @export
labelled_to_raw_form <- function(FORM,DB){
  use_missing_codes <- is.data.frame(DB$metadata$missing_codes)
  fields <- filter_fields_from_form(FORM = FORM,DB = DB)
  for(i in 1:nrow(fields)){ # i <-  1:nrow(fields) %>% sample(1)
    COL_NAME <- fields$field_name[i]
    has_choices <- fields$has_choices[i]
    if(has_choices){
      z <- fields$select_choices_or_calculations[i] %>% split_choices()
      FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
        OUT <- NA
        if(!is.na(C)){
          coded_redcap <- which(z$name==C)
          if(length(coded_redcap)>0){
            OUT <- z$code[coded_redcap]
          }else{
            if(use_missing_codes){
              coded_redcap2 <- which(DB$metadata$missing_codes$name==C)
              if(length(coded_redcap2)>0){
                OUT <- DB$metadata$missing_codes$code[coded_redcap2]
              }else{
                stop("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C)
              }
            }else{
              stop("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()
    }else{
      if(use_missing_codes){
        FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
          OUT <- C
          if(!is.na(C)){
            D <- which(DB$metadata$missing_codes$name==C)
            if(length(D)>0){
              OUT <- DB$metadata$missing_codes$code[D]
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }
    }
  }
  FORM
}
#' @title Raw to Labelled REDCap forms
#' @param FORM data.frame of raw REDCap to be converted to labelled REDCap
#' @inheritParams save_DB
#' @return DB object
#' @export
raw_to_labelled_form <- function(FORM,DB){
  if(nrow(FORM)>0){
    use_missing_codes <- is.data.frame(DB$metadata$missing_codes)
    metadata <- filter_fields_from_form(FORM = FORM,DB = DB)
    for(i in 1:nrow(metadata)){ # i <-  1:nrow(metadata) %>% sample(1)
      COL_NAME <- metadata$field_name[i]
      has_choices <- metadata$has_choices[i]
      if(has_choices){
        z <- metadata$select_choices_or_calculations[i] %>% split_choices()
        FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
          OUT <- NA
          if(!is.na(C)){
            coded_redcap <- which(z$code==C)
            if(length(coded_redcap)>0){
              OUT <- z$name[coded_redcap]
            }else{
              if(use_missing_codes){
                coded_redcap2 <- which(DB$metadata$missing_codes$code==C)
                if(length(coded_redcap2)>0){
                  OUT <- DB$metadata$missing_codes$name[coded_redcap2]
                }else{
                  warning("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
                }
              }else{
                warning("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C)
              }
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }else{
        if(use_missing_codes){
          z <- DB$metadata$missing_codes
          FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
            OUT <- C
            if(!is.na(C)){
              D <- which(z$code==C)
              if(length(D)>0){
                OUT <- z$name[D]
              }
            }
            OUT
          }) %>% unlist() %>% as.character()
        }
      }
    }
  }
  FORM
}
#' @noRd
stack_vars <- function(DB,vars,new_name,drop_na=T){
  DB <- validate_DB(DB)
  fields <- DB$metadata$fields
  if(!all(vars%in%fields$field_name))stop("all vars must be in metadata.")
  the_stack <- NULL
  for(var in vars){# var <- vars %>% sample1()
    DF <- filter_DB(DB,field_names = var)[[1]]
    colnames(DF)[which(colnames(DF)==var)] <- new_name
    the_stack <-the_stack %>% dplyr::bind_rows(DF)
  }
  if(drop_na){
    the_stack <- the_stack[which(!is.na(the_stack[[new_name]])),]
  }
  return(the_stack)
}
#' @noRd
get_original_field_names <- function(DB){
  if(DB$internals$is_transformed)return(DB$transformation$original_fields$field_name)
  return(DB$metadata$fields$field_name)
}
#' @noRd
get_all_field_names <- function(DB){
  return(DB$data %>% sapply(colnames) %>% unlist() %>% unique())
}
#' @noRd
field_names_to_form_names <- function(DB,field_names){
  form_key_cols <- DB$metadata$form_key_cols %>% unlist() %>% unique()
  field_names_keys <- field_names[which(field_names%in%form_key_cols)]
  form_names_keys <- field_names_keys %>% sapply(function(field_name){
    DB$metadata$form_key_cols%>% names()%>% sapply(function(FORM){
      if(!field_name%in%DB$metadata$form_key_cols[[FORM]])return(NULL)
      return(FORM)
    })
  }) %>% unlist() %>% as.character() %>% unique()
  fields <- DB$metadata$fields
  field_names_not_keys <- field_names[which(!field_names%in%form_key_cols)]
  form_names_not_keys <- fields$form_name[match(field_names_not_keys, fields$field_name)] %>% drop_nas()
  form_names <- c(form_names_not_keys,form_names_keys) %>% unique()
  return(form_names)
}
#' @noRd
form_names_to_field_names <- function(form_names,DB,original_only = F){
  field_names <- NULL
  if(original_only){
    fields <- get_original_fields(DB)
  }else{
    fields <- DB$metadata$fields
  }
  for(form_name in form_names){
    field_names <- field_names %>% append(fields$field_name[which(fields$form_name==form_name)])
  }
  return(unique(field_names))
}
#' @noRd
form_names_to_form_labels <- function(form_names,DB){
  return(
    DB$metadata$forms$form_label[
      match(
        x = form_names,
        table = DB$metadata$forms$form_name
      )
    ]
  )
}
#' @noRd
form_labels_to_form_names <- function(form_labels,DB){
  return(
    DB$metadata$forms$form_name[
      match(
        x = form_labels,
        table = DB$metadata$forms$form_label
      )
    ]
  )
}
#' @noRd
field_names_to_field_labels <- function(field_names,DB){
  return(
    DB$metadata$fields$field_label[
      match(
        x = field_names,
        table = DB$metadata$fields$field_name
      )
    ]
  )
}
#' @noRd
construct_header_list <- function(DF_list,md_elements = c("form_name","field_type","field_label"),fields){
  if(anyDuplicated(fields$field_name)>0)stop("dup names not allowed in fields")
  df_col_list <- DF_list %>% lapply(colnames)
  header_df_list <- df_col_list %>% lapply(function(field_names){
    x<- field_names%>% lapply(function(field_name){
      row <- which(fields$field_name==field_name)
      if(length(row)>0){
        return(as.character(fields[md_elements][row,]))
      }else{
        return(rep("",length(md_elements)))
      }
    }) %>% as.data.frame()
    colnames(x)<-field_names
    x<- x[which(apply(x, 1, function(row){any(row!="")})),]
    x
  })
  return(header_df_list)
}
#' @noRd
stripped_DB <- function (DB) {
  DB$redcap$log <- list()
  DB$data <- list()
  DB$data_update <- list()
  return(DB)
}
#' @noRd
filter_DF_list <- function(DF_list,DB,filter_field, filter_choices, form_names, field_names, warn_only = F, no_duplicate_cols = F){
  if(missing(field_names))field_names <- DB %>% get_all_field_names()
  if(is.null(field_names))field_names <- DB %>% get_all_field_names()
  if(missing(form_names))form_names <- names(DF_list)
  if(is.null(form_names))form_names <- names(DF_list)
  out_list <- list()
  form_key_cols <- DB$metadata$form_key_cols %>% unlist() %>% unique()
  is_key <- filter_field %in% form_key_cols
  if(!is_key){
    form_name <- field_names_to_form_names(DB,field_names = filter_field)
    is_repeating_filter <- DB$metadata$forms$repeating[which(DB$metadata$forms$form_name==form_name)]
  }
  for(FORM in form_names){
    DF <- DF_list[[FORM]]
    is_repeating_form <- DB$metadata$forms$repeating[which(DB$metadata$forms$form_name==FORM)]
    if(is_something(DF)){
      filter_field_final <- filter_field
      filter_choices_final <- filter_choices
      if(!is_key){
        if(is_repeating_filter){
          if(!is_repeating_form){
            filter_field_final <- DB$metadata$form_key_cols[[FORM]]
            filter_choices_final <- DF_list[[form_name]][[filter_field_final]][which(DF_list[[form_name]][[filter_field]]%in%filter_choices)] %>% unique()
          }
        }
      }
      rows <-which(DF_list[[FORM]][[filter_field_final]]%in%filter_choices_final)
      field_names_adj <- field_names
      if(no_duplicate_cols) field_names_adj <- field_names_adj %>% vec1_in_vec2(form_names_to_field_names(FORM,DB,original_only = F))
      cols <- colnames(DF)[which(colnames(DF)%in%field_names_adj)]
      if(length(rows)>0&&length(cols)>0){
        cols <- colnames(DF)[which(colnames(DF)%in%unique(c(DB$metadata$form_key_cols[[FORM]],field_names_adj)))]
        out_list[[FORM]] <- DF[rows,cols]
      }
    }
  }
  return(out_list)
}
#' @noRd
field_names_metadata <- function(DB,field_names,col_names){
  fields <- get_original_fields(DB) #DB$metadata$fields
  # if(!deparse(substitute(FORM))%in%DB$metadata$forms$form_name)stop("To avoid potential issues the form name should match one of the instrument names" )
  BAD <- field_names[which(!field_names%in%c(DB$metadata$fields$field_name,DB$redcap$raw_structure_cols,"arm_num","event_name"))]
  if(length(BAD)>0)stop("All column names in your form must match items in your metadata, `DB$metadata$fields$field_name`... ", paste0(BAD, collapse = ", "))
  # metadata <- DB$metadata$fields[which(DB$metadata$fields$form_name%in%instruments),]
  fields <- fields[which(fields$field_name%in%field_names),]
  # metadata <- metadata[which(metadata$field_name%in%field_names),]
  if( ! missing(col_names)){
    if(is_something(col_names))fields <- fields[[col_names]]
  }
  return(fields)
}
#' @noRd
filter_fields_from_form <- function(FORM,DB){
  forms <- DB %>% field_names_to_form_names(field_names = colnames(FORM))
  if(any(forms%in%get_original_forms(DB)$repeating))stop("All column names in your form must match only one form in your metadata, `DB$metadata$forms$form_name`, unless they are all non-repeating")
  fields <- DB %>% field_names_metadata(field_names = colnames(FORM))
  fields <- fields[which(fields$field_type!="descriptive"),]
  fields$has_choices <- !is.na(fields$select_choices_or_calculations)
  return(fields)
}
#' @noRd
labelled_to_raw_DB <- function(DB){
  DB <- validate_DB(DB)
  if(!DB$internals$data_extract_labelled)stop("DB is already raw/coded (not labelled values)")
  for(TABLE in names(DB$data)){
    DB$data[[TABLE]] <- labelled_to_raw_form(FORM = DB$data[[TABLE]],DB=DB)
  }
  DB$internals$data_extract_labelled <- F
  DB
}
#' @noRd
DF_list_to_text <- function(DF_list, DB,drop_nas = T,clean_names= T){
  output_list <- c()
  for (i in seq_along(DF_list)) {
    DF <- DF_list[[i]]
    the_raw_name <- names(DF_list)[[i]]
    the_name <- the_raw_name
    if(clean_names)the_name <- DB$metadata$forms$form_label[which(DB$metadata$forms$form_name==the_raw_name)]
    df_name <- paste0("----- ",the_name, " Table -----")
    output_list <- c(output_list, paste0("&nbsp;&nbsp;<strong>", df_name, "</strong><br>"))
    key_col_names <- DB$metadata$form_key_cols[[the_raw_name]]
    for (j in 1:nrow(DF)) {
      for (col_name in colnames(DF)) {
        entry <- DF[j, col_name]
        if (!col_name %in% key_col_names) {
          if(!is.na(entry)|!drop_nas){
            entry <- gsub("\\n","<br>",entry)
            col_name_clean <- col_name
            if(clean_names)col_name_clean <- DB$metadata$fields$field_label[which(DB$metadata$fields$field_name==col_name)]
            output_list <- c(output_list, paste0("&nbsp;&nbsp;<strong>", col_name_clean, ":</strong> <br>&nbsp;&nbsp;&nbsp;&nbsp;", entry,"<br>"))
          }
        }
      }
      # output_list <- c(output_list, "<br>")
    }
    output_list <- c(output_list, "<br>")
  }
  return(output_list)
}
#' @noRd
check_DB_for_IDs <- function(DB,required_percent_filled = 0.7){
  cols <- NULL
  if(is_something(DB)){
    if(is_something(DB$data)){
      DF <- DB$data[[DB$metadata$forms$form_name[which(!DB$metadata$forms$repeating)][[1]]]]
      IN_length <- DF %>% nrow()
      cols <- colnames(DF)[DF %>% sapply(function(IN){
        OUT <- F
        x <- IN %>% drop_nas()
        if((length(x)/IN_length)>required_percent_filled){
          OUT <- anyDuplicated(x)==0
        }
        return(OUT)
      }) %>% unlist() %>% which()]
    }
  }
  return(cols)
}
