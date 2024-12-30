#' @import RosyUtils
#' @import RosyApp
sum_records <- function(DB){
  records <- NULL
  if(DB$data %>% is_something()){
    cols <- DB$redcap$id_col
    if(is.data.frame(DB$metadata$arms)){
      if(nrow(DB$metadata$arms)>1){
        cols <- DB$redcap$id_col %>% append("arm_num")
      }
    }
    if(length(cols)==1){
      records <- data.frame(
        records =  names(DB$data) %>% lapply(function(IN){DB$data[[IN]][,cols]}) %>% unlist() %>% unique()
      )
      colnames(records) <- cols
    }
    if(length(cols) == 2){
      records <- names(DB$data) %>% lapply(function(IN){DB$data[[IN]][,cols]}) %>% dplyr::bind_rows() %>% unique()
      # records <- records[order(as.integer(records[[DB$redcap$id_col]])),]
    }
    rownames(records) <- NULL
    if(records[[DB$redcap$id_col]]%>% duplicated() %>% any())stop("duplicate ",DB$redcap$id_col, " in sum_records() function")
  }
  return(records)
}
get_log <- function(DB, records){
  log <- DB$redcap$log
  log <- log[which(!is.na(log$username)),]
  log <- log[which(!is.na(log$record)),]
  if(!missing(records)){
    if(!is.null(records)){
      log <- log[which(log$record%in%records),]
    }
  }
  return(log)
}
summarize_users_from_log <- function(DB,records){
  log <- get_log(DB,records)
  summary_users <- DB$redcap$users %>% dplyr::select(c("username","role_label","email" ,"firstname","lastname"))
  user_groups <- log %>% split(log$username)
  summary_users <- summary_users[which(summary_users$username%in%names(user_groups)),]
  user_groups <- user_groups[drop_nas(match(summary_users$username, names(user_groups)))]
  summary_users$last_timestamp <- user_groups %>% sapply(function(group) {
    group$timestamp[[1]]
  })
  summary_users$first_timestamp <- user_groups %>% sapply(function(group) {
    group$timestamp %>% dplyr::last()
  })
  summary_users$last_user <- user_groups %>% sapply(function(group) {
    group$username[[1]]
  })
  summary_users$unique_records_n <- user_groups %>% sapply(function(group) {
    ul(group$record)
  })
  return(summary_users)
}
summarize_records_from_log <- function(DB,records){
  log <- DB$redcap$log
  log <- log[which(!is.na(log$username)),]
  log <- log[which(!is.na(log$record)),]
  if(!missing(records)){
    if(!is.null(records)){
      log <- log[which(log$record%in%records),]
    }
  }
  #records -------------
  # all_records <- unique(log$record)
  summary_records <- DB$summary$all_records
  record_groups <- log %>% split(log$record)
  summary_records <- summary_records[which(summary_records[[DB$redcap$id_col]]%in%names(record_groups)),,drop = FALSE]
  # users_log_rows <- users %>% lapply(function(user){which(log$username==user)})
  # records_log_rows <- records %>% lapply(function(record){which(log$record==record)})
  record_groups <- record_groups[match(summary_records[[DB$redcap$id_col]], names(record_groups))]
  summary_records$last_timestamp <- record_groups %>% sapply(function(group) {
    group$timestamp[[1]]
  })
  summary_records$first_timestamp <- record_groups %>% sapply(function(group) {
    group$timestamp %>% dplyr::last()
  })
  summary_records$last_user <- record_groups %>% sapply(function(group) {
    group$username[[1]]
  })
  summary_records$unique_users_n <- record_groups %>% sapply(function(group) {
    ul(group$username)
  })
  return(summary_records)
}
get_subset_records <-function(DB,subset_name){
  subset_list <- DB$summary$subsets[[subset_name]]
  subset_records <- NULL
  if(subset_list$filter_field==DB$redcap$id_col){
    records <- unique(subset_list$subset_records[[DB$redcap$id_col]])
  }else{
    filter_choices <- subset_list$filter_choices
    form_name <- field_names_to_form_names(DB,field_names = subset_list$filter_field)
    records <- DB$data[[form_name]][[DB$redcap$id_col]][which(DB$data[[form_name]][[subset_list$filter_field]]%in%subset_list$filter_choices)] %>% unique()
  }
  subset_records <- DB$summary$all_records[which(DB$summary$all_records[[DB$redcap$id_col]]%in% records),]
  return(subset_records)
}
subset_records_due <- function(DB,subset_name){
  subset_list <- DB$summary$subsets[[subset_name]]
  if(is.null(subset_list$last_save_time))return(TRUE)
  if(!file.exists(subset_list$file_path))return(TRUE)
  subset_records <- get_subset_records(
    DB = DB,
    subset_name = subset_name
  )
  return(!identical(unname(subset_list$subset_records),unname(subset_records)))
}
check_subsets <- function(subset_names){
  if(missing(subset_names))subset_names <- DB$summary$subsets %>% names()
  needs_refresh <- NULL
  if(is.null(subset_names))bullet_in_console("There are no subsets at `DB$summary$subsets` which can be added with `add_DB_subset()`!")
  for(subset_name in subset_names){
    if(subset_records_due(DB = DB, subset_name=subset_name))needs_refresh <- needs_refresh %>% append(subset_name)
  }
  if(is.null(needs_refresh))bullet_in_console("Refresh of subsets not needed!",bullet_type = "v")
  return(needs_refresh)
}
#' @title Add a Subset to a REDCap Database
#' @description
#' Creates a subset of the main REDCap database (`DB`) based on specific filter criteria
#' and saves it to a specified directory. The subset can be further customized with
#' additional forms, fields, and deidentification options.
#'
#' @inheritParams save_DB
#' @param subset_name Character. The name of the subset to create.
#' @param filter_field Character. The name of the field in the database to filter on.
#' @param filter_choices Vector. The values of `filter_field` used to define the subset.
#' @param dir_other Character. The directory where the subset file will be saved.
#' Default is the `output` folder within the database directory.
#' @param file_name Character. The base name of the file where the subset will be saved.
#' Default is `<DB$short_name>_<subset_name>`.
#' @param form_names Character vector. Names of forms to include in the subset. Default is `NULL`, which includes all forms.
#' @param field_names Character vector. Names of specific fields to include in the subset. Default is `NULL`, which includes all fields.
#' @param deidentify Logical. Whether to deidentify the data in the subset. Default is `TRUE`.
#' @param force Logical. If `TRUE`, overwrite existing subset files with the same name. Default is `FALSE`.
#'
#' @return
#' A modified `DB` object that includes the newly created subset.
#' The subset is also saved as a file in the specified directory.
#'
#' @details
#' This function filters the main REDCap database using the specified `filter_field`
#' and `filter_choices`, then creates a new subset with optional deidentification.
#' It can be customized to include only specific forms or fields. The resulting subset
#' is saved to a file for future use.
#'
#' @seealso
#' \code{\link{save_DB}} for saving the main database or subsets.
#' @export
add_DB_subset <- function(
    DB,
    subset_name,
    filter_field,
    filter_choices,
    dir_other = file.path(DB$dir_path,"output"),
    file_name = paste0(DB$short_name,"_",subset_name),
    form_names = NULL,
    field_names = NULL,
    deidentify = T,
    force = F
){
  if(is.null(DB$summary$subsets[[subset_name]])||force){
    subset_records <- NULL
    if(filter_field==DB$redcap$id_col){
      records <- unique(filter_choices)
      filter_choices <- NULL
    }else{
      form_name <- field_names_to_form_names(DB,field_names = filter_field)
      records <- DB$data[[form_name]][[DB$redcap$id_col]][which(DB$data[[form_name]][[filter_field]]%in%filter_choices)] %>% unique()
    }
    subset_records <- DB$summary$all_records[which(DB$summary$all_records[[DB$redcap$id_col]]%in% records),]
    DB$summary$subsets[[subset_name]] <- list(
      subset_name = subset_name,
      filter_field = filter_field,
      filter_choices = filter_choices,
      form_names = form_names,
      field_names = field_names,
      subset_records = subset_records,
      dir_other = dir_other,
      file_name = file_name,
      last_save_time = NULL,
      deidentify = deidentify,
      file_path = file.path(dir_other,paste0(file_name,".xlsx"))
    )
  }
  return(DB)
}
generate_summary_save_list <- function(
    DB,
    deidentify = T,
    clean = T,
    drop_blanks = T,
    other_drops = NULL,
    include_metadata = T,
    annotate_metadata = T,
    include_record_summary = T,
    include_users = T,
    include_log = T
){
  records <- sum_records(DB)[[1]]
  if(deidentify){
    DB <- deidentify_DB(DB)
  }
  if(clean){
    DB <- DB %>% clean_DB(drop_blanks = drop_blanks,other_drops = other_drops)# problematic because setting numeric would delete missing codes
  }
  to_save_list <- DB$data
  if(include_metadata){
    if(annotate_metadata&&is_something(DB$data)){
      to_save_list$forms <- annotate_forms(DB)
      to_save_list$fields <- annotate_fields(DB)
      to_save_list$choices <- annotate_choices(DB)
    }else{
      to_save_list$forms <- DB$metadata$forms
      to_save_list$fields <- DB$metadata$fields
      to_save_list$choices <- DB$metadata$choices
    }
    # if(DB$internals$is_transformed){
    #   to_save_list$original_forms <- DB$transformation$original_forms
    #   to_save_list$original_fields <- DB$transformation$original_fields
    # }
  }
  if(include_record_summary){
    to_save_list$records <- summarize_records_from_log(DB,records= records)
  }
  if(include_users){
    to_save_list$users <- summarize_users_from_log(DB,records= records)
  }
  if(include_log){
    to_save_list$log <- get_log(DB,records = records)
  }
  # to_save_list$choices <- annotate_choices(DB)
  # to_save_list$choices <- annotate_choices(DB)
  return(to_save_list)
}
save_RosyREDCap_list <- function(
    DB,
    to_save_list,
    dir_other = file.path(DB$dir_path,"output"),
    file_name = paste0(DB$short_name,"_RosyREDCap"),
    separate = F,
    with_links = T
){
  link_col_list <- list()
  if(with_links){
    if(DB$internals$DB_type=="redcap"){
      add_links <- which(names(to_save_list)%in%names(DB$data))
      if(length(add_links)>0){
        to_save_list[add_links] <- to_save_list[add_links] %>% lapply(function(DF){add_redcap_links_to_DF(DF,DB)})
        link_col_list <- list(
          "redcap_link"
        )
        names(link_col_list) <- DB$redcap$id_col
      }
    }
  }
  if(DB$internals$use_csv){
    to_save_list %>% list_to_csv(
      dir = dir_other,
      file_name = file_name,
      overwrite = TRUE
    )
  }else{
    to_save_list %>% RosyUtils::list_to_excel(
      dir = dir_other,
      separate = separate,
      link_col_list = link_col_list,
      file_name = file_name,
      # str_trunc_length = 10000,
      header_df_list = to_save_list %>% construct_header_list(fields = DB$metadata$fields) %>% process_df_list(silent = T),
      key_cols_list = construct_key_col_list(DB),
      overwrite = TRUE
    )
  }
}
#' @title Generate a Summary from a Subset Name
#' @description
#' Generates a summary from a predefined subset of data within a REDCap project. The summary can be customized based on various options, such as cleaning the data, including metadata, and annotating metadata.
#'
#' @inheritParams save_DB
#' @param subset_name Character. The name of the subset from which to generate the summary.
#' @param clean Logical. If `TRUE`, the data will be cleaned before summarizing. Default is `TRUE`.
#' @param drop_blanks Logical. If `TRUE`, records with blank fields will be dropped. Default is `TRUE`.
#' @param include_metadata Logical. If `TRUE`, metadata will be included in the summary. Default is `TRUE`.
#' @param annotate_metadata Logical. If `TRUE`, metadata will be annotated in the summary. Default is `TRUE`.
#' @param include_record_summary Logical. If `TRUE`, a record summary will be included in the generated summary. Default is `TRUE`.
#' @param include_users Logical. If `TRUE`, user-related information will be included in the summary. Default is `TRUE`.
#' @param include_log Logical. If `TRUE`, the log of changes will be included in the summary. Default is `TRUE`.
#' @param add_to_global Logical. If `TRUE`, the generated summary will be added to the global environment. Default is `TRUE`.
#'
#' @return
#' A list containing the generated summary based on the specified options. The list includes filtered and cleaned data, metadata, and other summary details.
#'
#' @details
#' This function allows you to generate a summary of data from a specific subset of records within the REDCap project. The function provides flexible options for cleaning, annotating, and including metadata, as well as controlling whether to include record summaries, user information, and logs. The summary can be added to the global environment for further use, depending on the `add_to_global` flag.
#' @export
generate_summary_from_subset_name <- function(
    DB,
    subset_name,
    clean = T,
    drop_blanks = T,
    include_metadata = T,
    annotate_metadata = T,
    include_record_summary = T,
    include_users = T,
    include_log = T,
    add_to_global = T
){
  subset_list <- DB$summary$subsets[[subset_name]]
  if(subset_list$filter_field==DB$redcap$id_col){
    subset_list$filter_choices <- subset_list$subset_records[[DB$redcap$id_col]]
  }
  DB$data <- filter_DB(
    DB = DB,
    field_names = subset_list$field_names,
    form_names = subset_list$form_names,
    filter_field = subset_list$filter_field,
    filter_choices = subset_list$filter_choices
  )
  to_save_list <- DB %>% generate_summary_save_list(
    deidentify = subset_list$deidentify,
    clean = clean,
    drop_blanks = drop_blanks,
    include_metadata = include_metadata,
    annotate_metadata = annotate_metadata,
    include_record_summary = include_record_summary,
    include_users = include_users,
    include_log = include_log
  )
  if(add_to_global){
    add_list_to_global(to_save_list)
  }
  return(to_save_list)
}
#' @title Summarize REDCap Database
#' @description
#' Summarizes the REDCap database (`DB` object) by filtering and generating a summary list.
#'
#' @details
#' This function filters the REDCap database based on the provided parameters and generates a summary list. The summary can include metadata, record summaries, user information, and logs. The function also supports deidentification and cleaning of the data.
#'
#' @inheritParams save_DB
#' @param with_links Logical (TRUE/FALSE). If TRUE, includes links in the summary. Default is `TRUE`.
#' @param deidentify Logical (TRUE/FALSE). If TRUE, deidentifies the summary data. Default is `TRUE`.
#' @param clean Logical (TRUE/FALSE). If TRUE, cleans the summary data. Default is `TRUE`.
#' @param drop_blanks Logical (TRUE/FALSE). If TRUE, drops blank entries from the summary. Default is `TRUE`.
#' @param include_metadata Logical (TRUE/FALSE). If TRUE, includes metadata in the summary. Default is `TRUE`.
#' @param annotate_metadata Logical (TRUE/FALSE). If TRUE, annotates metadata in the summary. Default is `TRUE`.
#' @param include_record_summary Logical (TRUE/FALSE). If TRUE, includes a summary of records in the summary. Default is `TRUE`.
#' @param include_users Logical (TRUE/FALSE). If TRUE, includes user information in the summary. Default is `TRUE`.
#' @param include_log Logical (TRUE/FALSE). If TRUE, includes logs in the summary. Default is `TRUE`.
#' @param separate Logical (TRUE/FALSE). If TRUE, separates the summary into different sections. Default is `FALSE`.
#' @param force Logical (TRUE/FALSE). If TRUE, forces the summary generation even if there are issues. Default is `FALSE`.
#' @return List. Returns a list containing the summarized data, including records, metadata, users, logs, and any other specified data.
#' @seealso
#' \code{\link[RosyREDCap]{setup_RosyREDCap}} for initializing the `DB` object.
#' \code{\link[RosyREDCap]{update_DB}} for updating the `DB` object.
#' @family db_functions
#' @export
summarize_DB <- function(
    DB,
    with_links = T,
    deidentify = T,
    clean = T,
    drop_blanks = T,
    include_metadata = T,
    annotate_metadata = T,
    include_record_summary = T,
    include_users = T,
    include_log = T,
    separate = F,
    force = F
){
  DB <- DB %>% validate_DB()
  original_data <- DB$data
  do_it <- is.null(DB$internals$last_summary)
  last_data_update <- DB$internals$last_data_update
  if(!do_it){
    do_it <- DB$internals$last_summary<last_data_update
  }
  if(force | do_it){
    to_save_list <- DB %>% generate_summary_save_list(
      deidentify = deidentify,
      clean = clean,
      drop_blanks = drop_blanks,
      include_metadata = include_metadata,
      annotate_metadata = annotate_metadata,
      include_record_summary = include_record_summary,
      include_users = include_users,
      include_log = include_log
    )
    DB %>% save_RosyREDCap_list(
      to_save_list = to_save_list,
      separate = separate,
      with_links = with_links
    )
    DB$internals$last_summary <- last_data_update
  }
  subset_names <- check_subsets()
  if(force)subset_names <- DB$summary$subsets %>% names()
  if(is_something(subset_names)){
    for(subset_name in subset_names){
      DB$data <- original_data
      subset_list <- DB$summary$subsets[[subset_name]]
      DB$summary$subsets[[subset_name]]$subset_records <- get_subset_records(DB=DB,subset_name = subset_name)
      DB$summary$subsets[[subset_name]]$last_save_time <- Sys.time()
      to_save_list <- DB %>% generate_summary_from_subset_name(
        subset_name = subset_name,
        clean = clean,
        drop_blanks = drop_blanks,
        include_metadata = include_metadata,
        annotate_metadata = annotate_metadata,
        include_record_summary = include_record_summary,
        include_users = include_users,
        include_log = include_log,
        add_to_global = F
      )
      DB %>% save_RosyREDCap_list(
        to_save_list = to_save_list,
        dir_other = subset_list$dir_other,
        file_name = subset_list$file_name,
        separate = separate,
        with_links = with_links
      )
    }
  }
  DB$data <- original_data
  return(DB)
}
#' @title Run Quality Checks
#' @inheritParams save_DB
#' @export
run_quality_checks <- function(DB){
  DB <- validate_DB(DB)
  if(is_something(DB$quality_checks)){
    for (qual_check in names(DB$quality_checks)){
      the_function <- DB$quality_checks[[qual_check]]
      if(is.function(the_function)){
        DB <- the_function(DB)
      }
    }
  }
  return(DB)
}
