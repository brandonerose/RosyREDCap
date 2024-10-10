#' @import RosyUtils
#' @import RosyDB
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
summarize_users_from_log <- function(DB,records){
  log <- DB$redcap$log
  log <- log[which(!is.na(log$username)),]
  log <- log[which(!is.na(log$record)),]
  if(!missing(records)){
    if(!is.null(records)){
      log <- log[which(log$record%in%records),]
    }
  }
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
#' @import RosyUtils
#' @import RosyApp
#' @title summarize_RosyREDCap
#' @param drop_blanks optional logical for dropping blanks
#' @export
summarize_RosyREDCap <- function(
    DB,
    subset_name,
    drop_blanks = T,
    filter_field = NULL,
    filter_choices = NULL,
    form_names = NULL,
    field_names = NULL,
    warn_only = F,
    with_links=T,
    dir_other = file.path(DB$dir_path,"output"),
    file_name = paste0(subset_name,"_RosyREDCap"),
    separate = F
){
  DB <- DB %>% validate_RosyREDCap()
  DB$summary$subsets[[subset_name]] <- list(
    subset_name = subset_name,
    filter_field = filter_field,
    filter_choices = filter_choices,
    # id_col = NULL,
    last_save_time = Sys.time(),
    file_path = file.path(dir_other,paste0(file_name,".xlsx"))
  )
  original_metadata <- DB$metadata
  original_data <- DB$data
  DB$data <- filter_DB(
    DB = DB,
    field_names = field_names,
    form_names = form_names,
    filter_field = filter_field,
    filter_choices = filter_choices
  )
  to_save_list <- DB$data
  link_col_list <- list()
  if(with_links){
    if(DB$internals$DB_type=="redcap"){
      to_save_list <-to_save_list %>% lapply(function(DF){add_redcap_links_to_DF(DF,DB)})
      link_col_list <- list(
        "redcap_link"
      )
      names(link_col_list) <- DB$redcap$id_col
    }
  }
  to_save_list$forms <- annotate_forms(DB)
  to_save_list$fields <- annotate_fields(DB)
  to_save_list$choices <- annotate_choices(DB)
  # to_save_list$choices <- annotate_choices(DB)
  # to_save_list$choices <- annotate_choices(DB)
  if(DB$internals$use_csv){
    to_save_list %>% list_to_csv(
      dir = dir_other,
      file_name = file_name,
      overwrite = TRUE
    )
  }else{
    to_save_list %>% list_to_excel(
      dir = dir_other,
      separate = separate,
      link_col_list = link_col_list,
      file_name = file_name,
      header_df_list = to_save_list %>% construct_header_list(fields = DB$metadata$fields),
      key_cols_list = construct_key_col_list(DB),
      overwrite = TRUE
    )
  }
  original_metadata <- DB$metadata
  original_data <- DB$data
  return(DB)
}
