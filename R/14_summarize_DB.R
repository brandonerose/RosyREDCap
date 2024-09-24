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
