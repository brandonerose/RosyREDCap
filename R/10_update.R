#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
ignore_redcap_log <- function(collapse = T){
  ignores <- c(
    'export',
    'download ',
    'edit report',
    'Switch DAG',
    'Copy report',
    'Multi-Language',
    'File Repository ',
    'custom record dashboard',
    'User regenerate own API token',
    'Create report',
    ' external module'
  )
  if(collapse)return(paste0(ignores,collapse = "|"))
  return(ignores)
}
log_details_that_trigger_refresh <- function(){
  c(
    "Edit project field",
    "Delete project field",
    "Create project field",
    "Make project customizations",
    "Delete data collection instrument",
    "Download instrument from Shared Library",
    "Create data collection instrument",
    "Tag new identifier fields"
  )
}
#' @title Shows DB in the env
#' @param DB DB from load_RosyREDCap or setup_DB
#' @param force logical for force a fresh update
#' @param day_of_log numbers of days to be checked in the log
#' @param labelled logical for whether or not to return raw or labelled REDCap. Default is TRUE.
#' @param get_files logical for whether or not to get files from redcap.
#' @param original_file_names logical for whether or not to use original file names.
#' @return messages for confirmation
#' @export
update_RosyREDCap <- function(
    DB,
    force = F,
    day_of_log = 10,
    labelled = T,
    get_files = F,
    original_file_names = F,
    entire_log = F,
    ask_about_overwrites = T
) {
  IDs <- NULL
  will_update <- T
  was_updated <- F
  DB <- validate_RosyREDCap(DB)
  if(!is.null(DB$internals$data_extract_labelled)){
    if(DB$internals$data_extract_labelled!=labelled){
      if(!force){
        force <- T
        warning("The DB that was loaded was ",ifelse(DB$internals$data_extract_labelled,"labelled","RAW"), " and you chose ",ifelse(labelled,"labelled","RAW"),". Therefore, we will set force to TRUE for a full update of data to avoid data conflicts",immediate. = T)
      }
    }
  }
  DB <- test_REDCap(DB)
  # DB$internals$last_metadata_update <- Sys.time()-lubridate::days(1)
  # DB$internals$last_data_update <- Sys.time()-lubridate::days(1)
  if(!is.null(DB$transformation$data_updates)){
    do_it <- T
    bullet_in_console("There is data in 'DB$transformation$data_updates' that has not been pushed to REDCap yet...")
    print(DB$transformation$data_updates)
    if(ask_about_overwrites){
      do_it <- utils::menu(choices = c("Yes", "No and stop the function!"),title = "Would you like to push these updates now?") == 1
    }
    if(!do_it)stop("Stopped as requested!")
    DB <- upload_transform_to_DB(DB)
  }
  if(!force){ # check log interim
    if(
      is.null(DB$internals$last_metadata_update)||
      is.null(DB$internals$last_data_update)||
      is.null(DB$internals$last_full_update)
    ){
      force <- T
    }else{
      ilog <- check_redcap_log(
        DB,
        begin_time = as.character(strptime(DB$redcap$log$timestamp[1],format = "%Y-%m-%d %H:%M") - lubridate::days(1))
      ) %>% clean_redcap_log() %>% unique()
      if(nrow(ilog)<=nrow(DB$redcap$log)){
        head_of_log <- DB$redcap$log %>% utils::head(n = nrow(ilog))
      }else{
        head_of_log <- DB$redcap$log
      }
      df1 <- ilog %>% rbind(head_of_log) %>% unique()
      #dup <- df1[which(duplicated(rbind(df1, head_of_log),fromLast = T)[1:nrow(df1)]), ]
      ilog <- df1[which(!duplicated(rbind(df1, head_of_log),fromLast = T)[1:nrow(df1)]), ]
      if(nrow(ilog)>0){
        DB$redcap$log <- ilog %>% dplyr::bind_rows(DB$redcap$log) %>% unique()
        ilog$timestamp <- NULL
        ilog_metadata <- ilog[which(is.na(ilog$record)),]
        ilog_metadata <- ilog_metadata[which(ilog_metadata$details%in%log_details_that_trigger_refresh()),] #inclusion
        # ilog_metadata <- ilog_metadata[grep(ignore_redcap_log(),ilog_metadata$details,ignore.case = T,invert = T) %>% unique(),]
        if(nrow(ilog_metadata)>0){
          force <- T
          message(paste0("Update because: Metadata was changed!"))
        }else{
          ilog_data <- ilog[which(!is.na(ilog$record)),]
          ilog_data <- ilog_data[which(ilog_data$action_type!="Users"),]
          deleted_records<-ilog_data$record[which(ilog_data$action_type%in%c("Delete"))]
          if(length(deleted_records)>0){
            warning("There were recent records deleted from redcap Consider running with 'force = T'. Records: ",deleted_records %>% paste0(collapse = ", "),immediate. = T)
          }
          IDs <- ilog_data$record %>% unique()
          if(length(IDs)==0){
            IDs <- NULL
            will_update <- F
          }
        }
      }else{
        will_update <- F
      }
    }
  }
  if(force){
    DB$data <- list()
    DB$data_update <- list()
    DB$summary <- list()
    DB <- DB %>% get_REDCap_metadata()
    DB$data <- DB %>% get_REDCap_data(labelled = labelled)
    DB$internals$data_extract_labelled <- labelled
    log <- DB$redcap$log # in case there is a log already
    if(entire_log){
      DB$redcap$log <- log %>% dplyr::bind_rows(
        DB %>% check_redcap_log(begin_time = DB$redcap$project_info$creation_time) %>% unique()
      )
    }else{
      DB$redcap$log <- log %>% dplyr::bind_rows(
        DB %>% check_redcap_log(last = day_of_log,units = "days") %>% unique()
      )
    }
    DB <- annotate_fields(DB)
    DB <- annotate_choices(DB)
    DB$summary$all_records <- sum_records(DB)
    DB$summary$all_records$last_api_call <-
      DB$internals$last_full_update <-
      DB$internals$last_metadata_update <-
      DB$internals$last_data_update <- Sys.time()
    bullet_in_console(paste0("Full ",DB$short_name," update!"),bullet_type = "v")
    was_updated <- T
  }else{
    if(will_update){
      DB$data <- DB$data %>% all_character_cols_list()
      if(length(deleted_records)>0){
        DB$summary$all_records <- DB$summary$all_records[which(!DB$summary$all_records[[DB$redcap$id_col]]%in%deleted_records),]
        IDs <- IDs[which(!IDs%in%deleted_records)]
        DB$data <- remove_records_from_list(DB = DB,records = deleted_records,silent = T)
      }
      data_list <- DB %>% get_REDCap_data(labelled = labelled,records = IDs)
      missing_from_summary <- IDs[which(!IDs%in%DB$summary$all_records[[DB$redcap$id_col]])]
      if(length(missing_from_summary)>0){
        x <- data.frame(
          record = missing_from_summary,
          last_api_call = NA
        )
        colnames(x)[1] <- DB$redcap$id_col
        DB$summary$all_records <- DB$summary$all_records %>% dplyr::bind_rows(x)
        DB$summary$all_records <- DB$summary$all_records[order(DB$summary$all_records[[DB$redcap$id_col]],decreasing = T),]
      }
      DB$summary$all_records$last_api_call[which(DB$summary$all_records[[DB$redcap$id_col]]%in%IDs)] <-
        DB$internals$last_data_update <-
        Sys.time()
      DB$data <- remove_records_from_list(DB = DB,records = IDs,silent = T)
      if(DB$internals$is_transformed){
        DB2 <- stripped_DB(DB)
        DB2$internals$is_transformed <- F
        DB2$metadata$forms <- DB2$transformation$original_forms
        DB2$metadata$fields <- DB2$transformation$original_fields
        DB2$data <- data_list
        DB2 <- RosyDB::transform_DB(DB2, ask = ask_about_overwrites)
        if(!is.null(DB2$data_update$from_transform)){
          DB2 <- upload_transform_to_DB(DB2)
        }
        data_list <- DB2$data %>% process_df_list(silent = T) %>% all_character_cols_list()
      }
      if(any(!names(data_list)%in%names(DB$data)))stop("Imported data names doesn't match DB$data names. If this happens run `untransform_DB()` or `update_RosyREDCap(DB, force = T)`")
      for(TABLE in names(data_list)){
        DB$data[[TABLE]] <- DB$data[[TABLE]] %>% all_character_cols() %>% dplyr::bind_rows(data_list[[TABLE]])
      }
      message("Updated: ",paste0(IDs,collapse = ", "))
      was_updated <- T
    }else{
      message("Up to date already!")
    }
  }
  if(get_files){#test now
    get_REDCap_files(DB,original_file_names=original_file_names)
  }
  if(was_updated){
    DB <- annotate_fields(DB)
    DB <- annotate_choices(DB)
    if(!is.null(DB$dir_path)) {
      save_DB(DB)
    }
  }
  DB
}
