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
#' @param DB DB from load_DB or setup_DB
#' @param force logical for force a fresh update
#' @param day_of_log numbers of days to be checked in the log
#' @param labelled logical for whether or not to return raw or labelled REDCap. Default is TRUE.
#' @param get_files logical for whether or not to get files from redcap.
#' @param original_file_names logical for whether or not to use original file names.
#' @return messages for confirmation
#' @export
update_DB <- function(
    DB,
    force = F,
    day_of_log = 10,
    labelled = T,
    get_files = F,
    original_file_names = F,
    entire_log = T
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
  DB <- test_redcap(DB)
  # DB$internals$last_metadata_update <- Sys.time()-lubridate::days(1)
  # DB$internals$last_data_update <- Sys.time()-lubridate::days(1)
  if(!force){ # check log interim
    if(is.null(DB$internals$last_metadata_update)||is.null(DB$internals$last_data_update)||is.null(DB$internals$last_full_update)){
      force <- T
    }else{
      ilog <- check_redcap_log(
        DB,
        begin_time = as.character(strptime(DB$REDCap$log$timestamp[1],format = "%Y-%m-%d %H:%M") - lubridate::days(1))
      ) %>% clean_redcap_log() %>% unique()
      if(nrow(ilog)<=nrow(DB$REDCap$log)){
        head_of_log <- DB$REDCap$log %>% head(n = nrow(ilog))
      }else{
        head_of_log <- DB$REDCap$log
      }
      df1 <- ilog %>% rbind(head_of_log) %>% unique()
      #dup <- df1[which(duplicated(rbind(df1, head_of_log),fromLast = T)[1:nrow(df1)]), ]
      ilog <- df1[which(!duplicated(rbind(df1, head_of_log),fromLast = T)[1:nrow(df1)]), ]
      if(nrow(ilog)>0){
        DB$REDCap$log <- ilog %>% dplyr::bind_rows(DB$REDCap$log) %>% unique()
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
    DB$data_transform <- list()
    DB$data_upload <- list()
    DB$summary <- list()
    DB <- DB %>% get_redcap_metadata()
    DB$data <- DB %>% get_redcap_data(labelled = labelled)
    DB$internals$data_extract_labelled <- labelled
    log <- DB$REDCap$log # in case there is a log already
    if(entire_log){
      DB$REDCap$log <- log %>% dplyr::bind_rows(
        DB %>% check_redcap_log(begin_time = DB$REDCap$project_info$creation_time) %>% unique()
      )
    }else{
      DB$REDCap$log <- log %>% dplyr::bind_rows(
        DB %>% check_redcap_log(last = day_of_log,units = "days") %>% unique()
      )
    }
    DB$summary$all_records <- sum_records(DB)
    DB$summary$all_records$last_api_call <-
      DB$internals$last_full_update <-
      DB$internals$last_metadata_update <-
      DB$internals$last_data_update <-
      Sys.time()
    message("Full update!")
    was_updated <- T
  }else{
    if(will_update){
      DB$data <- DB$data %>% all_character_cols_list()
      if(length(deleted_records)>0){
        DB$summary$all_records <- DB$summary$all_records[which(!DB$summary$all_records[[DB$REDCap$id_col]]%in%deleted_records),]
        IDs <- IDs[which(!IDs%in%deleted_records)]
        DB$data <- remove_records_from_list(data_list = DB$data,records = deleted_records,silent = T)
      }
      data_list <- DB %>% get_redcap_data(labelled = labelled,records = IDs)
      missing_from_summary <- IDs[which(!IDs%in%DB$summary$all_records[[DB$REDCap$id_col]])]
      if(length(missing_from_summary)>0){
        x <- data.frame(
          record = missing_from_summary,
          last_api_call = NA
        )
        colnames(x)[1] <- DB$REDCap$id_col
        DB$summary$all_records <- DB$summary$all_records %>% dplyr::bind_rows(x)
        DB$summary$all_records <- DB$summary$all_records[order(DB$summary$all_records[[DB$REDCap$id_col]],decreasing = T),]
      }
      DB$summary$all_records$last_api_call[which(DB$summary$all_records[[DB$REDCap$id_col]]%in%IDs)] <-
        DB$internals$last_data_update <-
        Sys.time()
      DB$data <- remove_records_from_list(data_list = DB$data,records = IDs,silent = T)
      for(TABLE in names(data_list)){
        DB$data[[TABLE]] <- DB$data[[TABLE]] %>% dplyr::bind_rows(data_list[[TABLE]])
      }
      message("Updated: ",paste0(IDs,collapse = ", "))
      was_updated <- T
    }else{
      message("Up to date already!")
    }
  }
  if(get_files){#test now
    get_redcap_files(DB,original_file_names=original_file_names)
  }
  if(was_updated){
    if(!is.null(DB$dir_path)) {
      save_DB(DB)
    }
  }
  DB
}
remove_records_from_list <- function(data_list,records,silent=F){
  if(!is_df_list(data_list))stop("data_list is not a list of data.frames as expected.")
  if(length(records)==0)stop("no records supplied to remove_records_from_list, but it's used in update which depends on records.")
  forms <- names(data_list)[
    which(
      names(data_list) %>%
        sapply(function(form){
          nrow(data_list[[form]])>0
        })
    )]
  for(TABLE in forms){
    data_list[[TABLE]] <- data_list[[TABLE]][which(!data_list[[TABLE]][[DB$REDCap$id_col]]%in%records),]
  }
  if(!silent)message("Removed: ",paste0(records,collapse = ", "))
  return(data_list)
}
