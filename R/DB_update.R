#' @import RosyApp
#' @title Update REDCap Database
#' @description
#' Updates the REDCap database (`DB` object) by fetching the latest data from the REDCap server.
#'
#' @details
#' This function updates the REDCap database by fetching the latest data from the REDCap server. It supports various options such as forcing a fresh update, checking logs for a specified number of days, and retrieving files from REDCap. The function can also handle metadata-only updates and batch processing.
#'
#' @inheritParams save_DB
#' @param set_token_if_fails Logical (TRUE/FALSE). If TRUE, prompts the user to set the REDCap API token if the update fails. Default is `TRUE`.
#' @param force Logical (TRUE/FALSE). If TRUE, forces a fresh update. Default is `FALSE`.
#' @param day_of_log Integer. Number of days to be checked in the log. Default is `10`.
#' @param labelled Logical (TRUE/FALSE). If TRUE, returns labelled REDCap data. If FALSE, returns raw data. Default is `TRUE`.
#' @param get_files Logical (TRUE/FALSE). If TRUE, retrieves files from REDCap. Default is `FALSE`.
#' @param original_file_names Logical (TRUE/FALSE). If TRUE, uses original file names for retrieved files. Default is `FALSE`.
#' @param entire_log Logical (TRUE/FALSE). If TRUE, retrieves the entire log. Default is `FALSE`.
#' @param metadata_only Logical (TRUE/FALSE). If TRUE, updates only the metadata. Default is `FALSE`.
#' @param ask_about_overwrites Logical (TRUE/FALSE). If TRUE, prompts the user before overwriting existing data. Default is `TRUE`.
#' @param save_to_dir Logical (TRUE/FALSE). If TRUE, saves the updated data to the directory. Default is `TRUE`.
#' @param batch_size Integer. Number of records to process in each batch. Default is `2000`.
#' @return Messages for confirmation.
#' @seealso
#' \link{setup_DB} for initializing the `DB` object.
#' @family db_functions
#' @export
update_DB <- function(
    DB,
    set_token_if_fails = T,
    force = F,
    day_of_log = 10,
    labelled = T,
    get_files = F,
    original_file_names = F,
    entire_log = F,
    metadata_only = F,
    ask_about_overwrites = T,
    save_to_dir = T,
    batch_size = 2000
) {
  IDs <- NULL
  will_update <- T
  was_updated <- F
  DB <- validate_DB(DB)
  if(!is.null(DB$internals$data_extract_labelled)){
    if(DB$internals$data_extract_labelled!=labelled){
      if(!force){
        force <- T
        warning("The DB that was loaded was ",ifelse(DB$internals$data_extract_labelled,"labelled","RAW"), " and you chose ",ifelse(labelled,"labelled","RAW"),". Therefore, we will set force to TRUE for a full update of data to avoid data conflicts",immediate. = T)
      }
    }
  }
  DB <- test_REDCap_token(DB,set_if_fails = set_token_if_fails)
  connected <- DB$internals$last_test_connection_outcome
  if(metadata_only)force <- T
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
      ilog <- get_REDCap_log(
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
    DB <- DB %>% get_REDCap_metadata(include_users = !metadata_only)
    DB$internals$is_transformed <- F
    if(!metadata_only){
      DB$data <- list()
      DB$data_update <- list()
      DB$summary <- list()
      DB$data <- DB %>% get_REDCap_data(labelled = labelled,batch_size=batch_size)
      DB$internals$data_extract_labelled <- labelled
      log <- DB$redcap$log # in case there is a log already
      if(entire_log){
        DB$redcap$log <- log %>% dplyr::bind_rows(
          DB %>% get_REDCap_log(begin_time = DB$redcap$project_info$creation_time) %>% unique()
        )
      }else{
        DB$redcap$log <- log %>% dplyr::bind_rows(
          DB %>% get_REDCap_log(last = day_of_log,units = "days") %>% unique()
        )
      }
      # DB <- annotate_fields(DB)
      # DB <- annotate_choices(DB)
      DB$summary$all_records <- sum_records(DB)
      DB$summary$all_records$last_api_call <-
        DB$internals$last_full_update <-
        DB$internals$last_metadata_update <-
        DB$internals$last_data_update <- Sys.time()
      bullet_in_console(paste0("Full ",DB$short_name," update!"),bullet_type = "v")
      was_updated <- T
    }
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
        DB2 <- transform_DB(DB2, ask = ask_about_overwrites)
        if(!is.null(DB2$data_update$from_transform)){
          DB2 <- upload_transform_to_DB(DB2)
        }
        data_list <- DB2$data %>% process_df_list(silent = T) %>% all_character_cols_list()
      }
      if(any(!names(data_list)%in%names(DB$data)))stop("Imported data names doesn't match DB$data names. If this happens run `untransform_DB()` or `update_DB(DB, force = T)`")
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
  if(was_updated&&save_to_dir&&!is.null(DB$dir_path)){
    save_DB(DB)
  }
  DB
}
