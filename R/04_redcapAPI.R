#' @import RosyUtils
redcap_api_base <- function(url,token,content,additional_args=NULL){
  body  <- list(
    "token"= token,
    content=content,
    format='csv',
    returnFormat='json'
  )
  if(!missing(additional_args)||is.null(additional_args)){
    body <- body %>% append(additional_args)
  }
  httr::POST(
    url = url,
    body = body,
    encode = "form"
  )
}
process_response <- function(response,error_action){
  content <- httr::content(response)
  if(httr::http_error(response)){
    if(!missing(error_action)){
      if(!is.null(error_action)){
        if(!error_action%in%c("stop","warn"))stop("error_action must be 'stop' or 'warn'")
        general_error <- response$status_code
        specific_error <- http_errors$Description[which(http_errors$Value==response$status_code)]
        message <- paste0("HTTP error ",general_error, ". ",specific_error,". ",content[["error"]])
        if(error_action=="stop")stop(message)
        warning(message,immediate. = T)
      }
    }
    return(NA)
  }
  return(all_character_cols(content))
}
test_redcap <- function(DB){
  ERROR  <- T
  while(ERROR){
    version <- redcap_api_base(DB$links$redcap_uri,validate_redcap_token(DB),"version")
    ERROR  <- version %>% httr::http_error()
    if(ERROR){
      warning('Your REDCap API token check failed. Invalid token or API privileges. Contact Admin! Consider rerunnning `setup_DB()`',immediate. = T)
      warning("HTTP error ",version %>% httr::status_code(), ". Check your token, internet connection, and redcap base link.",immediate. = T)
      message("Try going to REDCap --> '",DB$links$redcap_API,"' or run `link_API_token(DB)`")
      set_redcap_token(DB)
    }
  }
  message("Connected to REDCap!")
  DB$redcap$version <- version %>% httr::content(as="text") %>% as.character()
  DB
}
get_redcap_info <- function(DB,content,error_action=NULL,additional_args=NULL){
  if(!content%in%allowed_content)stop("Must use the following content... ",paste0(allowed_content,collapse = ", "))
  redcap_api_base(url=DB$links$redcap_uri,token = validate_redcap_token(DB),content = content,additional_args=additional_args) %>% process_response(error_action)
}
#' @title Drop redcap files to directory
#' @inheritParams save_DB
#' @param original_file_names logical for using original uploaded filenames vs system defined
#' @param overwrite logical rewriting over the downloaded files in your directory. A better alternative is deleting files you want to update.
#' @return message
#' @export
get_redcap_files <- function(DB,original_file_names = F,overwrite = F){
  file_rows <- which(DB$redcap$metadata$field_type=="file")
  out_dir <- file.path(DB$dir_path,"REDCap","files")
  if(length(file_rows)>0){
    dir.create(out_dir,showWarnings = F)
    for(field_name in DB$redcap$metadata$field_name[file_rows]){
      out_dir_folder <- file.path(out_dir,field_name)
      dir.create(out_dir_folder,showWarnings = F)
      form_name <- DB$redcap$metadata$form_name[which(DB$redcap$metadata$field_name == field_name)]
      is_repeating <- DB$redcap$instruments$repeating[which(DB$redcap$instruments$instrument_name==form_name)]
      form <- DB$data_extract[[form_name]]
      rows_to_save <- which(!is.na(form[[field_name]]))
      for(i in rows_to_save){
        file_name  <- form[[field_name]][i]
        record_id <- form[[DB$redcap$id_col]][i]
        repeat_instrument = form[["redcap_repeat_instrument"]][i]
        repeat_instance = form[["redcap_repeat_instance"]][i]
        redcap_event_name = form[["redcap_event_name"]][i]
        if(!original_file_names){
          if(anyDuplicated(file_name)>0){
            warning(paste0("You have duplicate file names in ",form_name,", ",field_name,". Therefore will use system generated names"),immediate. = T)
            original_file_names <- F
          }
        }
        file_name <- ifelse(original_file_names,file_name,paste0(form_name,"_",field_name,"_",ifelse(is_repeating,"inst_",""),repeat_instance,"ID_",record_id,".",tools::file_ext(file_name)))
        if(!file.exists(file.path(out_dir_folder,file_name))||overwrite){
          REDCapR::redcap_download_file_oneshot(
            redcap_uri = DB$links$redcap_uri,
            token = validate_redcap_token(DB),
            field = field_name,
            record = form[[DB$redcap$id_col]][i],
            directory = out_dir_folder,
            file_name = file_name,
            event = redcap_event_name,
            repeat_instrument = repeat_instrument,
            repeat_instance = repeat_instance,
            verbose = F
          )
          message("`",file_name,"` saved at --> ",out_dir_folder)
        }
      }
    }
  }
  message("Checked for files!")
}
get_redcap_metadata <- function(DB){
  DB$internals$last_metadata_update <- Sys.time()
  # info ----------
  DB$redcap$project_info <- get_redcap_info(DB,"project")
  DB$redcap$project_title <-  DB$redcap$project_info$project_title
  DB$redcap$project_id <- DB$redcap$project_info$project_id
  DB$redcap$is_longitudinal <- DB$redcap$project_info$is_longitudinal == "1"
  DB$redcap$missing_codes <- missing_codes2(DB)
  # instruments --------
  DB$redcap$instruments <- get_redcap_info(DB,"instrument","warn")
  DB$redcap$instruments$repeating <- F
  DB$redcap$has_repeating_instruments <- F
  DB$redcap$has_repeating_events <- F
  DB$redcap$has_repeating_instruments_or_events <- DB$redcap$project_info$has_repeating_instruments_or_events=="1"
  # if(DB$redcap$project_info$has_repeating_instruments_or_events=="1")
  repeating <- get_redcap_info(DB,"repeatingFormsEvents")
  if(is.data.frame(repeating)){
    DB$redcap$instruments$repeating <- DB$redcap$instruments$instrument_name%in%repeating$form_name
    #   DB$redcap$metadata <- DB$redcap$metadata %>%dplyr::bind_rows(
    #     data.frame(
    #       field_name="redcap_repeat_instance",form_name=DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)] ,field_label="REDCap Repeat Instance",field_type="text",select_choices_or_calculations=NA
    #     )
    #   ) %>% unique()
    #   DB$redcap$metadata <- DB$redcap$metadata %>%dplyr::bind_rows(
    #     data.frame(
    #       field_name="redcap_repeat_instrument",form_name=DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)] ,field_label="REDCap Repeat Instrument",field_type="text",select_choices_or_calculations=NA
    #     )
    #   ) %>% unique()
  }
  if(any(DB$redcap$instruments$repeating)){
    DB$redcap$has_repeating_instruments <- T
  }
  # metadata ----------
  DB$redcap$metadata <- get_redcap_info(DB,"metadata","stop")
  DB$redcap$metadata$section_header <- DB$redcap$metadata$section_header %>% remove_html_tags()
  DB$redcap$metadata$field_label <- DB$redcap$metadata$field_label %>% remove_html_tags()
  DB$redcap$id_col <- DB$redcap$metadata[1,1] %>% as.character() #RISKY?
  DB$redcap$instrument_key_cols <- get_key_col_list(DB)
  DB$redcap$raw_structure_cols <- DB$redcap$instrument_key_cols %>% unlist() %>% unique()
  instrument_names <- DB$redcap$instruments$instrument_name#[which(DB$redcap$instruments$instrument_name%in%unique(DB$redcap$metadata$form_name))]
  for (instrument_name in instrument_names){
    new_row <- data.frame(
      field_name = paste0(instrument_name,"_complete"),
      form_name = instrument_name,
      field_type = "radio",
      field_label = paste0(instrument_name,"_complete")  %>% strsplit("_") %>% unlist() %>% stringr::str_to_title() %>% paste0(collapse = " "),
      select_choices_or_calculations = "0, Incomplete | 1, Unverified | 2, Complete"
    )
    last_row <- nrow(DB$redcap$metadata)
    rows <- which(DB$redcap$metadata$form_name==instrument_name)
    if(length(rows)==0)rows <- last_row
    row <- dplyr::last(rows)
    top <- DB$redcap$metadata[1:row,]
    bottom <- NULL
    if(last_row>row){
      bottom <- DB$redcap$metadata[(row+1):last_row,]
    }
    DB$redcap$metadata <- top %>%
      dplyr::bind_rows(
        new_row
      ) %>%  dplyr::bind_rows(
        bottom
      )
  }
  if(any(DB$redcap$metadata$field_type=="checkbox")){
    for(field_name in DB$redcap$metadata$field_name[which(DB$redcap$metadata$field_type=="checkbox")]){
      x <- DB$redcap$metadata$select_choices_or_calculations[which(DB$redcap$metadata$field_name==field_name)] %>% split_choices()
      new_rows <- data.frame(
        field_name=paste0(field_name,"___",x$code),
        form_name=DB$redcap$metadata$form_name[which(DB$redcap$metadata$field_name==field_name)]  ,
        field_label=x$name,
        # field_label_full=paste0(DB$redcap$metadata$field_label[which(DB$redcap$metadata$field_name==field_name)]," - ",x$name),
        field_type="checkbox_choice",
        select_choices_or_calculations=c("0, Unchecked | 1, Checked")
      )
      row <- which(DB$redcap$metadata$field_name==field_name)
      last_row <- nrow(DB$redcap$metadata)
      top <- DB$redcap$metadata[1:row,]
      bottom <- NULL
      if(last_row>row){
        bottom <- DB$redcap$metadata[(row+1):last_row,]
      }
      DB$redcap$metadata <- top %>%
        dplyr::bind_rows(
          new_rows
        ) %>%  dplyr::bind_rows(
          bottom
        )
    }
  }
  if(any(DB$redcap$metadata$field_type=="yesno")){
    DB$redcap$metadata$select_choices_or_calculations[which(DB$redcap$metadata$field_type=="yesno")] <- c("0, No | 1, Yes")
  }
  # is longitudinal ------
  if(DB$redcap$is_longitudinal){
    DB$redcap$raw_structure_cols <- c(DB$redcap$raw_structure_cols,"arm_num","event_name") %>% unique()
    DB$redcap$arms <- get_redcap_info(DB,"arm")
    DB$redcap$has_arms <- T
    DB$redcap$has_multiple_arms <- nrow(DB$redcap$arms)>1
    DB$redcap$has_arms_that_matter <- DB$redcap$has_multiple_arms
    DB$redcap$event_mapping  <- get_redcap_info(DB,"formEventMapping","warn")
    DB$redcap$events <- get_redcap_info(DB,"event","warn")
    DB$redcap$events$forms <- DB$redcap$events$unique_event_name %>% sapply(function(events){
      DB$redcap$event_mapping$form[which(DB$redcap$event_mapping$unique_event_name==events)] %>% unique() %>% paste0(collapse = " | ")
    })
    if(DB$redcap$has_arms_that_matter){
      DB$redcap$has_arms_that_matter<- DB$redcap$arms$arm_num %>% lapply(function(arm){
        DB$redcap$event_mapping$form[which(DB$redcap$event_mapping$arm_num==arm)]
      }) %>% check_match() %>% magrittr::not()
    }
    # if(is.data.frame(DB$unique_events)){
    #   DB$redcap$events <- data.frame(
    #     event_name = unique(DB$unique_events$event_name),
    #     arms = unique(DB$unique_events$event_name) %>% sapply(function(event_name){
    #       DB$unique_events$arm_num[which(DB$unique_events$event_name==event_name)] %>% unique() %>% paste0(collapse = " | ")
    #     })
    #   )
    # }
    DB$redcap$instruments$repeating_via_events <- F
    DB$redcap$instruments$repeating_via_events[
      which(
        DB$redcap$instruments$instrument_name %>% sapply(function(instrument_name){
          # instrument_name <- DB$redcap$instruments$instrument_name %>% sample(1)
          anyDuplicated(DB$redcap$event_mapping$arm_num[which(DB$redcap$event_mapping$form==instrument_name)])>0
        })
      )
    ] <- T
  }else{
    DB$redcap$has_arms <- F
    DB$redcap$has_multiple_arms <- F
    DB$redcap$has_arms_that_matter <- F
    DB$redcap$event_mapping  <- NA
    DB$redcap$events <- NA
  }
  # other-------
  DB$redcap$users <- get_redcap_users(DB)
  DB$redcap$codebook <- metadata_to_codebook(DB$redcap$metadata)
  DB$redcap$log <- check_redcap_log(DB,last = 2,units = "mins")
  DB$redcap$users$current_user <- DB$redcap$users$username==DB$redcap$log$username[which(DB$redcap$log$details=="Export REDCap version (API)") %>% dplyr::first()]
  DB$links$redcap_home <- paste0(DB$links$redcap_base,"redcap_v",DB$redcap$version,"/index.php?pid=",DB$redcap$project_id)
  DB$links$redcap_record_home <- paste0(DB$links$redcap_base,"redcap_v",DB$redcap$version,"/DataEntry/record_home.php?pid=",DB$redcap$project_id)
  DB$links$redcap_record_subpage <- paste0(DB$links$redcap_base,"redcap_v",DB$redcap$version,"/DataEntry/index.php?pid=",DB$redcap$project_id)
  DB$links$redcap_records_dashboard <- paste0(DB$links$redcap_base,"redcap_v",DB$redcap$version,"/DataEntry/record_status_dashboard.php?pid=",DB$redcap$project_id)
  DB$links$redcap_API <- paste0(DB$links$redcap_base,"redcap_v",DB$redcap$version,"/API/project_api.php?pid=",DB$redcap$project_id)
  DB$links$redcap_API_playground <- paste0(DB$links$redcap_base,"redcap_v",DB$redcap$version,"/API/playground.php?pid=",DB$redcap$project_id)
  DB
}
get_redcap_structure <- function(DB, parse_codes = F){
  redcap <- NULL
  redcap$uri <- DB$links$redcap_uri
  redcap$version <- redcap_api_base(url = redcap$uri,token = validate_redcap_token(DB),"version") %>% httr::content(as="text") %>% as.character()
  redcap$project_info <- get_redcap_info(DB,"project")
  redcap$arms <- get_redcap_info(DB,"arm")
  redcap$events <- get_redcap_info(DB,"event","warn")
  redcap$event_mapping  <- get_redcap_info(DB,"formEventMapping","warn")
  redcap$instruments <- get_redcap_info(DB,"instrument","warn")
  redcap$repeating <- get_redcap_info(DB,"repeatingFormsEvents")
  redcap$metadata <- get_redcap_info(DB,"metadata","stop")
  if(parse_codes){
    T
  }
  return(redcap)
}
get_redcap_data <- function(DB,labelled=T,records=NULL){
  raw <- get_raw_redcap(
    DB = DB,
    labelled = F,
    records = records
  )# consider bug check that all records are included in data_list
  data_list <- raw_process_redcap(raw = raw, DB = DB, labelled = labelled)
  return(data_list)
}
get_redcap_users <- function(DB){
  userRole  <- get_redcap_info(DB,"userRole") %>% dplyr::select("unique_role_name","role_label")
  userRoleMapping <-  get_redcap_info(DB,"userRoleMapping")
  user <-  get_redcap_info(DB,"user")
  return(merge(merge(userRole,userRoleMapping,by="unique_role_name"),user, by="username"))
}
#' @title Check the REDCap log
#' @inheritParams save_DB
#' @param last numeric paired with units. Default is 24.
#' @param user optional user filter.
#' @param units character paired with last. Options are "mins","hours","days". Default is "hours".
#' @param begin_time character of time where the log should start from. Example 2023-07-11 13:15:06.
#' @param clean logical for cleaning of API data
#' @return data.frame of log that has been cleaned and has extra summary columns
#' @export
check_redcap_log <- function(DB,last=24,user = "",units="hours",begin_time="",clean = T,record = ""){
  if(units=="days"){
    x <- (Sys.time()-lubridate::days(last)) %>% format( "%Y-%m-%d %H:%M") %>% as.character()
  }
  if(units=="hours"){
    x <- (Sys.time()-lubridate::hours(last))%>% format( "%Y-%m-%d %H:%M") %>% as.character()
  }
  if(units=="mins"){
    x <- (Sys.time()- lubridate::minutes(last))%>% format( "%Y-%m-%d %H:%M") %>% as.character()
  }
  if(units=="years"){
    x <- (Sys.time()- lubridate::years(last))%>% format( "%Y-%m-%d %H:%M") %>% as.character()
  }
  if(begin_time!=""){
    x <- begin_time
  }
  log <- get_redcap_info(DB,"log",additional_args = list(beginTime=x,record=record,user = user))
  log <- log %>% clean_redcap_log(purge_api=clean)
  log
}
#' @title Check the REDCap log
#' @inheritParams save_DB
#' @param labelled T/F for clean vs raw labels
#' @param records optional records
#' @return data.frame of raw_redcap
#' @export
get_raw_redcap <- function(DB,labelled=T,records=NULL){
  if(missing(records)) records <- NULL
  raw <- REDCapR::redcap_read(redcap_uri=DB$links$redcap_uri, token=validate_redcap_token(DB),batch_size = 2000, interbatch_delay = 0.1,records = records, raw_or_label = ifelse(labelled,"label","raw"))$data %>% all_character_cols()
  return(raw)
}
#' @export
delete_redcap_records <- function(DB, records){
  BAD<-records[which(!records%in%DB$summary$all_records[[DB$redcap$id_col]])]
  if(length(BAD)>0)stop("Records not included in DB: ",records %>% paste0(collapse = ", "))
  for (record in records){
    httr::POST(
      url = DB$links$redcap_uri,
      body = list(
        "token"=validate_redcap_token(DB),
        content='record',
        action='delete',
        `records[0]`=record,
        returnFormat='json'
      ),
      encode = "form"
    ) %>% process_response(error_action = "warn")
  }
  message("Records deleted!")
}
