#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
redcap_api_request <- function(url,token,additional_args=NULL){
  body  <- list(
    "token"= token
  )
  if(!is.null(additional_args)){
    body <- body %>% append(additional_args)
  }
  httr::POST(
    url = url,
    body = body,
    encode = "form"
  )
}
process_redcap_response <- function(response,error_action="warn",method){
  bind <- T
  if(!missing(method)){
    if(method=="exp_rc_v"){
      content <- response %>% httr::content(as="text") %>% as.character()
      bind <- F
    }
  }
  if(bind)content <- httr::content(response) %>% dplyr::bind_rows() %>% all_character_cols()
  if(httr::http_error(response)){
    if(!error_action%in%c("stop","warn"))stop("error_action must be 'stop' or 'warn'")
    general_error <- response$status_code
    specific_error <- http_errors$Description[which(http_errors$Value==response$status_code)]
    message <- paste0("HTTP error ",general_error, ". ",specific_error,". ",content[["error"]])
    if(error_action=="stop")stop(message)
    warning(message,immediate. = T)
    if(!missing(method)){
      show_redcap_api_method_info(method)
    }
    return(NA)
  }
  return(content)
}
get_redcap_api_methods <- function(){
  REDCap_API$methods$method_short_name %>% sort()
}
show_redcap_api_method_info <- function(method){
  if(missing(method)){
    bullet_in_console("Missing method, so one will be chosen at random!",bullet_type = "x")
    method <- REDCap_API$methods$method_short_name %>% sample1()
  }
  if(!method%in%REDCap_API$methods$method_short_name)stop("Must use the following methods... ",as_comma_string(REDCap_API$methods$method_short_name))
  method_df <- REDCap_API$methods[which(REDCap_API$methods$method_short_name==method),]
  method_param_df <- REDCap_API$params[which(REDCap_API$params$method_short_name==method),]
  method_param_df_base <- method_param_df[which(!method_param_df$non_base),]
  method_param_df_req <- method_param_df[which(method_param_df$non_base_req_by_user),]
  method_param_df_opt <- method_param_df[which(method_param_df$non_base_opt_by_user),]
  bullet_in_console(paste0("REDCap API method '",method,"'..."))
  bullet_in_console("Help is here: ",url = method_df$help_url)
  bullet_in_console(paste0(method_df$description))
  if(nrow(method_param_df_base)>0){
    bullet_in_console(paste0("Default: ",method_param_df_base$Parameter %>% as_comma_string()),bullet_type = "v")
  }
  if(nrow(method_param_df_req)>0){
    x <- method_param_df_req$Parameter
    x[which(method_param_df_req$is_longitudinal_only)] <- paste0(x[which(method_param_df_req$is_longitudinal_only)]," (if longitudinal)")
    x[which(method_param_df_req$has_repeating_instruments_or_events_only)] <- paste0(x[which(method_param_df_req$has_repeating_instruments_or_events_only)]," (if has repeating)")
    bullet_in_console(paste0("Required by User: ", as_comma_string(x)),bullet_type = ">")
  }
  if(nrow(method_param_df_opt)>0){
    bullet_in_console(paste0("Optional by User: ",method_param_df_opt$Parameter %>% as_comma_string()),bullet_type = ">")
  }
}
run_redcap_api_method <- function(DB,url,token,method,error_action = "warn",additional_args=NULL,only_get = F){
  if(!missing(DB)){
    url <- DB$links$redcap_uri
    token <- validate_REDCap_token(DB)
  }
  allowed_methods <- REDCap_API$methods$method_short_name %>% sort()
  if(only_get){
    allowed_methods <- REDCap_API$methods$method_short_name[which(REDCap_API$methods$action_type=="export")] %>% sort()
  }
  if(!method%in%allowed_methods)stop("Can only use the following methods... ",as_comma_string(allowed_methods))
  method_param_df <- REDCap_API$params[which(REDCap_API$params$method_short_name==method),]
  method_param_df <- method_param_df[which(method_param_df$Parameter!="token"),]
  base_rows <- which(!method_param_df$non_base)
  if(!is.null(additional_args)){
    if("content"%in%names(additional_args))stop("Do not supply default-defined content additional_args... This is automatically tied to method!")
  }
  for(i in base_rows){
    if(!method_param_df$Parameter[i]%in%names(additional_args)){
      additional_args <- additional_args %>% append(
        setNames(method_param_df$default[i], method_param_df$Parameter[i]) %>% as.list()
      )
    }
  }
  redcap_api_request(url=url,token = token,additional_args=additional_args) %>% process_redcap_response(error_action=error_action,method=method) %>% return()
}
get_REDCap <- function(DB,method,error_action = "warn",additional_args=NULL){
  run_redcap_api_method(
    url = DB$links$redcap_uri,
    token = validate_REDCap_token(DB),
    method = method,
    additional_args = additional_args,
    error_action = error_action,
    only_get = T
  )
}
get_REDCap_version <- function(DB){
  return(
    get_REDCap(
      DB = DB,
      method = "exp_rc_v",
      error_action = "stop"
    )
  )
}
#' @title Drop redcap files to directory
#' @param original_file_names logical for using original uploaded filenames vs system defined
#' @param overwrite logical rewriting over the downloaded files in your directory. A better alternative is deleting files you want to update.
#' @return message
#' @export
get_REDCap_files <- function(DB,original_file_names = F,overwrite = F){
  file_rows <- which(DB$metadata$fields$field_type=="file")
  out_dir <- file.path(DB$dir_path,"REDCap","files")
  if(length(file_rows)>0){
    dir.create(out_dir,showWarnings = F)
    for(field_name in DB$metadata$fields$field_name[file_rows]){
      out_dir_folder <- file.path(out_dir,field_name)
      dir.create(out_dir_folder,showWarnings = F)
      form_name <- DB$metadata$fields$form_name[which(DB$metadata$fields$field_name == field_name)]
      is_repeating <- DB$metadata$forms$repeating[which(DB$metadata$forms$form_name==form_name)]
      form <- DB$data[[form_name]]
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
          REDCapR::redcap_file_download_oneshot(
            redcap_uri = DB$links$redcap_uri,
            token = validate_REDCap_token(DB),
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
get_REDCap_metadata <- function(DB,include_users = T){
  DB$internals$last_metadata_update <- Sys.time()
  DB$metadata <- list()
  # info ----------
  DB$redcap$project_info <- get_REDCap(DB,method = "exp_proj")
  DB$redcap$project_title <-  DB$redcap$project_info$project_title
  DB$redcap$project_id <- DB$redcap$project_info$project_id
  DB$redcap$is_longitudinal <- DB$redcap$project_info$is_longitudinal == "1"
  DB$metadata$missing_codes <- missing_codes2(DB)
  # instruments --------
  DB$metadata$forms <- get_REDCap(DB,method = "exp_instr") %>% rename_forms_redcap_to_default()
  DB$metadata$forms$repeating <- F
  DB$redcap$has_repeating_forms <- F
  DB$redcap$has_repeating_events <- F
  DB$redcap$has_repeating_forms_or_events <- DB$redcap$project_info$has_repeating_instruments_or_events=="1"
  # if(DB$redcap$project_info$has_repeating_instruments_or_events=="1")
  repeatingFormsEvents <- get_REDCap(DB,method = "exp_repeating_forms_events")
  if(is.data.frame(repeatingFormsEvents)){
    DB$metadata$forms$repeating <- DB$metadata$forms$form_name%in%repeatingFormsEvents$form_name
  }
  if(any(DB$metadata$forms$repeating)){
    DB$redcap$has_repeating_forms <- T
  }
  # metadata ----------
  DB$metadata$fields <- get_REDCap(DB,method = "exp_metadata",error_action = "stop")
  DB$metadata$fields$section_header <- DB$metadata$fields$section_header %>% remove_html_tags()
  DB$metadata$fields$field_label <- DB$metadata$fields$field_label %>% remove_html_tags()
  DB$redcap$id_col <- DB$metadata$fields[1,1] %>% as.character() #RISKY?
  DB$metadata$form_key_cols <- get_key_col_list(DB)
  DB$redcap$raw_structure_cols <- DB$metadata$form_key_cols %>% unlist() %>% unique()
  form_names <- DB$metadata$forms$form_name#[which(DB$metadata$forms$form_name%in%unique(DB$metadata$fields$form_name))]
  for (form_name in form_names){
    new_row <- data.frame(
      field_name = paste0(form_name,"_complete"),
      form_name = form_name,
      field_type = "radio",
      field_label = paste0(form_name,"_complete")  %>% strsplit("_") %>% unlist() %>% stringr::str_to_title() %>% paste0(collapse = " "),
      select_choices_or_calculations = "0, Incomplete | 1, Unverified | 2, Complete"
    )
    last_row <- nrow(DB$metadata$fields)
    rows <- which(DB$metadata$fields$form_name==form_name)
    if(length(rows)==0)rows <- last_row
    row <- dplyr::last(rows)
    top <- DB$metadata$fields[1:row,]
    bottom <- NULL
    if(last_row>row){
      bottom <- DB$metadata$fields[(row+1):last_row,]
    }
    DB$metadata$fields <- top %>%
      dplyr::bind_rows(
        new_row
      ) %>%  dplyr::bind_rows(
        bottom
      )
  }
  if(any(DB$metadata$fields$field_type=="checkbox")){
    for(field_name in DB$metadata$fields$field_name[which(DB$metadata$fields$field_type=="checkbox")]){
      x <- DB$metadata$fields$select_choices_or_calculations[which(DB$metadata$fields$field_name==field_name)] %>% split_choices()
      new_rows <- data.frame(
        field_name=paste0(field_name,"___",x$code),
        form_name=DB$metadata$fields$form_name[which(DB$metadata$fields$field_name==field_name)]  ,
        field_label=x$name,
        # field_label_full=paste0(DB$metadata$fields$field_label[which(DB$metadata$fields$field_name==field_name)]," - ",x$name),
        field_type="checkbox_choice",
        select_choices_or_calculations=c("0, Unchecked | 1, Checked")
      )
      row <- which(DB$metadata$fields$field_name==field_name)
      last_row <- nrow(DB$metadata$fields)
      top <- DB$metadata$fields[1:row,]
      bottom <- NULL
      if(last_row>row){
        bottom <- DB$metadata$fields[(row+1):last_row,]
      }
      DB$metadata$fields <- top %>%
        dplyr::bind_rows(
          new_rows
        ) %>%  dplyr::bind_rows(
          bottom
        )
    }
  }
  if(any(DB$metadata$fields$field_type=="yesno")){
    DB$metadata$fields$select_choices_or_calculations[which(DB$metadata$fields$field_type=="yesno")] <- c("0, No | 1, Yes")
  }
  DB$metadata$fields <- DB %>% annotate_fields(summarize_data = F)
  DB$metadata$choices <- fields_to_choices(fields = DB$metadata$fields)
  # is longitudinal ------
  if(DB$redcap$is_longitudinal){
    DB$redcap$raw_structure_cols <- c(DB$redcap$raw_structure_cols,"arm_num","event_name") %>% unique()
    DB$metadata$arms <- get_REDCap(DB,method = "exp_arms")
    DB$redcap$has_arms <- T
    DB$redcap$has_multiple_arms <- nrow(DB$metadata$arms)>1
    DB$redcap$has_arms_that_matter <- DB$redcap$has_multiple_arms
    DB$metadata$event_mapping  <- get_REDCap(DB,method = "exp_inst_event_maps")
    DB$metadata$events <- get_REDCap(DB,method = "exp_events")
    DB$metadata$events$repeating <- F
    DB$metadata$event_mapping$repeating <- F
    if(is.data.frame(repeatingFormsEvents)){
      DB$metadata$events$repeating <- DB$metadata$events$unique_event_name%in%repeatingFormsEvents$event_name[which(is.na(repeatingFormsEvents$form_name))]
      repeatingFormsEvents_ind <- repeatingFormsEvents[which(!is.na(repeatingFormsEvents$event_name)&!is.na(repeatingFormsEvents$form_name)),]
      if(nrow(repeatingFormsEvents_ind)>0){
        rows_event_mapping <- 1:nrow(repeatingFormsEvents_ind) %>% sapply(function(i){ which(DB$metadata$event_mapping$unique_event_name == repeatingFormsEvents_ind$event_name[i] & DB$metadata$event_mapping$form == repeatingFormsEvents_ind$form_name[i])})
        DB$metadata$event_mapping$repeating[rows_event_mapping] <- T
      }
    }
    DB$metadata$events$forms <- DB$metadata$events$unique_event_name %>% sapply(function(events){
      DB$metadata$event_mapping$form[which(DB$metadata$event_mapping$unique_event_name==events)] %>% unique() %>% paste0(collapse = " | ")
    })
    if(DB$redcap$has_arms_that_matter){
      DB$redcap$has_arms_that_matter<- DB$metadata$arms$arm_num %>% lapply(function(arm){
        DB$metadata$event_mapping$form[which(DB$metadata$event_mapping$arm_num==arm)]
      }) %>% check_match() %>% magrittr::not()
    }
    # if(is.data.frame(DB$unique_events)){
    #   DB$metadata$events <- data.frame(
    #     event_name = unique(DB$unique_events$event_name),
    #     arms = unique(DB$unique_events$event_name) %>% sapply(function(event_name){
    #       DB$unique_events$arm_num[which(DB$unique_events$event_name==event_name)] %>% unique() %>% paste0(collapse = " | ")
    #     })
    #   )
    # }
    DB$metadata$forms$repeating_via_events <- F
    DB$metadata$forms$repeating_via_events[
      which(
        DB$metadata$forms$form_name %>% sapply(function(form_name){
          # form_name <- forms$form_name %>% sample(1)
          anyDuplicated(DB$metadata$event_mapping$arm_num[which(DB$metadata$event_mapping$form==form_name)])>0
        })
      )
    ] <- T
  }else{
    DB$redcap$has_arms <- F
    DB$redcap$has_multiple_arms <- F
    DB$redcap$has_arms_that_matter <- F
    DB$metadata$event_mapping  <- NA
    DB$metadata$events <- NA
  }
  # other-------
  if(include_users){
    DB$redcap$users <- get_REDCap_users(DB)
    DB$redcap$log <- check_REDCap_log(DB,last = 2,units = "mins")
    DB$redcap$users$current_user <- DB$redcap$users$username==DB$redcap$log$username[which(DB$redcap$log$details=="Export REDCap version (API)") %>% dplyr::first()]
  }
  DB$links$redcap_home <- paste0(DB$links$redcap_base,"redcap_v",DB$redcap$version,"/index.php?pid=",DB$redcap$project_id)
  DB$links$redcap_record_home <- paste0(DB$links$redcap_base,"redcap_v",DB$redcap$version,"/DataEntry/record_home.php?pid=",DB$redcap$project_id)
  DB$links$redcap_record_subpage <- paste0(DB$links$redcap_base,"redcap_v",DB$redcap$version,"/DataEntry/index.php?pid=",DB$redcap$project_id)
  DB$links$redcap_records_dashboard <- paste0(DB$links$redcap_base,"redcap_v",DB$redcap$version,"/DataEntry/record_status_dashboard.php?pid=",DB$redcap$project_id)
  DB$links$redcap_API <- paste0(DB$links$redcap_base,"redcap_v",DB$redcap$version,"/API/project_api.php?pid=",DB$redcap$project_id)
  DB$links$redcap_API_playground <- paste0(DB$links$redcap_base,"redcap_v",DB$redcap$version,"/API/playground.php?pid=",DB$redcap$project_id)
  return(DB)
}
get_REDCap_data <- function(DB,labelled=T,records=NULL,batch_size=2000){
  forms <- get_original_forms(DB)
  data_list <- list()
  raw <- get_raw_REDCap(
    DB = DB,
    labelled = F,
    records = records,
    batch_size = batch_size
  )
  data_list <- raw %>% raw_process_redcap(DB=DB,labelled=labelled)
  return(data_list)
}
get_REDCap_users <- function(DB){
  users  <- get_REDCap(DB,method = "exp_users")
  userRole  <- get_REDCap(DB,method = "exp_user_roles") %>% dplyr::select("unique_role_name","role_label")
  userRoleMapping <-  get_REDCap(DB,method = "exp_user_role_maps")
  final <- merge(merge(userRole,userRoleMapping,by="unique_role_name"),users, by="username",all.y = T)
  return(final)
}
get_REDCap_structure <- function(){
}
rename_forms_redcap_to_default <- function(forms){
  the_names <- colnames(forms)
  the_names[which(the_names=="instrument_name")] <- "form_name"
  the_names[which(the_names=="instrument_label")] <- "form_label"
  colnames(forms) <- the_names
  return(forms)
}
rename_forms_default_to_redcap <- function(forms){
  the_names <- colnames(forms)
  the_names[which(the_names=="form_name")] <- "instrument_name"
  the_names[which(the_names=="form_label")] <- "instrument_label"
  colnames(forms) <- the_names
  return(forms)
}
#' @title Check the REDCap log
#' @param last numeric paired with units. Default is 24.
#' @param user optional user filter.
#' @param units character paired with last. Options are "mins","hours","days". Default is "hours".
#' @param begin_time character of time where the log should start from. Example 2023-07-11 13:15:06.
#' @param clean logical for cleaning of API data
#' @return data.frame of log that has been cleaned and has extra summary columns
#' @export
check_REDCap_log <- function(DB,last=24,user = "",units="hours",begin_time="",clean = T,record = ""){
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
  log <- get_REDCap(DB,"exp_logging",additional_args = list(beginTime=x,record=record,user = user))
  if(is.data.frame(log)){
    log <- log %>% clean_redcap_log(purge_api=clean)
  }
  log # deal with if NA if user does not have log privelages.
}
#' @title Check the REDCap log
#' @param labelled T/F for clean vs raw labels
#' @param records optional records
#' @return data.frame of raw_redcap
#' @export
get_raw_REDCap <- function(DB,labelled=F,records=NULL,batch_size = 1000){
  raw <- REDCapR::redcap_read(
    redcap_uri = DB$links$redcap_uri,
    token = validate_REDCap_token(DB),
    # forms = forms,
    # events = events,
    batch_size = batch_size,
    interbatch_delay = 0.1,
    records = records,
    raw_or_label = ifelse(labelled, "label", "raw")
  )$data %>% all_character_cols()
  return(raw)
}
#' @title Check the REDCap log
#' @export
delete_redcap_records <- function(DB, records){
  BAD<-records[which(!records%in%DB$summary$all_records[[DB$redcap$id_col]])]
  if(length(BAD)>0)stop("Records not included in DB: ",records %>% paste0(collapse = ", "))
  for (record in records){
    httr::POST(
      url = DB$links$redcap_uri,
      body = list(
        "token"=validate_REDCap_token(DB),
        content='record',
        action='delete',
        `records[0]`=record,
        returnFormat='json'
      ),
      encode = "form"
    ) %>% process_redcap_response(error_action = "warn")
  }
  message("Records deleted!")
}
#' @title push_redcap_dictionary
push_redcap_dictionary <- function(){
  redcap_api_request(
    url,
    token,
    additional_args=NULL
  )
  return()
}
