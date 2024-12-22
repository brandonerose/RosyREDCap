#' @import RosyUtils
#' @import RosyApp
#' @title Shows DB in the env
#' @param records character vector of records you want dropped to your directory
#' @param allow_mod logical for whether non-instrument names are allowed
#' @param deidentify logical for deidentification
#' @param dir_other optional character string of another file path where the files should be saved
#' @param smart logical for whether to only save when data is new
#' @param include_metadata logical for whether to only include redcap and not metadata
#' @param include_other logical for whether to only include redcap and not metadata
#' @param file_name optional character string for adding to the front of file names
#' @param str_trunc_length optional integer for truncation
#' @param with_links optional logical for including links in excel sheets
#' @param merge_non_repeating optional logical for merging non-repeating instruments
#' @param forms optional character vector for only selected forms
#' @return messages for confirmation
#' @export
drop_REDCap_to_directory <- function(
    DB,
    records,
    smart = T,
    include_metadata = T,
    include_other = T,
    with_links = T,
    forms,
    merge_non_repeating = T,
    separate = F,
    str_trunc_length = 32000,
    file_name
) {
  DB <- validate_DB(DB)
  root_dir <- get_dir(DB)
  output_dir <- file.path(root_dir,"output")
  redcap_dir <- file.path(root_dir,"REDCap")
  redcap_metadata_dir <- file.path(redcap_dir,"metadata")
  redcap_other_dir <- file.path(redcap_dir,"other")
  redcap_upload_dir <- file.path(redcap_dir,"upload")
  due_for_save_metadata <- T
  due_for_save_data <- T
  if(smart){
    if(!is.null(DB$internals$last_metadata_dir_save)) due_for_save_metadata <- DB$internals$last_metadata_update > DB$internals$last_metadata_dir_save
    if(!is.null(DB$internals$last_data_dir_save)) due_for_save_data <- DB$internals$last_data_update > DB$internals$last_data_dir_save
  }
  redcap_dir %>% dir.create(showWarnings = F)
  redcap_metadata_dir %>% dir.create(showWarnings = F)
  redcap_other_dir %>% dir.create(showWarnings = F)
  redcap_upload_dir %>% dir.create(showWarnings = F)
  if(due_for_save_metadata){
    if(include_metadata){
      DB$internals$last_metadata_dir_save <- DB$internals$last_metadata_update
      names_generic <- c(
        "forms",
        "fields",
        "choices",
        "arms",
        "events",
        "event_mapping",
        "missing_codes"
      )
      names_redcap <- c(
        "instruments",
        "metadata",
        "codebook",
        "arms",
        "events",
        "event_mapping",
        "missing_codes"
      )
      for (i in 1:length(names_generic)){ #,"log" #taking too long
        z<- DB$metadata[names_generic[i]]
        if(is_something(z[[1]])){
          tn <- names_redcap[i]
          if(DB$internals$use_csv){
            list_to_csv(
              list = z,
              dir = redcap_metadata_dir,
              file_name = tn
            )
          }else{
            list_to_excel(
              list = z,
              dir = redcap_metadata_dir,
              file_name = tn,
              str_trunc_length = str_trunc_length,
              overwrite = TRUE
            )
          }
        }
      }
    }
    if(include_other){
      for (i in 1:length(names_generic)){ #,"log" #taking too long
        z<- DB$metadata[names_generic[i]]
        if(is_something(z[[1]])){
          tn <- names_redcap[i]
          if(DB$internals$use_csv){
            list_to_csv(
              list = z,
              dir = redcap_metadata_dir,
              file_name = tn
            )
          }else{
            list_to_excel(
              list = z,
              dir = redcap_metadata_dir,
              file_name = tn,
              str_trunc_length = str_trunc_length,
              overwrite = TRUE
            )
          }
        }
      }
      for (x in c("project_info",
                  #"log",
                  "users")){ #,"log" #taking too long
        if(DB$internals$use_csv){
          list_to_csv(
            list = DB$redcap[x],
            dir = redcap_other_dir,
            file_name = x
          )
        }else{
          list_to_excel(
            list = DB$redcap[x],
            dir = redcap_other_dir,
            file_name = x,
            str_trunc_length = str_trunc_length,
            overwrite = TRUE
          )
        }
      }
    }
  }
  if(due_for_save_data){
    DB$internals$last_data_dir_save <- DB$internals$last_data_update
    # if(merge_non_repeating) DB <- merge_non_repeating_DB(DB)
    to_save_list <- DB[["data"]]
    if(!missing(records)) to_save_list<- filter_DB(DB,filter_field = DB$redcap$id_col,filter_choices = records) %>% process_df_list()
    link_col_list <- list()
    if(with_links){
      to_save_list <-to_save_list %>% lapply(function(DF){add_redcap_links_to_DF(DF,DB)})
      link_col_list <- list(
        "redcap_link"
      )
      names(link_col_list) <- DB$redcap$id_col
    }
    if(missing(file_name))file_name <- DB$short_name
    if(DB$internals$use_csv){
      list_to_csv(
        list = to_save_list,
        dir = redcap_dir,
        file_name = file_name
      )
    }else{
      list_to_excel(
        list = to_save_list,
        dir = redcap_dir,
        link_col_list = link_col_list,
        file_name = file_name,
        separate = separate,
        # header_df_list = to_save_list %>% construct_header_list(metadata = DB$metadata$fields),
        # key_cols_list = construct_key_col_list(DB,data_choice = "data"),
        str_trunc_length = str_trunc_length,
        overwrite = TRUE
      )
    }
    # if(merge_non_repeating) DB <- unmerge_non_repeating_DB(DB)
  }
  return(DB)
}
#' @title Reads DB from the dropped REDCap files in dir/REDCap/upload
#' @param allow_all logical TF for allowing DB$data names that are not also form names
#' @param drop_nonredcap_vars logical TF for dropping non-redcap variable names
#' @param drop_non_form_vars logical TF for dropping non-form variable names
#' @param stop_or_warn character string of whether to stop, warn, or do nothing when forbidden cols are present
#' @return messages for confirmation
#' @export
read_from_REDCap_upload <- function(DB,allow_all=T,drop_nonredcap_vars=T,drop_non_form_vars=T,stop_or_warn="warn"){
  DB <- validate_DB(DB)
  path <- file.path(get_dir(DB),"REDCap","upload")
  if(!file.exists(path))stop("No REDCap files found at path --> ",path)
  x <- list.files.real(path) %>% basename()
  df <- data.frame(
    file_name = basename(x),
    file_name_no_ext = gsub("\\.xlsx|\\.xls","",x),
    match = NA
  )
  df$match <- strsplit(df$file_name_no_ext,"_") %>% sapply(function(IN){IN[length(IN)]})
  df$match[which(!df$match%in%c(DB$internals$merge_form_name,DB$metadata$forms$form_name))] <- NA
  if(!allow_all){
    df <- df[which(!is.na(df$match)),]
  }
  if(DB$data_update %>% is_something())stop("Already files in DB$data_update, clear that first")
  DB[["data_update"]] <- list()
  for(i in 1:nrow(df)){#not done yet
    the_file <- readxl::read_xlsx(file.path(path,df$file_name[i]),col_types = "text") %>% all_character_cols() # would
    drop_cols <- NULL
    if(drop_nonredcap_vars){
      x <- colnames(the_file)[which(!colnames(the_file)%in%c(DB$redcap$raw_structure_cols,DB$metadata$fields$field_name))]
      drop_cols<-drop_cols %>%
        append(x) %>%
        unique()
    }
    if(drop_non_form_vars){
      form <- df$match[i]
      if(form == DB$internals$merge_form_name)form <- DB$metadata$forms$form_name[which(!DB$metadata$forms$repeating)]
      x<-colnames(the_file)[which(!colnames(the_file)%in%c(DB$redcap$raw_structure_cols,DB$metadata$fields$field_name[which(DB$metadata$fields$form_name%in%form)]))]
      drop_cols<-drop_cols %>%
        append(x) %>%
        unique()
    }
    message1 <- paste0("forbidden cols name: ",df$file_name[i],"; ",x %>% paste0(collapse = ", "))
    if(length(x)>0){
      if(stop_or_warn=="stop") stop(message1)
      if(stop_or_warn=="warn") warning(message1,immediate. = T)
    }
    the_file <- the_file[,which(!colnames(the_file)%in%drop_cols)]
    DB[["data_update"]][[df$match[i]]] <- the_file
  }
  DB
}
default_sheet_drops <- function(DB){
  DB$summary  %>% process_df_list() %>% names()
}
read_xl_to_DB_for_upload <- function(DB,file_path,drop_sheets = default_sheet_drops(DB)){
  #add data_update check
  if(!endsWith(file_path,".xlsx"))stop("File type must be '.xlsx' --> ",file_path)
  if(!file.exists(file_path))stop("Path does not exist --> ",file_path)
  data_list <- file_path %>% openxlsx::loadWorkbook() %>% wb_to_list()
  if(is_something(drop_sheets)){
    message("dropping sheets from `drop_sheets` (Default is names from DB$summary)... ",paste0(drop_sheets, collapse = ", "))
    for(drop_sheet in drop_sheets){
      data_list[[drop_sheet]] <- NULL
    }
  }
  if(length(data_list)==0) {
    message("nothing to return")
    return(DB)
  }
  DB$data_update <- data_list
  return(DB)
}
