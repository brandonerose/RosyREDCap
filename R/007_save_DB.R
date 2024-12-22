#' @import RosyUtils
#' @import RosyApp
#' @title drop_DB_to_dir
#' @return messages for confirmation
#' @export
drop_DB_to_dir <- function(
    DB,
    include_metadata = T,
    forms,
    str_trunc_length = 32000,
    separate = F,
    file_name
) {
  DB <- validate_DB(DB)
  root_dir <- get_dir(DB)
  output_dir <- file.path(root_dir,"output")
  project_dir <- file.path(root_dir,DB$short_name)
  metadata_dir <- file.path(project_dir,"metadata")
  other_dir <- file.path(project_dir,"other")
  upload_dir <- file.path(project_dir,"upload")
  project_dir %>% dir.create(showWarnings = F)
  metadata_dir %>% dir.create(showWarnings = F)
  other_dir %>% dir.create(showWarnings = F)
  upload_dir %>% dir.create(showWarnings = F)
  if(include_metadata){
    for (x in names(DB$metadata)){ #,"log" #taking too long
      # print(x)
      if(is_something(DB$metadata[[x]])){
        if(is.data.frame(DB$metadata[[x]])){
          if(DB$internals$use_csv){
            list_to_csv(
              list = DB$metadata[x],
              dir = other_dir,
              file_name = x
            )
          }else{
            list_to_excel(
              list = DB$metadata[x],
              dir = metadata_dir,
              file_name = x,
              str_trunc_length = str_trunc_length,
              overwrite = TRUE
            )
          }
        }
      }
    }
  }
  to_save_list <- DB[["data"]]
  if(missing(file_name))file_name <- DB$short_name
  if(DB$internals$use_csv){
    list_to_csv(
      list = to_save_list,
      dir = project_dir,
      file_name = file_name
    )
  }else{
    list_to_excel(
      list = to_save_list,
      dir = project_dir,
      # link_col_list = link_col_list,
      file_name = file_name,
      separate = separate,
      # header_df_list = to_save_list %>% construct_header_list(metadata = DB$redcap$metadata),
      # key_cols_list = construct_key_col_list(DB,data_choice = "data"),
      str_trunc_length = str_trunc_length,
      overwrite = TRUE
    )
  }
  # if(merge_non_repeating) DB <- unmerge_non_repeating_DB(DB)
  return(DB)
}
#' @title Reads DB from the dropped REDCap files in dir/REDCap/upload
#' @inheritParams save_DB
#' @param allow_all logical TF for allowing DB$data names that are not also form names
#' @param drop_nonredcap_vars logical TF for dropping non-redcap variable names
#' @param drop_non_form_vars logical TF for dropping non-form variable names
#' @param stop_or_warn character string of whether to stop, warn, or do nothing when forbidden cols are present
#' @return messages for confirmation
#' @export
read_DB_from_dir <- function(DB,allow_all=T,drop_non_form_vars=T,stop_or_warn="warn"){
  DB <- validate_DB(DB)
  path <- file.path(get_dir(DB),DB$short_name,"upload")
  if(!file.exists(path))stop("No files found at path --> ",path)
  x <- list.files.real(path)
  DF <- data.frame(
    file_name = x,
    file_name_no_ext = gsub("\\.xlsx|\\.xls","",x),
    match = NA
  )
  DF$match <- strsplit(DF$file_name_no_ext,"_") %>% sapply(function(IN){IN[length(IN)]})
  DF$match[which(!DF$match%in%c(DB$internals$merge_form_name,DB$metadata$forms$form_name))] <- NA
  if(!allow_all){
    DF <- DF[which(!is.na(DF$match)),]
  }
  if(DB$data_update %>% is_something())stop("Already files in DB$data_update, clear that first")
  DB[["data_update"]] <- list()
  # for(i in 1:nrow(DF)){#not done yet
  #   the_file <- readxl::read_xlsx(file.path(path,DF$file_name[i]),col_types = "text") %>% all_character_cols() # would
  #   drop_cols <- NULL
  #   if(drop_nonredcap_vars){
  #     x <- colnames(the_file)[which(!colnames(the_file)%in%c(DB$redcap$raw_structure_cols,DB$redcap$metadata$field_name))]
  #     drop_cols<-drop_cols %>%
  #       append(x) %>%
  #       unique()
  #   }
  #   if(drop_non_form_vars){
  #     form <- DF$match[i]
  #     if(form == DB$internals$merge_form_name)form <- DB$redcap$forms$form_name[which(!DB$redcap$forms$repeating)]
  #     x<-colnames(the_file)[which(!colnames(the_file)%in%c(DB$redcap$raw_structure_cols,DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name%in%form)]))]
  #     redcapols<-drop_cols %>%
  #       append(x) %>%
  #       unique()
  #   }
  #   message1 <- paste0("forbidden cols name: ",DF$file_name[i],"; ",x %>% paste0(collapse = ", "))
  #   if(length(x)>0){
  #     if(stop_or_warn=="stop") stop(message1)
  #     if(stop_or_warn=="warn") warning(message1,immediate. = T)
  #   }
  #   the_file <- the_file[,which(!colnames(the_file)%in%drop_cols)]
  #   DB[["data_update"]][[DF$match[i]]] <- the_file
  # }
  DB
}
default_sheet_drops <- function(DB){
  DB$metadata  %>% process_df_list() %>% names()
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
