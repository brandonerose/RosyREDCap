#' @import rosyutils
#' @title Shows DB in the env
#' @inheritParams save_DB
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
drop_redcap_dir <- function(
    DB,
    records,
    smart = T,
    include_metadata = T,
    include_other = T,
    with_links = F,
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
      for (x in c("project_info","metadata","instruments","codebook")){ #,"log" #taking too long
        list_to_excel(
          list = DB$redcap[x],
          dir = redcap_metadata_dir,
          file_name = x,
          str_trunc_length = str_trunc_length,
          overwrite = TRUE
        )
      }
    }
    if(include_other){
      for (x in c("log","users")){ #,"log" #taking too long
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
  if(due_for_save_data){
    DB$internals$last_data_dir_save <- DB$internals$last_data_update
    # if(merge_non_repeating) DB <- merge_non_repeating_DB(DB)
    to_save_list <- DB[["data_extract"]]
    if(!missing(records)) to_save_list<- filter_DB(DB,records = records)
    link_col_list <- list()
    if(with_links){
      to_save_list <-to_save_list %>% lapply(function(DF){add_redcap_links_to_DF(DF,DB)})
      link_col_list <- list(
        "redcap_link"
      )
      names(link_col_list) <- DB$redcap$id_col
    }
    if(missing(file_name))file_name <- DB$short_name
    list_to_excel(
      list = to_save_list,
      dir = redcap_dir,
      link_col_list = link_col_list,
      file_name = file_name,
      separate = separate,
      # header_df_list = to_save_list %>% construct_header_list(metadata = DB$redcap$metadata),
      # key_cols_list = construct_key_col_list(DB,data_choice = "data_extract"),
      str_trunc_length = str_trunc_length,
      overwrite = TRUE
    )
    # if(merge_non_repeating) DB <- unmerge_non_repeating_DB(DB)
  }
  return(DB)
}
#' @title Reads DB from the dropped REDCap files in dir/REDCap/upload
#' @inheritParams save_DB
#' @param allow_all logical TF for allowing DB$data_extract names that are not also instrument names
#' @param drop_nonredcap_vars logical TF for dropping non-redcap variable names
#' @param drop_non_instrument_vars logical TF for dropping non-instrument variable names
#' @param stop_or_warn character string of whether to stop, warn, or do nothing when forbidden cols are present
#' @return messages for confirmation
#' @export
read_redcap_dir <- function(DB,allow_all=T,drop_nonredcap_vars=T,drop_non_instrument_vars=T,stop_or_warn="warn"){
  DB <- validate_DB(DB)
  path <- file.path(get_dir(DB),"REDCap","upload")
  if(!file.exists(path))stop("No REDCap files found at path --> ",path)
  x <- list.files.real(path)
  df <- data.frame(
    file_name = x,
    file_name_no_ext = gsub("\\.xlsx|\\.xls","",x),
    match = NA
  )
  df$match <- strsplit(df$file_name_no_ext,"_") %>% sapply(function(IN){IN[length(IN)]})
  df$match[which(!df$match%in%c(DB$internals$merge_form_name,DB$redcap$instruments$instrument_name))] <- NA
  if(!allow_all){
    df <- df[which(!is.na(df$match)),]
  }
  if(DB$data_upload %>% is_something())stop("Already files in DB$data_upload, clear that first")
  DB[["data_upload"]] <- list()
  for(i in 1:nrow(df)){#not done yet
    the_file <- readxl::read_xlsx(file.path(path,df$file_name[i]),col_types = "text") %>% rosyutils::all_character_cols() # would
    drop_cols <- NULL
    if(drop_nonredcap_vars){
      x <- colnames(the_file)[which(!colnames(the_file)%in%c(DB$redcap$raw_structure_cols,DB$redcap$metadata$field_name))]
      drop_cols<-drop_cols %>%
        append(x) %>%
        unique()
    }
    if(drop_non_instrument_vars){
      form <- df$match[i]
      if(form == DB$internals$merge_form_name)form <- DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating)]
      x<-colnames(the_file)[which(!colnames(the_file)%in%c(DB$redcap$raw_structure_cols,DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name%in%form)]))]
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
    DB[["data_upload"]][[df$match[i]]] <- the_file
  }
  DB
}
default_sheet_drops <- function(DB){
  DB$summary  %>% rosyutils:::process_df_list() %>% names()
}
read_xl_to_DB_for_upload <- function(DB,file_path,drop_sheets = default_sheet_drops(DB)){
  #add data_upload check
  file_path <-file.path(sarcoma_path,"sarcoma_NGS_survival_paper","PSDB_NGS_Paper_missing_important_brose.xlsx")
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
  DB$data_upload <- data_list
  return(DB)
}
