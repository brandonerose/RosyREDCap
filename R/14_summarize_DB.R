#' @import RosyUtils
sum_records <- function(DB,data_choice = "data_extract"){
  records <- NULL
  if(DB[[data_choice]] %>% is_something()){
    cols <- DB$redcap$id_col
    if(is.data.frame(DB$redcap$arms)){
      if(nrow(DB$redcap$arms)>1){
        cols <- DB$redcap$id_col %>% append("arm_num")
      }
    }
    if(length(cols)==1){
      records <- data.frame(
        records =  names(DB[[data_choice]]) %>% lapply(function(IN){DB[[data_choice]][[IN]][,cols]}) %>% unlist() %>% unique()
      )
      colnames(records) <- cols
    }
    if(length(cols) == 2){
      records <- names(DB[[data_choice]]) %>% lapply(function(IN){DB[[data_choice]][[IN]][,cols]}) %>% dplyr::bind_rows() %>% unique()
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
#' @title Summarize DB
#' @inheritParams save_DB
#' @param records character vector of records to be summarized
#' @param drop_blanks optional logical for dropping blanks
#' @export
summarize_DB <- function(DB,records = NULL,drop_blanks = T){
  #project --------
  # DB$summary$users <- DB$redcap$users
  df_names0 <- df_names1 <- df_names2 <- c("metadata","instruments","event_mapping","events","arms")
  data_choice <- "data_extract"
  if(DB$internals$was_remapped){
    df_names2 <- c(paste0(df_names1,"_new"),paste0(df_names1,"_remap"))
    df_names1 <- c(df_names1,paste0(df_names1,"_remap"))
    data_choice <- "data_transform"
  }
  if(!is.null(records)) DB[[data_choice]] <- DB %>% filter_DB(records = records,data_choice = data_choice)
  for(i in 1:length(df_names1)){
    x <- DB[[DB$internals$reference_metadata]][[df_names2[i]]]
    if(!is.null(x)) DB$summary[[df_names1[i]]] <- x
  }
  #records belong to arms 1 to 1 ----------
  DB$summary$all_records_n <- 0
  if(!is.null(DB$summary$all_records)){
    if(!is.null(records)){
      DB$summary$selected_records <- DB$summary$all_records[which( DB$summary$all_records[[DB$redcap$id_col]]%in% records),]
      DB$summary$selected_records_n <- DB$summary$selected_records %>% nrow()
    }
    DB$summary$all_records_n <- DB$summary$all_records %>% nrow()
    DB$summary$record_log_sum <- summarize_records_from_log(DB, records = records)
  }
  DB$summary$user_log_sum <- summarize_users_from_log(DB, records = records)
  #arms----------------
  DB$summary$arms_n <- 0
  if(is.data.frame(DB$redcap$arms)){
    DB$summary$arms_n <- DB$redcap$arms %>% nrow()
    id_pairs <- DB$redcap$instruments$instrument_name %>%  lapply(function(IN){DB$data_extract[[IN]][,c(DB$redcap$id_col,"arm_num")]}) %>% dplyr::bind_rows() %>% unique()
    DB$redcap$arms$arm_records_n <- DB$redcap$arms$arm_num %>% sapply(function(arm){
      which(id_pairs$arm_num==arm)%>% length()
    })
  }
  #events belong to arms many to 1 ----------------
  # DB$summary$events_n <- DB$redcap$events %>% nrow()
  DB$summary$events_n <- 0
  if(is.data.frame(DB$redcap$events)){
    DB$summary$events_n <- DB$redcap$events %>% nrow()
    DB$summary$event_names_n <- DB$redcap$events$event_name %>% unique() %>% length()
    # 1:nrow(DB$redcap$event_mapping) %>% lapply(function(i){
    #   (DB$data_extract[[DB$redcap$event_mapping$form[i]]][['redcap_event_name']]==DB$redcap$event_mapping$unique_event_name[i]) %>% which() %>% length()
    # })
    # for(event in ){
    #   DB$summary[[paste0(event,"_records_n")]] <- DB$data_extract[[]][which(DB$redcap$arms$arm_num==arm)]
    # }
  }
  #instruments/forms belong to events many to 1 (if no events/arms) ----------------
  DB$summary$instruments_n <- 0
  if(is.data.frame(DB$summary$instruments)){ # can add expected later
    DB$summary$instruments_n <- DB$summary$instruments %>% nrow()
    DB$summary$instruments <- DB  %>% annotate_instruments(DB$summary$instruments)
  }
  #fields belong to instruments/forms 1 to 1 ----------------
  DB$summary$metadata_n <- 0
  DB$summary$metadata_n <- DB$redcap$metadata[which(!DB$redcap$metadata$field_type%in%c("checkbox_choice","descriptive")),] %>% nrow()
  # DB$redcap$metadata$field_type[which(!DB$redcap$metadata$field_type%in%c("checkbox_choice","descriptive"))] %>% table()
  DB$summary$metadata <- DB %>%  annotate_metadata(metadata = DB$summary$metadata,data_choice = ifelse(DB$internals$was_remapped,"data_transform","data_extract"))
  #metadata/codebook =============
  codebook <- metadata_to_codebook(DB$summary$metadata) %>% annotate_codebook(DB$summary$metadata,data_choice = "data_transform",DB = DB)
  if(drop_blanks) codebook <- codebook[which(codebook$n>0),]
  DB$summary$codebook <- codebook
  #cross_codebook ------
  return(DB)
}
#' @export
rmarkdown_DB <- function (DB,dir_other){
  if(missing(dir_other)){
    dir <- get_dir(DB) %>% file.path("output")
  }else{
    dir  <- dir_other
  }
  filename <- paste0(DB$short_name,"_full_summary_",gsub("-","_",Sys.Date()),".pdf")
  rmarkdown::render(
    input = system.file("rmarkdown","pdf.Rmd",package = pkg_name),
    output_format = "pdf_document",
    output_file = dir %>% file.path(filename),
    output_dir = dir,
    quiet = F
  )
}
#' @export
save_summary <- function(DB,with_links=T,dir_other = file.path(DB$dir_path,"output"),file_name = paste0(DB$short_name,"_RosyREDCap"),separate = F, csv = F){
  DB <- DB %>% validate_DB()
  to_save_list <- append(DB[["data_transform"]],DB[["summary"]])
  to_save_list <- to_save_list[which(to_save_list %>% sapply(is.data.frame))]
  to_save_list <- to_save_list[which((to_save_list %>% sapply(nrow) %>% unlist())>0)]
  link_col_list <- list()
  if(with_links){
    to_save_list <-to_save_list %>% lapply(function(DF){add_redcap_links_to_DF(DF,DB)})
    link_col_list <- list(
      "redcap_link"
    )
    names(link_col_list) <- DB$redcap$id_col
  }
  if(csv){
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
      header_df_list = to_save_list %>% construct_header_list(),
      key_cols_list = construct_key_col_list(DB),
      overwrite = TRUE
    )
  }
}
#' @export
stack_vars <- function(DB,vars,new_name,drop_na=T){
  DB <- validate_DB(DB)
  metadata <- DB$redcap$metadata
  if(DB$internals$was_remapped){
    metadata <- DB$remap$metadata_remap
  }
  if(!all(vars%in%metadata$field_name))stop("all vars must be in metadata.")
  the_stack <- NULL
  for(var in vars){# var <- vars %>% sample1()
    DF <- filter_DB(DB,field_names = var)[[1]]
    colnames(DF)[which(colnames(DF)==var)] <- new_name
    the_stack <-the_stack %>% dplyr::bind_rows(DF)
  }
  if(drop_na){
    the_stack <- the_stack[which(!is.na(the_stack[[new_name]])),]
  }
  return(the_stack)
}
get_default_data_choice <- function(DB){ifelse(!DB$internals$was_remapped,"data_extract","data_transform")}
get_default_metadata <- function(DB){
  if(DB$internals$was_remapped){
    return(DB$remap$metadata_new)
  }else{
    return(DB$redcap$metadata)
  }
}
get_default_instruments <- function(DB){
  if(DB$internals$was_remapped){
    return(DB$remap$instruments_new)
  }else{
    return(DB$redcap$metadata)
  }
}
#' @export
get_all_field_names <- function(DB,data_choice){
  if(missing(data_choice))data_choice <- get_default_data_choice(DB)
  return(DB[[data_choice]] %>% sapply(colnames) %>% unlist() %>% unique())
}
