#' @import RosyUtils
add_redcap_links_to_DF <- function(DF,DB){# add instance links
  if(DB$redcap$id_col%in%colnames(DF)){
    DF_structure_cols <- DB$redcap$raw_structure_cols[which(DB$redcap$raw_structure_cols%in%colnames(DF))]
    DF_structure_cols <- DB$redcap$raw_structure_cols[which(DB$redcap$raw_structure_cols%in%colnames(DF)&DB$redcap$raw_structure_cols!=DB$redcap$id_col)]
    link_head <- DB$links$redcap_record_home
    link_tail <- "&id=" %>% paste0(DF[[DB$redcap$id_col]])
    if("redcap_repeat_instrument"%in%DF_structure_cols){
      link_head <- DB$links$redcap_record_subpage
      link_tail <- link_tail %>% paste0("&page=",DF[["redcap_repeat_instrument"]])
    }
    if("redcap_repeat_instance"%in%DF_structure_cols){
      link_head <- DB$links$redcap_record_subpage
      link_tail <- link_tail %>% paste0("&instance=",DF[["redcap_repeat_instance"]])
    }
    DF$redcap_link <- paste0(link_head,link_tail)
    if("arm_num"%in%colnames(DF)){
      DF$redcap_link <- DF$redcap_link %>% paste0("&arm=", DF[["arm_num"]])
    }
  }
  return(DF)
}
count_DB_upload_cells <- function(DB){
  DB$data_update %>% lapply(function(x){nrow(x)*ncol(x)}) %>% unlist() %>% sum()
}
husk_of_instrument <- function (DB,FORM,field_names) {
  DF <- DB$data[[FORM]]
  cols<- colnames(DF)[which(colnames(DF)%in%DB$redcap$raw_structure_cols)]
  DF2 <- NULL
  for(col in cols){
    DF2[[col]] <- DF[[col]]
  }
  DF2 <-as.data.frame(DF2)
  return(DF2)
}
all_DB_to_char_cols <- function(DB){
  DB$data <-DB$data %>% all_character_cols_list()
  DB$data_transform <-DB$data_transform %>% all_character_cols_list()
  DB$data_update <-DB$data_update %>% all_character_cols_list()
  return(DB)
}
add_redcap_links_table<-function(DF,DB){
  if(nrow(DF)>0){
    DF[[DB$redcap$id_col]] <- paste0("<a href='",paste0("https://redcap.miami.edu/redcap_v",DB$redcap$version,"/DataEntry/record_home.php?pid=",DB$redcap$project_id,"&id=",DF[[DB$redcap$id_col]],"&arm=1"),"' target='_blank'>",DF[[DB$redcap$id_col]],"</a>")
  }
  DF
}
clean_RC_col_names <- function(DF, DB){
  colnames(DF)<-colnames(DF) %>% sapply(function(COL){
    x<-DB$metadata$fields$field_label[which(DB$metadata$fields$field_name==COL)]
    if(length(x)>1){
      x<-x[[1]]
    }
    ifelse(length(x)>0,x,COL)
  }) %>% unlist() %>% return()
  DF
}
clean_RC_df_for_DT <- function(DF, DB){
  DF %>%
    add_redcap_links_table(DB) %>%
    clean_RC_col_names(DB) %>% return()
}
remove_records_from_list <- function(DB,records,silent=F){
  data_list <- DB$data
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
    data_list[[TABLE]] <- data_list[[TABLE]][which(!data_list[[TABLE]][[DB$redcap$id_col]]%in%records),]
  }
  if(!silent)message("Removed: ",paste0(records,collapse = ", "))
  return(data_list)
}
