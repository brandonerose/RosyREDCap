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
  DB$data_upload %>% lapply(function(x){nrow(x)*ncol(x)}) %>% unlist() %>% sum()
}
split_choices <- function(x){
  oops <- x
  x <- gsub("\n", " | ",x)  #added this to account for redcap metadata output if not a number
  x <- x %>% strsplit(" [:|:] ") %>% unlist()
  check_length <- length(x)
  # code <- x %>% stringr::str_extract("^[^,]+(?=, )")
  # name <- x %>% stringr::str_extract("(?<=, ).*$")
  result <- x %>% stringr::str_match("([^,]+), (.*)")
  # x <- data.frame(
  #   code=x %>% strsplit(", ") %>% sapply(`[`, 1),
  #   name=x %>% strsplit(", ")%>% sapply(`[`, -1) %>% sapply(function(y){paste0(y,collapse = ", ")})
  # )
  x <- data.frame(
    code=result[,2],
    name=result[,3]
  )
  rownames(x) <- NULL
  if(nrow(x)!=check_length)stop("split choice error: ",oops)
  x
}
husk_of_instrument <- function (DB,data_choice="data",FORM,field_names) {
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
  DB$data_upload <-DB$data_upload %>% all_character_cols_list()
  return(DB)
}
add_redcap_links_table<-function(DF,DB){
  if(nrow(DF)>0){
    DF[[DB$redcap$id_col]] <- paste0("<a href='",paste0("https://redcap.miami.edu/redcap_v",DB$redcap$version,"/DataEntry/record_home.php?pid=",DB$redcap$project_id,"&id=",DF[[DB$redcap$id_col]],"&arm=1"),"' target='_blank'>",DF[[DB$redcap$id_col]],"</a>")
  }
  DF
}
clean_RC_col_names <- function(DF, DB, data_choice){
  if(missing(data_choice))data_choice <- DB$internals$reference_state
  if(data_choice == "data"){
    redcap_remap <- "redcap"
  }
  if(data_choice == "data_transform"){
    redcap_remap <- "remap"
  }
  colnames(DF)<-colnames(DF) %>% sapply(function(COL){
    x<-DB[[redcap_remap]][["metadata"]]$field_label[which(DB[[redcap_remap]][["metadata"]]$field_name==COL)]
    if(length(x)>1){
      x<-DB[[redcap_remap]][["metadata"]]$field_label[which(DB[[redcap_remap]][["metadata"]]$field_name==COL&DB[[redcap_remap]][["metadata"]]$form_name==INS)]
    }
    ifelse(length(x)>0,x,COL)
  }) %>% unlist() %>% return()
  DF
}
clean_RC_df_for_DT <- function(DF, DB, data_choice){
  if(missing(data_choice))data_choice <- DB$internals$reference_state
  DF %>%
    add_redcap_links_table(DB) %>%
    clean_RC_col_names(DB,data_choice = data_choice) %>% return()
}
