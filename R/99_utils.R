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
validate_env_name <- function(env_name) {
  # Check if the name is empty
  if(is.null(env_name)) stop("env_name is NULL")
  if (nchar(env_name) == 0) {
    stop("Short name cannot be empty.")
  }
  # Check if the name starts with a number
  if (grepl("^\\d", env_name)) {
    stop("Short name cannot start with a number.")
  }
  # Check if the name contains any invalid characters
  if (grepl("[^A-Za-z0-9_]", env_name)) {
    stop("Short name can only contain letters, numbers, and underscores.")
  }
  return(env_name)
}
validate_web_link <- function(link) {
  if(is.null(link)) stop("link is NULL")
  # Check if the link starts with "https://" or "http://"
  if (!grepl("^https?://", link)) {
    stop("Invalid web link. It must start with 'http://' or 'https://'.")
  }
  # Remove trailing slash if present
  link <- gsub("/$", "", link)
  # Check if the link ends with one of the specified web endings
  if (!grepl("\\.(edu|com|org|net|gov|io|xyz|info|co|uk)$", link)) {
    stop("Invalid web link. It must end with a valid web ending (.edu, .com, etc.).")
  }
  # Add a trailing slash
  link <- paste0(link, "/")
  return(link)
}
count_DB_upload_cells <- function(DB){
  DB$data_upload %>% lapply(function(x){nrow(x)*ncol(x)}) %>% unlist() %>% sum()
}
addSlashIfNeeded <- function(input_string) {
  if (!endsWith(input_string, "/")) {
    output_string <- gsub("$", "/", input_string)
  } else {
    output_string <- input_string
  }
  return(output_string)
}
remove_html_tags <- function(text_vector) {
  # Regular expression to match HTML tags
  html_pattern <- "<[^>]+>"
  # Use gsub to remove the HTML tags from each element in the vector
  cleaned_vector <- gsub(html_pattern, "", text_vector)
  return(cleaned_vector)
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
husk_of_instrument <- function (DB,data_choice="data_extract",FORM,field_names) {
  DF <- DB[[data_choice]][[FORM]]
  cols<- colnames(DF)[which(colnames(DF)%in%DB$redcap$raw_structure_cols)]
  DF2 <- NULL
  for(col in cols){
    DF2[[col]] <- DF[[col]]
  }
  DF2 <-as.data.frame(DF2)
  return(DF2)
}
all_DB_to_char_cols <- function(DB){
  DB$data_extract <-DB$data_extract %>% all_character_cols_list()
  DB$data_transform <-DB$data_transform %>% all_character_cols_list()
  DB$data_upload <-DB$data_upload %>% all_character_cols_list()
  return(DB)
}
