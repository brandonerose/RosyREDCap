process_df_list <- function(list,drop_empty = T,silent = F){
  if(is_something(list)){
    if(!is_df_list(list))stop("list must be ...... a list :)")
    if(drop_empty){
      is_a_df_with_rows <- list %>% sapply(function(IN){
        is_df <- is.data.frame(IN)
        if(is_df){
          return(nrow(IN)>0)
        }else{
          return(F)
        }
      })
      keeps <- which(is_a_df_with_rows)
      drops <-which(!is_a_df_with_rows)
      if(length(drops)>0){
        if(!silent)bullet_in_console("Dropping non-data.frames and empties... ", paste0(names(drops),collapse = ", "))
      }
      list <- list[keeps]
    }
    if(length(list)>0){
      if(!is_named_df_list(list)){
        names(list) <- paste0(seq_along(list))
      }
    }
  }
  return(list)
}
is_something <- function(thing,row=0){
  out <- FALSE
  if(is.function(thing))return(T)
  if(!is.null(thing)){
    if(is.data.frame(thing)){
      if(nrow(thing)>row){
        out <-TRUE
      }
    }else{
      if(length(thing)>0){
        if(is.list(thing)){
          out <- TRUE
        }else{
          if(length(thing)==1){
            if(!is.na(thing)){
              if(is.character(thing)){
                if(thing!=""){
                  out <-TRUE
                }
              }else{
                out <-TRUE
              }
            }
          }else{
            out <-TRUE
          }
        }
      }
    }
  }
  return(out)
}
bullet_in_console <- function(text = "",url = NULL,bullet_type = "i",collapse = T, file = NULL,verbosity=2){
  url_if <- ""
  file_if <- ""
  if(length(url)>0){
    # url %>% lapply(function(IN){validate_web_link(IN)}) # doesnt work for /subheaders/
    # url_if <- " {.url {url}}"
    url_names <- names(url)
    if(is.list(url)){
      url_names <- url %>% unlist()
      if(is_named_list(url))url_names <- names(url)
      url <- unlist(url)
    }
    if(is.null(url_names))url_names <- url
    if(collapse)url_if <- paste0(url_if,collapse = " and ")
    url_if <- paste0(" {cli::col_blue(cli::style_hyperlink('",url_names,"', '",url,"'))}")
  }
  if(length(file)>0){
    file_names <- names(file)
    if(is.list(file)){
      file_names <- file %>% unlist()
      if(is_named_list(file))file_names <- names(file)
      file <- unlist(file)
    }
    if(is.null(file_names))file_names <- file
    if(collapse)file_if <- paste0(file_if,collapse = " and ")
    file_if <- paste0(" {cli::col_blue(cli::style_hyperlink('", file_names, "', 'file://", normalizePath(file), "'))}")
  }
  for(i in 1:length(url_if))text[i] <- paste0(text[i],url_if[i])
  for(i in 1:length(file_if))text[i] <- paste0(text[i],file_if[i])
  names(text)[1:length(text)]<- bullet_type
  return(cli::cli_bullets(text))
  # * = • = bullet
  # > = → = arrow
  # v = ✔ = success
  # x = ✖ = danger
  # ! = ! = warning
  # i = ℹ = info
}
find_df_diff <- function (new, old,ref_cols=NULL,message_pass=""){
  new <- all_character_cols(new)
  old <- all_character_cols(old)
  if (!all(colnames(new) %in% colnames(old))) {
    stop("All new DF columns must be included in old DF")
  }
  if (!all(ref_cols %in% colnames(new))| !all(ref_cols %in% colnames(old))) {
    stop("ref_cols must be included in both dfs")
  }
  if (length(ref_cols)>1){
    new$key <- apply( new[ , ref_cols] , 1 , paste , collapse = "_" )
    old$key <- apply( old[ , ref_cols ] , 1 , paste , collapse = "_" )
  }else{
    new$key <- new[ , ref_cols]
    old$key <- old[ , ref_cols]
  }
  if (anyDuplicated(old$key) > 0) {
    stop("Keys must lead to unique rows! (old DF)")
  }
  if (anyDuplicated(new$key) > 0) {
    stop("Keys must lead to unique rows! (new DF)")
  }
  new_keys <- integer(0)
  if(any(!new$key %in% old$key)){
    # warning("You have at least one new key compared to old DF therefore all columns will be included by default",immediate. = T)
    new_keys <- which(!new$key %in% old$key)
  }
  indices <- data.frame(
    row = integer(0),
    col = integer(0)
  )
  for(new_key in new_keys){
    indices <- indices %>% dplyr::bind_rows(
      data.frame(
        row = new_key,
        col = which(!colnames(new)%in%c(ref_cols,"key"))
      )
    )
  }
  for (KEY in new$key[which(new$key%in%old$key)]){
    row <- which(new$key == KEY)
    row_old <- which(old$key == KEY)
    for (COL in colnames(new)[which(!colnames(new)%in%c(ref_cols,"key"))]){
      col <- which(colnames(new) == COL)
      if(!identical(new[row,COL],old[row_old,COL])){
        indices <- indices %>% dplyr::bind_rows(
          data.frame(
            row = row,
            col = col
          )
        )
      }
    }
  }
  if(nrow(indices)>0){
    rows <- indices$row %>% unique() %>% sort()
    cols <- which(colnames(new)%in%ref_cols) %>% append(indices$col %>% unique() %>% sort())
    OUT <- new[rows,cols]
    message(message_pass,nrow(OUT), " rows have updates")
  }else{
    OUT <- NULL
    message(message_pass,"No changes!")
  }
  OUT
}
find_df_diff2 <- function (new, old,ref_cols=NULL,message_pass="",view_old = T, n_row_view = 20){
  new <- all_character_cols(new)
  old <- all_character_cols(old)
  if (!all(colnames(new) %in% colnames(old))) {
    stop("All new DF columns must be included in old DF")
  }
  if (!all(ref_cols %in% colnames(new))| !all(ref_cols %in% colnames(old))) {
    stop("ref_cols must be included in both dfs")
  }
  if (length(ref_cols)>1){
    new_keys <- apply( new[ , ref_cols] , 1 , paste , collapse = "_" )
    old_keys <- apply( old[ , ref_cols ] , 1 , paste , collapse = "_" )
  }else{
    new_keys <- new[ , ref_cols]
    old_keys <- old[ , ref_cols]
  }
  if (anyDuplicated(old_keys) > 0) {
    stop("Keys must lead to unique rows! (old DF)")
  }
  if (anyDuplicated(new_keys) > 0) {
    stop("Keys must lead to unique rows! (new DF)")
  }
  appended_old_col_suffix <- "__old"
  if(any(endsWith(unique(colnames(old),colnames(new)),appended_old_col_suffix)))stop("colnames cant end with '",appended_old_col_suffix,"'")
  merged_df <- merge(new, old, by = ref_cols, suffixes = c("",appended_old_col_suffix ),all.x = T)
  placeholder <- "NA_placeholder"
  rows_to_keep <- NULL
  cols_to_view <- cols_to_keep <- which(colnames(merged_df) %in% ref_cols)
  COLS <- colnames(new)[which(!colnames(new)%in%ref_cols)]
  for (COL in COLS){
    vector1 <- merged_df[[COL]]
    compare_COL <- paste0(COL, appended_old_col_suffix)
    vector2 <- merged_df[[compare_COL]]
    vector1_no_na <- ifelse(is.na(vector1), placeholder, vector1)
    vector2_no_na <- ifelse(is.na(vector2), placeholder, vector2)
    # Compare vectors element-wise
    are_not_equal <- which(vector1_no_na != vector2_no_na)
    if(length(are_not_equal)>0){
      rows_to_keep <- rows_to_keep %>% append(are_not_equal)
      additional_cols <- which(colnames(merged_df) == COL)
      cols_to_keep <- cols_to_keep %>% append(additional_cols)
      if(view_old){
        cols_to_view <- cols_to_view %>% append(additional_cols) %>% append(which(colnames(merged_df) == compare_COL))
      }
    }
  }
  if(length(rows_to_keep)>0){
    rows_to_keep <- rows_to_keep %>% unique()
    cols_to_keep <- cols_to_keep %>% unique()
    if(view_old){
      rows_to_keep2 <- rows_to_keep
      done <- F
      while ( ! done) {
        length_of_rows_to_keep <- length(rows_to_keep2)
        if(length_of_rows_to_keep==0){
          done <- T
        }else{
          indices <- 1:ifelse(length_of_rows_to_keep<n_row_view,length_of_rows_to_keep,n_row_view)
          rows_to_keep3 <- rows_to_keep2[indices]
          print.data.frame(merged_df[rows_to_keep3,unique(cols_to_view)])
          choice <- utils::menu(choices = c("Check more rows","Proceed with no more checking", "Stop the function"),title = "What would you like to do?")
          if(choice==3)stop("Stopped as requested!")
          if(choice==2)done <- T
          if(choice==1)rows_to_keep2 <- rows_to_keep2[-indices]
        }
      }
    }
    message(message_pass,length(rows_to_keep), " rows have updates")
    return(merged_df[rows_to_keep,cols_to_keep])
  }else{
    message(message_pass,"No changes!")
    return(NULL)
  }
}
find_df_list_diff <- function(new_list, old_list,ref_col_list,view_old = T, n_row_view = 20){
  if(!is_something(new_list)){
    message("new_list is empty")
    return(list())
  }
  if(!is_something(old_list)){
    message("old_list is empty")
    return(list())
  }
  if(!is_df_list(new_list))stop("new_list must be a list of data.frames")
  if(!is_df_list(old_list))stop("old_list must be a list of data.frames")
  if(any(!names(new_list)%in%names(old_list)))stop("All new_list names must be included in the set of old_list names.")
  if(!is.list(ref_col_list)){
    ref_col_list <- names(new_list) %>% lapply(function(IN){
      ref_col_list
    })
    names(ref_col_list) <- names(new_list)
  }
  for(df_name in names(new_list)){
    new_list[[df_name]] <- find_df_diff2(new = new_list[[df_name]], old = old_list[[df_name]],ref_cols = ref_col_list[[df_name]], message_pass = paste0(df_name,": "),view_old = view_old, n_row_view = n_row_view)
  }
  return(new_list)
}
all_character_cols <- function(DF){
  as.data.frame(lapply(DF,as.character))
}
all_character_cols_list <- function(list){
  lapply(list,all_character_cols)
}
