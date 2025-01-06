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
bullet_in_console <- function(text = "",url = NULL,bullet_type = "i",collapse = T, file = NULL,silent=F){
  if(silent)return(invisible())
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
    file_if <- paste0(
      " {cli::col_blue(cli::style_hyperlink('",
      sanitize_path(file_names),
      "', '",
      sanitize_path(paste0("file://",file)),
      "'))}"
    )
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
generate_hex <- function(length = 32) {
  toupper(paste0(sample(c(0:9, letters[1:6]), length, replace = TRUE), collapse = ""))
}
sanitize_path <- function(path) {
  sanitized <- gsub("\\\\", "/", path)
  sanitized <- normalizePath(sanitized, winslash = "/", mustWork = FALSE)
  return(sanitized)
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
as_comma_string <- function(vec){
  paste0(vec,collapse = ", ")
}
vec1_in_vec2 <- function(vec1,vec2){
  vec1[which(vec1 %in% vec2)]
}
vec1_not_in_vec2 <- function(vec1,vec2){
  vec1[which(!vec1 %in% vec2)]
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
ul <- function(x){
  length(unique(x))
}
wl <- function(x){
  length(which(x))
}
drop_nas <- function(x) {
  x[!sapply(x, is.na)]
}
excel_to_list <- function(path){
  sheets <- readxl::excel_sheets(path)
  clean_sheets <- clean_env_names(sheets)
  out <- list()
  for (i in 1:length(sheets)){
    out[[i]]<- rio::import(path,col_types="text",sheet = i)
  }
  names(out) <- clean_sheets
  return(out)
}
csv_to_list <- function(paths){
  paths <- sanitize_path(paths)
  OUT <- list()
  clean_names <- paths %>% basename() %>% tools::file_path_sans_ext() %>% clean_env_names()
  for (i in 1:length(paths)){
    OUT[[i]]<- utils::read.csv(paths[i],stringsAsFactors = F,na.strings = c("","NA"))
  }
  names(OUT) <- clean_names
  return(OUT)
}
csv_folder_to_list <- function(folder){
  folder <- sanitize_path(folder)
  if(!dir.exists(folder))stop("Folder does not exist: ",folder)
  paths <- list.files.real(folder)
  paths <- paths[which(paths %>% endsWith(".csv"))]
  return(csv_to_list(paths = paths))
}
is_named_df_list <- function(x,strict = F){
  is_named_list(x) && is_df_list(x,strict = strict)
}
is_named_list <- function(x,silent =T,recursive = F) {
  if (!is.list(x))return(FALSE)
  if (is.null(names(x)))return(FALSE)
  named_all <- TRUE
  if(recursive){
    for (n in names(x)) {
      element <- x[[n]]
      if (is.list(element)) {
        named_all <- named_all && is_named_list(element)
        if(!silent&&!named_all)message("'",n, "' is not named")
      }
    }
  }
  return(named_all)  # Return the result
}
wb_to_list <- function(wb){
  # wb <- openxlsx::loadWorkbook(file = path)
  # test for if user does not have excel
  sheets <- openxlsx::sheets(wb)
  clean_sheets <- clean_env_names(sheets)
  out <- list()
  for (i in 1:length(sheets)){
    col_row <- 1
    x <- openxlsx::getTables(wb,sheet = i)
    if(length(x)>0){
      col_row <- as.integer(gsub("[A-Za-z]", "", unlist(x %>% attr("refs") %>% strsplit(":"))[[1]]))# test for xlsx without letters for cols
    }
    out[[i]]<- openxlsx::read.xlsx(wb,sheet = i,startRow = col_row)
  }
  names(out) <- clean_sheets
  return(out)
}
DF_to_wb <- function(
    DF,
    DF_name,
    wb = openxlsx::createWorkbook(),
    link_col_list = list(),
    str_trunc_length = 32000,
    header_df = NULL,
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = T,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = T,
    key_cols = NULL
) {
  if(nchar(DF_name)>31)stop(DF_name, " is longer than 31 char")
  DF <- DF %>% lapply(stringr::str_trunc, str_trunc_length, ellipsis = "") %>% as.data.frame()
  hyperlink_col <- NULL
  if(freeze_keys){
    all_cols <- colnames(DF)
    if(any(!key_cols%in%all_cols))stop("all key_cols must be in the DFs")
    freeze_key_cols <- which(all_cols%in%key_cols)
    if(length(freeze_key_cols)>0){
      if(!is_consecutive_srt_1(freeze_key_cols)){
        warning("please keep your key cols on the left consecutively. Fixing ",DF_name,": ",paste0(key_cols,collapse = ", "),".",immediate. = T)
        non_key_cols <- 1:ncol(DF)
        non_key_cols <- non_key_cols[which(!non_key_cols%in%freeze_key_cols)]
        new_col_order <- c(freeze_key_cols,non_key_cols)
        if(is_something(header_df)){
          header_df <- header_df[,new_col_order]
        }
        DF <- DF[,new_col_order]
      }
    }
  }
  if (nrow(DF)>0){
    openxlsx::addWorksheet(wb, DF_name)
    startRow_header <-pad_rows + 1
    startRow_table <- startRow_header
    startCol <-pad_cols + 1
    if(is_something(header_df)){
      openxlsx::writeData(wb, sheet = DF_name, x = header_df,startRow = startRow_header,startCol = startCol,colNames = F)
      startRow_table <- startRow_header + nrow(header_df)
    }
    if(length(link_col_list)>0){
      has_names <- !is.null(names(link_col_list))
      for(i in seq_along(link_col_list)){
        if(link_col_list[[i]]%in%colnames(DF)){
          class (DF[[link_col_list[[i]]]]) <- "hyperlink"
        }else{
          # warning("",immediate. = T)
        }
        if(has_names){
          if(names(link_col_list)[i]%in%colnames(DF)){
            hyperlink_col <- which(colnames(DF)==names(link_col_list)[i])
            openxlsx::writeData(wb, sheet = DF_name, x = DF[[link_col_list[[i]]]],startRow = startRow_table+1,startCol = hyperlink_col + pad_cols)
            DF[[link_col_list[[i]]]] <- NULL
          }else{
            # warning("",immediate. = T)
          }
        }
      }
    }
    openxlsx::writeDataTable(wb, sheet = DF_name, x = DF,startRow = startRow_table,startCol = startCol, tableStyle = tableStyle)
    style_cols <- seq(ncol(DF))+pad_cols
    openxlsx::addStyle(
      wb,
      sheet = DF_name,
      style = header_style,
      rows = seq(from=startRow_header,to=startRow_table),
      cols = style_cols,
      gridExpand = T,
      stack = T
    )
    openxlsx::addStyle(
      wb,
      sheet = DF_name,
      style = body_style,
      rows = seq(nrow(DF))+startRow_table,
      cols = style_cols,
      gridExpand = T,
      stack = T
    )
    if(freeze_header||freeze_keys){
      firstActiveRow <- NULL
      if(freeze_header){
        firstActiveRow <- startRow_table+1
      }
      firstActiveCol <- NULL
      if(freeze_keys){
        firstActiveCol <- startCol
        freeze_key_cols <- which(colnames(DF)%in%key_cols)
        if(length(freeze_key_cols)>0){
          if (is_consecutive_srt_1(freeze_key_cols)){
            firstActiveCol <- firstActiveCol + freeze_key_cols[length(freeze_key_cols)]
          }else{
            warning("key_cols must be consecutive and start from the left most column.",immediate. = T)
          }
        }
        openxlsx::freezePane(wb, DF_name, firstActiveRow = firstActiveRow, firstActiveCol = firstActiveCol)
      }
    }
    return(wb)
  }
}
list_to_wb <- function(
    list,
    link_col_list = list(),
    str_trunc_length = 32000,
    header_df_list = NULL,
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = T,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = T,
    key_cols_list = NULL,
    drop_empty = T
){
  wb <- openxlsx::createWorkbook()
  list <- process_df_list(list,drop_empty = drop_empty)
  list_names <- names(list)
  list_link_names <- list()
  if(length(link_col_list)>0){
    if(is_named_list(link_col_list)){
      if(!all(names(link_col_list)%in%list_names)){
        for(list_name in list_names){
          list_link_names[[list_name]] <- link_col_list
        }
      }
    }
  }
  list_names_rename <- stringr::str_trunc(list_names,width = 31,side = "right",ellipsis = "")
  BAD <- dw(list_names_rename)
  if(length(BAD)>0){
    warning("Duplicated names when trimmed from right 31 max in Excel: ",list_names[BAD] %>% paste0(collapse = ", "),immediate. = T)
    message("Use CSV or shorten the names and make sure they are unique if they are trimmed to 31 char. For now will make unique by adding number.")
    list_names_rename <- unique_trimmed_strings(list_names_rename, max_length = 31)
  }
  for(i in seq_along(list_names)){
    header_df <- header_df_list[[list_names[i]]]
    key_cols <- key_cols_list[[list_names[i]]]
    wb <- DF_to_wb(
      DF = list[[list_names[i]]],
      DF_name = list_names_rename[i],
      wb = wb,
      link_col_list = list_link_names[[list_names[i]]],
      str_trunc_length = str_trunc_length,
      header_df = header_df,
      tableStyle = tableStyle,
      header_style = header_style,
      body_style = body_style,
      freeze_header = freeze_header,
      pad_rows = pad_rows,
      pad_cols = pad_cols,
      freeze_keys = freeze_keys,
      key_cols = key_cols
    )
  }
  return(wb)
}
unique_trimmed_strings <- function(strings,max_length) {
  trim_string <- function(s, max_length) {
    substr(s, 1, max_length)
  }
  trimmed_strings <- sapply(strings, trim_string, max_length = max_length)
  # Initialize a vector to store unique strings
  unique_strings <- character(length(trimmed_strings))
  # Initialize a counter to keep track of occurrences
  counts <- integer(length(trimmed_strings))
  for (i in seq_along(trimmed_strings)) {
    base_string <- trimmed_strings[i]
    new_string <- base_string
    counter <- 1
    # Keep adjusting the string until it's unique
    while (new_string %in% unique_strings) {
      new_string <- paste0(stringr::str_trunc(base_string,width = max_length-(counter),side = "right",ellipsis = ""), counter)
      counter <- counter + 1
    }
    unique_strings[i] <- new_string
    counts[i] <- counter
  }
  return(unique_strings)
}
list_to_excel <- function(
    list,
    dir,
    file_name = NULL,
    separate = FALSE,
    overwrite = TRUE,
    link_col_list = list(),
    str_trunc_length = 32000,
    header_df_list = NULL,
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = T,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = T,
    key_cols_list = NULL,
    drop_empty = T
) {
  wb <- openxlsx::createWorkbook()
  list <- process_df_list(list,drop_empty = drop_empty)
  list_names <- names(list)
  if(length(list)==0)return(warning("empty list cannot be saved",immediate. = T))
  if(separate){
    for(i in seq_along(list)){
      sub_list <- list[i]
      file_name2 <- names(sub_list)
      if(!is.null(file_name)){
        file_name2 <- paste0(file_name,"_",file_name2)
      }
      save_wb(
        wb = list_to_wb(
          list = sub_list,
          link_col_list = link_col_list,
          str_trunc_length = str_trunc_length,
          header_df_list = header_df_list,
          tableStyle = tableStyle,
          header_style = header_style,
          body_style = body_style,
          freeze_header = freeze_header,
          pad_rows = pad_rows,
          pad_cols = pad_cols,
          freeze_keys = freeze_keys,
          key_cols_list = key_cols_list[[list_names[i]]],
          drop_empty = drop_empty
        ),
        dir = dir,
        file_name = file_name2,
        overwrite = overwrite
      )
    }
  }else{
    save_wb(
      wb = list_to_wb(
        list = list,
        link_col_list = link_col_list,
        str_trunc_length = str_trunc_length,
        header_df_list = header_df_list,
        tableStyle = tableStyle,
        header_style = header_style,
        body_style = body_style,
        freeze_header = freeze_header,
        pad_rows = pad_rows,
        pad_cols = pad_cols,
        freeze_keys = freeze_keys,
        key_cols_list = key_cols_list,
        drop_empty = drop_empty
      ),
      dir = dir,
      file_name = file_name,
      overwrite = overwrite
    )
  }
}
list_to_csv <- function(list,dir,file_name=NULL,overwrite = TRUE, drop_empty = T){
  list <- process_df_list(list,drop_empty = drop_empty)
  list_names <- names(list)
  for(i in seq_along(list)){
    sub_list <- list[i]
    file_name2 <- names(sub_list)
    if(!is.null(file_name)){
      file_name2 <- paste0(file_name,"_",file_name2)
    }
    save_csv(
      DF = sub_list[[1]],
      dir = dir,
      file_name = file_name2,
      overwrite = overwrite
    )
  }
}
save_wb <- function(wb,dir,file_name,overwrite =TRUE){
  if(!dir.exists(dir))stop("dir doesn't exist")
  path <- file.path(dir,paste0(file_name,".xlsx")) %>% sanitize_path()
  openxlsx::saveWorkbook(
    wb = wb,
    file = path,
    overwrite = overwrite
  )
  bullet_in_console(paste0("Saved '", basename(path),"'!"),file = path)
}
save_csv <- function(DF,dir,file_name,overwrite =TRUE){
  if(!dir.exists(dir))stop("dir doesn't exist")
  path <- file.path(dir,paste0(file_name,".csv")) %>% sanitize_path()
  write_it <- T
  if(!overwrite){
    if(file.exists(path)){
      write_it <- F
      bullet_in_console(paste0("Already a file!"),file = path)
    }
  }
  if(write_it){
    utils::write.csv(
      x = DF,
      file = path
    )
    bullet_in_console(paste0("Saved '", basename(path),"'!"),file = path)
  }
}
default_header_style <-
  openxlsx::createStyle(
    fgFill = "#74DFFF",
    halign = "center",
    valign = "center",
    textDecoration = "Bold",
    fontSize = 14,
    fontColour = "black",
    border = "TopBottomLeftRight",
    # borderColour = "black"
  )
default_body_style <-
  openxlsx::createStyle(
    halign = "left",
    valign = "center",
    # border = "Bottom",
    # fontColour = "black",
    fontSize = 12
  )
dw <- function(x){
  which(duplicated(x))
}
is_consecutive_srt_1 <- function(vec) {
  if (vec[1] != 1) {
    return(FALSE)
  }
  if(length(vec)>1){
    for (i in 2:length(vec)) {
      if (vec[i] != vec[i-1] + 1) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}
remove_html_tags <- function(text_vector) {
  # Regular expression to match HTML tags
  html_pattern <- "<[^>]+>"
  # Use gsub to remove the HTML tags from each element in the vector
  cleaned_vector <- gsub(html_pattern, "", text_vector)
  return(cleaned_vector)
}
matches <- function(x,ref,count_only=F){
  final_match <- list()
  final_match[1:length(x)] <- NA
  next_match <- match(x,ref)
  next_match_index <- which(!is.na(next_match))
  while(length(next_match_index)>0){
    final_match[next_match_index] <- next_match_index %>% lapply(function(index){
      if(all(is.na(final_match[[index]]))){
        return(next_match[index])
      }else{
        return(c(final_match[[index]],next_match[index]))
      }
    })
    ref[next_match[which(!is.na(next_match))]] <- NA
    next_match <- match(x,ref)
    next_match_index <- which(!is.na(next_match))
  }
  if(count_only){
    final_match <- final_match %>% sapply(function(IN){
      if(is.na(IN[1]))return(NA)
      return(length(IN))
    })
  }
  return(final_match)
}
choice_vector_string <- function(vec){
  if(!is_something(vec))return(NA)
  return(paste0(paste0(1:length(vec),", ",vec),collapse = " | "))
}
function_to_string <- function(func) {
  # Deparse the function and collapse into a single string using "\n"
  deparse(func) %>% paste(collapse = "\n")
}
clean_function <- function(func) {
  if (!is.function(func)) {
    stop("Input must be a function")
  }
  environment(func) <- emptyenv()
  return(func)
}
size <- function(x){
  format(utils::object.size(x),units = "auto")
}
file_size_mb <- function(path){
  (file.info(path)[["size"]]/1048576) %>% round(1) %>% paste0(" Mb")
}
drop_if <- function(x,drops) {
  x[which(!x%in%drops)]
}
sample1 <- function(x){
  sample(x,1)
}
list.files.real <- function(path,full.names = T, recursive = F){
  grep('~$', sanitize_path(list.files(path,full.names = full.names,recursive = recursive)), fixed = TRUE, value = TRUE, invert = TRUE)
}
wrap_text <- function(text, max_length = 40, spacer = "\n") {
  words <- unlist(strsplit(text, " "))
  current_line <- ""
  result <- ""
  for (word in words) {
    if (nchar(current_line) + nchar(word) + 1 > max_length) {
      result <- paste0(result, current_line, spacer)
      current_line <- word
    } else {
      if (nchar(current_line) == 0) {
        current_line <- word
      } else {
        current_line <- paste0(current_line, " ", word)
      }
    }
  }
  result <- paste0(result, current_line)
  return(result)
}
clean_env_names <- function(env_names,silent = F,lowercase=T){
  cleaned_names <- character(length(env_names))
  for (i in seq_along(env_names)) {
    name <- env_names[i]
    is_valid <- is_env_name(name, silent = TRUE)
    if (is_valid) cleaned_names[i] <- name
    if (!is_valid) {
      if (!silent) message("Invalid environment name: '", name)
      cleaned_name <- gsub("__","_",gsub(" ","_",gsub("-","",name)))
      if(lowercase)cleaned_name <- tolower(cleaned_name)
      if(cleaned_name%in%cleaned_names){
        if (!silent) message("Non-unique environment name: '", name, "', added numbers...")
        cleaned_name <- cleaned_name %>% paste0("_",max(wl(cleaned_name%in%cleaned_names))+1)
      }
      cleaned_names[i] <- cleaned_name
    }
  }
  return(cleaned_names)
}
is_df_list <- function(x,strict=F){
  if (!is.list(x)) return(FALSE)
  if (length(x)==0) return(FALSE)
  if (is_nested_list(x)) return(FALSE)
  out <- sapply(x,is.data.frame)
  if(strict)return(all(out))
  return(any(out))
}
check_match <- function(vec_list) {
  sorted_vecs <- lapply(vec_list, sort)
  all(sapply(sorted_vecs[-1], function(x) identical(sorted_vecs[[1]], x)))
}
is_env_name <- function(env_name,silent=FALSE) {
  result <- tryCatch({
    if (is.null(env_name)) stop("env_name is NULL")
    if (nchar(env_name) == 0) {
      stop("Short name cannot be empty.")
    }
    if (grepl("^\\d", env_name)) {
      stop("Short name cannot start with a number.")
    }
    if (grepl("[^A-Za-z0-9_]", env_name)) {
      stop("Short name can only contain letters, numbers, and underscores.")
    }
    return(TRUE)  # Return TRUE if all checks pass
  }, error = function(e) {
    if(!silent)message(e$message)
    return(FALSE)  # Return FALSE if any error occurs
  })
  return(result)
}
is_nested_list <- function(x) {
  if (!is.list(x)) return(FALSE)
  if (is.data.frame(x)) return(FALSE)
  OUT <- length(x)==0
  for (i in seq_along(x)) {
    OUT <- OUT || is_nested_list(x[[i]])
    # print(OUT)
  }
  return(OUT)
}
clean_num <- function(num){
  formatC(num, format="d", big.mark=",")
}
