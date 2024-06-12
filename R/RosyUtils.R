# data
#' @title find the difference between two data.frames
#' @description
#' This function will compare two data.frames: new and old.
#' You define the reference columns with ref_cols.
#' Reference columns are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @param new a new data.frame to compare to old. All new cols must be included in the set of the old ones.
#' @param old a reference data.frame to be compared to
#' @param ref_cols character vector of reference columns. They are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @return messages and data.frame of only changes and reference cols
#' @export
find_df_diff <- function (new, old,ref_cols=NULL,message_pass=""){
  new <- all_character_cols(new)
  old <- all_character_cols(old)
  if (!all(colnames(new) %in% colnames(old))) {
    stop("All new df columns must be included in old df")
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
    stop("Keys must lead to unique rows! (old df)")
  }
  if (anyDuplicated(new$key) > 0) {
    stop("Keys must lead to unique rows! (new df)")
  }
  new_keys <- integer(0)
  if(any(!new$key %in% old$key)){
    # warning("You have at least one new key compared to old df therefore all columns will be included by default",immediate. = T)
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
#' @title find the difference between two data.frames find_df_diff2
#' @description
#' This function will compare two data.frames: new and old.
#' You define the reference columns with ref_cols.
#' Reference columns are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @param new a new data.frame to compare to old. All new cols must be included in the set of the old ones.
#' @param old a reference data.frame to be compared to
#' @param ref_cols character vector of reference columns. They are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @param view_old logical for viewing old
#' @return messages and data.frame of only changes and reference cols
#' @export
find_df_diff2 <- function (new, old,ref_cols=NULL,message_pass="",view_old = T, n_row_view = 20){
  new <- all_character_cols(new)
  old <- all_character_cols(old)
  if (!all(colnames(new) %in% colnames(old))) {
    stop("All new df columns must be included in old df")
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
    stop("Keys must lead to unique rows! (old df)")
  }
  if (anyDuplicated(new_keys) > 0) {
    stop("Keys must lead to unique rows! (new df)")
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
#' @title find the difference between two lists of related data.frames
#' @description
#' This function will compare two data.frames: new and old.
#' You define the reference columns with ref_cols.
#' Reference columns are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @param new_list a new data.frame to compare to old. All new cols must be included in the set of the old ones.
#' @param old_list a reference data.frame to be compared to
#' @param ref_col_list character vector of reference columns. They are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @param view_old logical for viewing old
#' @return messages and data.frame of only changes and reference cols
#' @export
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
#' @title all_character_cols
#' @export
all_character_cols <- function(df){
  as.data.frame(lapply(df,as.character))
}
#' @title all_character_cols_list
#' @export
all_character_cols_list <- function(list){
  lapply(list,all_character_cols)
}
#' @title collapse_DF
#' @export
collapse_DF <- function(DF,ref_id,list_mod){
  if( ! ref_id %in% colnames(DF))stop("`ref_id` must be a colname in `DF`")
  new_DF <- NULL
  new_DF[[ref_id]] <- unique(DF[[ref_id]])
  other_cols <-colnames(DF)[which(!colnames(DF) %in% ref_id)]
  if(!missing(list_mod)){
    for(i in 1:length(list_mod)){
      new_DF[[names(list_mod[i])]] <- new_DF[[ref_id]] %>% sapply(function(ID){
        sub_df <- DF[which(DF[[ref_id]]==ID),list_mod[[i]]]
        n_col <- ncol(sub_df)
        n_row <- nrow(sub_df)
        out_final <- NULL
        for(j in 1:n_row){
          out <- NULL
          for(k in 1:n_col){
            out <- out %>% append(paste0(colnames(sub_df)[k]," - ",sub_df[j,k]))
          }
          out <- out %>% paste(collapse = " | ")
          out_final <- out_final %>% append(out)
        }
        out_final <- out_final %>% paste(collapse = " || ")
        out_final
      }) %>% as.character()
    }
    DF <- DF[,which(!colnames(DF)%in%list_mod[[i]])]
    other_cols <- other_cols[which(!other_cols%in%list_mod[[i]])]
  }
  other_col <- other_cols %>% sample(1)
  for (other_col in other_cols) {
    new_DF[[other_col]] <- sapply( new_DF[[ref_id]], function(ID){
      x<- unique(DF[[other_col]][which(DF[[ref_id]]==ID)]) %>% drop_nas()
      if(length(x)==0)return(NA)
      return(paste0(x,collapse = " | "))
    })
  }
  as.data.frame(new_DF)
}
#' @title clean_df_cols
#' @export
clean_df_cols <- function(df) {
  str <- tolower(colnames(df))
  str <- gsub("[^a-z0-9\\s]+", " _", str)
  str <- gsub("\\s+", "_", str)
  str <- gsub("^_|_$", "", str)
  str <- gsub("^_|_$", "", str)
  str <- gsub("__", "_", str)
  str <- gsub("__", "_", str)
  if(anyDuplicated(str)>0)stop("Duplicate col names!")
  colnames(df) <- str
  return(df)
}
#' @title clean_env_names
#' @export
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
#' @title addSlashIfNeeded
#' @export
addSlashIfNeeded <- function(input_string) {
  if (!endsWith(input_string, "/")) {
    output_string <- gsub("$", "/", input_string)
  } else {
    output_string <- input_string
  }
  return(output_string)
}
#' @title remove_html_tags
#' @export
remove_html_tags <- function(text_vector) {
  # Regular expression to match HTML tags
  html_pattern <- "<[^>]+>"
  # Use gsub to remove the HTML tags from each element in the vector
  cleaned_vector <- gsub(html_pattern, "", text_vector)
  return(cleaned_vector)
}
#' @title check_match
#' @export
check_match <- function(vec_list) {
  sorted_vecs <- lapply(vec_list, sort)
  all(sapply(sorted_vecs[-1], function(x) identical(sorted_vecs[[1]], x)))
}
#' @title vec1_in_vec2
#' @export
vec1_in_vec2 <- function(vec1,vec2){
  vec1[which(vec1 %in% vec2)]
}
#' @title vec1_not_in_vec2
#' @export
vec1_not_in_vec2 <- function(vec1,vec2){
  vec1[which(!vec1 %in% vec2)]
}
#' @title find_in_DF_list
#' @export
find_in_df_list <- function(df_list,text,exact = F){
  df_list <- process_df_list(df_list)
  out <- data.frame(
    record_id = character(0),
    col = character(0),
    row = character(0)
  )
  if (!exact){
    text <- tolower(text)
  }
  for(form in names(df_list)){
    DF <- df_list[[form]]
    for(col in colnames(DF)){
      if (!exact){
        DF[[col]] <- tolower(DF[[col]])
      }
      rows <- which(grepl(text,DF[[col]]))
      if(length(rows)>0){
        out <- out %>%dplyr::bind_rows(
          data.frame(
            record_id = DF[[DB$redcap$id_col]][rows],
            col = col,
            row = as.character(rows)
          )
        )
      }
    }
  }
  return(out)
}
# dates
#' @export
age <- function(dob, age.day = lubridate::today(), units = "years", floor = TRUE) {
  calc.age = lubridate::interval(dob, age.day) / lubridate::duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}
#' @export
is_date <- function(date) {
  OUT <- grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d{4}-\\d{2}$|^\\d{4}$", date)
  if(OUT){
    OUT2 <- date %>% strsplit(split = "-") %>% unlist()
    year <- OUT2[[1]]
    check_date <- year
    if(length(OUT2)==1){
      check_date<-check_date %>% paste0("-01")
      OUT2[[2]] <- "01"
    }
    if(length(OUT2)==2){
      check_date<-check_date %>% paste0("-01")
      OUT2[[3]] <- "01"
    }
    year <- year %>% as.integer()
    month <- OUT2[[2]] %>% as.integer()
    day <- OUT2[[3]] %>% as.integer()
    this_year <-
      OUT <- month>=1&&month<=12&&day>=1&&day<=31&&year>=1900&&year<=lubridate::year(Sys.Date())
  }
  OUT
}
#' @export
is_date_full <- function(date) {
  grepl("^\\d{4}-\\d{2}-\\d{2}$", date)
}
#' @export
extract_dates <- function(input_string) {
  # Regular expression pattern to match different date formats
  date_patterns <- c(
    "\\b(0?[1-9]|1[0-2])/(0?[1-9]|[12][0-9]|3[01])/(\\d{2})\\b", # MM/DD/YY
    "\\b(0?[1-9]|1[0-2])/(0?[1-9]|[12][0-9]|3[01])/(\\d{4})\\b", # MM/DD/YYYY
    "\\b(0?[1-9]|1[0-2])-(0?[1-9]|[12][0-9]|3[01])-(\\d{2})\\b", # MM-DD-YY
    "\\b(0?[1-9]|1[0-2])-(0?[1-9]|[12][0-9]|3[01])-(\\d{4})\\b"  # MM-DD-YYYY
  )
  # Initialize an empty list to store matches
  matched_dates <- list()
  # Extract date matches using str_extract_all for each pattern
  for (pattern in date_patterns) {
    matches <- stringr::str_extract_all(input_string, pattern)
    matched_dates <- matched_dates %>% append(matches)
  }
  return(unlist(matched_dates))
}
#' @export
extract_dates2 <- function(input_string) {
  # Regular expression pattern to match different date formats
  date_patterns <- c(
    "\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])"
  )#"\\d{4}-(0?[1-9]|1[0-2])-(0?[1-9]|[12][0-9]|3[01])"
  # Initialize an empty list to store matches
  matched_dates <- list()
  # Extract date matches using str_extract_all for each pattern
  for (pattern in date_patterns) {
    matches <- stringr::str_extract_all(input_string, pattern)
    matched_dates <- matched_dates %>% append(matches)
  }
  if(length(matched_dates[[1]])==0)return(NA)
  return(unlist(matched_dates))
}
#' @export
delete_dates2 <- function(input_string){
  # Extract valid dates and remove the rest of the text
  date_patterns <- c(
    "\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])"
  )
  gsub(paste(date_patterns, collapse = "|"), "", input_string)
}
#' @export
convert_dates <- function(input_string) {
  if(!is.na(input_string)){
    input_string <- input_string %>% trimws()
    if(input_string!=""){
      dates <- extract_dates(input_string)
      output_string <- input_string
      for (pattern in dates) {
        pattern <- gsub("-","/",pattern)
        split_pattern <- pattern %>% strsplit("/") %>% unlist()
        month <- split_pattern[[1]] %>% as.integer()%>% stringr::str_pad(2,"left",0)
        day <- split_pattern[[2]] %>% as.integer()%>% stringr::str_pad(2,"left",0)
        year <- split_pattern[[3]] %>% as.integer()
        if(stringr::str_length(year)%in%c(1,2)){
          year <-year %>% stringr::str_pad(2,"left",0)
          if(year>=0&&year<25){
            year <- paste0("20",year)
          }
          if(year>=50&&year<=99){
            year <- paste0("19",year)
          }
        }
        output_string <- gsub(pattern, paste0(year,"-",month,"-",day), output_string, perl = TRUE)
      }
      return(output_string)
    }
  }
}
#' @export
date_imputation<-function(dates_in,date_imputation){
  #followup add min max
  z <- sapply(dates_in,is_date) %>% as.logical()
  x<-which(z&!is_date_full(dates_in))
  y<-which(!z)
  date_out<-dates_in
  if(length(y)>0){
    date_out[y] <- NA
  }
  if(length(x)>0){
    if(missing(date_imputation)) date_imputation <- NULL
    if(is.null(date_imputation)){
      date_out[x]<- NA
    }
    if(!is.null(date_imputation)){
      date_out[x] <- dates_in[x] %>% sapply(function(date){
        admiral::impute_dtc_dt(
          date,
          highest_imputation = "M", # "n" for normal date
          date_imputation = date_imputation
          # min_dates = min_dates %>% lubridate::ymd() %>% as.list(),
          # max_dates = max_dates %>% lubridate::ymd() %>% as.list()
        )
      })
    }
  }
  date_out
}
# excel
#' @title excel_to_list
#' @export
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
#' @title wb_to_list
#' @export
wb_to_list <- function(wb){
  # wb <- openxlsx::loadWorkbook(file = path)
  # test for if user does not have excel
  sheets <- openxlsx::sheets(wb)
  clean_sheets <- clean_env_names(sheets)
  out <- list()
  for (i in 1:length(sheets)){
    col_row <- 1
    x<-openxlsx::getTables(wb,sheet = i)
    if(length(x)>0){
      col_row <-  as.integer(gsub("[A-Za-z]", "", unlist(x %>% attr("refs") %>% strsplit(":"))[[1]]))# test for xlsx without letters for cols
    }
    out[[i]]<- openxlsx::read.xlsx(wb,sheet = i,startRow = col_row)
  }
  names(out) <- clean_sheets
  return(out)
}
#' @title DF_to_wb
#' @export
DF_to_wb <- function(
    DF,
    DF_name,
    wb = openxlsx::createWorkbook(),
    link_col_list = list(),
    str_trunc_length = 32000,
    header_df,
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
  DF <-  DF %>% lapply(stringr::str_trunc, str_trunc_length, ellipsis = "") %>% as.data.frame()
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
    if(missing(header_df))  header_df<- data.frame()
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
        firstActiveRow <-  startRow_table+1
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
#' @title list_to_wb
#' @export
list_to_wb <- function(
    list,
    link_col_list = list(),
    str_trunc_length = 32000,
    header_df_list = list(),
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = T,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = T,
    key_cols_list = NULL
){
  if(missing(header_df_list))  header_df_list<- list()
  wb <- openxlsx::createWorkbook()
  list <- process_df_list(list)
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
  BAD<-dw(list_names_rename)
  if(length(BAD)>0)stop("Duplicated names when trimmed: ",list_names[BAD] %>% paste0(collapse = ", "))
  for(i in seq_along(list_names)){
    wb <- DF_to_wb(
      DF = list[[list_names[i]]],
      DF_name = list_names_rename[i],
      wb = wb,
      link_col_list = list_link_names[[list_names[i]]],
      str_trunc_length = str_trunc_length,
      header_df = header_df_list[[list_names[i]]],
      tableStyle = tableStyle,
      header_style = header_style,
      body_style = body_style,
      freeze_header = freeze_header,
      pad_rows = pad_rows,
      pad_cols = pad_cols,
      freeze_keys = freeze_keys,
      key_cols = key_cols_list[[list_names[i]]]
    )
  }
  return(wb)
}
#' @title list_to_wb
#' @export
list_to_excel <- function(
    list,
    dir,
    file_name = NULL,
    separate = FALSE,
    overwrite = TRUE,
    link_col_list = list(),
    str_trunc_length = 32000,
    header_df_list,
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = T,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = T,
    key_cols_list = NULL
) {
  if(missing(header_df_list))  header_df_list<- list()
  wb <- openxlsx::createWorkbook()
  list <- process_df_list(list)
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
          key_cols_list = key_cols_list[[list_names[i]]]
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
        key_cols_list = key_cols_list
      ),
      dir = dir,
      file_name = file_name,
      overwrite = overwrite
    )
  }
}
#' @export
list_to_csv <- function(list,dir,file_name=NULL,overwrite = TRUE){
  list <- process_df_list(list)
  list_names <- names(list)
  for(i in seq_along(list)){
    sub_list <- list[i]
    file_name2 <- names(sub_list)
    if(!is.null(file_name)){
      file_name2 <- paste0(file_name,"_",file_name2)
    }
    save_csv(
      df = sub_list[[1]],
      dir = dir,
      file_name = file_name2,
      overwrite = overwrite
    )
  }
}
#' @title save_wb
#' @export
save_wb <- function(wb,dir,file_name,overwrite =TRUE){
  if(!dir.exists(dir))stop("dir doesn't exist")
  path <- file.path(dir,paste0(file_name,".xlsx"))
  openxlsx::saveWorkbook(
    wb = wb,
    file = path,
    overwrite = overwrite
  )
  message("Saved at -> ","'",path,"'")
}
#' @title save_csv
#' @export
save_csv <- function(df,dir,file_name,overwrite =TRUE){
  if(!dir.exists(dir))stop("dir doesn't exist")
  path <- file.path(dir,paste0(file_name,".csv"))
  write_it <- T
  if(!overwrite){
    if(file.exists(path)){
      write_it <- F
      message("Already a file at -> ","'",path,"'")
    }
  }
  if(write_it){
    write.csv(
      x = df,
      file = path
    )
    message("Saved at -> ","'",path,"'")
  }
}
#' @title process_df_list
#' @export
process_df_list <- function(list){
  if(!is_df_list(list))stop("list must be ...... a list :)")
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
    message("Dropping non-data.frames and empties... ", paste0(names(drops),collapse = ", "))
  }
  list <- list[keeps]
  if(length(list)>0){
    if(!is_named_df_list(list)){
      names(list) <- paste0(seq_along(list))
    }
  }
  return(list)
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
# files
#' @title full.file.info
#' @param path a file path
#' @param showWarnings logical for showing warnings
#' @return file_info data.frame
#' @export
full.file.info <- function(path,showWarnings = T) {
  if(showWarnings){
    if(! file.exists(path))warning("path does not exist: ",path,immediate. = T)
  }
  file_info <- data.frame(
    file = list.files(path) ,
    path = list.files(path, full.names = T)
  )
  file_info <- cbind(
    file_info,
    file.info(file_info$path)
  )
  rownames(file_info) <- NULL
  return(file_info)
}
#' @title sync_dir
#' @param from a file path for from
#' @param to a file path for to
#' @param top_level logical for being at top level of recursive function
#' @return message
#' @export
sync_dir <- function(from,to,top_level=T){
  if(top_level){
    if(!file.exists(from))stop("from path '",from, "' doesn't exist")
    if(!file.info(from)[["isdir"]])stop("from path '",from, "' must be a folder")
    if(!file.exists(to)){
      dir.create(to,showWarnings = F)
    }#stop("to path '",to, "' doesn't exist")
    if(!file.info(to)[["isdir"]])stop("to path '",to, "' must be a folder")
  }
  file_info_from <- full.file.info(from)
  file_info_to <- full.file.info(to,showWarnings=F)
  if(nrow(file_info_from)>0){
    for(i in 1:nrow(file_info_from)){
      file_from <- file_info_from$file[i]
      isdir_from <- file_info_from$isdir[i]
      path_from <- file_info_from$path[i]
      mtime_from <- file_info_from$mtime[i]
      COPY_TF <- T
      add_or_update <- "Adding"
      MATCHING_FILE_ROW <- which(file_info_to$file==file_from)
      if(length(MATCHING_FILE_ROW)>0){
        if(length(MATCHING_FILE_ROW)>1){stop("Strange case of from and to file names seen more than once")}
        isdir_to <- file_info_to$isdir[MATCHING_FILE_ROW]
        if(isdir_to!=isdir_from){stop("Strange case of from and to paths not being both file-file or folder-folder")}
        add_or_update <- "Updating"
        file_to <- file_info_to$file[MATCHING_FILE_ROW]
        path_to <- file_info_to$path[MATCHING_FILE_ROW]
        mtime_to <- file_info_to$mtime[MATCHING_FILE_ROW]
        if(isdir_from){
          COPY_TF <- F # no need to copy folders that exist
        }
        if(!isdir_from){#if it's a file... check mtimes
          COPY_TF <- mtime_from > mtime_to
        }
      }
      if(COPY_TF){
        file.copy(
          from = path_from,
          to = to,
          overwrite = T,
          recursive = T
        )
        message(add_or_update," file: ",file_from, " to '", to, "'")
      }
      if(!COPY_TF&&isdir_from){
        sync_dir( #recursive dive down if it's a folder
          from = path_from,
          to = path_to,
          top_level = F
        )
      }
    }
  }else{
    warning(from, " is empty!",immediate. = T)
  }
  if(top_level){message("Up to date!")}
}
#' @title list.files.real
#' @export
list.files.real <- function(path){
  grep('~$', list.files(path), fixed = TRUE, value = TRUE, invert = TRUE)
}
# flags
#' @export
add_df_flag <- function(DF,flag_field_name,id_field_name,ids,flag_name,read_split=" [:|:] ",write_split = " | ",remove_previous = T){
  if(any(!ids%in%DF[[id_field_name]]))stop( "Some of your record IDs are not included in the set of the current database IDs! Add them first...")
  flag_vector <- DF[[flag_field_name]] %>% strsplit(split=read_split)
  DF[[flag_field_name]] <- 1:nrow(DF) %>% sapply(function(ROW){
    if(!is.na(DF[[flag_field_name]][ROW])){
      IN <- flag_vector[[ROW]]
    }else{
      IN <- NULL
    }
    if(remove_previous){
      IN<-IN[which(IN!=flag_name)] #remove flag_name
    }
    if(DF[[id_field_name]][ROW]%in%ids){
      IN <- IN %>% append(flag_name) # add id if it should be there
    }
    if(is.null(IN))return(NA)
    return(IN %>% sort() %>% unique() %>% trimws() %>% paste0(collapse = " | ") )#sort and clean
  })
  return(DF)
}
#' @export
check_df_flag <- function(DF,flag_field_name,split=" [|] "){
  #add warning for grep conflicts
  DF[[flag_field_name]] %>% strsplit(split = split) %>% unlist() %>% unique() %>% drop_nas() %>% sort() %>% return()
}
#' @export
remove_df_flag <- function(DF,flag_field_name,flag_name){
  if(nrow(DF)>0){
    flag_vector<- DF[[flag_field_name]] %>% strsplit(" [:|:] ")
    DF[[flag_field_name]] <- 1:nrow(DF) %>% sapply(function(ROW){
      if(!is.na(DF[[flag_field_name]][ROW])){
        IN <- flag_vector[[ROW]]
      }else{
        IN <- NULL
      }
      IN<-IN[which(IN!=flag_name)] #remove flag_name
      if(!is.null(IN)){
        return(IN %>% sort() %>% unique() %>% trimws() %>% paste0(collapse = " | ") )#sort and clean
      }else{
        return(NA)
      }
    })
  }
  return(DF)
}
#' @export
get_df_flag_rows <- function(DF,flag_field_name,flag_name){
  return(which(DF[[flag_field_name]] %>% strsplit(" [:|:] ") %>% sapply(function(ROW){flag_name%in%drop_nas(ROW)})))
}
#' @export
combine_two_split_vector_flags <- function(v1,v2,read_split=" [:|:] ",write_split = " | "){
  combined_list <- Map(function(x, y) c(x, y), strsplit(v1,split = read_split), strsplit(v2,split = read_split))
  v3 <- sapply(combined_list,function(E){
    x<-sort(drop_nas(unique(E)))
    if(length(x)==0)return(NA)
    return(paste0(x,collapse = write_split))
  })
}
# lists
#' @title add_list_to_global
#' @param x a named list
#' @param only_dfs logical for only including data.frames in output
#' @return environment adds
#' @export
add_list_to_global <- function(x,only_dfs=F){
  if(!is.list(x))stop("x must be a list")
  if(length(x)==0)stop("length(x) has to be at least 1")
  if(class(x)=="character"){
    if(is.null(names(x)))stop("if x is a character vector it must have names")
    x<-as.list(x)
  }
  if(class(x)!="list")stop("x must be a named list or named character vector")
  if(is.null(names(x)))stop("x list must have names")
  if(only_dfs)x <- x[sapply(x,is.data.frame)]
  list2env(x,envir = .GlobalEnv)
}
# other
#' @title sample1
#' @export
sample1 <- function(x){
  sample(x,1)
}
#' @title ul
#' @export
ul <- function(x){
  length(unique(x))
}
#' @title wl
#' @export
wl <- function(x){
  length(which(x))
}
#' @title dwl
#' @export
dwl <- function(x){
  length(which(duplicated(x)))
}
#' @title dw
#' @export
dw <- function(x){
  which(duplicated(x))
}
#' @title drop_nas
#' @export
drop_nas <- function(x) {
  x[!sapply(x, is.na)]
}
#' @title vec_which_duplicated
#' @export
vec_which_duplicated <- function(vec){
  vec[which(duplicated(vec))]
}
#' @title clean_num
#' @export
clean_num<-function(num){
  formatC(num, format="d", big.mark=",")
}
#' @title size
#' @export
size <- function(x){
  format(object.size(x),units = "auto")
}
# packages
#' @title combine_R_files
#' @param source_dir a file path for your source (such as R folder)
#' @param destination_dir a file path for your destination (such as dev folder)
#' @param filename a file name (ends in .R)
#' @param header_symbol single character for your header separator in combined file
#' @param max_new_lines integer for max number of new lines
#' @param new_lines characeter vector for new lines
#' @return message
#' @export
combine_R_files <- function(source_dir= file.path(getwd(),"R"), destination_dir=file.path(getwd(),"dev"),filename="combined.R",header_symbol = "=",max_new_lines=0,new_lines=character(0)) {
  file_list <- list.files(source_dir, pattern = "\\.R$", full.names = TRUE)
  combined_text <- character(0)
  for (file in file_list) {# file <- file_list %>% sample(1)
    file_name <- tools::file_path_sans_ext(basename(file))
    header <- paste0("# ", file_name, " ")
    header <- paste0(header,  paste0(rep(header_symbol,80-nchar(header)), collapse=""))
    combined_text <- c(combined_text, header,new_lines, readLines(file))
  }
  message(length(combined_text)," lines")
  combined_text <-paste(combined_text, collapse = "\n")
  combined_text <- gsub(paste0("\\n{",max_new_lines+2,",}"), "\n", combined_text)
  destination_file <- file.path(destination_dir, filename)
  writeLines(combined_text, destination_file)
  cat("Combined file saved to:", destination_file, "\n")
}
#' @title split_R_files
#' @inheritParams combine_R_files
#' @return message
#' @export
split_R_files <- function(source_dir= file.path(getwd(),"dev"), destination_dir=file.path(getwd(),"R"),filename = "combined.R",header_symbol = "=",new_lines=character(0)){
  file_content <- readLines(file.path(source_dir,filename))
  split_indices <- grep(paste0("^# .* ",paste0(rep(header_symbol,4),collapse=""), collapse=""), file_content)
  split_indices <- as.list(split_indices)
  scripts <- NULL
  while (length(split_indices)>0) {
    start_index <- split_indices[[1]]+1
    if(length(split_indices)==1){
      end_index <- length(file_content)
    }else{
      end_index <- split_indices[[2]]-1
    }
    scripts[[gsub(paste0("#| |",header_symbol), "", file_content[split_indices[[1]]])]] <- file_content[start_index:end_index]
    split_indices[[1]] <- NULL
  }
  for(i in seq_along(scripts)){
    output_file <- file.path(destination_dir, paste0(names(scripts)[i], ".R"))
    writeLines(
      new_lines %>% append(scripts[[i]]) %>%paste0(collapse = "\n"),
      con = output_file
    )
    cat("File saved:", output_file, "\n")
  }
}
#' @title warn_or_stop
#' @param m message character string
#' @param warn_only logical for only warn
#' @return message
#' @export
warn_or_stop <- function(m,warn_only=F){
  if(warn_only)return(warning(m,immediate. = T))
  return(stop(m))
}
# utils-pipe
#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
# validation
#' @title is_something
#' @export
is_something <- function(thing,row=0){
  out <- FALSE
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
              out <-TRUE
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
#' @title is_nested_list
#' @export
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
#' @title is_named_list
#' @export
is_named_list <- function(x,silent =T,recursive = F) {
  if (!is.list(x))return(FALSE)
  if (is.null(names(x)))return(FALSE)
  named_all <- TRUE
  if(recursive){
    for (n in names(x)) {
      element <- x[[n]]
      if (is.list(element)) {
        named_all <- named_all && is_named_list_all(element)
        if(!silent&&!named_all)message("'",n, "' is not named")
      }
    }
  }
  return(named_all)  # Return the result
}
#' @title is_df_list
#' @export
is_df_list <- function(x,strict=F){
  if (!is.list(x)) return(FALSE)
  if (length(x)==0) return(FALSE)
  if (is_nested_list(x)) return(FALSE)
  out <- sapply(x,is.data.frame)
  if(strict)return(all(out))
  return(any(out))
}
#' @title is_named_df_list
#' @export
is_named_df_list <- function(x,strict = F){
  is_named_list(x) && is_df_list(x,strict = strict)
}
#' @title is_env_name
#' @export
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
#' @title is_web_link
#' @export
is_web_link <- function(link, silent = FALSE,strict = FALSE) {
  result <- tryCatch({
    if(is.null(link)) stop("link is NULL")
    # Check if the link starts with "https://" or "http://"
    if (!grepl("^https?://", link)) {
      stop("Invalid web link. It must start with 'http://' or 'https://'.")
    }
    # Remove trailing slash if present
    link <- gsub("/$", "", link)
    # Check if the link ends with one of the specified web endings
    allowed_endings <- c('edu','com','org','net','gov','io','xyz','info','co','uk')
    searching <- paste0("\\.(",paste0(allowed_endings,collapse = "|"),")",ifelse(strict,"$",""))
    if (!grepl(searching, link)) {
      stop("Invalid web link. It must end with a valid web ending (.edu, .com, etc.).")
    }
    return(TRUE)  # Return TRUE if all checks pass
  }, error = function(e) {
    if(!silent) message(e$message)
    return(FALSE)  # Return FALSE if any error occurs
  })
  return(result)
}
#' @title is_consecutive_srt_1
#' @export
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
