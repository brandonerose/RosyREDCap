#' @import RosyUtils
#' @import RosyApp
get_key_col_list <- function(DB){
  if(!is_something(DB$metadata$forms))stop("Empty --> `DB$metadata$forms`")
  out_list <- 1:nrow(DB$metadata$forms) %>% lapply(function(i){
    out <- DB$redcap$id_col
    if(DB$redcap$is_longitudinal)out <- append(out,"redcap_event_name")
    if(DB$metadata$forms$repeating[i]){
      out <- append(out,"redcap_repeat_instrument")
      out <- append(out,"redcap_repeat_instance")
    }
    return(out)
  })
  names(out_list) <- DB$metadata$forms$form_name
  return(out_list)
}
raw_process_redcap <- function(raw,DB, labelled){
  # key_cols <-DB$redcap$raw_structure_cols
  # key_cols <- key_cols[which(!key_cols%in%c("arm_num","event_name"))]
  # paste0(raw[[DB$redcap$id_col]],"_",raw$redcap_event_name,"_",raw$redcap_repeat_instrument,"_",raw$redcap_repeat_instance)
  forms <- get_original_forms(DB)
  fields <- get_original_fields(DB)
  # arms <- DB$metadata$arms
  events <- DB$metadata$events
  event_mapping <- DB$metadata$event_mapping
  data_list <- list()
  if(nrow(raw)>0){
    raw  <- raw %>% all_character_cols()
    add_ons <- c(DB$redcap$id_col,"arm_num","event_name","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")
    if(DB$redcap$is_longitudinal){
      raw$id_temp <- 1:nrow(raw)
      raw <-  merge(raw,DB$metadata$events[,c("arm_num","event_name","unique_event_name")],by.x="redcap_event_name",by.y="unique_event_name",sort = F,all.x = T)
      add_ons  <- add_ons[which(add_ons%in%colnames(raw))]
      cols <- c(add_ons, colnames(raw)) %>% unique()
      raw <- raw[order(raw$id_temp),cols%>% sapply(function(c){which(colnames(raw)==c)}) %>% as.integer()]
      raw$id_temp <- NULL
    }
    add_ons  <- add_ons[which(add_ons%in%colnames(raw))]
    if(any(!DB$redcap$raw_structure_cols %in% colnames(raw)))stop("raw is missing one of the following... and that's weird: ", DB$redcap$raw_structure_cols %>% paste0(collapse = ", "))
    form_names <- forms$form_name[which(forms$form_name%in%unique(fields$form_name))]
    # form_name <- form_names %>% sample1()
    has_repeating_forms <- DB$redcap$has_repeating_forms
    for(form_name in form_names){
      add_ons_x <- add_ons
      #form_name <-  forms$form_name %>% sample(1)
      is_repeating_form <- form_name%in%forms$form_name[which(forms$repeating)]
      is_longitudinal <- DB$redcap$is_longitudinal
      rows  <- 1:nrow(raw)
      if(is_repeating_form){
        if(!"redcap_repeat_instrument"%in%colnames(raw))stop("redcap_repeat_instrument not in colnames(raw)")
        if(is_longitudinal){
          # rows <- which(raw$redcap_repeat_instrument==form_name)
          rows <- which(raw$redcap_repeat_instrument==form_name|raw$redcap_event_name%in%event_mapping$unique_event_name[which(!event_mapping$repeating&event_mapping$form==form_name)])
        }else{
          rows <- which(raw$redcap_repeat_instrument==form_name)
        }
      }else{
        add_ons_x <- add_ons_x[which(!add_ons_x%in%c("redcap_repeat_instrument","redcap_repeat_instance"))]
        if(is_longitudinal){
          rows <- which(raw$redcap_event_name%in%unique(event_mapping$unique_event_name[which(event_mapping$form==form_name)]))
        }else{
          if(has_repeating_forms) rows <- which(is.na(raw$redcap_repeat_instrument))
        }
      }
      if(is_something(rows)){
        cols <- unique(c(add_ons_x,fields$field_name[which(fields$form_name==form_name&fields$field_name%in%colnames(raw))]))
        raw_subset <- raw[rows,cols]
        if(labelled){
          raw_subset <- raw_to_labelled_form(FORM = raw_subset, DB=DB)
        }
        data_list[[form_name]] <- raw_subset
      }
    }
  }
  return(data_list)
}
sort_redcap_log <- function(log){
  log[order(log$timestamp,decreasing = T),]
}
clean_redcap_log <- function(log,purge_api=T){
  log$record_id <- log$action %>% sapply(function(A){ifelse(grepl("Update record |Delete record |Create record ",A),gsub("Update record|Delete record|Create record|[:(:]API[:):]|Auto|calculation| |[:):]|[:(:]","",A),NA)})
  log$action_type <- log$action %>% sapply(function(A){ifelse(grepl("Update record |Delete record |Create record ",A),(A %>% strsplit(" ") %>% unlist())[1],NA)})
  comments <- which(log$action=="Manage/Design"&grepl("Add field comment|Edit field comment|Delete field comment",log$details))
  if(length(comments)>0){
    log$record_id[comments] <- stringr::str_extract(log$details[comments], "(?<=Record: )[^,]+")
    log$action_type[comments] <- "Comment"
  }
  rows <- which(is.na(log$record)&!is.na(log$record_id))
  log$record[rows] <- log$record_id[rows]
  rows <- which(!is.na(log$record)&is.na(log$record_id))
  log$action_type[rows] <- "Users"
  log$record_id <- NULL
  # rows <- which(!is.na(log$record)&is.na(log$record_id))
  # log$record_id[rows] <- log$record[rows]
  log$details <- gsub("[[:cntrl:]]", "", log$details)
  if(purge_api){
    log <- log[which(!log$details%in%c("Export Logging (API)","Export REDCap version (API)","export_format: CSV, rawOrLabel: raw", "Download data dictionary (API)")),]
    log <- log[which(!startsWith(log$details,"Export ")),]
    log <- log[which(!startsWith(log$details,"Delete file from ")),]
    log <- log[which(!startsWith(log$details,"Upload file to ")),]
    log <- log[which(!startsWith(log$details,"export_format")),]
    log <- log[which(!startsWith(log$details,"Switch DAG ")),]
    log <- log[which(!startsWith(log$details,"Reorder project fields")),]
    log <- log[which(!startsWith(log$details,"Download ")),]
  }
  log <- sort_redcap_log(log)
  return(log)
}
all_missing_codes <- function(){
  data.frame(
    code = c(
      'NI',
      'INV',
      'UNK',
      'NASK',
      'ASKU',
      'NAV',
      'MSK',
      'NA',
      'NAVU',
      'NP',
      'QS',
      'QI',
      'TRC',
      'UNC',
      'DER',
      'PINF',
      'NINF',
      'OTH'
    ),
    name = c(
      'No information',
      'Invalid',
      'Unknown',
      'Not asked',
      'Asked but unknown',
      'Temporarily unavailable',
      'Masked',
      'Not applicable',
      'Not available',
      'Not present',
      'Sufficient quantity',
      'Insufficient quantity',
      'Trace',
      'Unencoded',
      'Derived',
      'Positive infinity',
      'Negative infinity',
      'Other'
    )
  )
}
missing_codes2 <- function(DB){
  included <- "missing_data_codes"%in%colnames(DB$redcap$project_info)
  if(included){
    is_na  <- is.na(DB$redcap$project_info$missing_data_codes)
    if(!is_na){
      return(DB$redcap$project_info$missing_data_codes %>% split_choices())
    }
    if(is_na){
      return(NA)
    }
  }
  if(!included){
    return(NA)
  }
}
#' @title add REDCap ID to any dataframe using a ref_id
#' @description
#'  add REDCap ID to any dataframe using a ref_id
#' @inheritParams save_DB
#' @param DF dataframe
#' @param ref_id column name that matches a REDCap variable name that could be an ALT id such as MRN
#' @return original dataframe with REDCap id_col added as the first column
#' @export
add_ID_to_DF <- function(DF,DB,ref_id){
  if(!ref_id%in%DB$metadata$fields$field_name)stop("The ref_id not valid. Must be a REDCap raw colname")
  form <- DB$metadata$fields$form_name[which(DB$metadata$fields$field_name==ref_id)]
  # if(DB$internals$data_extract_merged){
  #   if(form %in% DB$metadata$forms$form_name[which(!DB$metadata$forms$repeating)]){
  #     form <- DB$internals$merge_form_name
  #   }
  # }
  id_col <- DF[[ref_id]] %>% sapply(function(ID){
    DB$data[[form]][[DB$redcap$id_col]][which(DB$data[[form]][[ref_id]]==ID)]
  }) %>% as.data.frame()
  colnames(id_col) <- DB$redcap$id_col
  DF <- cbind(id_col,DF)
  DF
}
#' @title Deidentify the REDCap Database
#' @description
#' Removes or masks identifying information from the REDCap database (`DB`). This can be done either based on the `identifier` field in the metadata or by specifying custom identifiers.
#'
#' @inheritParams save_DB
#' @param identifiers Optional character vector of column names that should be excluded from the `DB`. If not provided, fields where `DB$metadata$fields$identifier == "y"` will be used as the default.
#' @param drop_free_text Logical. If `TRUE`, columns containing free text will also be excluded from the `DB`. Default is `FALSE`.
#'
#' @return
#' A `DB` object with deidentified forms.
#'
#' @details
#' This function modifies the `DB` object to exclude specified identifiers or any columns flagged as identifiers in the metadata. Free-text fields can also be optionally removed, ensuring the resulting dataset complies with deidentification standards.
#'
#' @seealso
#' \code{\link{save_DB}} for saving the modified database.
#'
#' @export
deidentify_DB <- function(DB,identifiers,drop_free_text = F){
  DB <- validate_DB(DB)
  missing_identifiers <- missing(identifiers)
  if(!missing_identifiers){
    identifiers <- identifiers %>% unique()
    bad_identifiers <- identifiers[which(!identifiers%in%DB$metadata$fields$field_name)]
    if(length(bad_identifiers)>0)stop("You have an identifier that is not included in the set of `DB$metadata$fields$field_name` --> ",bad_identifiers %>% paste0(collapse = ", "))
    if(DB$redcap$id_col%in%identifiers)stop("Your REDCap ID, ",DB$redcap$id_col,", should not be deidentified.") #If you want to pass a new set of random IDs to make this data use `scramble_ID_DB(DB)`.")
  }
  if(missing_identifiers){
    identifiers <-  DB$metadata$fields$field_name[which(DB$metadata$fields$identifier=="y")]
    if(length(identifiers)==0)warning("You have no identifiers marked in `DB$metadata$fields$identifier`. You can set it in REDCap Project Setup and update DB OR define your idenitifiers in this functions `identifiers` argument." ,immediate. = T)
  }
  if(drop_free_text){ # placeholder
    identifiers <- identifiers %>%
      append(
        DB$metadata$fields$field_name[which(DB$metadata$fields$field_type=="notes")]
      ) %>% unique()
  }
  if(is_something(DB$data)){
    drop_list <- Map(function(NAME, COLS) {identifiers[which(identifiers %in% COLS)]},names(DB$data), lapply(DB$data, colnames))
    drop_list <- drop_list[sapply(drop_list, length) > 0]
    if(length(drop_list)==0){
      bullet_in_console(paste0("Nothing to deidentify from --> ",identifiers %>% paste0(collapse = ", ")),bullet_type = "x")
    }else{
      bullet_in_console(paste0("Deidentified ",DB$short_name),bullet_type = "v")
    }
    for (FORM in names(drop_list)) {
      for(DROP in drop_list[[FORM]]){
        DB$data[[FORM]][[DROP]] <- NULL
        # message("Dropped '",DROP,"' from '",data_choice,"' --> '", FORM,"'")
      }
    }
  }
  return(DB)
}
construct_key_col_list <- function(DB){
  # fields <- DB$metadata$fields
  df_list <- DB$data
  df_col_list <- df_list %>% lapply(colnames)
  forms <- names(df_list)
  key_cols_list <- forms %>% lapply(function(form){
    df_col_list[[form]][which(df_col_list[[form]]%in%DB$redcap$raw_structure_cols)]
  })
  names(key_cols_list)<- forms
  return(key_cols_list)
}
