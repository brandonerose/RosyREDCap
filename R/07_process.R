#' @import RosyUtils
#' @import RosyDB
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
  names(out_list) <- DB$metadata$forms$instrument_name
  return(out_list)
}
raw_process_redcap <- function(raw,DB, labelled){
  if(nrow(raw)>0){
    raw  <- raw %>% all_character_cols()
    add_ons <- c(DB$redcap$id_col,"arm_num","event_name","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")
    if(DB$redcap$is_longitudinal){
      raw$id_temp <- 1:nrow(raw)
      raw <-  merge(raw,DB$metadata$events[,c("arm_num","event_name","unique_event_name")],by.x="redcap_event_name",by.y="unique_event_name",sort = F)
      add_ons  <- add_ons[which(add_ons%in%colnames(raw))]
      cols <- c(add_ons, colnames(raw)) %>% unique()
      raw <- raw[order(raw$id_temp),cols%>% sapply(function(c){which(colnames(raw)==c)}) %>% as.integer()]
      raw$id_temp <- NULL
    }
    add_ons  <- add_ons[which(add_ons%in%colnames(raw))]
    if(any(!DB$redcap$raw_structure_cols %in% colnames(raw)))stop("raw is missing one of the following... and that's weird: ", DB$redcap$raw_structure_cols %>% paste0(collapse = ", "))
    instrument_names <- DB$metadata$forms$instrument_name[which(DB$metadata$forms$instrument_name%in%unique(DB$metadata$fields$form_name))]
    data_list <- list()
    # instrument_name <- instrument_names %>% sample1()
    for(instrument_name in instrument_names){
      add_ons_x <- add_ons
      #instrument_name <-  DB$metadata$forms$instrument_name %>% sample(1)
      is_repeating_instrument <- instrument_name%in%DB$metadata$forms$instrument_name[which(DB$metadata$forms$repeating)]
      rows  <- 1:nrow(raw)
      if(!DB$redcap$is_longitudinal){
        if("redcap_repeat_instrument"%in%colnames(raw)){
          if(is_repeating_instrument){
            rows <- which(raw$redcap_repeat_instrument==instrument_name)
          }
          if(!is_repeating_instrument){
            rows <- which(is.na(raw$redcap_repeat_instrument))
          }
        }
      }
      if(DB$redcap$is_longitudinal){
        events_ins <- DB$metadata$event_mapping$unique_event_name[which(DB$metadata$event_mapping$form==instrument_name)] %>% unique()
        rows <- which(raw$redcap_event_name%in%events_ins)
      }
      if(!is_repeating_instrument){
        add_ons_x <- add_ons_x[which(!add_ons_x%in%c("redcap_repeat_instrument","redcap_repeat_instance"))]
      }
      if(is_something(rows)){
        cols <- unique(c(add_ons_x,DB$metadata$fields$field_name[which(DB$metadata$fields$form_name==instrument_name&DB$metadata$fields$field_name%in%colnames(raw))]))
        raw_subset <- raw[rows,cols]
        if(labelled){
          raw_subset <- raw_to_labelled_form(FORM = raw_subset, DB=DB)
        }
        data_list[[instrument_name]] <- raw_subset
      }
    }
  }
  return(data_list)
}
#' @title Select REDCap records from DB
#' @inheritParams save_DB
#' @param records character vector of the IDs you want to filter the DB by
#' @param data_choice character vector of the IDs you want to filter the DB by
#' @param field_names character vector of field_names to be included
#' @param form_names character vector of form_names to be included
#' @param add_filter_var character string of extra variable name to be filtered by if present in a data.frame
#' @param add_filter_vals character vector of extra variable values to be filtered by if present in a data.frame
#' @param warn_only logical for warn_only or stop
#' @return DB object that has been filtered to only include the specified records
#' @export
filter_DB <- function(DB, records,data_choice="data",field_names,form_names,add_filter_var,add_filter_vals,warn_only = F){#, ignore_incomplete=F, ignore_unverified = F
  if(missing(records)) records <- DB$summary$all_records[[DB$redcap$id_col]]
  if(is.null(records)) records <- DB$summary$all_records[[DB$redcap$id_col]]
  if(missing(field_names)){
    field_names <- DB %>% get_all_field_names(data_choice = data_choice)
  }
  if(missing(form_names))form_names <- names(DB$data)
  if (length(records)==0)stop("Must supply records")
  selected <- list()
  BAD  <- records[which(!records%in%DB$summary$all_records[[DB$redcap$id_col]])]
  GOOD  <- records[which(records%in%DB$summary$all_records[[DB$redcap$id_col]])]
  if(length(BAD)>0){
    m <- paste0("Following records are not found in DB: ", BAD %>% paste0(collapse = ", "))
    warn_or_stop(m,warn_only = warn_only)
  }
  run_add_filter <- !missing(add_filter_var)&&!missing(add_filter_vals)
  for(FORM in form_names){
    OUT <- DB$data[[FORM]][which(DB$data[[FORM]][[DB$redcap$id_col]]%in%GOOD),]
    cols <- colnames(OUT)[which(colnames(OUT)%in%field_names)]
    if(length(cols)>0){
      if(run_add_filter){
        if(add_filter_var %in% colnames(OUT)){
          OUT <-  OUT[which(OUT[[add_filter_var]] %in% add_filter_vals),]
        }
      }
      # if(nrow(OUT)>0){
      selected[[FORM]] <- OUT[,colnames(OUT)[which(colnames(OUT)%in%c(DB$redcap$raw_structure_cols,field_names))]]
      # }
    }
  }
  return(selected)
}
#' @title field_names_to_instruments
#' @inheritParams save_DB
#' @param only_unique logical for unique
#' @return instrument names
#' @export
field_names_to_instruments <- function(DB,field_names,only_unique = T){
  instruments <- DB$metadata$fields$form_name[match(field_names, DB$metadata$fields$field_name)]
  if(only_unique)instruments <- unique(instruments)
  return(instruments)
}
field_names_metadata <- function(DB,field_names,col_names){
  # if(!deparse(substitute(FORM))%in%DB$metadata$forms$instrument_name)stop("To avoid potential issues the form name should match one of the instrument names" )
  BAD <- field_names[which(!field_names%in%c(DB$metadata$fields$field_name,DB$redcap$raw_structure_cols,"arm_num","event_name"))]
  if(length(BAD)>0)stop("All column names in your form must match items in your metadata, `DB$metadata$fields$field_name`... ", paste0(BAD, collapse = ", "))
  # metadata <- DB$metadata$fields[which(DB$metadata$fields$form_name%in%instruments),]
  metadata <- DB$metadata$fields[which(DB$metadata$fields$field_name%in%field_names),]
  # metadata <- metadata[which(metadata$field_name%in%field_names),]
  if( ! missing(col_names)){
    if(is_something(col_names))metadata <- metadata[[col_names]]
  }
  return(metadata)
}
filter_metadata_from_form <- function(FORM,DB){
  instruments <- DB %>% field_names_to_instruments(field_names = colnames(FORM))
  if(any(instruments%in%DB$metadata$forms$repeating))stop("All column names in your form must match only one form in your metadata, `DB$metadata$forms$instrument_name`, unless they are all non-repeating")
  metadata <- DB %>% field_names_metadata(field_names = colnames(FORM))
  metadata <- metadata[which(metadata$field_type!="descriptive"),]
  metadata$has_choices <- !is.na(metadata$select_choices_or_calculations)
  return(metadata)
}
instruments_to_field_names <- function(instruments,DB){
  field_names <- NULL
  for(instrument in instruments){
    field_names <- field_names %>% append(DB$metadata$fields$field_name[which(DB$metadata$fields$form_name==instrument)])
  }
  return(unique(field_names))
}
#' @title Clean to Raw REDCap forms
#' @inheritParams save_DB
#' @param FORM data.frame of labelled REDCap to be converted to raw REDCap (for uploads)
#' @return DB object that has been filtered to only include the specified records
#' @export
labelled_to_raw_form <- function(FORM,DB){
  use_missing_codes <- is.data.frame(DB$metadata$missing_codes)
  metadata <- filter_metadata_from_form(FORM = FORM,DB = DB)
  for(i in 1:nrow(metadata)){ # i <-  1:nrow(metadata) %>% sample(1)
    COL_NAME <- metadata$field_name[i]
    has_choices <- metadata$has_choices[i]
    if(has_choices){
      z <- metadata$select_choices_or_calculations[i] %>% split_choices()
      FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
        OUT <- NA
        if(!is.na(C)){
          coded_redcap <- which(z$name==C)
          if(length(coded_redcap)>0){
            OUT <- z$code[coded_redcap]
          }else{
            if(use_missing_codes){
              coded_redcap2 <- which(DB$metadata$missing_codes$name==C)
              if(length(coded_redcap2)>0){
                OUT <- DB$metadata$missing_codes$code[coded_redcap2]
              }else{
                stop("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C)
              }
            }else{
              stop("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()
    }else{
      if(use_missing_codes){
        FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
          OUT <- C
          if(!is.na(C)){
            D <- which(DB$metadata$missing_codes$name==C)
            if(length(D)>0){
              OUT <- DB$metadata$missing_codes$code[D]
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }
    }
  }
  FORM
}
#' @title Raw to Labelled REDCap forms
#' @inheritParams save_DB
#' @param FORM data.frame of raw REDCap to be converted to labelled REDCap
#' @return DB object
#' @export
raw_to_labelled_form <- function(FORM,DB){
  if(nrow(FORM)>0){
    use_missing_codes <- is.data.frame(DB$metadata$missing_codes)
    metadata <- filter_metadata_from_form(FORM = FORM,DB = DB)
    for(i in 1:nrow(metadata)){ # i <-  1:nrow(metadata) %>% sample(1)
      COL_NAME <- metadata$field_name[i]
      has_choices <- metadata$has_choices[i]
      if(has_choices){
        z <- metadata$select_choices_or_calculations[i] %>% split_choices()
        FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
          OUT <- NA
          if(!is.na(C)){
            coded_redcap <- which(z$code==C)
            if(length(coded_redcap)>0){
              OUT <- z$name[coded_redcap]
            }else{
              if(use_missing_codes){
                coded_redcap2 <- which(DB$metadata$missing_codes$code==C)
                if(length(coded_redcap2)>0){
                  OUT <- DB$metadata$missing_codes$name[coded_redcap2]
                }else{
                  warning("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
                }
              }else{
                warning("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C)
              }
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }else{
        if(use_missing_codes){
          z <- DB$metadata$missing_codes
          FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
            OUT <- C
            if(!is.na(C)){
              D <- which(z$code==C)
              if(length(D)>0){
                OUT <- z$name[D]
              }
            }
            OUT
          }) %>% unlist() %>% as.character()
        }
      }
    }
  }
  FORM
}
labelled_to_raw_DB <- function(DB){
  DB <- validate_RosyREDCap(DB)
  if(!DB$internals$data_extract_labelled)stop("DB is already raw/coded (not labelled values)")
  for(TABLE in names(DB$data)){
    DB$data[[TABLE]] <- labelled_to_raw_form(FORM = DB$data[[TABLE]],DB=DB)
  }
  DB$internals$data_extract_labelled <- F
  DB
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
  if(purge_api){
    log <- log[which(!log$details%in%c("Export Logging (API)","Export REDCap version (API)","export_format: CSV, rawOrLabel: raw", "Download data dictionary (API)")),]
    log <- log[which(!startsWith(log$details,"Export ")),]
    log <- log[which(!startsWith(log$details,"Delete file from ")),]
    log <- log[which(!startsWith(log$details,"Upload file to ")),]
    log <- log[which(!startsWith(log$details,"export_format")),]
    log <- log[which(!startsWith(log$details,"Switch DAG ")),]
    log <- log[which(!startsWith(log$details,"Reorder project fields")),]
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
merge_instruments <- function(instruments,DB,data_choice = "data", exact = T){
  DB <- validate_RosyREDCap(DB)
  old_list <- DB$data
  old <- list()
  if(!is_something(old_list)){
    message("old_list is empty")
    return(list())
  }
  n_instruments <- length(instruments)
  if(n_instruments>0){
    if(any(!instruments%in%DB$metadata$forms$instrument_name))stop("All instruments must be included in set of DB$metadata$forms$instrument_name")
    old <- old_list[[instruments[[1]]]]
    if(n_instruments>1){
      # if(any(DB$metadata$forms$repeating[which(DB$metadata$forms$instrument_name%in%instruments)])){
      #   stop("Your upload form (",TABLE,") contains field names from more than one repeating instrument! This is not allowed.")
      # }
      all_ref_cols <- DB$metadata$form_key_cols[instruments] %>% unlist() %>% unique()
      for(i in 2:length(instruments)){
        ref_cols <- DB$metadata$form_key_cols[instruments[[i]]]%>% unlist()
        if(!exact){
          ref_cols <- ref_cols %>% vec1_in_vec2(all_ref_cols)
        }
        old <- merge(old,old_list[[instruments[[i]]]],by=ref_cols)
      }
    }
  }
  return(old)
}
#' @title Merge non-repeating, not ready for multi-event projects
#' @inheritParams save_DB
#' @return DB object that has merged all non repeating forms
#' @export
merge_non_repeating_DB <- function(DB){ # need to adjust for events, currently destructive
  if(DB$internals$data_extract_merged)stop("Already merged!")
  data_choice <- "data"
  all_instrument_names <- DB$metadata$forms$instrument_name
  keep_instruments <- NULL
  instrument_names <- DB$metadata$forms$instrument_name[which(!DB$metadata$forms$repeating)]
  if(DB$redcap$is_longitudinal){
    instrument_names <- DB$metadata$forms$instrument_name[which(!DB$metadata$forms$repeating&!DB$metadata$forms$repeating_via_events)]
    keep_instruments <- all_instrument_names[which(!all_instrument_names%in% instrument_names)]
    data_choice <- "data_transform"
  }
  DB$data[[DB$internals$merge_form_name]] <- merge_multiple(DB$data,instrument_names)
  if(data_choice=="data") {
    for(instrument_name in instrument_names){
      DB[["data"]][[instrument_name]] <- NULL
    }
    DB$internals$data_extract_merged <- T
  }else{
    for(keep in keep_instruments){
      DB[["data_transform"]][[keep]] <- DB[["data"]][[keep]]
    }
  }
  DB
}
#' @title merge_multiple
#' @export
merge_multiple <- function(named_data_list,instrument_names){
  instrument_names <- instrument_names %>% as.list()
  if (length(instrument_names)==1) warning('No need to merge you only have one form that is non-repeating')
  merged <- named_data_list[[instrument_names[[1]]]]
  merged$redcap_event_name <- NULL
  # merged$arm_num <- NULL
  merged$event_name <- NULL
  merged$redcap_repeat_instrument <- NULL
  merged$redcap_repeat_instance <- NULL
  instrument_names[[1]] <- NULL
  while (length(instrument_names)>0) {
    dfx <- named_data_list[[instrument_names[[1]]]]
    dfx$redcap_event_name <- NULL
    # dfx$arm_num <- NULL
    dfx$event_name <- NULL
    dfx$redcap_repeat_instrument <- NULL
    dfx$redcap_repeat_instance <- NULL
    (in_common <- colnames(merged)[which(colnames(merged)%in%colnames(dfx))])
    merged <- merge(merged,dfx,by=in_common,all = T)
    instrument_names[[1]] <- NULL
  }
  merged
}
#' @title Unmerge non-repeating, not ready for multi-event projects
#' @inheritParams save_DB
#' @return DB object that has merged all non repeating forms
#' @export
unmerge_non_repeating_DB <- function(DB){
  if(!DB$internals$data_extract_merged)stop("No DB$data named as 'merged'!")
  instrument_names <- DB$data[[DB$internals$merge_form_name]] %>% colnames() %>% sapply(function(COL){DB$metadata$fields$form_name[which(DB$metadata$fields$field_name==COL)]}) %>% unique() %>% as.list()
  merged <- DB$data[[DB$internals$merge_form_name]]
  while (length(instrument_names)>0) {
    instrument_name  <- instrument_names[[1]]
    DB$data[[instrument_name]] <- merged[,unique(c(DB$redcap$id_col,DB$metadata$fields$field_name[which(DB$metadata$fields$form_name==instrument_name&DB$metadata$fields$field_name%in%colnames(merged))]))]
    instrument_names[[1]] <- NULL
  }
  DB$data[[DB$internals$merge_form_name]] <- NULL
  DB$internals$data_extract_merged <- F
  DB
}
#' @title add REDCap ID to any dataframe using a ref_id
#' @description
#'  add REDCap ID to any dataframe using a ref_id
#' @param DF dataframe
#' @inheritParams save_DB
#' @param ref_id column name that matches a REDCap variable name that could be an ALT id such as MRN
#' @return original dataframe with REDCap id_col added as the first column
#' @export
add_ID_to_DF <- function(DF,DB,ref_id){
  if(!ref_id%in%DB$metadata$fields$field_name)stop("The ref_id not valid. Must be a REDCap raw colname")
  form <- DB$metadata$fields$form_name[which(DB$metadata$fields$field_name==ref_id)]
  # if(DB$internals$data_extract_merged){
  #   if(form %in% DB$metadata$forms$instrument_name[which(!DB$metadata$forms$repeating)]){
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
#' @title Deidentify the REDCap DB according to REDCap or your choices
#' @inheritParams save_DB
#' @param identifiers optional character vector of column names that should be excluded from DB. Otherwise `DB$metadata$fields$identifier =="y` will be used.
#' @return DB object that has deidentified forms
#' @export
deidentify_DB <- function(DB,identifiers,drop_free_text = F){
  DB <- validate_RosyREDCap(DB)
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
  for (data_choice in c("data","data_transform","data_upload")){
    if(is_something(DB$data)){
      drop_list <- Map(function(NAME, COLS) {identifiers[which(identifiers %in% COLS)]},names(DB$data), lapply(DB$data, colnames))
      drop_list <- drop_list[sapply(drop_list, length) > 0]
      if(length(drop_list)==0)message("Nothing to deidentify from --> ",identifiers %>% paste0(collapse = ", "))
      for (FORM in names(drop_list)) {
        for(DROP in drop_list[[FORM]]){
          DB$data[[FORM]][[DROP]] <- NULL
          message("Dropped '",DROP,"' from '",data_choice,"' --> '", FORM,"'")
        }
      }
    }
  }
  return(DB)
}
construct_header_list <- function(df_list,md_elements = c("form_name","field_type","field_label"),metadata = get_default_metadata(DB)){
  if(anyDuplicated(metadata$field_name)>0)stop("dup names not allowed in metadata")
  df_col_list <- df_list %>% lapply(colnames)
  header_df_list <- df_col_list %>% lapply(function(field_names){
    x<- field_names%>% lapply(function(field_name){
      row <- which(metadata$field_name==field_name)
      if(length(row)>0){
        return(as.character(metadata[md_elements][row,]))
      }else{
        return(rep("",length(md_elements)))
      }
    }) %>% as.data.frame()
    colnames(x)<-field_names
    x<- x[which(apply(x, 1, function(row){any(row!="")})),]
    x
  })
  return(header_df_list)
}
construct_key_col_list <- function(DB,data_choice=get_default_data_choice(DB)){
  metadata <- get_default_metadata(DB)
  df_list <- DB$data
  df_col_list <- df_list %>% lapply(colnames)
  forms <- names(df_list)
  key_cols_list <- forms %>% lapply(function(form){
    df_col_list[[form]][which(df_col_list[[form]]%in%DB$redcap$raw_structure_cols)]
  })
  names(key_cols_list)<- forms
  return(key_cols_list)
}
