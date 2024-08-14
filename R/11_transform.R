#' @import RosyUtils
remap_process <- function(DB){
  DB <- validate_DB(DB)
  if(is.data.frame(DB$redcap$metadata)){
    metadata_remap <- DB$remap$metadata_remap
    BAD <- DB$redcap$metadata$field_name[which(!DB$redcap$metadata$field_name%in%metadata_remap$field_name)]
    if(length(BAD)>0)stop("Missing mappings: ",BAD %>% paste0(collapse = ", "))
    x<-which(!metadata_remap$field_name%in%DB$redcap$metadata$field_name)
    if(length(x)>0)stop("remap missing variable: ",paste0(x, collapse = ", "))
    x<- 1:nrow(metadata_remap) %>% sapply(function(i){DB$redcap$metadata$select_choices_or_calculations[which(DB$redcap$metadata$field_name==metadata_remap$field_name[i])]!=metadata_remap$select_choices_or_calculations[i]}) %>% which()
    if(length(x)>0)stop("remap with differing choices: ",paste0(x, collapse = ", "))
    instruments_remap <- DB$redcap$instruments
    instruments_remap$instrument_name_remap <- instruments_remap$instrument_name %>% sapply(function(instrument_name){
      x<-metadata_remap$form_name_remap[which(metadata_remap$form_name==instrument_name)] %>% unique()
      if(length(x)>1)stop("instrument_names cannot match multiple instrument_name_remaps: ",instrument_name)
      return(x)
    })
    if(DB$redcap$is_longitudinal){
      if(DB$redcap$has_arms_that_matter){
        #can add remapping of arms but not smart if they matter
      }else{
        # DB$redcap$events->x
        event_mapping_remap <- DB$remap$event_mapping_remap
        events_remap <- DB$redcap$events
        events_remap$unique_event_name_remap <- events_remap$unique_event_name %>% sapply(function(unique_event_name){
          x<-event_mapping_remap$unique_event_name_remap[which(event_mapping_remap$unique_event_name==unique_event_name)] %>% unique()
          if(length(x)>1)stop("unique_event_names cannot match multiple unique_event_name_remaps: ",unique_event_name)
          return(x)
        })
        events_new <- events_remap[,c("unique_event_name_remap","event_name")] %>% unique()
        colnames(events_new)[1] <- "unique_event_name"
        event_mapping_new <- event_mapping_remap[,c("unique_event_name_remap","event_name","form")] %>% unique()
        event_mapping_new$form <- event_mapping_new$form %>% sapply(function(instrument_name){
          x<-metadata_remap$form_name_remap[which(metadata_remap$form_name==instrument_name)] %>% unique()
          if(length(x)>1)stop("instrument_names cannot match multiple instrument_name_remaps: ",instrument_name)
          return(x)
        })
        event_mapping_new <- event_mapping_new %>% unique()
        colnames(event_mapping_new)[1] <- "unique_event_name"
        events_new$former_unique_event_names <- events_new$unique_event_name %>% sapply(function(unique_event_name){
          events_remap$unique_event_name[which(events_remap$unique_event_name_remap==unique_event_name)] %>% unique() %>% paste0(collapse = " | ")
        })
        event_mapping_new$former_unique_event_names <- event_mapping_new$unique_event_name %>% sapply(function(unique_event_name){
          event_mapping_remap$unique_event_name[which(event_mapping_remap$unique_event_name_remap==unique_event_name)] %>% unique() %>% paste0(collapse = " | ")
        })
        x<- event_mapping_new[which(event_mapping_new$form%in%instruments_remap$instrument_name_remap),]
        instruments_remap$repeating_via_events[
          which(
            instruments_remap$instrument_name_remap %>% sapply(function(instrument_name_remap){
              # instrument_name <- DB$redcap$instruments$instrument_name %>% sample(1)
              if(DB$internals$merge_form_name==instrument_name_remap){
                F
              }else{
                anyDuplicated(event_mapping_new$form[which(event_mapping_new$form==instrument_name_remap)])>0
              }
            })
          )
        ] <- T
        instruments_remap$repeating_via_events[
          which(
            instruments_remap$instrument_name_remap %>% sapply(function(instrument_name_remap){
              anyDuplicated(metadata_remap$field_name_remap[which(metadata_remap$form_name_remap==instrument_name_remap)])>0
            })
          )
        ] <- T
      }
      DB$remap$events_new <- events_new
      DB$remap$event_mapping_new <- event_mapping_new
    }
    non_reps <- DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating)]
    ins_new_cols <- c("instrument_name_remap","repeating")
    if(DB$redcap$is_longitudinal){
      non_reps <- DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating&!DB$redcap$instruments$repeating_via_events)]
      ins_new_cols <- c("instrument_name_remap","repeating","repeating_via_events")
    }
    if(length(non_reps)>0){
      metadata_remap$form_name_remap[which( metadata_remap$form_name%in%non_reps)] <- DB$internals$merge_form_name
    }
    instruments_new <- instruments_remap[,ins_new_cols] %>% unique()
    instruments_new$former_instrument_names <- instruments_new$instrument_name_remap %>% sapply(function(instrument_name_remap){
      instruments_remap$instrument_name[which(instruments_remap$instrument_name_remap==instrument_name_remap)] %>% unique() %>% paste0(collapse = " | ")
    })
    colnames(instruments_new)[1] <- "instrument_name"
    metadata_new <- metadata_remap
    metadata_new$field_name <- metadata_new$field_name_remap
    metadata_new$field_name_remap <- NULL
    metadata_new$form_name <- metadata_new$form_name_remap
    metadata_new$form_name_remap <- NULL
    keep_cols <- c(
      "field_name",
      "form_name",
      "field_type",
      "select_choices_or_calculations" # could handle choice remaps if labelled
    )
    drop_cols <- colnames(metadata_new)[which(!colnames(metadata_new)%in%keep_cols)]
    metadata_new <- metadata_new[,keep_cols] %>% unique()
    if(metadata_new$field_name %>% anyDuplicated() %>% magrittr::is_greater_than(0))stop("metadata_new has duplicate field names")
    for (col in drop_cols){
      metadata_new[[col]] <- metadata_new$field_name %>% sapply(function(field_name){
        metadata_remap[[col]][which(metadata_remap$field_name==field_name)[1]]
      })
    }
    DB$remap$metadata_new <- DB %>% annotate_metadata(metadata = metadata_new,skim = F)
    DB$remap$instruments_new <- instruments_new
    DB$remap$instruments_remap <- instruments_remap
    # if(save_file) metadata_new %>% rio::export(file = DB$dir_path %>% file.path("input","metadata_new_default.xlsx"))
  }
  return(DB)
}
#' @title Generate Default Re-Map
#' @inheritParams save_DB
#' @param save_file logical for whether or not to save sample default remap file to input folder of directory
#' @param merge_non_repeating logical for whether or not to merge non-repeating instruments
#' @export
generate_default_remap <- function(DB,save_file=!is.null(DB$dir_path),merge_non_repeating = T){
  DB <- validate_DB(DB)
  if(is.data.frame(DB$redcap$metadata)){
    metadata_remap <-   DB$redcap$metadata
    metadata_remap$field_name_remap <- metadata_remap$field_name
    metadata_remap$form_name_remap <- metadata_remap$form_name
    if(merge_non_repeating){
      rows <- which(metadata_remap$form_name%in%DB$redcap$instruments$instrument_name[which(!(DB$redcap$instruments$repeating))])
      if(DB$redcap$is_longitudinal){
        rows <- which(metadata_remap$form_name%in%DB$redcap$instruments$instrument_name[which(!(DB$redcap$instruments$repeating|DB$redcap$instruments$repeating_via_events))])
      }
      metadata_remap$form_name_remap[rows] <- DB$internals$merge_form_name
    }
    if(DB$redcap$is_longitudinal){
      if(DB$redcap$has_arms_that_matter){
        #can add remapping of arms but not smart if they matter
      }else{
        # DB$redcap$events->x
        event_mapping_remap <- DB$redcap$event_mapping
        event_mapping_remap <- merge(event_mapping_remap, DB$redcap$events[,c("event_name","unique_event_name")], by="unique_event_name")
        event_mapping_remap$unique_event_name_remap <- tolower(gsub(" ","_",event_mapping_remap$event_name))
        event_mapping_remap$form_remap <-  event_mapping_remap$form %>% sapply(function(instrument_name){
          x<-metadata_remap$form_name_remap[which(metadata_remap$form_name==instrument_name)] %>% unique()
          if(length(x)>1)stop("instrument_names cannot match multiple instrument_name_remaps: ",instrument_name)
          return(x)
        })
      }
      if(save_file) event_mapping_remap %>% rio::export(file = DB$dir_path %>% file.path("input","event_mapping_remap_default.xlsx"))
      DB$remap$event_mapping_remap <- event_mapping_remap
    }
    metadata_remap <- DB %>% annotate_metadata(metadata = metadata_remap,skim = F)
    if(save_file) metadata_remap %>% rio::export(file = DB$dir_path %>% file.path("input","metadata_remap_default.xlsx"))
    DB$remap$metadata_remap <- metadata_remap
  }
  DB <- remap_process(DB)
  return(DB)
}
#' @title Generate custom remap files from input
#' @inheritParams save_DB
#' @return DB object that has DB$remap populated from input folder
#' @export
generate_custom_remap_from_dir <- function(DB){
  DB <- validate_DB(DB)
  input_folder <- DB$dir_path %>% file.path("input")
  # input_folder %>% file.path(c("metadata_remap.xlsx","event_mapping_remap.xlsx"))
  # input_folder %>% list.files(full.names = T)
  for(file in c("metadata_remap","event_mapping_remap")){
    path <- input_folder %>% file.path(paste0(file,".xlsx"))
    if(file.exists(path)){
      DB$remap[[file]] <- rio::import(file = path)
    }
  }
  DB <- remap_process(DB)
  return(DB)
}
#' @title Transform DB
#' @inheritParams save_DB
#' @param records character vector of records to be included in transform
#' @param force logical for merging forcing transoformation even if up to date
#' @param terminal_transformation logical for whether this transformation is terminal/destructive
#' @param merge_non_rep_to_reps logical for merging non-repeating to every repeating instrument
#' @return DB object that has DB$data_transform, can be based on a remap file from input folder or default
#' @export
transform_DB <- function(DB, merge_non_rep_to_reps = F, records=NULL,force = F, terminal_transformation = F){
  DB  <- validate_DB(DB)
  will_update <- force
  if(is.null(DB$internals$last_data_transformation)) will_update <- T
  if(!is.null(DB$internals$last_data_transformation)){
    if(DB$internals$last_data_transformation<DB$internals$last_data_update) will_update <- T
  }
  if(will_update){
    selected <- DB %>% filter_DB(records = records,data_choice = "data_extract")
    if(! is_something(DB$remap$instruments_new)){
      DB <- generate_default_remap(DB)
    }
    instrument_names <- DB$remap$instruments_new$instrument_name
    for (instrument_name in instrument_names) {# instrument_name <- instrument_names %>%  sample (1)
      # add terminal_transformation
      if(instrument_name == DB$internals$merge_form_name){
        old_instruments <- DB$remap$instruments_remap$instrument_name[which(DB$remap$instruments_remap$instrument_name_remap == instrument_name)]
        DB$data_transform[[instrument_name]] <- merge_multiple(selected, old_instruments)
      }else{
        old_instruments <- DB$remap$instruments_remap$instrument_name[which(DB$remap$instruments_remap$instrument_name_remap == instrument_name)]
        final_out <- NULL
        for(old_instrument in old_instruments){# old_instrument <- old_instruments %>%  sample (1)
          keep <- selected[[old_instrument]]
          if(!is.null(keep)){
            if("redcap_event_name"%in%colnames(keep)){
              keep$redcap_event_name <- keep$redcap_event_name %>% sapply(function(unique_event_name){DB$remap$event_mapping_remap$unique_event_name_remap[which(DB$remap$event_mapping_remap$unique_event_name==unique_event_name)] %>% unique()})
              # keep$event_name <- keep$event_name %>% sapply(function(event_name){DB$remap$event_mapping_remap$[which(DB$remap$event_mapping_remap$unique_event_name==unique_event_name)] %>% unique()})
            }
            final_out <- final_out %>% dplyr::bind_rows(keep)
          }
        }
        DB$data_transform[[instrument_name]] <- final_out
      }
    }
    if(merge_non_rep_to_reps){
      if(DB$redcap$is_longitudinal){
        repeating_rows <- which(DB$remap$instruments_new$repeating|DB$remap$instruments_new$repeating_via_events)
      }else{
        repeating_rows <- which(DB$remap$instruments_new$repeating)
      }
      merged <- DB$data_transform[[DB$internals$merge_form_name]]
      for(instrument_name in DB$remap$instruments_new$instrument_name[repeating_rows]){
        original_rep <- DB$data_transform[[instrument_name]]
        shared_cols <- DB$redcap$raw_structure_cols[which((DB$redcap$raw_structure_cols %in% colnames(merged))&(DB$redcap$raw_structure_cols %in% colnames(original_rep)))]
        DB$data_transform[[instrument_name]] <- merged %>% merge(original_rep,by = shared_cols)
      }
    }
    DB$internals$last_data_transformation <- DB$internals$last_data_update
    DB$internals$reference_state <- "data_transform"
    DB$internals$reference_metadata <- "remap"
    DB$internals$was_remapped <- T
    save_DB(DB)
  }
  return(DB)
}
#' @title Horizontal Transform
#' @inheritParams summarize_DB
#' @export
generate_horizontal_transform <- function(DB,records){
  DB <- validate_DB(DB)
  if(missing(records)) records <- DB$summary$all_records[[DB$redcap$id_col]]
  data_extract <- filter_DB(DB,records = records)
  FINAL_out <- NULL
  forms <- names(data_extract)
  col_names <- NULL
  max_by_record <- NULL
  ID_col <- DB$redcap$id_col
  max_by_record <- data.frame(
    record = records,
    max = records %>% sapply(function(record){forms %>% sapply(function(form){length(which(data_extract[[form]][[ID_col]]==record))}) %>% max()})
  )
  for(form in forms){# form <- forms %>% sample(1)
    col_names <- col_names %>% dplyr::bind_rows(
      data.frame(
        number = NA,
        form_name = form,
        field_name = names(data_extract[[form]])
      )
    )
  }
  col_names$number <-1:nrow(col_names)
  form_list <- forms %>% sapply(function(IN){col_names$number[which(col_names$form_name==IN)]})
  for(form in forms){# form <- forms %>% sample(1)
    the_cols <- form_list[[form]]
    out <- NULL
    # out <- data.frame(matrix(nrow = 0,ncol = the_cols %>% length()))
    # colnames(out) <- the_cols
    for(record in records){ # record <- records %>% sample(1)
      the_max <- max_by_record$max[which(max_by_record$record==record)]
      df <- data_extract[[form]][which(data_extract[[form]][[ID_col]]==record),]
      colnames(df) <- the_cols
      n_blanks <- the_max-nrow(df)
      if(n_blanks>0){
        df_blanks <- data.frame(matrix(NA, ncol = ncol(df), nrow = n_blanks))
        colnames(df_blanks) <- colnames(df)
        df <- rbind(df,df_blanks)
      }
      out <- out %>% dplyr::bind_rows (df)
    }
    message("Done with ",form)
    FINAL_out <- FINAL_out %>% dplyr::bind_cols (out)
  }
  col_names2 <- col_names
  col_names2$number <- NULL
  col_names2 <- col_names2 %>% t() %>% as.data.frame()
  colnames(col_names2)<-colnames(FINAL_out)
  col_names2 <- all_character_cols(col_names2)
  FINAL_out <- all_character_cols(FINAL_out)
  FINAL_out <-col_names2 %>%  dplyr::bind_rows(FINAL_out)
  return(FINAL_out)
}
extract_instrument_from_merged <- function(DB,instrument_name){
  merged <- DB$data_extract[[DB$internals$merge_form_name]]
  if(nrow(merged)>0){
    add_ons <- c(DB$redcap$id_col,"arm_num","event_name","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")
    add_ons  <- add_ons[which(add_ons%in%colnames(merged))]
    if(!instrument_name%in%DB$redcap$instruments$instrument_name)stop("instrument_name must be included in set of DB$redcap$instruments$instrument_name")
    #instrument_name <-  DB$redcap$instruments$instrument_name %>% sample(1)
    is_repeating_instrument <- instrument_name%in%DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)]
    rows  <- 1:nrow(merged)
    if(is_repeating_instrument){
      # rows <- which(merged$redcap_repeat_instrument==instrument_name)
    }
    if(!is_repeating_instrument){
      rows <- which(!is.na(merged[[paste0(instrument_name,"_complete")]]))
    }
    #
    # if(!DB$redcap$is_longitudinal){
    #   if("redcap_repeat_instrument"%in%colnames(merged)){
    #     if(is_repeating_instrument){
    #       rows <- which(merged$redcap_repeat_instrument==instrument_name)
    #     }
    #     if(!is_repeating_instrument){
    #       rows <- which(is.na(merged$redcap_repeat_instrument))
    #     }
    #   }
    # }
    # if(DB$redcap$is_longitudinal){
    #   events_ins <- DB$redcap$event_mapping$unique_event_name[which(DB$redcap$event_mapping$form==instrument_name)] %>% unique()
    #   rows <- which(merged$redcap_event_name%in%events_ins)
    # }
    # if(!is_repeating_instrument){
    #   add_ons <- add_ons[which(!add_ons%in%c("redcap_repeat_instrument","redcap_repeat_instance"))]
    # }
    cols <- unique(c(add_ons,DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name==instrument_name&DB$redcap$metadata$field_name%in%colnames(merged))]))
    return(merged[rows,cols])
  }
}
# add_DB_flag <- function(DB,flag_field_name,id_field_name,ids,flag_name,read_split=" [:|:] ",write_split = " | ",remove_previous = T){
#
# }
