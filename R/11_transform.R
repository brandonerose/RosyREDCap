#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
#' @title Horizontal Transform
#' @inheritParams setup_RosyREDCap
#' @export
generate_horizontal_transform <- function(DB,records){
  DB <- validate_RosyREDCap(DB)
  if(missing(records)) records <- DB$summary$all_records[[DB$redcap$id_col]]
  data <- filter_DB(DB,records = records)
  FINAL_out <- NULL
  forms <- names(data)
  col_names <- NULL
  max_by_record <- NULL
  ID_col <- DB$redcap$id_col
  max_by_record <- data.frame(
    record = records,
    max = records %>% sapply(function(record){forms %>% sapply(function(form){length(which(data[[form]][[ID_col]]==record))}) %>% max()})
  )
  for(form in forms){# form <- forms %>% sample(1)
    col_names <- col_names %>% dplyr::bind_rows(
      data.frame(
        number = NA,
        form_name = form,
        field_name = names(data[[form]])
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
      df <- data[[form]][which(data[[form]][[ID_col]]==record),]
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
  merged <- DB$data[[DB$internals$merge_form_name]]
  if(nrow(merged)>0){
    add_ons <- c(DB$redcap$id_col,"arm_num","event_name","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")
    add_ons  <- add_ons[which(add_ons%in%colnames(merged))]
    if(!instrument_name%in%DB$metadata$forms$instrument_name)stop("instrument_name must be included in set of DB$metadata$forms$instrument_name")
    #instrument_name <-  DB$metadata$forms$instrument_name %>% sample(1)
    is_repeating_instrument <- instrument_name%in%DB$metadata$forms$instrument_name[which(DB$metadata$forms$repeating)]
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
    #   events_ins <- DB$metadata$event_mapping$unique_event_name[which(DB$metadata$event_mapping$form==instrument_name)] %>% unique()
    #   rows <- which(merged$redcap_event_name%in%events_ins)
    # }
    # if(!is_repeating_instrument){
    #   add_ons <- add_ons[which(!add_ons%in%c("redcap_repeat_instrument","redcap_repeat_instance"))]
    # }
    cols <- unique(c(add_ons,DB$metadata$fields$field_name[which(DB$metadata$fields$form_name==instrument_name&DB$metadata$fields$field_name%in%colnames(merged))]))
    return(merged[rows,cols])
  }
}
# add_DB_flag <- function(DB,flag_field_name,id_field_name,ids,flag_name,read_split=" [:|:] ",write_split = " | ",remove_previous = T){
#
# }
#' @title upload_transform_to_DB Transform
#' @inheritParams setup_RosyREDCap
#' @export
upload_transform_to_DB <- function(DB){
  if(is_something(DB$data_update$transform)){
    for(i in 1:length(DB$data_update$transform)){
      DB$data_update$transform[[i]] %>% labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
    }
    bullet_in_console("Successfully uploaded to REDCap!",bullet_type = "v")
  }else{
    bullet_in_console("Nothing to upload!")
  }
}
