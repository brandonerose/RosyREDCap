#' @import rosyutils
#' @title Upload to REDCap
#' @description
#' This will only overwrite and new data. It will not directly delete and data.
#' Because this is the only function that can mess up your data, use it at your own risk.
#' Remember all changes are saved in the redcap log if there's an issue. Missing rows and columns are fine!
#' @param to_be_uploaded data.frame in raw coded form. If you worked with clean data pass your data to `labelled_to_raw_form(FORM,DB)` first.
#' @inheritParams save_DB
#' @param batch_size numeric of how big the REDCap batch upload is. Default 500.
#' @return messages
#' @export
upload_form_to_redcap <- function(to_be_uploaded,DB,batch_size=500){
  REDCapR::redcap_write(
    ds_to_write = to_be_uploaded %>% rosyutils::all_character_cols(),
    batch_size=batch_size,
    interbatch_delay=0.2,
    continue_on_error=FALSE,
    redcap_uri = DB$links$redcap_uri,
    token = validate_redcap_token(DB),
    overwrite_with_blanks=TRUE
  )
}
#' @title Upload from your directory to REDCap
#' @description
#' This function is meant to be run after `DB_import <- read_redcap_dir(DB)`.
#' It compares DB_import to DB and only uploads the changes.
#' This will only overwrite and new data. It will not directly delete and data.
#' Because this is the only function that can mess up your data, use it at your own risk.
#' Remember all changes are saved in the redcap log if there's an issue.
#' @inheritParams save_DB
#' @param batch_size numeric of how big the REDCap batch upload is. Default 500.
#' @param ask logical for if you want to preview uploads first
#' @return messages
#' @export
upload_DB_to_redcap <- function(DB,batch_size = 500, ask = T, view_old = T, n_row_view = 20){
  warning("This function is not ready for primetime yet! Use at your own risk!",immediate. = T)
  DB <- validate_DB(DB)
  # if(ask){
  #   if(count_DB_upload_cells(DB)>5000){
  #     choice  <- utils::menu(choices = c("YES - Move forward with larger upload","NO - I want to stop and double check what I'm about to upload"),title = "This is a large upload. Do you want to proceed?")
  #     if(choice!=1)stop("Double check DB object prior to upload")
  #   }
  # }
  warning("Right now this function only updates repeating instruments. It WILL NOT clear repeating instrument instances past number 1. SO, you will have to delete manually on REDCap.",immediate. = T)
  if(!is_something(DB$data_upload))stop("`DB$data_upload` is empty")
  any_updates <- F
  entire_list <- DB$data_upload
  for(TABLE in names(entire_list)){
    to_be_uploaded <- entire_list[[TABLE]]
    if(is_something(to_be_uploaded)){
      DB$data_upload <- list()
      DB$data_upload[[TABLE]] <- to_be_uploaded
      DB <- find_upload_diff(DB, view_old = view_old, n_row_view = n_row_view)
      to_be_uploaded <- DB$data_upload[[TABLE]]
      if(is_something(to_be_uploaded)){
        if(DB$internals$data_extract_labelled){
          to_be_uploaded <- to_be_uploaded %>% labelled_to_raw_form(DB)
        }
        do_it <- 1
        if(ask){
          do_it  <- utils::menu(choices = c("Yes upload","No and go to next"),title = "Do you want to upload this?")
        }
        if(do_it==1){
          upload_form_to_redcap(to_be_uploaded=to_be_uploaded,DB=DB,batch_size=batch_size)
          DB$data_upload[[TABLE]] <- NULL
          any_updates <- T
          DB$internals$last_data_upload <- Sys.time()
        }
      }
    }
  }
  if(any_updates){
    DB <- update_DB(DB)
  }
  return(DB)
}
#' @title Find the DB_import and DB differences
#' @inheritParams save_DB
#' @param compare what data_choice to be compare
#' @param to what data_choice to be compared to
#' @return upload_list
#' @export
find_upload_diff <- function(DB, view_old = F, n_row_view = 20){
  DB <- validate_DB(DB)
  new_list <- DB$data_upload
  old_list <- list()
  if(any(!names(new_list)%in%DB$redcap$instruments$instrument_name))warning("All upload names should ideally match the DB instrument names, `DB$redcap$instruments$instrument_name`",immediate. = T)
  already_used <- NULL
  for(TABLE in names(new_list)){#TABLE <- names(new_list) %>% sample(1)
    new <-  new_list[[TABLE]]
    ref_cols <- DB$redcap$raw_structure_cols
    ref_cols <- ref_cols[which(ref_cols%in%colnames(new))]
    data_cols <- colnames(new)[which(!colnames(new)%in%ref_cols)]
    instruments <- field_names_to_instruments(DB,data_cols)
    if(any(instruments%in%already_used))stop("rosyredcap will not allow you to upload items from same form multiple times in one loop without refreshing.")
    old <- merge_instruments(instruments = instruments, DB = DB,data_choice = "data_extract",exact = T)
    drop <- data_cols %>% vec1_not_in_vec2(instruments_to_field_names(instruments=instruments,DB=DB))
    if(length(drop)>0){
      message("Dropping field_names that aren't part of REDCap metadata: ",paste0(drop, collapse = ", "))
      old <- old[,which(!colnames(old)%in%drop)]
    }
    old_list[[TABLE]] <- old# rosyutils::find_df_diff2(new= new , old =  old, ref_cols = ref_cols, message_pass = paste0(TABLE,": "))
    already_used <- already_used %>%append(instruments) %>% unique()
  }
  new_list <- find_df_list_diff(new_list = new_list, old_list = old_list, ref_col_list = DB$redcap$instrument_key_cols[names(new_list)],view_old = view_old, n_row_view = n_row_view)
  if(is_something(new_list)){
    DB$data_upload <- new_list
    return(DB)
  }
  message("No upload updates!")
  DB$data_upload <- list()
  return(DB)
}
#' @export
check_field <- function(DB,DF, field_name,autofill_new=T){
  form <- field_names_to_instruments(DB,field_name)
  records <- DF[[DB$redcap$id_col]] %>% unique()
  BAD<-records[which(!records%in%DB$summary$all_records[[DB$redcap$id_col]])]
  if(length(BAD)>0)stop("Records not included in DB: ",records %>% paste0(collapse = ", "))
  # is_repeating <- form%in% DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)]
  cols_mandatory_structure <- DB$redcap$instrument_key_cols[[form]]
  cols_mandatory <- c(cols_mandatory_structure,field_name)
  old <- DB$data_extract[[form]][,cols_mandatory]
  old <- old[which(old[[DB$redcap$id_col]]%in% records),]
  new <- DF
  missing_structure_cols<-cols_mandatory[which(!cols_mandatory%in%colnames(new))]
  cols <- cols_mandatory[which(cols_mandatory %in% colnames(new))]
  new <- new[,cols]
  included_records <- records[which(records %in% old[[DB$redcap$id_col]])]
  if(length(missing_structure_cols)>0){
    included_records_many_rows <- included_records[which(included_records %>% sapply(function(ID){
      length(which(old[[DB$redcap$id_col]]==ID))>1
    }))]
    if(length(included_records_many_rows)>0)stop("DF is missing structural columns (",missing_structure_cols %>% paste0(collapse = ", "),") and has ",form," rows with multiple entries... remove them or add the intended columns: ",included_records_many_rows %>% paste0(collapse = ", "))
    if("redcap_repeat_instrument"%in%missing_structure_cols)new$redcap_repeat_instrument<- form
    if("redcap_repeat_instance"%in%missing_structure_cols){
      new$redcap_repeat_instance<- new[[DB$redcap$id_col]] %>% sapply(function(ID){
        if(ID %in% included_records)return(old$redcap_repeat_instance[which(old[[DB$redcap$id_col]]==ID)])
        return("1")
      })
    }
    #add event?
  }
  z<- new %>% rosyutils::find_df_diff2(old,ref_cols = cols_mandatory_structure)
  if(!is.null(z)){
    i_of_old_name_change <- which(!colnames(old)%in% cols_mandatory_structure)
    colnames(old)[i_of_old_name_change] <- paste0(colnames(old)[i_of_old_name_change],"_old")
    z_old <- z %>% merge(old,by =cols_mandatory_structure)
    # add autoallow NA
    if(nrow(z)>0){
      # message("fix these in REDCap --> ",paste0(out,collapse = " | "))
      choices <- c("upload new","keep old","manual entry","launch redcap link only")
      for ( i in 1:nrow(z)){
        OUT <- z[i,]
        IN<-z_old[i,]
        new_answer <- IN[[field_name]]
        old_answer <- IN[[paste0(field_name,"_old")]]
        ask <- T
        if(autofill_new){
          if(is.na(old_answer)&&!is.na(new_answer)){
            ask <- F
          }
        }
        if(ask){
          print.data.frame(z_old[i,])
          choice <- utils::menu(choices,title=paste0("What would you like to do?"))
        }else{
          choice <- 1
        }
        if(choice==1){
          OUT %>% labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
          message("Uploaded: ",OUT %>% paste0(collapse = " | "))
        }
        if(choice==2){
          message("Did not change anything")
        }
        if(choice==3){
          DB %>% link_REDCap_record(OUT[[DB$redcap$id_col]])
          OUT[[field_name]] <- readline("What would you like it to be? ")
          print.data.frame(OUT)
          OUT %>% rosyredcap::labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
        }
        if(choice==4){#account for repeat? instance
          DB %>% link_REDCap_record(OUT[[DB$redcap$id_col]],form,instance = OUT[["redcap_repeat_instance"]])
        }
      }
    }
  }
}
#' @export
edit_redcap_while_viewing <- function(DB,records, field_name_to_change, field_names_to_view=NULL,upload_individually = T){
  change_form <- field_names_to_instruments(DB,field_name_to_change)
  view_forms <- field_names_to_instruments(DB,field_names_to_view)
  # field_names_to_view <- c(field_name_to_change,field_names_to_view) %>% unique()
  # if(length(view_forms)>1)stop("only one form combinations are allowed.")
  if(missing(records))records <- DB$data_extract[[view_forms]][[DB$redcap$id_col]]
  all_forms <- c(change_form,view_forms) %>% unique()
  ref_cols_change <- DB$redcap$instrument_key_cols[[change_form]]
  ref_cols_view <- DB$redcap$instrument_key_cols[[view_forms]]
  # if(missing(optional_DF)){
  #   optional_DF <- DB[[data_choice]][[all_forms]][,unique(c(ref_cols,field_names_to_view))]
  # }
  # if(any(!ref_cols%in%colnames(DF)))stop("DF must contain all ref_cols")
  if(length(records)>0){
    # message("fix these in REDCap --> ",paste0(out,collapse = " | "))
    rows_of_choices <- which(DB$redcap$codebook$field_name==field_name_to_change)
    has_choices <- length(rows_of_choices)>0
    choices1 <- c("Do Nothing", "Edit","Launch Redcap Link Only")
    if(has_choices){
      choices2 <- c("Do Nothing",DB$redcap$codebook$name[rows_of_choices],"Launch Redcap Link Only")
    }else{
      choices2 <- c("Do Nothing","Manual Entry","Launch Redcap Link Only")
    }
    is_repeating_form <- change_form %in% DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)]

    # i <- 1:nrow(optional_DF) %>% sample1()
    OUT <- NULL
    # record <- records%>% sample(1)
    for (record in records){
      record_was_updated <- F
      VIEW <- filter_DB(DB, records = record, form_names = view_forms)[[1]]
      VIEW_simp <- VIEW[,c(ref_cols_view)]
      print(VIEW_simp)
      VIEW <- VIEW[,c(ref_cols_view,field_names_to_view)]
      CHANGE <-filter_DB(DB, records = record, form_names = change_form)[[1]]
      CHANGE <- CHANGE[,c(ref_cols_view,field_name_to_change)]
      for(i in 1:nrow(VIEW)){
        x <- VIEW[i,] %>% as.list()
        for (j in 1:length(x)){
          cat(names(x)[j], ": \n\n",x[[j]], "\n\n")
        }
        print(CHANGE)
        # old_answer <- merge(VIEW,OUT[i,],by=ref_cols,suffixes = c("","_old"))[[field_name_to_change]]
        # if(length(old_answer)>0){
        #   OUT[i,field_name_to_change] <- old_answer
        #   message("Old answer (",field_name_to_change, "): ",old_answer)
        # }
        choice1 <- utils::menu(choices1,title=paste0("What would you like to do?"))
        if(choice1 == 3){
          DB %>% link_REDCap_record(record = record)
        }
        if(choice1 == 2){
          for(j in 1:nrow(CHANGE)){
            message("Old answer (",field_name_to_change, "): ",CHANGE[j,field_name_to_change])
            choice2 <- utils::menu(choices2,title=paste0("What would you like to do?"))
            choice <- choices2[choice2]
            OUT_sub <- CHANGE[j,]
            if(choice %in% c("Manual Entry","Do Nothing","Launch Redcap Link Only")){
              if(choice=="Do Nothing"){
                message("Did not change anything")
              }
              if(choice=="Manual Entry"){
                OUT_sub[[field_name_to_change]] <- readline("What would you like it to be? ")
                if(upload_individually){
                  OUT_sub %>% rosyredcap::labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
                  message("Uploaded: ",OUT_sub %>% paste0(collapse = " | "))
                  record_was_updated <- T
                }else{
                  OUT <- OUT %>% dplyr::bind_rows(OUT_sub)
                }
              }
              if(choice=="Launch Redcap Link Only"){#account for repeat? instance
                DB %>% link_REDCap_record(record = record,page = change_form,instance = CHANGE[j,"redcap_repeat_instance"])
              }
            }else{
              OUT_sub[[field_name_to_change]] <- choice
              if(upload_individually){
                OUT_sub %>% rosyredcap::labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
                message("Uploaded: ",OUT_sub %>% paste0(collapse = " | "))
                record_was_updated <- T
              }else{
                OUT <- OUT %>% dplyr::bind_rows(OUT_sub)
              }
            }
          }
          if(is_repeating_form){
            choice3 <- 2
            the_max <- 0
            if(nrow(CHANGE)>0)the_max <- CHANGE$redcap_repeat_instance %>% as.integer() %>% max()
            while (choice3 == 2) {
              choice3 <- utils::menu(c("No","Yes"),title=paste0("Would you like to add an additional instance?"))
              if(choice3 == 2){
                OUT_sub <- data.frame(
                  record_id = record,
                  redcap_repeat_instrument = change_form,
                  redcap_repeat_instance = as.character(the_max + 1)
                )
                colnames(OUT_sub)[1]<-DB$redcap$id_col
                choice2 <- utils::menu(choices2,title=paste0("What would you like to do?"))
                choice <- choices2[choice2]
                if(choice %in% c("Manual Entry","Do Nothing","Launch Redcap Link Only")){
                  if(choice=="Do Nothing"){
                    message("Did not change anything")
                  }
                  if(choice=="Manual Entry"){
                    OUT_sub[[field_name_to_change]] <- readline("What would you like it to be? ")
                    if(upload_individually){
                      OUT_sub %>% rosyredcap::labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
                      message("Uploaded: ",OUT_sub %>% paste0(collapse = " | "))
                      record_was_updated <- T
                    }else{
                      OUT <- OUT %>% dplyr::bind_rows(OUT_sub)
                    }
                    the_max <- the_max + 1
                  }
                  if(choice=="Launch Redcap Link Only"){#account for repeat? instance
                    DB %>% link_REDCap_record(record = record,form = change_form,instance = CHANGE[j,"redcap_repeat_instance"])
                  }
                }else{
                  OUT_sub[[field_name_to_change]] <- choice
                  if(upload_individually){
                    OUT_sub %>% rosyredcap::labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
                    message("Uploaded: ",OUT_sub %>% paste0(collapse = " | "))
                    record_was_updated <- T
                  }else{
                    OUT <- OUT %>% dplyr::bind_rows(OUT_sub)
                  }
                  the_max <- the_max + 1
                }
              }
            }
          }
        }
      }
      if(record_was_updated)DB <- update_DB(DB)
    }
    if(!upload_individually)OUT %>% rosyredcap::labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
  }
}
