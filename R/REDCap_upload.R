#' @import RosyApp
#' @title Upload to REDCap
#' @description
#' This will only overwrite and new data. It will not directly delete and data.
#' Because this is the only function that can mess up your data, use it at your own risk.
#' Remember all changes are saved in the redcap log if there's an issue. Missing rows and columns are fine!
#' @inheritParams save_DB
#' @param to_be_uploaded data.frame in raw coded form. If you worked with clean data pass your data to `labelled_to_raw_form(FORM,DB)` first.
#' @param batch_size numeric of how big the REDCap batch upload is. Default 500.
#' @return messages
#' @export
upload_form_to_REDCap <- function(to_be_uploaded,DB,batch_size=500){
  REDCapR::redcap_write(
    ds_to_write = to_be_uploaded %>% all_character_cols(),
    batch_size=batch_size,
    interbatch_delay=0.2,
    continue_on_error=FALSE,
    redcap_uri = DB$links$redcap_uri,
    token = validate_REDCap_token(DB),
    overwrite_with_blanks=TRUE
  )
}
#' @title Upload from your directory to REDCap
#' @description
#' This function is designed to upload changes from a locally modified `DB` object to REDCap. It should be run after using `DB_import <- read_from_REDCap_upload(DB)`.
#' The function compares the imported data (`DB_import`) to the existing data in `DB` and only uploads new or changed data. It will not directly delete any data.
#' This function has the potential to modify your data, so it should be used cautiously. Any changes made through this function will be logged in the REDCap log.
#'
#' @inheritParams save_DB
#' @param batch_size Numeric. The number of records to upload in each batch. Default is 500.
#' @param ask Logical. If TRUE, the function will prompt you to preview the data that will be uploaded before proceeding. Defaults to TRUE.
#' @param view_old Logical. If TRUE, the function will show a preview of the old records before upload. Defaults to TRUE.
#' @param n_row_view Numeric. The number of rows to display when previewing old data. Default is 20.
#'
#' @return A series of messages indicating the progress and status of the upload.
#'
#' @details
#' This function uploads changes to a REDCap project, based on the differences between the locally imported data (`DB_import`) and the existing data (`DB`).
#' It uploads changes in batches as specified by `batch_size` and allows you to preview the changes before the upload if `ask` is set to TRUE.
#' It will not delete any data from REDCap, and it is intended to only upload new or modified records.
#' This function is not fully ready for production use and should be used with caution. Any issues during the upload will be logged in the REDCap system log.
#' @export
upload_DB_to_REDCap <- function(DB,batch_size = 500, ask = T, view_old = T, n_row_view = 20){
  warning("This function is not ready for primetime yet! Use at your own risk!",immediate. = T)
  DB <- validate_DB(DB)
  # if(ask){
  #   if(count_DB_upload_cells(DB)>5000){
  #     choice  <- utils::menu(choices = c("YES - Move forward with larger upload","NO - I want to stop and double check what I'm about to upload"),title = "This is a large upload. Do you want to proceed?")
  #     if(choice!=1)stop("Double check DB object prior to upload")
  #   }
  # }
  warning("Right now this function only updates repeating forms. It WILL NOT clear repeating form instances past number 1. SO, you will have to delete manually on REDCap.",immediate. = T)
  if(!is_something(DB$data_update))stop("`DB$data_update` is empty")
  any_updates <- F
  entire_list <- DB$data_update
  for(TABLE in names(entire_list)){
    to_be_uploaded <- entire_list[[TABLE]]
    if(is_something(to_be_uploaded)){
      DB$data_update <- list()
      DB$data_update[[TABLE]] <- to_be_uploaded
      DB <- find_upload_diff(DB, view_old = view_old, n_row_view = n_row_view)
      to_be_uploaded <- DB$data_update[[TABLE]]
      if(is_something(to_be_uploaded)){
        if(DB$internals$data_extract_labelled){
          to_be_uploaded <- to_be_uploaded %>% labelled_to_raw_form(DB)
        }
        do_it <- 1
        if(ask){
          do_it  <- utils::menu(choices = c("Yes upload","No and go to next"),title = "Do you want to upload this?")
        }
        if(do_it==1){
          upload_form_to_REDCap(to_be_uploaded=to_be_uploaded,DB=DB,batch_size=batch_size)
          DB$data_update[[TABLE]] <- NULL
          any_updates <- T
          DB$internals$last_data_update <- Sys.time()
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
#' @description
#' This function compares the data in the `DB` object (new data) with the previous or reference data to identify differences. It returns a list of differences for upload. The function ensures that the new data matches the structure defined by the metadata and provides warnings when discrepancies are found.
#'
#' @inheritParams save_DB
#' @param view_old Logical. If TRUE, it will display a preview of the old data (default is FALSE).
#' @param n_row_view Numeric. Defines how many rows of the old data to view (default is 20).
#'
#' @return A list of differences between the new and old data (`upload_list`).
#'
#' @details
#' The function compares the data in `DB$data_update` (new data) with the current data in the database (`DB$data`). If the form names in the new data do not match the `DB$metadata$forms$form_name`, a warning is issued. The function goes through each table in the new data and compares it with the old data, recording the differences.
#'
#' The `compare` and `to` parameters allow users to specify specific data choices to compare, though their exact usage will depend on how the function is fully implemented.
#' @export
find_upload_diff <- function(DB, view_old = F, n_row_view = 20){
  DB <- validate_DB(DB)
  new_list <- DB$data_update
  old_list <- list()
  if(any(!names(new_list)%in%DB$metadata$forms$form_name))warning("All upload names should ideally match the DB form names, `DB$metadata$forms$form_name`",immediate. = T)
  already_used <- NULL
  for(TABLE in names(new_list)){#TABLE <- names(new_list) %>% sample(1)
    new <-  new_list[[TABLE]]
    ref_cols <- DB$redcap$raw_structure_cols
    ref_cols <- ref_cols[which(ref_cols%in%colnames(new))]
    data_cols <- colnames(new)[which(!colnames(new)%in%ref_cols)]
    form_names <- field_names_to_form_names(DB,data_cols)
    if(any(form_names%in%already_used))stop("RosyREDCap will not allow you to upload items from same form multiple times in one loop without refreshing.")
    # old <- merge_forms(forms = form_names, DB = DB,data_choice = "data",exact = T)
    drop <- data_cols %>% vec1_not_in_vec2(form_names_to_field_names(form_names=form_names,DB=DB))
    if(length(drop)>0){
      message("Dropping field_names that aren't part of REDCap metadata: ",paste0(drop, collapse = ", "))
      old <- old[,which(!colnames(old)%in%drop)]
    }
    old_list[[TABLE]] <- old# find_df_diff2(new= new , old =  old, ref_cols = ref_cols, message_pass = paste0(TABLE,": "))
    already_used <- already_used %>%append(form_names) %>% unique()
  }
  new_list <- find_df_list_diff(new_list = new_list, old_list = old_list, ref_col_list = DB$metadata$form_key_cols[names(new_list)],view_old = view_old, n_row_view = n_row_view)
  if(is_something(new_list)){
    DB$data_update <- new_list
    return(DB)
  }
  message("No upload updates!")
  DB$data_update <- list()
  return(DB)
}
#' @noRd
check_field <- function(DB,DF, field_name,autofill_new=T){
  form <- field_names_to_form_names(DB,field_name)
  records <- DF[[DB$redcap$id_col]] %>% unique()
  BAD<-records[which(!records%in%DB$summary$all_records[[DB$redcap$id_col]])]
  if(length(BAD)>0)stop("Records not included in DB: ",records %>% paste0(collapse = ", "))
  # is_repeating <- form%in% DB$metadata$forms$form_name[which(DB$metadata$forms$repeating)]
  cols_mandatory_structure <- DB$metadata$form_key_cols[[form]]
  cols_mandatory <- c(cols_mandatory_structure,field_name)
  old <- DB$data[[form]][,cols_mandatory]
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
  z<- new %>% find_df_diff2(old,ref_cols = cols_mandatory_structure)
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
          OUT %>% labelled_to_raw_form(DB) %>% upload_form_to_REDCap(DB)
          message("Uploaded: ",OUT %>% paste0(collapse = " | "))
        }
        if(choice==2){
          message("Did not change anything")
        }
        if(choice==3){
          DB %>% link_REDCap_record(OUT[[DB$redcap$id_col]])
          OUT[[field_name]] <- readline("What would you like it to be? ")
          print.data.frame(OUT)
          OUT %>% labelled_to_raw_form(DB) %>% upload_form_to_REDCap(DB)
        }
        if(choice==4){#account for repeat? instance
          DB %>% link_REDCap_record(OUT[[DB$redcap$id_col]],form,instance = OUT[["redcap_repeat_instance"]])
        }
      }
    }
  }
}
#' @title Edit REDCap Data While Viewing
#' @description
#' Allows for editing a specific field in a REDCap project while simultaneously viewing the corresponding records and fields from other forms. Supports viewing and updating records individually, with flexible field selection.
#'
#' @inheritParams save_DB
#' @param optional_DF Optional data frame. A data frame containing the data to be edited. If not provided, the function will pull the data from the REDCap database using the specified `field_name_to_change`.
#' @param records Character or numeric vector. The records to be edited. If not provided, the function will use the unique values from the specified forms.
#' @param field_name_to_change Character. The field name to be changed in the REDCap database.
#' @param field_names_to_view Optional character vector. A list of field names to view alongside the field being edited. Defaults to `NULL`, in which case only the field being changed will be viewed.
#' @param upload_individually Logical. If `TRUE`, each change is uploaded individually. Default is `TRUE`.
#'
#' @return
#' A modified `DB` object with changes to the specified field(s) in the REDCap project.
#'
#' @details
#' This function is useful when you want to edit specific fields in a REDCap project while also reviewing related data from other forms in the project. The `field_name_to_change` must be provided, and you can also specify additional fields to view while editing. The data is either passed through `optional_DF` or pulled from the project based on the provided field names.
#'
#' @seealso
#' \code{\link{save_DB}} for saving the modified database.
#'
#' @export
edit_REDCap_while_viewing <- function(DB,optional_DF,records, field_name_to_change, field_names_to_view=NULL,upload_individually = T){
  change_form <- field_names_to_form_names(DB,field_name_to_change)
  view_forms <- field_names_to_form_names(DB,field_names_to_view)
  field_names_to_view <- c(field_name_to_change,field_names_to_view) %>% unique()
  # if(length(view_forms)>1)stop("only one form combinations are allowed.")
  if(missing(records)) records <- DB$data[[view_forms]][[DB$redcap$id_col]] %>% unique()
  all_forms <- c(change_form,view_forms) %>% unique()
  ref_cols_change <- DB$metadata$form_key_cols[[change_form]]
  # ref_cols_view <- DB$metadata$form_key_cols[[view_forms]]
  if(missing(optional_DF)){
    optional_DF <- DB[["data"]][[change_form]][,unique(c(ref_cols_change,field_names_to_view))]
  }
  if(is.null(field_names_to_view)) field_names_to_view <- colnames(optional_DF)
  # if(any(!ref_cols%in%colnames(DF)))stop("DF must contain all ref_cols")
  if(length(records)>0){
    # message("fix these in REDCap --> ",paste0(out,collapse = " | "))
    rows_of_choices <- which(DB$metadata$choices$field_name==field_name_to_change)
    has_choices <- length(rows_of_choices)>0
    choices1 <- c("Do Nothing", "Edit","Launch Redcap Link Only")
    if(has_choices){
      choices2 <- c("Do Nothing",DB$metadata$choices$name[rows_of_choices],"Launch Redcap Link Only")
    }else{
      choices2 <- c("Do Nothing","Manual Entry","Launch Redcap Link Only")
    }
    is_repeating_form <- change_form %in% DB$metadata$forms$form_name[which(DB$metadata$forms$repeating)]
    OUT <- NULL
    for (record in records){ # record <- records%>% sample(1)
      record_was_updated <- F
      VIEW <- optional_DF[which(optional_DF[[DB$redcap$id_col]]==record),]
      VIEW_simp <- VIEW[,unique(c(DB$redcap$id_col,field_names_to_view))] %>% unique()
      row.names(VIEW_simp) <- NULL
      VIEW_simp %>%t() %>% print()
      CHANGE <- filter_DB(DB,filter_field = DB$redcap$id_col,filter_choices = records,form_names = change_form)[[1]]
      row.names(CHANGE) <- NULL
      CHANGE <- CHANGE[,unique(c(ref_cols_change,field_name_to_change))]
      if(nrow(CHANGE)==0){
        print("Nothing in CHANGE. If you choose edit it will add an instance...")
        blank_row <- data.frame(
          record
        )
        colnames(blank_row)[[1]] <- DB$redcap$id_col
        if("redcap_repeat_instance"%in%ref_cols_change){
          blank_row$redcap_repeat_instance <- "1"
          blank_row$redcap_repeat_instrument <- change_form
        }
        blank_row[[field_name_to_change]] <- NA
      }else{
        print(CHANGE)
      }
      choice1 <- utils::menu(choices1,title=paste0("What would you like to do?"))
      if(choice1 == 3){
        DB %>% link_REDCap_record(record = record)
      }
      if(choice1 == 2){
        if(nrow(CHANGE)==0)CHANGE <- blank_row
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
                OUT_sub %>% labelled_to_raw_form(DB) %>% upload_form_to_REDCap(DB)
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
              OUT_sub %>% labelled_to_raw_form(DB) %>% upload_form_to_REDCap(DB)
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
                    OUT_sub %>% labelled_to_raw_form(DB) %>% upload_form_to_REDCap(DB)
                    message("Uploaded: ",OUT_sub %>% paste0(collapse = " | "))
                    record_was_updated <- T
                  }else{
                    OUT <- OUT %>% dplyr::bind_rows(OUT_sub)
                  }
                  the_max <- the_max + 1
                }
                if(choice=="Launch Redcap Link Only"){#account for repeat? instance
                  DB %>% link_REDCap_record(record = record,page = change_form,instance = CHANGE[j,"redcap_repeat_instance"])
                }
              }else{
                OUT_sub[[field_name_to_change]] <- choice
                if(upload_individually){
                  OUT_sub %>% labelled_to_raw_form(DB) %>% upload_form_to_REDCap(DB)
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
  if(!upload_individually)OUT %>% labelled_to_raw_form(DB) %>% upload_form_to_REDCap(DB)
}
