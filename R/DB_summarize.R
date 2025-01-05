#' @import RosyApp
#' @title Clean DB columns for plotting using the metadata
#' @description
#' This function cleans the columns of a `DB` object, transforming choice fields into factors and ensuring numeric columns are set correctly for table processing or plotting (e.g., using `table1`). It handles the transformation of missing values and optional removal of certain codes based on user input.
#'
#' @inheritParams save_DB
#' @param drop_blanks Logical. If TRUE, will drop choice fields with zero occurrences (n = 0). Default is FALSE.
#' @param other_drops A list of additional fields or choices to drop from the data. Defaults to NULL.
#'
#' @return A cleaned `DB` object ready for table or plot processing.
#'
#' @details
#' The function works by cleaning up the data frame list (`DB$data`) according to the metadata (`DB$metadata$fields`). It converts choice fields into factors, numeric fields are treated appropriately, and any unwanted or missing codes can be dropped based on the parameters provided. The function also ensures that the data is only cleaned once by checking the internal `is_clean` flag.
#'
#' @note
#' The function will not proceed with cleaning if `DB$internals$is_clean` is already TRUE, signaling that the DB has already been cleaned.
#' @export
clean_DB <- function(DB,drop_blanks=F,other_drops=NULL){ # problematic because setting numeric would delete missing codes
  # DB <-  DB %>% annotate_fields(skim = F)
  if(!is_something(DB))return(DB)
  if(!is_something(DB$data))return(DB)
  if(DB$internals$is_clean){
    bullet_in_console("Already Clean",bullet_type = "v")
    return(DB)
  }
  DB$data <- clean_DF_list(
    DF_list = DB$data,
    fields = DB$metadata$fields,
    drop_blanks = drop_blanks,
    other_drops = other_drops
  )
  DB$internals$is_clean <- T
  return(DB)
}
#' @noRd
fields_to_choices <- function(fields){
  fields <- fields[which(fields$field_type%in%c("radio","dropdown","checkbox_choice","yesno")),]
  # fields$field_name[which(fields$field_type=="checkbox_choice")] <- fields$field_name[which(fields$field_type=="checkbox_choice")] %>% strsplit("___") %>% sapply(function(X){X[[1]]})
  fields <- fields[which(!is.na(fields$select_choices_or_calculations)),]
  choices <- NULL
  for(i in 1:nrow(fields)){
    field_name <- fields$field_name[i]
    form_name <- fields$form_name[i]
    # form_label <- fields$form_label[i]
    field_label <- fields$field_label[i]
    field_type <- fields$field_type[i]
    selections <- fields$select_choices_or_calculations[i] %>% split_choices()
    choices <- choices %>% dplyr::bind_rows(
      data.frame(
        form_name = form_name,
        # form_label = form_label,
        field_name = field_name,
        field_type = field_type,
        field_label =  ifelse(!is.na(field_label),field_label,field_name),
        code = selections$code,
        name =selections$name
      )
    )
  }
  choices$label <- paste(choices$form_name,"-",choices$field_label,"-",choices$name)
  # choices$label2 <- paste(choices$form_label,"-",choices$field_label,"-",choices$name)
  rownames(choices) <- NULL
  return(choices)
}
#' @noRd
add_labels_to_checkbox <- function (fields){
  rows <- which(fields$field_type=="checkbox_choice")
  x <- fields$field_name[rows] %>% strsplit("___") %>% sapply(function(x){x[[1]]})
  y <- fields$field_label[rows]
  z <- paste0(fields$field_label[match(x,fields$field_name)]," - ",y)
  fields$field_label[rows] <- z
  return(fields)
}
#' @noRd
annotate_fields <- function(DB,summarize_data = T){
  fields <- DB$metadata$fields#[,colnames(get_original_fields(DB))]
  fields <- fields[which(fields$field_type!="descriptive"),]
  fields <- add_labels_to_checkbox(fields)
  fields <- fields[which(fields$field_type!="checkbox"),]
  fields$field_label[which(is.na(fields$field_label))] <- fields$field_name[which(is.na(fields$field_label))]
  fields  <- unique(fields$form_name) %>%
    lapply(function(IN){
      fields[which(fields$form_name==IN),]
    }) %>% dplyr::bind_rows()
  if(!"field_type_R"%in%colnames(fields))fields$field_type_R <- "character"
  fields$field_type_R[which(fields$field_type %in% c("radio","yesno","dropdown","checkbox_choice"))] <- "factor"
  fields$field_type_R[which(fields$text_validation_type_or_show_slider_number == "integer")] <- "integer"
  fields$field_type_R[which(fields$text_validation_type_or_show_slider_number == "date_mdy")] <- "date"
  fields$field_type_R[which(fields$text_validation_type_or_show_slider_number == "date_ymd")] <- "date"
  fields$field_type_R[which(fields$text_validation_type_or_show_slider_number == "datetime_dmy")] <- "datetime"
  fields$in_original_redcap <- T
  fields$original_form_name <- fields$form_name
  if(DB$internals$is_transformed){
    fields$in_original_redcap <- fields$field_name %in% DB$transformation$original_fields$field_name
    fields$original_form_name <- DB$transformation$original_fields$form_name[match(fields$field_name,DB$transformation$original_fields$field_name)]
  }
  if(!"units" %in% colnames(fields))fields$units <- NA
  if(!"field_label_short" %in% colnames(fields)) fields$field_label_short <- fields$field_label
  # if(!"field_label_short" %in% colnames(fields))fields$ <- fields$field_label
  if(summarize_data){
    skimmed <- NULL
    for (form in unique(fields$form_name)){
      COLS <- fields$field_name[which(fields$form_name==form)]
      CHECK_THIS <- DB$data[[form]]
      COLS <- COLS[which(COLS %in% colnames(CHECK_THIS))]
      skimmed <- skimmed %>% dplyr::bind_rows(CHECK_THIS[,COLS] %>% skimr::skim())
    }
    FOR_ORDERING <- fields$field_name
    fields <- fields %>% merge(skimmed,by.x = "field_name",by.y = "skim_variable",all = T)
    fields <- FOR_ORDERING %>%
      lapply(function(IN){
        fields[which(fields$field_name==IN),]
      }) %>% dplyr::bind_rows()
  }
  # bullet_in_console("Annotated `DB$metadata$fields`",bullet_type = "v")
  return(fields)
}
#' @noRd
annotate_forms <- function(DB,summarize_data = T){
  forms <- DB$metadata$forms
  # forms <- get_original_forms(DB)
  # if(!is.null(DB$metadata$form_key_cols)){
  #   forms$key_cols <- forms$form_name %>% sapply(function(IN){
  #     DB$metadata$form_key_cols[[IN]] %>% paste0(collapse = "+")
  #   })
  #   forms$key_names <- forms$form_name %>% sapply(function(IN){
  #     row_match <- which(forms$form_name==IN)
  #     if(!forms$repeating[row_match])return(DB$metadata$form_key_cols[[IN]])
  #     return(paste0(forms$form_name[row_match],"_key"))
  #   })
  # }
  #add metadata info like n fields
  if(summarize_data){
    for(status in c("Incomplete","Unverified","Complete")){
      forms[[tolower(status)]] <- forms$form_name %>% sapply(function(form_name){
        form_name %>% strsplit(" [:|:] ") %>% unlist() %>% sapply(function(form_name){
          (DB$data[[form_name]][[paste0(form_name,"_complete")]]==status) %>% which() %>% length()
        }) %>% paste0(collapse = " | ")
      })
    }
  }
  return(forms)
}
#' @noRd
annotate_choices <- function(DB,summarize_data = T){
  forms <- DB$metadata$forms
  fields <- DB$metadata$fields
  choices <- DB$metadata$choices
  # choices$field_name_raw <- choices$field_name
  # choices$field_name_raw[which(choices$field_type=="checkbox_choice")] <- choices$field_name[which(choices$field_type=="checkbox_choice")] %>%
  #   strsplit("___") %>%
  #   sapply(function(X){X[[1]]})
  # choices$field_label_raw <- choices$field_label
  # choices$field_label_raw[which(choices$field_type=="checkbox_choice")] <- choices$field_name_raw[which(choices$field_type=="checkbox_choice")] %>%
  #   sapply(function(X){
  #     DB$metadata$fields$field_label[which(fields$field_name==X)] %>% unique()
  #   })
  if(summarize_data){
    choices$n <- 1:nrow(choices) %>% lapply(function(i){
      DF <- DB$data[[choices$form_name[i]]]
      if(is.null(DF))return(0)
      if(nrow(DF)==0)return(0)
      sum(DF[,choices$field_name[i]]==choices$name[i],na.rm = T)
      # print(i)
    }) %>% unlist()
    choices$n_total <- 1:nrow(choices) %>% lapply(function(i){
      DF <- DB$data[[choices$form_name[i]]]
      if(is.null(DF))return(0)
      if(nrow(DF)==0)return(0)
      sum(!is.na(DF[,choices$field_name[i]]),na.rm = T)
    }) %>% unlist()
    choices$perc <-  (choices$n/choices$n_total) %>% round(4)
    choices$perc_text <- choices$perc %>% magrittr::multiply_by(100) %>% round(1) %>% paste0("%")
    # DB$summary$choices <- choices
    # bullet_in_console("Annotated `DB$summary$choices`",bullet_type = "v")
  }
  return(choices)
}
#' @noRd
fields_with_no_data <- function(DB){
  DB$metadata$fields$field_name[which(is.na(DB$metadata$fields$complete_rate)&!DB$metadata$fields$field_type%in%c("checkbox","descriptive"))]
}
#' @noRd
reverse_clean_DB <- function(DB){ # problematic because setting numeric would delete missing codes
  DB$data <- all_character_cols_list(DB$data)
  DB$data_update <-DB$data_update %>% all_character_cols_list()
  DB$internals$is_clean <- F
  return(DB)
}
#' @noRd
clean_DF_list <- function(DF_list,fields,drop_blanks = T,other_drops = NULL){
  #add check for DF_list#
  for(TABLE in names(DF_list)){
    DF_list[[TABLE]] <- clean_DF(
      DF = DF_list[[TABLE]],
      fields = fields,
      drop_blanks = drop_blanks,
      other_drops = other_drops
    )
  }
  return(DF_list)
}
#' @noRd
clean_DF <- function(DF,fields,drop_blanks = T,other_drops = NULL){
  for(COLUMN in colnames(DF)){
    if(COLUMN %in% fields$field_name){
      ROW <- which(fields$field_name==COLUMN)
      units <- NULL
      if(!is.na(fields$units[ROW])){
        units <- fields$units[ROW]
      }
      class <- fields$field_type_R[ROW][[1]]
      label <- ifelse(is.na(fields$field_label[ROW]),COLUMN,fields$field_label[ROW])[[1]]
      levels <- NULL
      if(!is.na(class)){
        if(class == "factor"){
          select_choices <- fields$select_choices_or_calculations[ROW]
          if(!is.na(select_choices)){
            levels <- split_choices(select_choices)[[2]]
          }else{
            levels <- unique(DF[[COLUMN]]) %>% drop_nas()
          }
          if(any(duplicated(levels))){
            DUPS <- levels %>% duplicated() %>% which()
            warning("You have a variable (",COLUMN,") with dupplicate names (",levels[DUPS] %>% paste0(collapse = ", "),"). This is not great but for this proccess they will be merged and treated as identical responses.")
            levels <- levels %>% unique()
          }
          if(drop_blanks){
            levels <- levels[which(levels%in%unique(DF[[COLUMN]]))]
          }
          if(!is.null(other_drops)){
            if(length(other_drops)>0)levels <- levels[which(!levels%in%other_drops)]
          }
        }
        if(class == "integer"){
          DF[[COLUMN]] <- as.integer(DF[[COLUMN]])
        }
        if(class == "numeric"){
          DF[[COLUMN]] <- as.numeric(DF[[COLUMN]])
        }
        DF
      }
      DF[[COLUMN]] <- DF[[COLUMN]] %>% clean_column_for_table(
        class = class,
        label = label,
        units = units,
        levels = levels
      )
    }
  }
  return(DF)
}
#' @noRd
clean_column_for_table <- function(col,class,label,units,levels){
  if(!missing(class)){
    if(!is.null(class)){
      if(!is.na(class)){
        if(class=="integer"){
          col <-   col %>% as.integer()
        }
        if(class=="factor"){
          col <-   col %>% factor(levels = levels,ordered = T)
        }
        if(class=="numeric"){
          col <-   col %>% as.numeric()
        }
      }
    }
  }
  if(!missing(label)){
    if (!is.null(label)) {
      if(!is.na(label)){
        attr(col, "label") <- label
      }
    }
  }
  if(!missing(units)){
    if (!is.null(units)) {
      if(!is.na(units)){
        attr(col, "units") <- units
      }
    }
  }
  col
}
# generate_cross_codebook <- function(){
#   codebook <- DB$metadata$choices
#   metadata <- get_original_field_names(DB)
#   data_choice<-get_default_data_choice(DB)
#
#   cross_codebook <- codebook[,c("form_name","field_name","name")]
#   cross_codebook$label <- paste0(cross_codebook$form_name, " - ",cross_codebook$field_name, " - ", cross_codebook$name)
#   # Generate all combinations
#   all_combinations <- expand.grid(cross_codebook$label, cross_codebook$label)
#
#   # Filter out self pairs
#   all_combinations <- all_combinations[all_combinations$Var1 != all_combinations$Var2, ]
#
#   # Extract variable names
#   variable1 <- cross_codebook$label[match(all_combinations$Var1, cross_codebook$label)]
#   variable2 <- cross_codebook$label[match(all_combinations$Var2, cross_codebook$label)]
#   # Remove duplicates where variable1 and variable2 are interchanged
#   all_combinations <- data.frame(
#     variable1 = pmin(variable1, variable2),
#     variable2 = pmax(variable1, variable2)
#   ) %>% unique()
#   form_field_pairs <- paste0(codebook$form_name," - ",codebook$field_name)
#   for(form_field_pair in form_field_pairs){ #form_field_pair<-form_field_pairs %>% sample(1)
#     keep_rows <- which(!((all_combinations$variable1 %>% startsWith(form_field_pair)) & (all_combinations$variable2 %>% startsWith(form_field_pair))))
#     all_combinations <- all_combinations[keep_rows,]
#   }
#   cross_codebook1 <- cross_codebook
#   colnames(cross_codebook1) <- colnames(cross_codebook1) %>% paste0(1)
#   cross_codebook2 <- cross_codebook
#   colnames(cross_codebook2) <- colnames(cross_codebook2) %>% paste0(2)
#   all_combinations <- all_combinations %>% merge(cross_codebook1,by.x="variable1",by.y = "label1")
#   all_combinations <- all_combinations %>% merge(cross_codebook2,by.x="variable2",by.y = "label2")
#   rownames(all_combinations) <- NULL
#   i <- 1:nrow(all_combinations) %>% sample1()
#   all_combinations$n <- 1:nrow(all_combinations) %>% lapply(function(i){
#     x<-DB$data[[all_combinations$form_name1[i]]]
#     x<-DB$data[[all_combinations$form_name1[i]]]
#     x[which(x[[all_combinations[i]]])]
#
#     ==codebook$name[i],na.rm = T)
#   }) %>% unlist()
#   codebook$n_total <- 1:nrow(codebook) %>% lapply(function(i){
#     sum(!is.na(DB$data[[codebook$form_name[i]]][,codebook$field_name[i]]),na.rm = T)
#   }) %>% unlist()
#   codebook$perc <-  (codebook$n/codebook$n_total) %>% round(4)
#   codebook$perc_text <- codebook$perc %>% magrittr::multiply_by(100) %>% round(1) %>% paste0("%")
#   return(codebook)
# }
#' @title Add a Subset to a REDCap Database
#' @description
#' Creates a subset of the main REDCap database (`DB`) based on specific filter criteria
#' and saves it to a specified directory. The subset can be further customized with
#' additional forms, fields, and deidentification options.
#'
#' @inheritParams save_DB
#' @param subset_name Character. The name of the subset to create.
#' @param filter_field Character. The name of the field in the database to filter on.
#' @param filter_choices Vector. The values of `filter_field` used to define the subset.
#' @param dir_other Character. The directory where the subset file will be saved.
#' Default is the `output` folder within the database directory.
#' @param file_name Character. The base name of the file where the subset will be saved.
#' Default is `<DB$short_name>_<subset_name>`.
#' @param form_names Character vector. Names of forms to include in the subset. Default is `NULL`, which includes all forms.
#' @param field_names Character vector. Names of specific fields to include in the subset. Default is `NULL`, which includes all fields.
#' @param deidentify Logical. Whether to deidentify the data in the subset. Default is `TRUE`.
#' @param force Logical. If `TRUE`, overwrite existing subset files with the same name. Default is `FALSE`.
#'
#' @return
#' A modified `DB` object that includes the newly created subset.
#' The subset is also saved as a file in the specified directory.
#'
#' @details
#' This function filters the main REDCap database using the specified `filter_field`
#' and `filter_choices`, then creates a new subset with optional deidentification.
#' It can be customized to include only specific forms or fields. The resulting subset
#' is saved to a file for future use.
#'
#' @seealso
#' \code{\link{save_DB}} for saving the main database or subsets.
#' @export
add_DB_subset <- function(
    DB,
    subset_name,
    filter_field,
    filter_choices,
    dir_other = file.path(DB$dir_path,"output"),
    file_name = paste0(DB$short_name,"_",subset_name),
    form_names = NULL,
    field_names = NULL,
    deidentify = T,
    force = F
){
  if(is.null(DB$summary$subsets[[subset_name]])||force){
    subset_records <- NULL
    if(filter_field==DB$redcap$id_col){
      records <- unique(filter_choices)
      filter_choices <- NULL
    }else{
      form_name <- field_names_to_form_names(DB,field_names = filter_field)
      records <- DB$data[[form_name]][[DB$redcap$id_col]][which(DB$data[[form_name]][[filter_field]]%in%filter_choices)] %>% unique()
    }
    subset_records <- DB$summary$all_records[which(DB$summary$all_records[[DB$redcap$id_col]]%in% records),]
    DB$summary$subsets[[subset_name]] <- list(
      subset_name = subset_name,
      filter_field = filter_field,
      filter_choices = filter_choices,
      form_names = form_names,
      field_names = field_names,
      subset_records = subset_records,
      dir_other = dir_other,
      file_name = file_name,
      last_save_time = NULL,
      deidentify = deidentify,
      file_path = file.path(dir_other,paste0(file_name,".xlsx"))
    )
  }
  return(DB)
}
#' @noRd
generate_summary_save_list <- function(
    DB,
    deidentify = T,
    clean = T,
    drop_blanks = T,
    other_drops = NULL,
    include_metadata = T,
    annotate_metadata = T,
    include_record_summary = T,
    include_users = T,
    include_log = T
){
  records <- sum_records(DB)[[1]]
  if(deidentify){
    DB <- deidentify_DB(DB)
  }
  if(clean){
    DB <- DB %>% clean_DB(drop_blanks = drop_blanks,other_drops = other_drops)# problematic because setting numeric would delete missing codes
  }
  to_save_list <- DB$data
  if(include_metadata){
    if(annotate_metadata&&is_something(DB$data)){
      to_save_list$forms <- annotate_forms(DB)
      to_save_list$fields <- annotate_fields(DB)
      to_save_list$choices <- annotate_choices(DB)
    }else{
      to_save_list$forms <- DB$metadata$forms
      to_save_list$fields <- DB$metadata$fields
      to_save_list$choices <- DB$metadata$choices
    }
    # if(DB$internals$is_transformed){
    #   to_save_list$original_forms <- DB$transformation$original_forms
    #   to_save_list$original_fields <- DB$transformation$original_fields
    # }
  }
  if(include_record_summary){
    to_save_list$records <- summarize_records_from_log(DB,records= records)
  }
  if(include_users){
    to_save_list$users <- summarize_users_from_log(DB,records= records)
  }
  if(include_log){
    to_save_list$log <- get_log(DB,records = records)
  }
  # to_save_list$choices <- annotate_choices(DB)
  # to_save_list$choices <- annotate_choices(DB)
  return(to_save_list)
}
#' @noRd
save_RosyREDCap_list <- function(
    DB,
    to_save_list,
    dir_other = file.path(DB$dir_path,"output"),
    file_name = paste0(DB$short_name,"_RosyREDCap"),
    separate = F,
    with_links = T
){
  link_col_list <- list()
  if(with_links){
    if(DB$internals$DB_type=="redcap"){
      add_links <- which(names(to_save_list)%in%names(DB$data))
      if(length(add_links)>0){
        to_save_list[add_links] <- to_save_list[add_links] %>% lapply(function(DF){add_redcap_links_to_DF(DF,DB)})
        link_col_list <- list(
          "redcap_link"
        )
        names(link_col_list) <- DB$redcap$id_col
      }
    }
  }
  if(DB$internals$use_csv){
    to_save_list %>% list_to_csv(
      dir = dir_other,
      file_name = file_name,
      overwrite = TRUE
    )
  }else{
    to_save_list %>% list_to_excel(
      dir = dir_other,
      separate = separate,
      link_col_list = link_col_list,
      file_name = file_name,
      # str_trunc_length = 10000,
      header_df_list = to_save_list %>% construct_header_list(fields = DB$metadata$fields) %>% process_df_list(silent = T),
      key_cols_list = construct_key_col_list(DB),
      overwrite = TRUE
    )
  }
}
#' @title Generate a Summary from a Subset Name
#' @description
#' Generates a summary from a predefined subset of data within a REDCap project. The summary can be customized based on various options, such as cleaning the data, including metadata, and annotating metadata.
#'
#' @inheritParams save_DB
#' @param subset_name Character. The name of the subset from which to generate the summary.
#' @param clean Logical. If `TRUE`, the data will be cleaned before summarizing. Default is `TRUE`.
#' @param drop_blanks Logical. If `TRUE`, records with blank fields will be dropped. Default is `TRUE`.
#' @param include_metadata Logical. If `TRUE`, metadata will be included in the summary. Default is `TRUE`.
#' @param annotate_metadata Logical. If `TRUE`, metadata will be annotated in the summary. Default is `TRUE`.
#' @param include_record_summary Logical. If `TRUE`, a record summary will be included in the generated summary. Default is `TRUE`.
#' @param include_users Logical. If `TRUE`, user-related information will be included in the summary. Default is `TRUE`.
#' @param include_log Logical. If `TRUE`, the log of changes will be included in the summary. Default is `TRUE`.
#'
#' @return
#' A list containing the generated summary based on the specified options. The list includes filtered and cleaned data, metadata, and other summary details.
#'
#' @details
#' This function allows you to generate a summary of data from a specific subset of records within the REDCap project. The function provides flexible options for cleaning, annotating, and including metadata, as well as controlling whether to include record summaries, user information, and logs.
#' @export
generate_summary_from_subset_name <- function(
    DB,
    subset_name,
    clean = T,
    drop_blanks = T,
    include_metadata = T,
    annotate_metadata = T,
    include_record_summary = T,
    include_users = T,
    include_log = T
){
  subset_list <- DB$summary$subsets[[subset_name]]
  if(subset_list$filter_field==DB$redcap$id_col){
    subset_list$filter_choices <- subset_list$subset_records[[DB$redcap$id_col]]
  }
  DB$data <- filter_DB(
    DB = DB,
    field_names = subset_list$field_names,
    form_names = subset_list$form_names,
    filter_field = subset_list$filter_field,
    filter_choices = subset_list$filter_choices
  )
  to_save_list <- DB %>% generate_summary_save_list(
    deidentify = subset_list$deidentify,
    clean = clean,
    drop_blanks = drop_blanks,
    include_metadata = include_metadata,
    annotate_metadata = annotate_metadata,
    include_record_summary = include_record_summary,
    include_users = include_users,
    include_log = include_log
  )
  return(to_save_list)
}
#' @title Summarize REDCap Database
#' @description
#' Summarizes the REDCap database (`DB` object) by filtering and generating a summary list.
#'
#' @details
#' This function filters the REDCap database based on the provided parameters and generates a summary list. The summary can include metadata, record summaries, user information, and logs. The function also supports deidentification and cleaning of the data.
#'
#' @inheritParams save_DB
#' @param with_links Logical (TRUE/FALSE). If TRUE, includes links in the summary. Default is `TRUE`.
#' @param deidentify Logical (TRUE/FALSE). If TRUE, deidentifies the summary data. Default is `TRUE`.
#' @param clean Logical (TRUE/FALSE). If TRUE, cleans the summary data. Default is `TRUE`.
#' @param drop_blanks Logical (TRUE/FALSE). If TRUE, drops blank entries from the summary. Default is `TRUE`.
#' @param include_metadata Logical (TRUE/FALSE). If TRUE, includes metadata in the summary. Default is `TRUE`.
#' @param annotate_metadata Logical (TRUE/FALSE). If TRUE, annotates metadata in the summary. Default is `TRUE`.
#' @param include_record_summary Logical (TRUE/FALSE). If TRUE, includes a summary of records in the summary. Default is `TRUE`.
#' @param include_users Logical (TRUE/FALSE). If TRUE, includes user information in the summary. Default is `TRUE`.
#' @param include_log Logical (TRUE/FALSE). If TRUE, includes logs in the summary. Default is `TRUE`.
#' @param separate Logical (TRUE/FALSE). If TRUE, separates the summary into different sections. Default is `FALSE`.
#' @param force Logical (TRUE/FALSE). If TRUE, forces the summary generation even if there are issues. Default is `FALSE`.
#' @return List. Returns a list containing the summarized data, including records, metadata, users, logs, and any other specified data.
#' @seealso
#' \link{setup_DB} for initializing the `DB` object.
#' \link{update_DB} for updating the `DB` object.
#' @family db_functions
#' @export
summarize_DB <- function(
    DB,
    with_links = T,
    deidentify = T,
    clean = T,
    drop_blanks = T,
    include_metadata = T,
    annotate_metadata = T,
    include_record_summary = T,
    include_users = T,
    include_log = T,
    separate = F,
    force = F
){
  DB <- DB %>% validate_DB()
  original_data <- DB$data
  do_it <- is.null(DB$internals$last_summary)
  last_data_update <- DB$internals$last_data_update
  if(!do_it){
    do_it <- DB$internals$last_summary<last_data_update
  }
  if(force | do_it){
    to_save_list <- DB %>% generate_summary_save_list(
      deidentify = deidentify,
      clean = clean,
      drop_blanks = drop_blanks,
      include_metadata = include_metadata,
      annotate_metadata = annotate_metadata,
      include_record_summary = include_record_summary,
      include_users = include_users,
      include_log = include_log
    )
    DB %>% save_RosyREDCap_list(
      to_save_list = to_save_list,
      separate = separate,
      with_links = with_links
    )
    DB$internals$last_summary <- last_data_update
  }
  subset_names <- check_subsets(DB)
  if(force)subset_names <- DB$summary$subsets %>% names()
  if(is_something(subset_names)){
    for(subset_name in subset_names){
      DB$data <- original_data
      subset_list <- DB$summary$subsets[[subset_name]]
      DB$summary$subsets[[subset_name]]$subset_records <- get_subset_records(DB=DB,subset_name = subset_name)
      DB$summary$subsets[[subset_name]]$last_save_time <- Sys.time()
      to_save_list <- DB %>% generate_summary_from_subset_name(
        subset_name = subset_name,
        clean = clean,
        drop_blanks = drop_blanks,
        include_metadata = include_metadata,
        annotate_metadata = annotate_metadata,
        include_record_summary = include_record_summary,
        include_users = include_users,
        include_log = include_log
      )
      DB %>% save_RosyREDCap_list(
        to_save_list = to_save_list,
        dir_other = subset_list$dir_other,
        file_name = subset_list$file_name,
        separate = separate,
        with_links = with_links
      )
    }
  }
  DB$data <- original_data
  return(DB)
}
#' @title Run Quality Checks
#' @inheritParams save_DB
#' @return DB object
#' @export
run_quality_checks <- function(DB){
  DB <- validate_DB(DB)
  if(is_something(DB$quality_checks)){
    for (qual_check in names(DB$quality_checks)){
      the_function <- DB$quality_checks[[qual_check]]
      if(is.function(the_function)){
        DB <- the_function(DB)
      }
    }
  }
  return(DB)
}
#' @noRd
sum_records <- function(DB){
  records <- NULL
  if(DB$data %>% is_something()){
    cols <- DB$redcap$id_col
    if(is.data.frame(DB$metadata$arms)){
      if(nrow(DB$metadata$arms)>1){
        cols <- DB$redcap$id_col %>% append("arm_num")
      }
    }
    if(length(cols)==1){
      records <- data.frame(
        records =  names(DB$data) %>% lapply(function(IN){DB$data[[IN]][,cols]}) %>% unlist() %>% unique()
      )
      colnames(records) <- cols
    }
    if(length(cols) == 2){
      records <- names(DB$data) %>% lapply(function(IN){DB$data[[IN]][,cols]}) %>% dplyr::bind_rows() %>% unique()
      # records <- records[order(as.integer(records[[DB$redcap$id_col]])),]
    }
    rownames(records) <- NULL
    if(records[[DB$redcap$id_col]]%>% duplicated() %>% any())stop("duplicate ",DB$redcap$id_col, " in sum_records() function")
  }
  return(records)
}
#' @noRd
get_log <- function(DB, records){
  log <- DB$redcap$log
  log <- log[which(!is.na(log$username)),]
  log <- log[which(!is.na(log$record)),]
  if(!missing(records)){
    if(!is.null(records)){
      log <- log[which(log$record%in%records),]
    }
  }
  return(log)
}
#' @noRd
summarize_users_from_log <- function(DB,records){
  log <- get_log(DB,records)
  summary_users <- DB$redcap$users %>% dplyr::select(c("username","role_label","email" ,"firstname","lastname"))
  user_groups <- log %>% split(log$username)
  summary_users <- summary_users[which(summary_users$username%in%names(user_groups)),]
  user_groups <- user_groups[drop_nas(match(summary_users$username, names(user_groups)))]
  summary_users$last_timestamp <- user_groups %>% sapply(function(group) {
    group$timestamp[[1]]
  })
  summary_users$first_timestamp <- user_groups %>% sapply(function(group) {
    group$timestamp %>% dplyr::last()
  })
  summary_users$last_user <- user_groups %>% sapply(function(group) {
    group$username[[1]]
  })
  summary_users$unique_records_n <- user_groups %>% sapply(function(group) {
    ul(group$record)
  })
  return(summary_users)
}
#' @noRd
summarize_records_from_log <- function(DB,records){
  log <- DB$redcap$log
  log <- log[which(!is.na(log$username)),]
  log <- log[which(!is.na(log$record)),]
  if(!missing(records)){
    if(!is.null(records)){
      log <- log[which(log$record%in%records),]
    }
  }
  #records -------------
  # all_records <- unique(log$record)
  summary_records <- DB$summary$all_records
  record_groups <- log %>% split(log$record)
  summary_records <- summary_records[which(summary_records[[DB$redcap$id_col]]%in%names(record_groups)),,drop = FALSE]
  # users_log_rows <- users %>% lapply(function(user){which(log$username==user)})
  # records_log_rows <- records %>% lapply(function(record){which(log$record==record)})
  record_groups <- record_groups[match(summary_records[[DB$redcap$id_col]], names(record_groups))]
  summary_records$last_timestamp <- record_groups %>% sapply(function(group) {
    group$timestamp[[1]]
  })
  summary_records$first_timestamp <- record_groups %>% sapply(function(group) {
    group$timestamp %>% dplyr::last()
  })
  summary_records$last_user <- record_groups %>% sapply(function(group) {
    group$username[[1]]
  })
  summary_records$unique_users_n <- record_groups %>% sapply(function(group) {
    ul(group$username)
  })
  return(summary_records)
}
#' @noRd
get_subset_records <-function(DB,subset_name){
  subset_list <- DB$summary$subsets[[subset_name]]
  subset_records <- NULL
  if(subset_list$filter_field==DB$redcap$id_col){
    records <- unique(subset_list$subset_records[[DB$redcap$id_col]])
  }else{
    filter_choices <- subset_list$filter_choices
    form_name <- field_names_to_form_names(DB,field_names = subset_list$filter_field)
    records <- DB$data[[form_name]][[DB$redcap$id_col]][which(DB$data[[form_name]][[subset_list$filter_field]]%in%subset_list$filter_choices)] %>% unique()
  }
  subset_records <- DB$summary$all_records[which(DB$summary$all_records[[DB$redcap$id_col]]%in% records),]
  return(subset_records)
}
#' @noRd
subset_records_due <- function(DB,subset_name){
  subset_list <- DB$summary$subsets[[subset_name]]
  if(is.null(subset_list$last_save_time))return(TRUE)
  if(!file.exists(subset_list$file_path))return(TRUE)
  subset_records <- get_subset_records(
    DB = DB,
    subset_name = subset_name
  )
  return(!identical(unname(subset_list$subset_records),unname(subset_records)))
}
#' @noRd
check_subsets <- function(DB,subset_names){
  if(missing(subset_names))subset_names <- DB$summary$subsets %>% names()
  needs_refresh <- NULL
  if(is.null(subset_names))bullet_in_console("There are no subsets at `DB$summary$subsets` which can be added with `add_DB_subset()`!")
  for(subset_name in subset_names){
    if(subset_records_due(DB = DB, subset_name=subset_name))needs_refresh <- needs_refresh %>% append(subset_name)
  }
  if(is.null(needs_refresh))bullet_in_console("Refresh of subsets not needed!",bullet_type = "v")
  return(needs_refresh)
}
#' @title Select REDCap Records from DB
#' @description
#' This function filters the records in the `DB` object by specified criteria, such as field names, form names, and optional filtering based on a specific field and its values. It returns a modified `DB` object containing only the records that match the filter criteria.
#'
#' @inheritParams save_DB
#' @param field_names A character vector of field names to be included in the filtered data. If missing, all fields are included.
#' @param form_names A character vector of form names to be included in the filtered data. If missing, all forms are included.
#' @param filter_field A character string representing an extra variable name to be filtered by. This field must be present in the data frame.
#' @param filter_choices A character vector of values to filter by for the `filter_field`. Only records with these values in the specified field will be included.
#' @param warn_only A logical flag (`TRUE` or `FALSE`). If `TRUE`, the function will issue a warning instead of stopping if the filtering criteria do not match any records. Defaults to `FALSE`.
#' @param no_duplicate_cols A logical flag (`TRUE` or `FALSE`). If `TRUE`, the function will avoid including duplicate columns in the output. Defaults to `FALSE`.
#'
#' @return A modified `DB` object with filtered records and columns based on the provided criteria.
#'
#' @details
#' This function filters the data in the `DB` object according to the specified form and field names and optional filter criteria. If no field names or form names are provided, it defaults to using all fields and forms in the database.
#' The function uses the helper `filter_DF_list` to apply the filtering logic to the `DB$data` list.
#'
#' @export
filter_DB <- function(DB, filter_field, filter_choices, form_names, field_names, warn_only = F, no_duplicate_cols = F){#, ignore_incomplete=F, ignore_unverified = F
  if(missing(field_names))field_names <- DB %>% get_all_field_names()
  if(is.null(field_names))field_names <- DB %>% get_all_field_names()
  if(missing(form_names))form_names <- names(DB$data)
  if(is.null(form_names))form_names <- names(DB$data)
  return(
    filter_DF_list(
      DF_list = DB$data,
      DB = DB,
      filter_field = filter_field,
      filter_choices = filter_choices,
      form_names = form_names,
      field_names = field_names,
      warn_only = warn_only,
      no_duplicate_cols = no_duplicate_cols
    )
  )
}
#' @title rmarkdown_DB
#' @description
#' Generates an RMarkdown report for the given REDCap database (`DB` object).
#' This function creates an RMarkdown file in the specified directory or default directory,
#' allowing users to create custom reports based on the database content.
#'
#' @details
#' This function checks if a directory is specified, and if not, defaults to the `output` folder
#' within the project's directory. It generates the RMarkdown file that can then be used for further
#' processing or rendering into HTML, PDF, or other formats.
#'
#' @inheritParams save_DB
#' @param dir_other Character string specifying the directory where the RMarkdown report will be saved.
#' If not provided, it defaults to the `output` directory inside the project's main directory.
#' @return A message indicating the creation of the RMarkdown report and the path to the generated file.
#' @seealso
#' \code{\link[RosyREDCap]{save_DB}} for saving the `DB` object.
#' @family db_functions
#' @export
rmarkdown_DB <- function (DB,dir_other){
  if(missing(dir_other)){
    dir <- get_dir(DB) %>% file.path("output")
  }else{
    dir  <- dir_other
  }
  filename <- paste0(DB$short_name,"_full_summary_",gsub("-","_",Sys.Date()),".pdf")
  rmarkdown::render(
    input = system.file("rmarkdown","pdf.Rmd",package = pkg_name),
    output_format = "pdf_document",
    output_file = dir %>% file.path(filename),
    output_dir = dir,
    quiet = F
  )
}
#' @title Clean to Raw REDCap forms
#' @inheritParams save_DB
#' @param FORM data.frame of labelled REDCap to be converted to raw REDCap (for uploads)
#' @return DB object that has been filtered to only include the specified records
#' @export
labelled_to_raw_form <- function(FORM,DB){
  use_missing_codes <- is.data.frame(DB$metadata$missing_codes)
  fields <- filter_fields_from_form(FORM = FORM,DB = DB)
  for(i in 1:nrow(fields)){ # i <-  1:nrow(fields) %>% sample(1)
    COL_NAME <- fields$field_name[i]
    has_choices <- fields$has_choices[i]
    if(has_choices){
      z <- fields$select_choices_or_calculations[i] %>% split_choices()
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
#' @param FORM data.frame of raw REDCap to be converted to labelled REDCap
#' @inheritParams save_DB
#' @return DB object
#' @export
raw_to_labelled_form <- function(FORM,DB){
  if(nrow(FORM)>0){
    use_missing_codes <- is.data.frame(DB$metadata$missing_codes)
    metadata <- filter_fields_from_form(FORM = FORM,DB = DB)
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
#' @noRd
stack_vars <- function(DB,vars,new_name,drop_na=T){
  DB <- validate_DB(DB)
  fields <- DB$metadata$fields
  if(!all(vars%in%fields$field_name))stop("all vars must be in metadata.")
  the_stack <- NULL
  for(var in vars){# var <- vars %>% sample1()
    DF <- filter_DB(DB,field_names = var)[[1]]
    colnames(DF)[which(colnames(DF)==var)] <- new_name
    the_stack <-the_stack %>% dplyr::bind_rows(DF)
  }
  if(drop_na){
    the_stack <- the_stack[which(!is.na(the_stack[[new_name]])),]
  }
  return(the_stack)
}
#' @noRd
get_original_field_names <- function(DB){
  if(DB$internals$is_transformed)return(DB$transformation$original_fields$field_name)
  return(DB$metadata$fields$field_name)
}
#' @noRd
get_all_field_names <- function(DB){
  return(DB$data %>% sapply(colnames) %>% unlist() %>% unique())
}
#' @noRd
field_names_to_form_names <- function(DB,field_names){
  form_key_cols <- DB$metadata$form_key_cols %>% unlist() %>% unique()
  field_names_keys <- field_names[which(field_names%in%form_key_cols)]
  form_names_keys <- field_names_keys %>% sapply(function(field_name){
    DB$metadata$form_key_cols%>% names()%>% sapply(function(FORM){
      if(!field_name%in%DB$metadata$form_key_cols[[FORM]])return(NULL)
      return(FORM)
    })
  }) %>% unlist() %>% as.character() %>% unique()
  fields <- DB$metadata$fields
  field_names_not_keys <- field_names[which(!field_names%in%form_key_cols)]
  form_names_not_keys <- fields$form_name[match(field_names_not_keys, fields$field_name)] %>% drop_nas()
  form_names <- c(form_names_not_keys,form_names_keys) %>% unique()
  return(form_names)
}
#' @noRd
form_names_to_field_names <- function(form_names,DB,original_only = F){
  field_names <- NULL
  if(original_only){
    fields <- get_original_fields(DB)
  }else{
    fields <- DB$metadata$fields
  }
  for(form_name in form_names){
    field_names <- field_names %>% append(fields$field_name[which(fields$form_name==form_name)])
  }
  return(unique(field_names))
}
#' @noRd
form_names_to_form_labels <- function(form_names,DB){
  return(
    DB$metadata$forms$form_label[
      match(
        x = form_names,
        table = DB$metadata$forms$form_name
      )
    ]
  )
}
#' @noRd
form_labels_to_form_names <- function(form_labels,DB){
  return(
    DB$metadata$forms$form_name[
      match(
        x = form_labels,
        table = DB$metadata$forms$form_label
      )
    ]
  )
}
#' @noRd
field_names_to_field_labels <- function(field_names,DB){
  return(
    DB$metadata$fields$field_label[
      match(
        x = field_names,
        table = DB$metadata$fields$field_name
      )
    ]
  )
}
#' @noRd
construct_header_list <- function(DF_list,md_elements = c("form_name","field_type","field_label"),fields){
  if(anyDuplicated(fields$field_name)>0)stop("dup names not allowed in fields")
  df_col_list <- DF_list %>% lapply(colnames)
  header_df_list <- df_col_list %>% lapply(function(field_names){
    x<- field_names%>% lapply(function(field_name){
      row <- which(fields$field_name==field_name)
      if(length(row)>0){
        return(as.character(fields[md_elements][row,]))
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
#' @noRd
stripped_DB <- function (DB) {
  DB$redcap$log <- list()
  DB$data <- list()
  DB$data_update <- list()
  return(DB)
}
#' @noRd
filter_DF_list <- function(DF_list,DB,filter_field, filter_choices, form_names, field_names, warn_only = F, no_duplicate_cols = F){
  if(missing(field_names))field_names <- DB %>% get_all_field_names()
  if(is.null(field_names))field_names <- DB %>% get_all_field_names()
  if(missing(form_names))form_names <- names(DF_list)
  if(is.null(form_names))form_names <- names(DF_list)
  out_list <- list()
  form_key_cols <- DB$metadata$form_key_cols %>% unlist() %>% unique()
  is_key <- filter_field %in% form_key_cols
  if(!is_key){
    form_name <- field_names_to_form_names(DB,field_names = filter_field)
    is_repeating_filter <- DB$metadata$forms$repeating[which(DB$metadata$forms$form_name==form_name)]
  }
  for(FORM in form_names){
    DF <- DF_list[[FORM]]
    is_repeating_form <- DB$metadata$forms$repeating[which(DB$metadata$forms$form_name==FORM)]
    if(is_something(DF)){
      filter_field_final <- filter_field
      filter_choices_final <- filter_choices
      if(!is_key){
        if(is_repeating_filter){
          if(!is_repeating_form){
            filter_field_final <- DB$metadata$form_key_cols[[FORM]]
            filter_choices_final <- DF_list[[form_name]][[filter_field_final]][which(DF_list[[form_name]][[filter_field]]%in%filter_choices)] %>% unique()
          }
        }
      }
      rows <-which(DF_list[[FORM]][[filter_field_final]]%in%filter_choices_final)
      field_names_adj <- field_names
      if(no_duplicate_cols) field_names_adj <- field_names_adj %>% vec1_in_vec2(form_names_to_field_names(FORM,DB,original_only = F))
      cols <- colnames(DF)[which(colnames(DF)%in%field_names_adj)]
      if(length(rows)>0&&length(cols)>0){
        cols <- colnames(DF)[which(colnames(DF)%in%unique(c(DB$metadata$form_key_cols[[FORM]],field_names_adj)))]
        out_list[[FORM]] <- DF[rows,cols]
      }
    }
  }
  return(out_list)
}
#' @noRd
field_names_metadata <- function(DB,field_names,col_names){
  fields <- get_original_fields(DB) #DB$metadata$fields
  # if(!deparse(substitute(FORM))%in%DB$metadata$forms$form_name)stop("To avoid potential issues the form name should match one of the instrument names" )
  BAD <- field_names[which(!field_names%in%c(DB$metadata$fields$field_name,DB$redcap$raw_structure_cols,"arm_num","event_name"))]
  if(length(BAD)>0)stop("All column names in your form must match items in your metadata, `DB$metadata$fields$field_name`... ", paste0(BAD, collapse = ", "))
  # metadata <- DB$metadata$fields[which(DB$metadata$fields$form_name%in%instruments),]
  fields <- fields[which(fields$field_name%in%field_names),]
  # metadata <- metadata[which(metadata$field_name%in%field_names),]
  if( ! missing(col_names)){
    if(is_something(col_names))fields <- fields[[col_names]]
  }
  return(fields)
}
#' @noRd
filter_fields_from_form <- function(FORM,DB){
  forms <- DB %>% field_names_to_form_names(field_names = colnames(FORM))
  if(any(forms%in%get_original_forms(DB)$repeating))stop("All column names in your form must match only one form in your metadata, `DB$metadata$forms$form_name`, unless they are all non-repeating")
  fields <- DB %>% field_names_metadata(field_names = colnames(FORM))
  fields <- fields[which(fields$field_type!="descriptive"),]
  fields$has_choices <- !is.na(fields$select_choices_or_calculations)
  return(fields)
}
#' @noRd
labelled_to_raw_DB <- function(DB){
  DB <- validate_DB(DB)
  if(!DB$internals$data_extract_labelled)stop("DB is already raw/coded (not labelled values)")
  for(TABLE in names(DB$data)){
    DB$data[[TABLE]] <- labelled_to_raw_form(FORM = DB$data[[TABLE]],DB=DB)
  }
  DB$internals$data_extract_labelled <- F
  DB
}
#' @noRd
DF_list_to_text <- function(DF_list, DB,drop_nas = T,clean_names= T){
  output_list <- c()
  for (i in seq_along(DF_list)) {
    DF <- DF_list[[i]]
    the_raw_name <- names(DF_list)[[i]]
    the_name <- the_raw_name
    if(clean_names)the_name <- DB$metadata$forms$form_label[which(DB$metadata$forms$form_name==the_raw_name)]
    df_name <- paste0("----- ",the_name, " Table -----")
    output_list <- c(output_list, paste0("&nbsp;&nbsp;<strong>", df_name, "</strong><br>"))
    key_col_names <- DB$metadata$form_key_cols[[the_raw_name]]
    for (j in 1:nrow(DF)) {
      for (col_name in colnames(DF)) {
        entry <- DF[j, col_name]
        if (!col_name %in% key_col_names) {
          if(!is.na(entry)|!drop_nas){
            entry <- gsub("\\n","<br>",entry)
            col_name_clean <- col_name
            if(clean_names)col_name_clean <- DB$metadata$fields$field_label[which(DB$metadata$fields$field_name==col_name)]
            output_list <- c(output_list, paste0("&nbsp;&nbsp;<strong>", col_name_clean, ":</strong> <br>&nbsp;&nbsp;&nbsp;&nbsp;", entry,"<br>"))
          }
        }
      }
      # output_list <- c(output_list, "<br>")
    }
    output_list <- c(output_list, "<br>")
  }
  return(output_list)
}
#' @noRd
check_DB_for_IDs <- function(DB,required_percent_filled = 0.7){
  cols <- NULL
  if(is_something(DB)){
    if(is_something(DB$data)){
      DF <- DB$data[[DB$metadata$forms$form_name[which(!DB$metadata$forms$repeating)][[1]]]]
      IN_length <- DF %>% nrow()
      cols <- colnames(DF)[DF %>% sapply(function(IN){
        OUT <- F
        x <- IN %>% drop_nas()
        if((length(x)/IN_length)>required_percent_filled){
          OUT <- anyDuplicated(x)==0
        }
        return(OUT)
      }) %>% unlist() %>% which()]
    }
  }
  return(cols)
}
