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
