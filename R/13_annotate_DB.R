#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
annotate_metadata <- function(DB,metadata,data_choice="data",skim= T){
  metadata$field_label[which(is.na(metadata$field_label))] <- metadata$field_name[which(is.na(metadata$field_label))]
  metadata  <- unique(metadata$form_name) %>%
    lapply(function(IN){
      metadata[which(metadata$form_name==IN),]
    }) %>% dplyr::bind_rows()
  if(!"field_type_R"%in%colnames(metadata))metadata$field_type_R <- NA
  metadata$field_type_R[which(metadata$field_type %in% c("radio","yesno","dropdown"))] <- "factor"
  metadata$field_type_R[which(metadata$text_validation_type_or_show_slider_number == "integer")] <- "integer"
  metadata$field_type_R[which(metadata$text_validation_type_or_show_slider_number == "date_mdy")] <- "date"
  metadata$field_type_R[which(metadata$text_validation_type_or_show_slider_number == "date_ymd")] <- "date"
  metadata$field_type_R[which(metadata$text_validation_type_or_show_slider_number == "datetime_dmy")] <- "datetime"
  metadata$in_original_redcap <- metadata$field_name %in% DB$metadata$fields$field_name
  if(!"units" %in% colnames(metadata))metadata$units <- NA
  if(!"field_label_short" %in% colnames(metadata)) metadata$field_label_short <- metadata$field_label
  # if(!"field_label_short" %in% colnames(metadata))metadata$ <- metadata$field_label
  if(skim){
    skimmed <- NULL
    for (form in unique(metadata$form_name)){
      COLS <- metadata$field_name[which(metadata$form_name==form)]
      CHECK_THIS <- DB[[data_choice]][[form]]
      COLS <- COLS[which(COLS %in% colnames(CHECK_THIS))]
      skimmed <- skimmed %>% dplyr::bind_rows(CHECK_THIS[,COLS] %>% skimr::skim())
    }
    FOR_ORDERING <- metadata$field_name
    metadata <- metadata %>% merge(skimmed,by.x = "field_name",by.y = "skim_variable",all = T)
    metadata <- FOR_ORDERING %>%
      lapply(function(IN){
        metadata[which(metadata$field_name==IN),]
      }) %>% dplyr::bind_rows()
  }
  return(metadata)
}
metadata_to_codebook <- function(metadata){
  rows_with_choices <- which(metadata$field_type%in%c("radio","dropdown","checkbox_choice","yesno"))
  codebook <- NULL
  if(length(rows_with_choices)>0){
    for(field_name in metadata$field_name[rows_with_choices]){
      choices <- metadata$select_choices_or_calculations[which(metadata$field_name==field_name)] %>% split_choices()
      codebook <- codebook %>% dplyr::bind_rows(
        data.frame(
          field_name = field_name,
          code = choices$code,
          name =choices$name
        )
      )
    }
  }
  rownames(codebook) <- NULL
  return(codebook)
}
annotate_instruments <- function(DB,instruments){
  choice <- "instrument_name"
  if("former_instrument_names" %in% colnames(instruments)){
    choice <- "former_instrument_names"
  }
  for(status in c("Incomplete","Unverified","Complete")){
    instruments[[tolower(status)]] <- instruments[[choice]] %>% sapply(function(former_instrument_names){
      former_instrument_names %>% strsplit(" [:|:] ") %>% unlist() %>%  sapply(function(instrument_name){
        (DB[[get_default_data_choice(DB)]][[instrument_name]][[paste0(instrument_name,"_complete")]]==status) %>% which() %>% length()
      }) %>% paste0(collapse = " | ")
    })
  }
  return(instruments)
}
annotate_codebook <- function(codebook,metadata,data_choice="data",DB){
  codebook <- unique(metadata$field_name) %>%
    lapply(function(IN){
      codebook[which(codebook$field_name==IN),]
    }) %>% dplyr::bind_rows()
  codebook <- codebook %>% merge(
    metadata %>% dplyr::select(
      "form_name","field_name","field_label","field_type","field_type_R"),by="field_name",sort=F)
  codebook$form_name <- 1:nrow(codebook) %>% lapply(function(i){
    form_name <- codebook$form_name[i]
    field_name <- codebook$field_name[i]
    if(!form_name %in% names(DB[[data_choice]])){
      if(DB$internals$merge_form_name %in% names(DB[[data_choice]])){
        if(field_name%in%colnames(DB[[data_choice]]$merged))return(DB$internals$merge_form_name)
      }
      for(other in names(DB[[data_choice]])[which(!names(DB[[data_choice]])%in%DB$metadata$forms$instrument_name)]){
        if(field_name%in%colnames(DB[[data_choice]][[other]]))return(other)
      }
    }
    return(form_name)
  }) %>% unlist()
  codebook$field_name_raw <- codebook$field_name
  codebook$field_name_raw[which(codebook$field_type=="checkbox_choice")] <- codebook$field_name[which(codebook$field_type=="checkbox_choice")] %>%
    strsplit("___") %>%
    sapply(function(X){X[[1]]})
  codebook$field_label_raw <- codebook$field_label
  codebook$field_label_raw[which(codebook$field_type=="checkbox_choice")] <- codebook$field_name_raw[which(codebook$field_type=="checkbox_choice")] %>%
    sapply(function(X){
      metadata$field_label[which(metadata$field_name==X)] %>% unique()
    })
  codebook$n <- 1:nrow(codebook) %>% lapply(function(i){
    DF <- DB[[data_choice]][[codebook$form_name[i]]]
    if(nrow(DF)==0)return(0)
    sum(DF[,codebook$field_name[i]]==codebook$name[i],na.rm = T)
  }) %>% unlist()
  codebook$n_total <- 1:nrow(codebook) %>% lapply(function(i){
    DF <- DB[[data_choice]][[codebook$form_name[i]]]
    if(nrow(DF)==0)return(0)
    sum(!is.na(DF[,codebook$field_name[i]]),na.rm = T)
  }) %>% unlist()
  codebook$perc <-  (codebook$n/codebook$n_total) %>% round(4)
  codebook$perc_text <- codebook$perc %>% magrittr::multiply_by(100) %>% round(1) %>% paste0("%")
  return(codebook)
}
# generate_cross_codebook <- function(){
#   codebook <- DB$summary$codebook
#   metadata <- get_default_metadata(DB)
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
#     x<-DB[[data_choice]][[all_combinations$form_name1[i]]]
#     x<-DB[[data_choice]][[all_combinations$form_name1[i]]]
#     x[which(x[[all_combinations[i]]])]
#
#     ==codebook$name[i],na.rm = T)
#   }) %>% unlist()
#   codebook$n_total <- 1:nrow(codebook) %>% lapply(function(i){
#     sum(!is.na(DB[[data_choice]][[codebook$form_name[i]]][,codebook$field_name[i]]),na.rm = T)
#   }) %>% unlist()
#   codebook$perc <-  (codebook$n/codebook$n_total) %>% round(4)
#   codebook$perc_text <- codebook$perc %>% magrittr::multiply_by(100) %>% round(1) %>% paste0("%")
#   return(codebook)
# }
#' @title clean DB columns for plotting using the metadata
#' @description
#'  Turns choices into factors and integers to integer for table processing such as with table1 and plots
#' @inheritParams save_DB
#' @param drop_blanks logical for dropping n=0 choices
#' @param drop_unknowns logical for dropping missing codes
#' @return DB object cleaned for table or plots
#' @export
clean_DB <- function(DB,drop_blanks=F,drop_unknowns=F){
  for (data_choice in c("data","data_transform")) {
    redcap_remap <- ifelse(data_choice=="data","redcap","remap")
    metadata <-  DB %>% annotate_metadata(metadata = DB[[redcap_remap]]$metadata, skim = F)
    for(FORM in names(DB[[data_choice]])){
      DB[[data_choice]][[FORM]] <- DB[[data_choice]][[FORM]] %>% clean_DF(metadata=metadata,drop_blanks= drop_blanks,drop_unknowns=drop_unknowns)
    }
  }
  return(DB)
}
clean_DF <- function(DF,metadata,drop_blanks = T,drop_unknowns = T){
  for(COLUMN in colnames(DF)){
    if(COLUMN %in% metadata$field_name){
      ROW <- which(metadata$field_name==COLUMN)
      units <- NULL
      if(!is.na(metadata$units[ROW])){
        units <- metadata$units[ROW]
      }
      class <- metadata$field_type_R[ROW][[1]]
      label <- ifelse(is.na(metadata$field_label[ROW]),COLUMN,metadata$field_label[ROW])[[1]]
      levels <- NULL
      if(!is.na(class)){
        if(class == "factor"){
          select_choices <- metadata$select_choices_or_calculations[ROW]
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
          if(!drop_unknowns){
            levels <- levels %>% append(unique(DF[[COLUMN]])) %>% unique() %>% drop_nas()
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
#' @title clean column for plotting; manual addition of clean_DB
#' @description
#'  Turns choices into factors and integers to integer for table processing such as with table1 and plots
#' @param col the column vector
#' @param class character for column type: integer, factor, numeric
#' @param label character for label
#' @param units character for units
#' @param levels character vector of levels for factor
#' @return cleaned column
#' @export
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
    attr(col, "label") <- label
  }
  if(!missing(units)){
    attr(col, "units") <- units
  }
  col
}
