#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
#' @title choice_vector_in_redcap_form
#' @export
choice_vector_in_redcap_form <- function(vec){
  if(is_something(vec))return(NA)
  return(paste0(paste0(1:length(vec),", ",vec),collapse = " | "))
}
#' @import RosyUtils
#' @title add_new_varriable
#' @export
add_new_varriable <- function(
    DB,
    field_name,
    form_name,
    field_type,
    field_type_R = NA,
    field_label = NA,
    select_choices_or_calculations = NA,
    field_note = NA,
    identifier = "",
    units = NA,
    insert_after,
    data_col
) {
  DB <-validate_DB(DB)
  if(!DB$data_transform %>% is_something())stop("Must have transformed data to add new vars.")
  metadata_new_var <- data.frame(
    field_name = field_name,
    form_name = form_name,
    field_type = field_type,
    field_label = field_label,
    select_choices_or_calculations = choice_vector_in_redcap_form(select_choices_or_calculations),
    field_note = field_note,
    identifier = identifier,
    field_type_R = field_type_R,
    units = units,
    in_original_redcap = F,
    field_label_short = field_label
  )
  # metadata <- DB$
  DB$remap$metadata <-  DB$remap$metadata[which(DB$remap$metadata$field_name!=field_name),]
  if(!missing(insert_after)){
    i<-which(DB$remap$metadata$field_name == insert_after)
    if(length(i)==1){
      top <- DB$remap$metadata[1:i,]
      bottom <- DB$remap$metadata[(i+1):nrow(DB$remap$metadata),]
      DB$remap$metadata <- dplyr::bind_rows(top,metadata_new_var) %>% dplyr::bind_rows(bottom)
    }else{
      stop("insert_after error")
    }
  }else{
    DB$remap$metadata <-  DB$remap$metadata %>% dplyr::bind_rows(metadata_new_var)
  }
  message("added '",field_name,"' column")
  is_missing_data_col <- missing(data_col)
  if(is_missing_data_col)warning("if no `data_col` is provided, the column is only added to the metadata",immediate. = T)
  if(!is_missing_data_col)DB$data_transform[[form_name]][[field_name]] <- data_col
  return(DB)
}
