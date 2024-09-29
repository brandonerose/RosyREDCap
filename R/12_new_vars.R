#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
#' @title choice_vector_in_redcap_form
#' @export
choice_vector_in_redcap_form <- function(vec){
  if(!is_something(vec))return(NA)
  return(paste0(paste0(1:length(vec),", ",vec),collapse = " | "))
}
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
    data_func = NULL
) {
  DB <-validate_RosyREDCap(DB)
  # if(!DB$data_transform %>% is_something())stop("Must have transformed data to add new vars.")
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
    in_original_redcap = field_name%in%DB$metadata$fields$field_name,
    field_label_short = field_label
  )
  # if(any(DB$metadata$fields$field_name==field_name))stop("field_name already included")
  if(missing(insert_after)){
    i<-which(DB$metadata$fields$form_name == form_name&DB$metadata$fields$field_name == paste0(form_name,"_complete"))
    if(length(i)>0){
      if(i[[1]]>1){
        i <- i-1
      }
    }
    if(length(i)==0){
      i<-which(DB$metadata$fields$form_name == form_name)
    }
    if(length(i)>1){
      i <- i[[1]]
    }
    if(length(i)==0)i <- nrow(DB$metadata$fields$field_name)
  }else{
    i<-which(DB$metadata$fields$field_name == insert_after)
  }
  if(length(i)==0)stop("insert_after error")
  top <- DB$metadata$fields[1:i,]
  bottom <- DB$metadata$fields[(i+1):nrow(DB$metadata$fields),]
  # DB$metadata$fields <- dplyr::bind_rows(top,metadata_new_var) %>% dplyr::bind_rows(bottom)

  if(is.null(data_func))warning("if no `data_func` is provided, the column is only added to the metadata",immediate. = T)
  DB$transformation$new_vars[[field_name]]<-list()
  DB$transformation$new_vars[[field_name]]$field_row <- metadata_new_var
  DB$transformation$new_vars[[field_name]]$field_func <- data_func
  message("added '",field_name,"' column")
  return(DB)
}
