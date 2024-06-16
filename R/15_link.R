#' @import RosyUtils
#' @title Link to get a new API token for your project (if you access)
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
link_API_token <-  function(DB){
  DB$links$redcap_API %>% utils::browseURL()
}
#' @title Link view the API playground for your project (if you access)
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
link_API_playground <- function(DB){
  DB$links$redcap_API_playground %>% utils::browseURL()
}
#' @title Link view the REDCap project home page
#' @inheritParams save_DB
#' @return opens browser link
#' @export
link_REDCap_home <- function(DB){
  DB$links$redcap_base %>% utils::browseURL()
}
#' @title Shows DB in the env
#' @inheritParams save_DB
#' @return opens browser link
#' @export
link_REDCap_project <- function(DB){
  DB$links$redcap_home %>% utils::browseURL()
}
#' @title Shows DB in the env
#' @inheritParams save_DB
#' @param record REDCap record id or study id etc, any column names that match `DB$redcap$id_col`
#' @param page REDCap page for the record. Must be one of `DB$redcap$instruments$instrument_name`
#' @param instance REDCap instance if it's a repeating instrument
#' @return opens browser link
#' @export
link_REDCap_record <- function(DB,record,page,instance){
  link <- paste0(DB$links$redcap_base,"redcap_v",DB$redcap$version,"/DataEntry/record_home.php?pid=",DB$redcap$project_id)
  if(!missing(record)){
    if(!record%in%DB$summary$all_records[[DB$redcap$id_col]])stop(record," is not one of the records inside DB")
    if("arm_num"%in%colnames(DB$summary$all_records)){
      link <- link %>% paste0("&arm=", DB$summary$all_records$arm_num[which(DB$summary$all_records$participant_id==record)])
    }
    link <- link %>% paste0("&id=",record)
  }
  if(!missing(page)){
    link <- gsub("record_home","index",link)
    if(!page%in%DB$redcap$instruments$instrument_name)stop(page," has to be one of the instrument names: ",paste0(DB$redcap$instruments$instrument_name,collapse = ", "))
    link <- link %>% paste0("&page=",page)
    if(!missing(instance)){
      if(!page%in%DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)])stop("If you provide an instance, it has to be one of the repeating instrument names: ",paste0(DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)],collapse = ", "))
      link <- link %>% paste0("&instance=",instance)
    }
  }
  link %>% utils::browseURL()
}
