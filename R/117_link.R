#' @import RosyUtils
#' @import RosyApp
#' @rdname Links
#' @title Open Links to REDCap Pages
#' @description
#' Opens browser page for a given DB object.
#' @details
#' Uses [utils::browseURL()] to open the specified REDCap page.
#' In order for the function to work you must have ran \code{DB <- update_DB(DB)} successfully at least once.
#' If the link brings you to a page that doesn't work check the url. It's possible your institution may have changed redcap versions, which is part of the URL. In that case run `{DB <- update_DB(DB)}` again.
#' You may have to be signed into REDCap for it to work properly.
#' When in doubt, just seek out the page by navigating on your own in REDCap. Report issues if you can.
#' @param DB A validated `DB` object containing REDCap project data and settings. Generated using \code{DB <- \link{load_DB}("PROJ")} or \link{setup_DB()}
#' @return Nothing will be returned in R. Instead, a browser link
#' @family Link Functions
#' @export
link_API_token <-  function(DB){
  DB$links$redcap_API %>% utils::browseURL()
}
#' @rdname Links
#' @export
link_API_playground <- function(DB){
  DB$links$redcap_API_playground %>% utils::browseURL()
}
#' @rdname Links
#' @export
link_REDCap_home <- function(DB){
  DB$links$redcap_base %>% utils::browseURL()
}
#' @rdname Links
#' @export
link_REDCap_project <- function(DB){
  DB$links$redcap_home %>% utils::browseURL()
}
#' @param record REDCap record id or study id etc, any column names that match `DB$redcap$id_col`
#' @param page REDCap page for the record. Must be one of `DB$metadata$forms$form_name`
#' @param instance REDCap instance if it's a repeating instrument
#' @param text_only logical for only returning text
#' @rdname Links
#' @export
link_REDCap_record <- function(DB,record,page,instance,text_only = F){
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
    if(!page%in%DB$metadata$forms$form_name)stop(page," has to be one of the instrument names: ",paste0(DB$metadata$forms$form_name,collapse = ", "))
    link <- link %>% paste0("&page=",page)
    if(!missing(instance)){
      if(!page%in%DB$metadata$forms$form_name[which(DB$metadata$forms$repeating)])stop("If you provide an instance, it has to be one of the repeating instrument names: ",paste0(DB$metadata$forms$form_name[which(DB$metadata$forms$repeating)],collapse = ", "))
      link <- link %>% paste0("&instance=",instance)
    }
  }
  if(text_only)return(link)
  utils::browseURL(link)
}
