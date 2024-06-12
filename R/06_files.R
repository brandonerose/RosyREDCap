#' @import rosyutils
upload_file_to_redcap <- function(DB,file,record, field,repeat_instance = NULL,event = NULL){
  # DB <- validate_DB(DB)
  file <- normalizePath(file)
  if(!file.exists(file)) stop("File does not exist! --> ",file)
  body <- list(
    "token"=validate_redcap_token(DB),
    action='import',
    content='file',
    record =record,
    field =field,
    returnFormat='csv',
    file=httr::upload_file(file)
  )
  if(!is.null(event)){
    body$event <- event
  }
  if(!is.null(repeat_instance)){
    body$repeat_instance <- repeat_instance
  }
  response <- httr::POST(
    url = DB$links$redcap_uri,
    body = body,
    encode = "multipart"
  )
  if(httr::http_error(response))stop("File upload failed")
  message("File uploaded! --> ",file)
}
delete_file_from_redcap <- function(DB,record, field,repeat_instance = NULL, event = NULL){
  # DB <- validate_DB(DB)
  body <- list(
    "token"=validate_redcap_token(DB),
    action='delete',
    content='file',
    record =record,
    field =field,
    returnFormat='csv'
  )
  if(!is.null(event)){
    body$event <- event
  }
  if(!is.null(repeat_instance)){
    body$repeat_instance <- repeat_instance
  }
  response <- httr::POST(
    url = DB$links$redcap_uri,
    body = body
  )
  if(httr::http_error(response))stop("File Delete failed")
  message("File Deleted!")
}
