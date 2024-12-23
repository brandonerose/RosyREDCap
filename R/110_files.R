#' @import RosyUtils
#' @import RosyApp
#' @title Uploads a file to REDCap
#' @param file file location on your PC
#' @return messages for confirmation
#' @export
upload_file_to_REDCap_file_repository <- function(DB,file){
  DB <- validate_DB(DB)
  file <- normalizePath(file)
  if(!file.exists(file)) stop("File does not exist! --> ",file)
  response <- httr::POST(
    url = DB$links$redcap_uri,
    body = list(
      "token"=validate_REDCap_token(DB),
      action='import',
      content='fileRepository',
      returnFormat='json',
      file=httr::upload_file(file)
    ),
    encode = "multipart"
  )
  if(httr::http_error(response))stop("File upload failed")
  message("File Uploaded! --> ",file)
}
#' @title Checks REDCap for current files
#' @return data.frame of files
#' @export
get_REDCap_file_repository <- function(DB){
  DB <- validate_DB(DB)
  response <- httr::POST(
    url = DB$links$redcap_uri,
    body = list(
      "token"=validate_REDCap_token(DB),
      content='fileRepository',
      action='list',
      format='csv',
      folder_id='',
      returnFormat='json'
    ),
    encode = "form"
  )
  if(httr::http_error(response))stop("File check failed")
  message("Files checked!")
  httr::content(response)
}
#' @title Uploads a folder name to REDCap
#' @param name folder name
#' @return messages for confirmation
#' @export
add_redcap_folder <- function(DB,name){
  DB <- validate_DB(DB)
  response <- httr::POST(
    url = DB$links$redcap_uri,
    body = list(
      "token"=validate_REDCap_token(DB),
      content='fileRepository',
      action='createFolder',
      format='csv',
      name=name,
      folder_id='',
      returnFormat='json'
    ),
    encode = "form"
  )
  if(httr::http_error(response))stop("Folder add failed")
  message("Folder added!")
  httr::content(response)
}
#' @title Uploads a folder name to REDCap
#' @param doc_id from the file list `get_REDCap_file_repository(DB)`
#' @return messages for confirmation
#' @export
delete_redcap_file <- function(DB,doc_id){
  response <- httr::POST(
    url = DB$links$redcap_uri,
    body = list(
      "token"=validate_REDCap_token(DB),
      content='fileRepository',
      action='delete',
      doc_id=doc_id,
      returnFormat='json'
    ),
    encode = "form"
  )
  if(httr::http_error(response))stop("File delete failed")
  message("File deleted!")
}
#' @title upload_file_to_REDCap
#' @export
upload_file_to_REDCap <- function(DB,file,record, field,repeat_instance = NULL,event = NULL){
  # DB <- validate_DB(DB)
  file <- normalizePath(file)
  if(!file.exists(file)) stop("File does not exist! --> ",file)
  body <- list(
    "token"=validate_REDCap_token(DB),
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
#' @title delete_file_from_REDCap
#' @export
delete_file_from_REDCap <- function(DB,record, field,repeat_instance = NULL, event = NULL){
  # DB <- validate_DB(DB)
  body <- list(
    "token"=validate_REDCap_token(DB),
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
