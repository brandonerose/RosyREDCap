#' @import rosyutils
#' @title Uploads a file to REDCap
#' @inheritParams save_DB
#' @param file file location on your PC
#' @return messages for confirmation
#' @export
upload_file_to_redcap_fileRepository <- function(DB,file){
  DB <- validate_DB(DB)
  file <- normalizePath(file)
  if(!file.exists(file)) stop("File does not exist! --> ",file)
  response <- httr::POST(
    url = DB$redcap_uri,
    body = list(
      "token"=validate_redcap_token(DB),
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
#' @inheritParams save_DB
#' @return data.frame of files
#' @export
check_redcap_files <- function(DB){
  DB <- validate_DB(DB)
  response <- httr::POST(
    url = DB$redcap_uri,
    body = list(
      "token"=validate_redcap_token(DB),
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
#' @inheritParams save_DB
#' @param name folder name
#' @return messages for confirmation
#' @export
add_redcap_folder <- function(DB,name){
  DB <- validate_DB(DB)
  response <- httr::POST(
    url = DB$redcap_uri,
    body = list(
      "token"=validate_redcap_token(DB),
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
#' @inheritParams save_DB
#' @param doc_id from the file list `check_redcap_files(DB)`
#' @return messages for confirmation
#' @export
delete_redcap_file <- function(DB,doc_id){
  response <- httr::POST(
    url = DB$redcap_uri,
    body = list(
      "token"=validate_redcap_token(DB),
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
