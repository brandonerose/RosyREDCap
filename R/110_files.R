#' @import RosyUtils
#' @import RosyApp
#' @title Uploads a file to REDCap
#' @inheritParams save_DB
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
#' @inheritParams save_DB
#' @return data.frame of files
#' @keywords internal
#' @noRd
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
#' @inheritParams save_DB
#' @param name folder name
#' @return messages for confirmation
#' @export
add_REDCap_folder <- function(DB,name){
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
#' @inheritParams save_DB
#' @param doc_id from the file list `get_REDCap_file_repository(DB)`
#' @return messages for confirmation
#' @export
delete_REDCap_file <- function(DB,doc_id){
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
#' @title Upload File to REDCap
#' @description
#' This function uploads a file to a specific record and field in a REDCap project. It allows you to specify optional parameters like repeat instance and event to upload the file to a particular instance or event within a record.
#'
#' @inheritParams save_DB
#' @param file A character string specifying the file path of the file to be uploaded.
#' @param record A character string specifying the record ID to which the file will be uploaded.
#' @param field A character string specifying the field name to which the file will be associated.
#' @param repeat_instance Optional. A numeric or character string specifying the repeat instance of the record. Defaults to NULL.
#' @param event Optional. A character string specifying the event name to which the file will be uploaded. Defaults to NULL.
#'
#' @return A message indicating the file upload status.
#'
#' @details
#' This function uploads a specified file to a particular record and field in a REDCap project using the REDCap API. The file is uploaded as multipart data, and the function will automatically handle file existence checks. If provided, the `event` and `repeat_instance` parameters will be used to specify the precise location for the upload.
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
  bullet_in_console("File uploaded!",file= file,bullet_type = "v")
}
#' @title Delete a File from REDCap
#' @description
#' Deletes a file uploaded to a specific field in a REDCap project for a given record. Supports deleting files from repeating instances and events if specified.
#'
#' @inheritParams save_DB
#' @param record Character. The unique identifier of the record from which the file should be deleted.
#' @param field Character. The name of the field in REDCap where the file is stored.
#' @param repeat_instance Optional. Numeric. The repeating instance of the record (if applicable) from which the file should be deleted.
#' @param event Optional. Character. The unique event name (if applicable) from which the file should be deleted.
#'
#' @return
#' A message indicating whether the file was successfully deleted.
#'
#' @details
#' This function sends a request to the REDCap API to delete a file associated with a specific record and field. It supports optional parameters to handle repeating instruments or events, ensuring flexibility for more complex REDCap projects. If the API request fails, the function raises an error.
#'
#' @seealso
#' \code{\link{save_DB}} for managing database objects.
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
