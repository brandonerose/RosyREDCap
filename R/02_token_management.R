#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
has_redcap_token <- function(DB,silent=T){
  DB <- validate_RosyREDCap(DB)
  DB$redcap$token_name %>% validate_env_name()
  DB$redcap$token_name %>% Sys.getenv() %>% is_redcap_token() %>% return()
}
is_redcap_token <- function(token){
  pattern <- "^([0-9A-Fa-f]{32})(?:\\n)?$"
  token2 <-  sub(pattern,"\\1", token, perl = TRUE) %>% trimws(whitespace = "[\\h\\v]")
  info_message <- paste0("You can set it each session with `Sys.setenv(YOUR_token_name='YoUrNevErShaReToKeN')...` or for higher safety run `edit_r_environ()` from the `usethis` package and add `YOUR_token_name = 'YoUrNevErShaReToKeN'` to that file...(then restart R under session tab after saving file)... The way to tell it worked is to run the code, `Sys.getenv('YOUR_token_name')` or `Sys.getenv(DB$redcap$token_name)` and see if it returns your token!...")
  if(is.null(token)){
    bullet_in_console("The token is `NULL`, not a valid 32-character hexademical value.",bullet_type = "x")
    return(F)
  }else if (is.na(token)) {
    bullet_in_console("The token is `NA`, not a valid 32-character hexademical value.",bullet_type = "x")
    return(F)
  }else if (nchar(token) == 0L) {
    bullet_in_console(paste0("`Sys.getenv(DB$redcap$token_name)` returned no token or is an empty string. ",info_message),bullet_type = "x")
    return(F)
  }else if(token2 != token){
    bullet_in_console("remove whitespace or extra lines from your token.",bullet_type = "x")
    return(F)
  }else if (!grepl(pattern, token, perl = TRUE)) {
    bullet_in_console(paste0("The token from `Sys.getenv(DB$redcap$token_name)` is not a valid 32-character hexademical value.",info_message),bullet_type = "x")
    return(F)
  }
  return(T)
}
validate_redcap_token <- function(DB,silent=T,return=T,ask= T){
  validated <- has_redcap_token(DB)
  keep_checking <- T
  while (!validated && keep_checking) {
    if(ask){
      set_redcap_token(DB)
    }else{
      keep_checking <- F
    }
    message("Try going to REDCap --> '",DB$links$redcap_API,"' run `link_API_token(DB)")
  }
  if(!silent){
    if(validated){
      message("You have a valid token set in your session!")
    }else{
      message("Token validation failed!")
    }
  }
  if(return){
    return(Sys.getenv(DB$redcap$token_name))
  }
}
#' @title Sets a valid token for this session
#' @return messages for confirmation
#' @export
set_redcap_token <- function(DB){
  token <- readline("What is your PSDB REDCap API token: ")
  while (!is_redcap_token(token)) {
    warning("You set an invalid token. Try going to REDCap --> '",DB$links$redcap_API,"' or run `link_API_token(DB)`",immediate. = T)
    token <- readline("What is your PSDB REDCap API token: ")
  }
  args =list(args =list(token))
  names(args) = DB$redcap$token_name
  do.call(Sys.setenv, args)
  message("Token was set for this session only using `Sys.getenv('",DB$redcap$token_name,"')` <- 'TheSecretTokenYouJustEntered'")
  message("For higher safety run `usethis::edit_r_environ()` and add `",DB$redcap$token_name,"='YoUrNevErShaReToKeN'` to that file...(then restart R under session tab after saving file)... The way to tell it worked is to run the code, `Sys.getenv('",DB$redcap$token_name,"')` or `Sys.getenv(DB$redcap$token_name)` or `has_redcap_token(DB)`, and see if it returns your token!...'")
}
#' @title View the REDCap API token currently stored in the session
#' @return REDCap API token currently stored in the session
#' @export
view_redcap_token <- function(DB){
  DB  <- validate_RosyREDCap(DB)
  validate_redcap_token(DB,silent = F,return = T)
}
other_drops <- function(ignore = F){
  if(ignore)return(NULL)
  c(
    "Not applicable",
    "No information",
    "Not asked",
    "Unknown",
    "Unencoded",
    "Unknown / Not Reported",
    "Missing Dates",
    "Pediatric"
  ) %>% return()
}
