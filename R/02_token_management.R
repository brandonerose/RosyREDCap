#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
has_REDCap_token <- function(DB,silent=T){
  DB <- validate_RosyREDCap(DB, silent = silent)
  DB$redcap$token_name %>% validate_env_name()
  DB$redcap$token_name %>% Sys.getenv() %>% is_REDCap_token() %>% return()
}
is_REDCap_token <- function(token, silent = T){
  pattern <- "^([0-9A-Fa-f]{32})(?:\\n)?$"
  token2 <-  sub(pattern,"\\1", token, perl = TRUE) %>% trimws(whitespace = "[\\h\\v]")
  info_message <- paste0("You can set it each session with `Sys.setenv(YOUR_token_name='YoUrNevErShaReToKeN')...` or for higher safety run `usethis::edit_r_environ()` from the `usethis` package and add `YOUR_token_name = 'YoUrNevErShaReToKeN'` to that file...(then restart R under session tab after saving file)... The way to tell it worked is to run the code, `Sys.getenv('YOUR_token_name')` or `Sys.getenv(DB$redcap$token_name)` and see if it returns your token!...")
  if(is.null(token)){
    if(!silent)bullet_in_console("The token is `NULL`, not a valid 32-character hexademical value.",bullet_type = "x")
    return(F)
  }else if (is.na(token)) {
    if(!silent)bullet_in_console("The token is `NA`, not a valid 32-character hexademical value.",bullet_type = "x")
    return(F)
  }else if (nchar(token) == 0L) {
    if(!silent)bullet_in_console(paste0("`Sys.getenv(DB$redcap$token_name)` returned no token or is an empty string. ",info_message),bullet_type = "x")
    return(F)
  }else if(token2 != token){
    if(!silent)bullet_in_console("remove whitespace or extra lines from your token.",bullet_type = "x")
    return(F)
  }else if (!grepl(pattern, token, perl = TRUE)) {
    if(!silent)bullet_in_console(paste0("The token from `Sys.getenv(DB$redcap$token_name)` is not a valid 32-character hexademical value.",info_message),bullet_type = "x")
    return(F)
  }
  return(T)
}
validate_REDCap_token <- function(DB,silent=T,return=T){
  valid <- has_REDCap_token(DB, silent = silent)
  if(!silent){
    if(valid){
      bullet_in_console("You have a valid token set in your session!",bullet_type = "v")
    }else{
      paste0("Token validation failed!","Try going to REDCap --> '",DB$links$redcap_API,"' or run `link_API_token(DB)` to launch in browser") %>%
        bullet_in_console(bullet_type = "x")
    }
  }
  if(return){
    return(Sys.getenv(DB$redcap$token_name))
  }
}
#' @title View the REDCap API token currently stored in the session
#' @inheritParams setup_RosyREDCap
#' @return REDCap API token currently stored in the session
#' @export
view_REDCap_token <- function(DB){
  DB  <- validate_RosyREDCap(DB)
  validate_REDCap_token(DB,silent = F,return = T)
}
#' @title Test REDCap API token of DB object
#' @inheritParams setup_RosyREDCap
#' @return TRUE or FALSE based on API token test
#' @export
test_REDCap_token <- function(DB, silent = F){
  token <- validate_REDCap_token(DB, silent = silent)
  if(!is_REDCap_token(token,silent = silent))return(FALSE)

  if(connected){
    if(!silent)bullet_in_console("Connected to REDCap! 🚀",bullet_type = "v")
  }
  return(connected)
}
