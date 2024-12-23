#' @import RosyUtils
#' @import RosyApp
#' @title Set a REDCap API Token to Your Current R Session
#' @description
#' Prompts the user to input a valid REDCap API token and stores it as an environment variable for the current R session.
#' Instead of using this function you should consider setting your token within your REnviron file which can be edited with \code{\link{usethis::edit_r_environ()}}
#' @details
#' If a valid token already exists in the R session, the function notifies the user and asks whether they want to replace it.
#' The user is guided to provide a new token through the console.
#' It is strongly discouraged to include API tokens directly within R scripts.
#' The token is validated internally and stored using `Sys.setenv()`.
#' @inheritParams setup_DB
#' @param ask Logical (TRUE/FALSE). If TRUE, asks the user for confirmation before overwriting an existing valid token. Default is `TRUE`.
#' @return Invisible. A message is printed to confirm the token is successfully set.
#' @seealso
#' For more details, see the `vignette("Tokens")`.
#' For more details, see the \code{\link{vignette("Tokens")}}.
#' For the function to work you need to have a valid DB object from \code{\link{setup_DB()}}.
#' See our \href{https://brandonerose.github.io/RosyREDCap/articles/Tokens.html}{REDCap Tokens Article}
#' @family Token Functions
#' @keywords Token Functions
#' @export
set_REDCap_token <- function(DB,ask = T){
  DB  <- validate_DB(DB)
  token_name <- get_REDCap_token_name(DB)
  answer <- 1
  if(ask){
    token <- validate_REDCap_token(DB)
    if(is_valid_REDCap_token(token)){
      bullet_in_console(paste0("You already have a valid token in your R session (pending test connection) '",token,"'."))
      answer <- utils::menu(choices = c("Yes", "No"),title = "Are you sure you want to set something else?")
    }
  }
  if(answer == 1){
    has_valid_REDCap_token <- F
    if(is_something(DB$links$redcap_API)){
      if(!ask)bullet_in_console(paste0("You can request/regenerate/delete tokens at --> '",DB$links$redcap_API,"'"))
    }
    prompt <- paste0("What is your ",DB$short_name," REDCap API token: ")
    while (!has_valid_REDCap_token) {
      token <- readline(prompt)
      has_valid_REDCap_token <- is_valid_REDCap_token(token,silent = F)
    }
    do.call(Sys.setenv, setNames(list(token), token_name))
  }
  validate_REDCap_token(DB,silent = F)
  return(invisible())
}
#' @title View the REDCap API Token Stored in the Session
#' @description
#' Displays the REDCap API token currently stored in the session as an environment variable. It's essentially a wrapper for Sys.getenv("YOUR_TOKEN_NAME"), but it also validates that the token is formatted like a REDCap token and provides messgaes if not valid.
#' @details
#' This function retrieves the REDCap API token associated with the specified `DB` object and displays it as a message.
#' The token is not returned as an R object to maintain security.
#' Use this function to confirm the token currently in use without exposing it unnecessarily.
#' @inheritParams setup_DB
#' @return Invisible. Prints a message displaying the stored token.
#' @family Token Functions
#' @keywords Token Functions
#' @export
view_REDCap_token <- function(DB){
  DB  <- validate_DB(DB)
  return(message("🤫 ",validate_REDCap_token(DB,silent = F)))
}
#' @title Test REDCap API Token linked to a DB Object
#' @description
#' Validates the REDCap API token stored in the `DB` object by attempting a connection to the REDCap server.
#' @details
#' This function tests whether the API token stored in the `DB` object is valid by making a request to the REDCap server.
#' If the token is invalid, the function can optionally open the REDCap login page in a browser (`launch_browser`) and/or reset the token (`set_if_fails`) using the console.
#' @inheritParams setup_DB
#' @param set_if_fails Logical (TRUE/FALSE). If TRUE and test connection fails, asks user to paster token into consult using `set_REDCap_token(DB)` function. Default is `TRUE`.
#' @param launch_browser Logical (TRUE/FALSE). If TRUE, launches the REDCap login page in the default web browser when validation fails. Default is `TRUE`.
#' @return Logical. Returns `TRUE` if the API token is valid, otherwise `FALSE`.
#' @seealso
#' \href{../articles/Tokens.html}{pkgdown article on tokens}
#' \href{https://brandonerose.github.io/RosyREDCap/articles/Tokens.htm}{pkgdown article on tokens}
#' @family Token Functions
#' @keywords Token Functions
#' @export
test_REDCap_token <- function(DB, set_if_fails = T, launch_browser = T){
  token <- validate_REDCap_token(DB, silent = F)
  version <- get_REDCap_version(DB,show_method_help = F) %>% suppressWarnings()
  DB$internals$last_test_connection_attempt <- Sys.time()
  DB$internals$last_test_connection_outcome <- ERROR <- is.na(version)
  if(!set_if_fails) return(DB)
  if(ERROR && launch_browser){
    utils::browseURL(url = ifelse(is_something(DB$redcap$version),DB$links$redcap_API,DB$links$redcap_base))
    #this will fail to bring you to right URL if redcap version changes at the same time a previously valid token is no longer valid
  }
  while(ERROR){
    bullet_in_console('Your REDCap API token check failed. Invalid token or API privileges. Contact Admin!`',bullet_type = "x")
    if(set_if_fails){
      set_REDCap_token(DB,ask = F)
      version <- get_REDCap_version(DB,show_method_help = F) %>% suppressWarnings()
      DB$internals$last_test_connection_attempt <- Sys.time()
      DB$internals$last_test_connection_outcome <- ERROR <- is.na(version)
    }
  }
  bullet_in_console("Connected to REDCap! 🚀",bullet_type = "v")
  DB$redcap$version <- version
  return(DB)
}
is_valid_REDCap_token <- function(token, silent = T){
  pattern <- "^([0-9A-Fa-f]{32})(?:\\n)?$"
  trimmed_token <-  sub(pattern,"\\1", token, perl = TRUE) %>% trimws(whitespace = "[\\h\\v]")
  end_message <- "not a valid 32-character hexademical value."
  if(is.null(token)){
    if(!silent)bullet_in_console(paste0("The token is `NULL`, ",end_message),bullet_type = "x")
    return(F)
  }else if (is.na(token)) {
    if(!silent)bullet_in_console(paste0("The token is `NA`, ",end_message),bullet_type = "x")
    return(F)
  }else if (nchar(token) == 0L) {
    if(!silent)bullet_in_console(paste0("The token is `` (empty), ",end_message),bullet_type = "x")
    return(F)
  }else if(token != trimmed_token){
    if(!silent)bullet_in_console(paste0("The token contains whitespace (extra lines) and is therefore ",end_message),bullet_type = "x")
    return(F)
  }else if (!grepl(pattern, token, perl = TRUE)) {
    if(!silent)bullet_in_console(paste0("The token is ",end_message),bullet_type = "x")
    return(F)
  }
  return(T)
}
get_REDCap_token_name <- function(DB){
  token_name <- paste0("RosyREDCap_token_",validate_env_name(DB$short_name))
  return(token_name)
}
validate_REDCap_token <- function(DB,silent=T){
  token_name <- DB %>% get_REDCap_token_name()
  token <- DB %>% get_REDCap_token_name() %>% Sys.getenv()
  valid <- token %>% is_valid_REDCap_token(silent=silent)
  if(!silent){
    bullet_in_console(paste0("You can set REDCap tokens each session with `set_REDCap_token(DB)` or `Sys.setenv(",token_name,"='YoUrNevErShaReToKeNfRoMREDCapWebsiTe')`... or for higher security run `usethis::edit_r_environ()` and add
        `",token_name," = 'YoUrNevErShaReToKeNfRoMREDCapWebsiTe'` to that file...(then restart R under session tab after saving file)... The way to tell it worked is to run the
        code, `Sys.getenv('",token_name,"')`"))
    if(is_something(DB$links$redcap_API)){
      bullet_in_console(paste0("You can request/regenerate/delete tokens at --> '",DB$links$redcap_API,"' or run `link_API_token(DB)` to launch in browser"))
    }
    if(valid){
      bullet_in_console(paste0("Valid token for ",DB$short_name," is set in your R session (pending connection)!"),bullet_type = "v")
    }
  }
  return(token)
}
