#' @import RosyApp
#' @title Set a REDCap API Token to Your Current R Session
#' @description
#' Prompts the user to input a valid REDCap API token and stores it as an environment variable for the current R session.
#' Instead of using this function you should consider setting your token within your REnviron file which can be edited with \code{\link[usethis]{edit_r_environ}}.
#' @details
#' If a valid token already exists in the R session, the function notifies the user and asks whether they want to replace it.
#' The user is guided to provide a new token through the console.
#' It is strongly discouraged to include API tokens directly within R scripts.
#' The token is validated internally and stored using `Sys.setenv()`.
#' @inheritParams save_DB
#' @param ask Logical (TRUE/FALSE). If TRUE, asks the user for confirmation before overwriting an existing valid token. Default is `TRUE`.
#' @return Invisible. A message is printed to confirm the token is successfully set.
#' @seealso
#' For the function to work you need to have a valid DB object from \code{\link{setup_DB}()}.
#' See our \href{https://brandonerose.github.io/RosyREDCap/articles/Tokens.html}{REDCap Tokens Article}
#' @family Token Functions
#' @keywords Token Functions
#' @export
set_REDCap_token <- function(DB,ask = T){
  DB  <- validate_DB(DB)
  token_name <- get_REDCap_token_name(DB)
  is_a_test <- is_test_DB(DB)
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
      if(!ask){
        bullet_in_console(paste0("You can request/regenerate/delete with `link_API_token(DB)` or go here: "),url = DB$links$redcap_API)
      }
    }
    if(is_a_test){
      bullet_in_console(paste0("This is only a test so the token is: ",get_test_token(DB$short_name)),bullet_type = ">")
    }
    prompt <- paste0("What is your ",DB$short_name," REDCap API token: ")
    while (!has_valid_REDCap_token) {
      token <- readline(prompt)
      has_valid_REDCap_token <- is_valid_REDCap_token(token,silent = F,is_a_test = is_a_test)
    }
    do.call(Sys.setenv, stats::setNames(list(token), token_name))
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
#' @inheritParams save_DB
#' @return Invisible. Prints a message displaying the stored token.
#' @family Token Functions
#' @keywords Token Functions
#' @export
view_REDCap_token <- function(DB){
  DB  <- validate_DB(DB)
  token <- validate_REDCap_token(DB,silent = F)
  bullet_in_console(paste0("Never share your token: ",token),bullet_type = "!")
  return(invisible())
}
#' @title Test REDCap API Token linked to a DB Object
#' @description
#' Validates the REDCap API token stored in the `DB` object by attempting a connection to the REDCap server.
#' @details
#' This function tests whether the API token stored in the `DB` object is valid by making a request to the REDCap server.
#' If the token is invalid, the function can optionally open the REDCap login page in a browser (`launch_browser`) and/or reset the token (`set_if_fails`) using the console.
#' @inheritParams save_DB
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
  bullet_in_console("Connected to REDCap!",bullet_type = "v")
  DB$redcap$version <- version
  DB$internals$ever_connected <- T
  return(DB)
}
#' @noRd
is_valid_REDCap_token <- function(token, silent = T,is_a_test = F){
  pattern <- "^([0-9A-Fa-f]{32})(?:\\n)?$"
  trimmed_token <-  sub(pattern,"\\1", token, perl = TRUE) %>% trimws(whitespace = "[\\h\\v]")
  end_message <- "not a valid 32-character hexademical value."
  if(is.null(token)){
    if(!silent)bullet_in_console(paste0("The token is `NULL`, ",end_message),bullet_type = "x")
    return(F)
  }
  if (is.na(token)) {
    if(!silent)bullet_in_console(paste0("The token is `NA`, ",end_message),bullet_type = "x")
    return(F)
  }
  if (nchar(token) == 0L) {
    if(!silent)bullet_in_console(paste0("The token is `` (empty), ",end_message),bullet_type = "x")
    return(F)
  }
  if(token != trimmed_token){
    if(!silent)bullet_in_console(paste0("The token contains whitespace (extra lines) and is therefore ",end_message),bullet_type = "x")
    return(F)
  }
  if(is_a_test){
    allowed <- c(
      internal_TEST_classic_token,
      internal_TEST_repeating_token,
      internal_TEST_longitudinal_token,
      internal_TEST_multiarm_token
    )
    trimmed_token <-  token %>% trimws(whitespace = "[\\h\\v]")
    end_message <- "not a valid test token."
    if (!token %in% allowed) {
      if(!silent)bullet_in_console(paste0("The token is ",end_message),bullet_type = "x")
      return(F)
    }
  }else{
    if(!grepl(pattern, token, perl = TRUE)){
      if(!silent)bullet_in_console(paste0("The token is ",end_message),bullet_type = "x")
      return(F)
    }
  }
  return(T)
}
#' @noRd
get_REDCap_token_name <- function(DB){
  token_name <- paste0(internal_RosyREDCap_token_prefix,validate_env_name(DB$short_name))
  return(token_name)
}
#' @noRd
validate_REDCap_token <- function(DB,silent=T){
  token_name <- DB %>% get_REDCap_token_name()
  token <- DB %>% get_REDCap_token_name() %>% Sys.getenv()
  is_a_test <- is_test_DB(DB)
  valid <- token %>% is_valid_REDCap_token(silent=silent,is_a_test = is_a_test)
  message_about_token <- ifelse(is_a_test,get_test_token(DB$short_name),"YoUrNevErShaReToKeNfRoMREDCapWebsiTe")
  if(!silent){
    bullet_in_console(paste0("You can set REDCap tokens each session with `set_REDCap_token(DB)` or `Sys.setenv(",token_name,"='",message_about_token,"')`... or for higher security run `edit_r_environ()` from `usethis` package and add
        `",token_name," = '",message_about_token,"'` to that file...(then restart R under session tab after saving file)... The way to tell it worked is to run the
        code, `Sys.getenv('",token_name,"')`"))
    if(is_something(DB$links$redcap_API)){
      bullet_in_console(paste0("You can request/regenerate/delete with `link_API_token(DB)` or go here: "),url = DB$links$redcap_API)
    }
    if(valid){
      bullet_in_console(paste0("Valid token for ",DB$short_name," is set in your R session (pending connection)!"),bullet_type = "v")
    }
  }
  return(token)
}
#' @noRd
check_saved_RosyREDCap_tokens <- function(){
  the_names <- Sys.getenv() %>% names()
  the_names <- the_names[which(startsWith(the_names,internal_RosyREDCap_token_prefix))]
  if(length(the_names)==0){
    bullet_in_console("No known REDCap tokens saved in session...",bullet_type = "x")
    return(invisible())
  }
  the_names <- gsub(internal_RosyREDCap_token_prefix,"",the_names)
  ltn <- length(the_names)
  bullet_in_console(paste0("There are ",ltn," known REDCap tokens saved in the session: ",as_comma_string(the_names)),bullet_type = "x")
  return(invisible())
}
#' @noRd
internal_RosyREDCap_token_prefix <- "RosyREDCap_token_"
#' @noRd
internal_TEST_classic_token <- "FAKE32TESTTOKENCLASSIC1111111111"
#' @noRd
internal_TEST_repeating_token <- "FAKE32TESTTOKENREPEATING22222222"
#' @noRd
internal_TEST_longitudinal_token <- "FAKE32TESTTOKENLONGITUDINAL33333"
#' @noRd
internal_TEST_multiarm_token <- "FAKE32TESTTOKENMULTIARM444444444"
#' @noRd
get_test_token <- function(short_name){
  em <- '`short_name` must be character string of length 1 equal to one of the following: ' %>% paste0(as_comma_string(internal_allowed_test_short_names))
  if(!is.character(short_name))stop(em)
  if(length(short_name)!=1)stop(em)
  if(!is_test_short_name(short_name = short_name))stop(em)
  token <- NA
  if(short_name == "TEST_classic"){
    token <- internal_TEST_classic_token
  }
  if(short_name == "TEST_repeating"){
    token <- internal_TEST_repeating_token
  }
  if(short_name == "TEST_longitudinal"){
    token <- internal_TEST_longitudinal_token
  }
  if(short_name == "TEST_multiarm"){
    token <- internal_TEST_multiarm_token
  }
  return(token)
}
