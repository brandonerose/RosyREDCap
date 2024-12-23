## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library("RosyREDCap")
# Sys.getenv("RosyREDCap_token_TEST")
# #>[1] ""
# # Assuming you already setup or loaded a DB object you can set the token like this...
# DB <- load_DB("TEST")
# set_REDCap_token(DB)

## ----eval=FALSE---------------------------------------------------------------
# # Set your token manually
# Sys.setenv(RosyREDCap_token_TEST="a_FaKe_TOkEn") # again having this in a script is not advised but possible
# 
# # Get your token
# Sys.getenv("RosyREDCap_token_TEST")
# #>[1] "a_FaKe_TOkEn"

## ----eval=FALSE---------------------------------------------------------------
# #Install usethis if you don't have it.
# #install.packages("usethis")
# usethis::edit_r_environ()
# # Now save your token.... (without the comment symbol '#')
# # RosyREDCap_token_TEST = "faKeTokeN"
# # Save the file and Close
# # Restart R Session (session tab)
# # .rs.restartR() # this will also restart R session for you.
# Sys.getenv("RosyREDCap_token_TEST") # now should contain your token

## ----eval=FALSE---------------------------------------------------------------
# 
# view_REDCap_token(DB)
# 
# test_REDCap_token(DB)
# 

