## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example, eval=FALSE------------------------------------------------------
# library("RosyREDCap")
# 
# projects <- get_projects() # get list of cached projects
# 
# View(projects) # show your previously saved projects
# 
# DB <- setup_RosyREDCap(
#   short_name = "PROJECT1",
#   token_name = "PROJECT1_token", # see the article on setting your token if needed
#   redcap_base_link = "https://redcap.miami.edu/", # change to your institutions link
#   dir_path = getwd(), # or change to your intended file path
#   # force = T, # use this to force a refresh
#   merge_form_name = "patient",
#   use_csv = FALSE # if you don't have Microsoft change to TRUE
# )

## ----example2, eval=FALSE-----------------------------------------------------
# #1. set each time in your session (not recommended in saved/shared scripts!)
# Sys.setenv(PROJECT1_token = "YoUrNevErShaReToken")
# 
# #2. set to your private R sessions!
# usethis::edit_r_environ() #finds your file
# # Then you add --> PROJECT1_token = 'YoUrNevErShaReToken'
# # then save file and restart R
# 
# # If it worked you will see your token when you run...
# Sys.getenv("PROJECT1_token")
# #And if your DB object is setup properly...
# view_redcap_token(DB)

## ----eval=FALSE---------------------------------------------------------------
# 
# DB <- update_RosyREDCap(DB) # update from redcap by checking log and using saved object
# 
# DB <- add_forms_transformation_to_DB(DB,forms_tranformation = default_forms_transformation(DB))
# 
# DB <- transform_DB(DB) # transform to most basic forms, can be modified
# 
# DB <- drop_redcap_dir(DB)
# 
# #run shiny app!
# run_RosyREDCap()
# 
# # dev functions not ready for public yet
# DB <- clean_DB(DB)
# DB <- summarize_DB(DB) #can use for subsets!
# DB <- summarize_DB(DB)
# DB %>% save_summary() # will save summary data, look at the tabs!

## ----eval=FALSE---------------------------------------------------------------
# 
# DB$metadata %>% add_list_to_global()
# 
# DB$data %>% add_list_to_global()
# 
# #DB$summary %>% add_list_to_global() #not ready for public
# 
# listviewer::jsonedit(DB) # view object

