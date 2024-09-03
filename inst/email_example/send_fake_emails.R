# START ########################################################################

## Project: RosyREDCap
## Script: send_fake_emails.R
## Purpose: demo REDCap email sending pipeline
## Author: Brandon Rose, MD, MPH
## Email: thecodingdocs@gmail.com
## Notes: possible to do this without Microsoft on your PC but this example
##        converts docx to pdf which requires Microsoft desktop.

# SETUP ###################################################################

# .rs.restartR()
# remotes::install_github("brandonerose/Rosyverse")
# remotes::install_github("brandonerose/RosyREDCap")
# Rosyverse::update_all()
# Rosyverse::load_all()
RosyUtils::clear_env()

# FUNCTIONS ####################################################################

library('tidyverse')
library('magrittr')
Rosyverse::load_all() # remotes::install_github("brandonerose/Rosyverse")
library("RosyREDCap") # remotes::install_github("brandonerose/RosyREDCap")

# IMPORT #######################################################################

DB <- setup_DB(
  short_name = "TEST_email",
  token_name = "TEST_email_token",
  redcap_base_link<-"https://redcap.miami.edu/",
  dir_path = "~/OneDrive - University of Miami/Research/Other/TEST_email", # or change to your intended file path
  force = F,
  merge_form_name = "participant",
  use_csv = F
)

DB <- update_DB(DB,force=T) # update from redcap by checking log and using saved object
DB <- transform_DB(DB) # transform to most basic forms, can be modified
DB <- clean_DB(DB,drop_blanks = F,drop_unknowns = F)
DB <- summarize_DB(DB) #can use for subsets!
DB <- DB %>% drop_redcap_dir() #drops excel files with links to directory
DB <- summarize_DB(DB)
DB %>% save_summary()
DB$summary %>% add_list_to_global()
DB$data_transform %>% add_list_to_global() # adds merged DF to env

# EXPLORE ######################################################################

REDCap_diagram(DB, include_vars = T)

# TIDY #########################################################################



# TRANSFORM ####################################################################



# COMMUNICATE ##################################################################



# SAVE #########################################################################

# df %>% rio::export(file.path("output/cleanfile.xlsx"))

# SCRAP ########################################################################



# END ##########################################################################

