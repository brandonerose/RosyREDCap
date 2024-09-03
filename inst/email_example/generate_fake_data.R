# .rs.restartR()
# remotes::install_github("brandonerose/Rosyverse")
# Rosyverse::update_all()
# Rosyverse::load_all()
RosyUtils::clear_env()
Rosyverse::load_all()
library("RosyREDCap") ## remotes::install_github("brandonerose/RosyREDCap")
#setup ------
DB <- setup_DB(
  short_name = "TEST_email",
  token_name = "TEST_email_token",
  redcap_base_link<-"https://redcap.miami.edu/",
  dir_path = "~/OneDrive - University of Miami/Research/Other/TEST_email", # or change to your intended file path
  force = F,
  merge_form_name = "participant",
  use_csv = F
)
# import ------
# projects <- get_projects() # get list of cached projects
# DB <- load_DB(projects$dir_path[which(projects$short_name=="TEST_email")])
DB <- update_DB(DB,force=T) # update from redcap by checking log and using saved object
DB <- transform_DB(DB) # transform to most basic forms, can be modified
DB <- clean_DB(DB,drop_blanks = F,drop_unknowns = F)
DB <- summarize_DB(DB) #can use for subsets!
DB <- DB %>% drop_redcap_dir() #drops excel files with links to directory
DB <- summarize_DB(DB)
DB %>% save_summary()
DB$summary %>% add_list_to_global()
DB$data_transform %>% add_list_to_global()
codebook <- RosyREDCap:::metadata_to_codebook(metadata)

# generate fake! ------------------
participant <- participant[0,] %>% all_character_cols()
date_s <- as.Date((Sys.Date()-100):(Sys.Date()))
i <- 1
for(i in 1:1000){
  birth_sex <- c("Male","Female") %>% sample(1)
  date_of_enrollment <- date_s %>% sample1()
  date_t <- as.Date(date_of_enrollment:Sys.Date())
  date_of_test <- date_t %>% sample1()

  gender <- as.integer(birth_sex == "Female")
  the_name <- randomNames::randomNames(n=1,ethnicity = c(4,5,2) %>% sample(1),gender = gender,name.order = "first.last")%>% strsplit(", ") %>% unlist()
  salutation <- c("Mr.","Dr.") %>% sample(1)
  if(as.logical(gender)){
    salutation <- c("Ms.","Mrs.","Dr.") %>% sample(1)
  }
  first_name <- the_name[1]
  last_name <- the_name[2]
  email <- paste0(gsub(" ","",first_name),".",gsub(" ","",last_name),"@gmail.com") %>% tolower()
  participant <- participant %>%dplyr::bind_rows(
    data.frame(
      record_id = as.character(i),
      birth_sex = birth_sex,
      salutation = salutation,
      first_name = first_name,
      last_name = first_name,
      email = email,
      date_of_enrollment = as.character(date_of_enrollment),
      date_of_test = as.character(date_of_test),
      age_in_years_at_enrollment = 18:99 %>% sample1() %>% as.character(),
      demographics_complete = "Complete",
      study_results_complete = "Unverified"
    )
  )
}
metals <- PeriodicTable:::periodicTable$name[-1] %>% tolower() %>% paste0("_level")
for(M in metals){
  participant[[M]] <- sample(1:100,500, replace = T)
}


participant %>% RosyREDCap:::labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB,batch_size = 50)

# view -------

REDCap_diagram(DB)
REDCap_diagram(DB,include_vars = T)
REDCap_diagram(DB,include_vars = T,type = "DiagrammeR")


