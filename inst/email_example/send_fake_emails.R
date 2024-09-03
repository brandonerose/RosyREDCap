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
Rosyverse::update_all()
# Rosyverse::load_all()
RosyUtils::clear_env()

dir_path <- getwd() # set your own wd!
dir_path <- "/Users/brandonrose/Library/CloudStorage/OneDrive-UniversityofMiami/Research/Other/TEST_email"

file.copy(
  from = system.file("email_example",package = "RosyREDCap"),
  to = dir_path,#set you dir
  recursive = T
)

# FUNCTIONS ####################################################################

library('tidyverse') # install.packages("tidyverse")
library('magrittr') # install.packages("magrittr")
Rosyverse::load_all() # remotes::install_github("brandonerose/Rosyverse")
library("RosyREDCap") # remotes::install_github("brandonerose/RosyREDCap")
library("Microsoft365R") # install.packages("Microsoft365R")
library("blastula") # install.packages("blastula")

# SETUP OUTLOOK ################################################################

outlook<-Microsoft365R::get_business_outlook()

# IMPORT EMAIL DATA ############################################################

DB <- setup_DB(
  short_name = "TEST_email",
  token_name = "TEST_email_token",
  redcap_base_link<-"https://redcap.miami.edu/",
  dir_path = dir_path, # or change to your intended file path
  force = F,
  merge_form_name = "participant",
  use_csv = F
)

DB <- update_DB(DB) # update from redcap by checking log and using saved object
DB <- transform_DB(DB) # transform to most basic forms, can be modified
DB <- clean_DB(DB,drop_blanks = F,drop_unknowns = F)
DB <- summarize_DB(DB) #can use for subsets!
DB <- DB %>% drop_redcap_dir() #drops excel files with links to directory
DB <- summarize_DB(DB)
DB %>% save_summary(dir_)
DB$summary %>% add_list_to_global()
DB$data_transform %>% add_list_to_global() # adds merged DF to env

# TRANSFORM ####################################################################



# COMMUNICATE ##################################################################



# SAVE #########################################################################

# df %>% rio::export(file.path("output/cleanfile.xlsx"))

# SCRAP ########################################################################



# END ##########################################################################

list.files.real<-function(path){
  grep('~$', list.files(path), fixed = TRUE, value = TRUE, invert = TRUE)
}
get_file_list <- function(DB){
  file_list <- data.frame(
    record_id = DB$data$baseline_survey$record_id# %>% append(c("EXAMPLE","TEST2")
  )
  folders <- list(
    "result_upload_folder" = DB %>% rosyredcap::get_dir() %>% file.path("Results Upload"),
    "rb_doc_upload_folder" = DB %>% rosyredcap::get_dir() %>% file.path("output","report_back_doc"),
    "rb_pdf_upload_folder" = DB %>% rosyredcap::get_dir() %>% file.path("output","report_back_pdf")
  )

  for (folder in folders){
    stop_file_doesnt_exist(folder)
    if(folder%in%c(folders[[1]],folders[[3]])){
      file_check <- data.frame(
        file = folder %>% list.files.real() %>% .[grep(".pdf",.)]
      )
      if(folder==folders[[1]]){
        file_check$records<- file_check$file %>% strsplit("_") %>% sapply(function(L){
          x<- L[[1]]
          if(grepl("PID",x)){
            gsub("PID","",x) %>% trimws()
          }else{
            NA
          }
        })
      }

      if(folder==folders[[3]]){
        file_check$records<- gsub("FCI_PFAS_","",file_check$file) %>% strsplit("_") %>% sapply(function(L){L[[1]]})
      }

    }
    if(folder%in%c(folders[[2]])){
      file_check <- data.frame(
        file = folder %>% list.files.real() %>% .[grep(".docx",.)]
      )
      file_check$records<- file_check$file %>% strsplit("_") %>% sapply(function(L){L[[1]]})
    }
    if(length(file_check$file)==0)warning("No files at --> ",folder,immediate. = T)


    if(anyDuplicated(file_check$records)>0)stop("Duplicate records!")
    bad_records<-file_check$records[which(!file_check$records%in%file_list$record_id)]
    if(length(bad_records)>0)warning("These records are not included in the REDCap --> ",bad_records %>% paste0(collapse = ", "),immediate. = T)
    file_list[[names(folders[which(folders==folder)])]] <- file_list$record_id %>% sapply(function(ID){
      ID %in% file_check$records
    })
    file_list[[names(folders[which(folders==folder)]) %>% paste0("_path")]] <- 1:nrow(file_list) %>% sapply(function(ROW){
      OUT <- NA
      if(file_list[[names(folders[which(folders==folder)])]][ROW]){
        OUT <- file.path(folder,file_check$file[which(file_check$records==file_list$record_id[ROW])])
      }
      OUT
    })
  }
  file_list
}
age <- function(dob, age.day =  lubridate::today(), units = "years", floor = TRUE) {
  calc.age = lubridate::interval(dob, age.day) / lubridate::duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}
stop_file_doesnt_exist <- function(location){
  if(!file.exists(location))stop(location, " doesn't exist")
}
drop_nas <- function(x) {   x[!sapply(x, is.na)] }



# EXPLORE ######################################################################

PFAS %>% clean_DB() %>% make_table1(merged,variables = c("sex_at_birth","hispanic_","educational_attainment","annual_income","drinking_water")) #city_state is bad design

# EMAILPREP ####################################################################

file_list <- get_file_list(PFAS)


files_to_email_again<-file_list[which(file_list$record_id%in% merged$record_id[which(!is.na(merged$report_back_letter_sent_date))]),]
files_to_email<-file_list[which(file_list$rb_pdf_upload_folder& file_list$record_id%in% merged$record_id[which(is.na(merged$report_back_letter_sent_date))]),]


email_list <- merged %>% dplyr::select(record_id,first_name,last_name,suffix_name,preferred_email_address_of,followup_survey_2_complete)

email_list_new <- email_list %>% merge(files_to_email,by = "record_id")
email_list_again <- email_list %>% merge(files_to_email_again,by = "record_id")

email_list_new$survey_link <- NA

for (i in 1:nrow(email_list_new)){
  if(email_list_new$followup_survey_2_complete[i]!="Complete"){
    email_list_new$survey_link[i] <-
      httr::POST(
        url= "https://redcap.miami.edu/api/",
        body =  list("token"=Sys.getenv("PFAS_token"),
                     content='surveyLink',
                     format='csv',
                     instrument='followup_survey_2',
                     event='',
                     record=email_list_new$record_id[i],
                     returnFormat='json'
        ),
        encode = "form"
      ) %>% httr::content(as="text")
  }

}


# COMMUNICATE ##################################################################
i <- 5
# check_na <- merged[which(!is.na(merged$empowerdx_pdf)),]

# check_na[,which(colnames(check_na) %>% startsWith("result_"))] %>% lapply(function(x){any(is.na(x))}) %>% unlist() %>% any()
# check_na[,which(colnames(check_na) %>% startsWith("serum_equiv_"))] %>% lapply(function(x){any(is.na(x))}) %>% unlist() %>% any() %>% which()

records <- c(email_list_again$record_id ,email_list_new$record_id) %>% unique()
to_redcap <- data.frame(
  record_id = c(email_list_again$record_id ,email_list_new$record_id) %>% unique(),
  results_complete = "2",
  report_back_letter_sent_date = Sys.Date() %>% as.character(),
  results_sent = "1"
)
to_redcap %>% rosyredcap:::upload_form_to_redcap(PFAS)

for (i in 1:nrow(email_list_new)){
  full_name <-  email_list_new$first_name[i] %>% append(email_list_new$last_name[i]) %>% append(email_list_new$suffix_name[i]) %>%rosyredcap:::drop_nas() %>% paste0(collapse = " ")
  bl_body <- paste0(
    "Dear ",full_name,",",
    "
                    <br><br>Attached to this email are your PFAS test results from the finger-stick blood collection you provided to us as part of the Firefighter PFAS Postal Mail Study.
                    <br><br>Once you have reviewed your results letter, we ask that you complete a follow-up survey to provide us feedback on the results letter. Access Follow Up Survey here:  [",email_list_new$survey_link[i],"](",email_list_new$survey_link[i],")
                    <br><br>If you have any questions or would like to setup a one-on-one consultation, please don’t hesitate to contact me or the study coordinator, Alex Stewart, at [305-243-4339](tel:305-243-4339) or [acs254@miami.edu](mailto:acs254@miami.edu).
                    <br><br>Thank you for your participation in our research.
                    <br><br>Sincerely,
                    <br>Alberto J. Caban-Martinez, DO, PhD, MPH
                    <br>Associate Professor, Public Health Sciences
                    <br>Deputy Director, National Firefighter Cancer Initiative
                    <br>Email: [acaban@med.miami.edu](mailto:acaban@med.miami.edu) | Telephone: [305-243-7565](tel:305-243-7565)"
  )

  outlook_email_from_blastula <- outlook$create_email(
    compose_email(
      header = paste0("A personalized message from the FCI team!"),
      body = md(bl_body),
      footer=md(paste0("This message is only intended for ",full_name))
      # template = blastula_template(
      #   html_body,
      #   html_header,
      #   html_footer,
      #   title,
      #   content_width = "1000px",
      #   font_family = "Helvetica, sans-serif"
      # )
    ) %>% add_attachment(
      file = email_list_new$rb_pdf_upload_folder_path[i]
    ),
    subject = "Report Back Letter with Results from the Firefighter PFAS Postal Mail Study",
    to = email_list_new$preferred_email_address_of[i],
    cc = "acaban@med.miami.edu"
  )
}

for (i in 1:nrow(email_list_again)){
  full_name <-  email_list_again$first_name[i] %>% append(email_list_again$last_name[i]) %>% append(email_list_again$suffix_name[i]) %>%rosyredcap:::drop_nas() %>% paste0(collapse = " ")
  bl_body <- paste0(
    "Dear ",full_name,",",
    "
                    <br><br>Thanks so much for being a part of our PFAS study. We would like to inform you that we have made some changes to our PFAS test results report back letter.
                    <br><br>This email will include your new report back letter with those changes, however, please understand that these are not new results, and that they will not differ from the results previously reported to you.
                    <br><br>We have added additional information to clarify how we are reporting your PFAS results to you, which will hopefully aide you in better interpreting your results.
                    <br><br>Sincerely,
                    <br>Alberto J. Caban-Martinez, DO, PhD, MPH
                    <br>Associate Professor, Public Health Sciences
                    <br>Deputy Director, National Firefighter Cancer Initiative
                    <br>Email: [acaban@med.miami.edu](mailto:acaban@med.miami.edu) | Telephone: [305-243-7565](tel:305-243-7565)"
  )

  outlook_email_from_blastula <- outlook$create_email(
    compose_email(
      header = paste0("A personalized message from the FCI team!"),
      body = md(bl_body),
      footer=md(paste0("This message is only intended for ",full_name))
      # template = blastula_template(
      #   html_body,
      #   html_header,
      #   html_footer,
      #   title,
      #   content_width = "1000px",
      #   font_family = "Helvetica, sans-serif"
      # )
    ) %>% add_attachment(
      file = email_list_again$rb_pdf_upload_folder_path[i]
    ),
    subject = "Report Back Letter with Results from the Firefighter PFAS Postal Mail Study",
    to = email_list_again$preferred_email_address_of[i],
    cc = "acaban@med.miami.edu"
  )
}












outlook<-Microsoft365R::get_business_outlook()

outlook_email_from_blastula$add_attachment(email_list$report_back_letter[i])





















for(i in which(!is.na(PFAS$data$results$report_back_letter))){
  filename <- paste0("FCI_PFAS_Report_Back_Letter_ID",PFAS$data$results$record_id[i],".pdf")

  if(!file.exists(filename)){

    REDCapR::redcap_download_file_oneshot(
      redcap_uri = PFAS$redcap_uri,
      token = Sys.getenv(PFAS$token_name),
      field = "report_back_letter",
      record = PFAS$data$results$record_id[i],
      directory = file.path(getwd(),"output"),
      file_name = filename
    )
  }

}



outlook<-Microsoft365R::get_business_outlook()

email_list <- PFAS$data$consent_form %>% dplyr::select(record_id,first_name,last_name,preferred_email_address_of) %>% merge(all_results,by= "record_id")

email_list$report_back_letter <- file.path(getwd(),"output",email_list$report_back_letter)email_list$report_back_letter %>% file.exists()

