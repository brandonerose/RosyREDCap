mock_DB <- function(){
  short_name <- "TEST_PROJECT"
  redcap_base <- "https://redcap.miami.edu/"
  DB <- setup_DB(
    short_name = short_name,
    redcap_base = redcap_base,
    auto_check_token = F
  )
  DB$internals$ever_connected <- T
  fake_time <- Sys.time()
  DB$internals$last_data_dir_save <- fake_time
  DB$internals$last_metadata_update <- fake_time
  DB$internals$last_data_update <- fake_time
  DB$redcap$version <- "12.1.1"
  DB$redcap$token_name <- DB$redcap$token_name
  DB$redcap$project_id <- "01234"
  DB$redcap$project_title <- "A Fake Project"
  DB$redcap$id_col <- "record_id"
  DB$redcap$is_longitudinal <- F
  DB$redcap$has_repeating_forms_or_events <- F
  DB$redcap$has_multiple_arms <- F
  DB$links$redcap_home <- DB$links$redcap_base
  DB$links$redcap_API_playground <- DB$links$redcap_base
  return(DB)
}
