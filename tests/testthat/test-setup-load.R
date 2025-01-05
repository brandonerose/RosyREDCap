# 000_setup.R ========================
# 001_projects.R ========================
# 002_directories.R ========================
# 003_setup_DB.R ========================
# 004_transform_DB.R ========================
# 005_annotate_DB.R ========================
# 006_summarize_DB.R ========================
# 108_token_management.R ========================
# 109_REDCap_API.R ========================
# 110_files.R ========================
# 111_process.R ========================
# 112_to_and_from_dir.R ========================
# 113_upload.R ========================
# 114_update.R ========================
# 115_transform.R ========================
# 116_summarize_DB.R ========================
# 117_link.R ========================
# 118_diagram.R ========================
##test-cache_path ----------------
test_that("hoardr cache exsists",{
  expect_true(file.exists(cache_path()))
  expect_true(cache_exists())
})
test_that("test_dir works",{
  test_dir <- local_tempdir() %>% normalizePath()
  expect_true(file.exists(test_dir))
  test_file <- file.path(test_dir, "projects.rds")
  expect_false(file.exists(test_file))
  file.create(test_file)
  expect_true(file.exists(test_file))
})
test_that("fake_cache sets and clears",{
  test_dir <- local_tempdir() %>% normalizePath()
  fake_cache_location <- file.path(test_dir,"fake_cache")
  local_mocked_bindings(
    get_cache = function(...){
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      return(fake_cache)
    }
  )
  expect_false(file.exists(fake_cache_location))
  fake_cache <- get_cache()
  expect_true(file.exists(fake_cache_location))
  expect_true(file.exists(fake_cache$cache_path_get()))
  expect_equal(fake_cache$cache_path_get(),fake_cache_location)
  test_file <- file.path(fake_cache$cache_path_get(), "projects.rds")
  expect_false(file.exists(test_file))
  file.create(test_file)
  expect_true(file.exists(test_file))
  fake_cache$delete_all()
  expect_false(file.exists(test_file))
})
##test-cache_clear ----------------
test_that("cache_projects_exists, cache_clear works",{
  test_dir <- local_tempdir() %>% normalizePath()
  fake_cache_location <- file.path(test_dir,"fake_cache")
  local_mocked_bindings(
    get_cache = function(...){
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      return(fake_cache)
    }
  )
  expect_false(file.exists(fake_cache_location))
  fake_cache <- get_cache()
  expect_true(file.exists(fake_cache_location))
  expect_true(file.exists(fake_cache$cache_path_get()))
  expect_equal(fake_cache$cache_path_get(),fake_cache_location)
  test_file <- file.path(fake_cache$cache_path_get(), "projects.rds")
  expect_false(cache_projects_exists())
  file.create(test_file)
  expect_true(cache_projects_exists())
  cache_clear()
  expect_false(cache_projects_exists())
})
##test-get_projects ----------------
test_that("get_projects is df and has appropriate columns",{
  df <- get_projects()
  expect_s3_class(df,"data.frame")
  expect_true(all(colnames(df)%in%internal_blank_project_cols))
  df <- blank_project()
  expect_s3_class(df,"data.frame")
  expect_true(all(colnames(df)%in%internal_blank_project_cols))
})
##test-project_health_check ----------------
test_that("works",{
  # fake_one <- mocked_get_cache()
  # print(paste0("fake2: ",fake_one$cache_path_get()))
})
##test-nav_to_dir ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-check_folder_for_projects ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-setup_DB ----------------
test_that("setup_DB creates a valid DB object and valid directory", {
  test_dir <- local_tempdir() %>% normalizePath()
  expect_error(validate_dir(dir_path = test_dir))
  short_name <- "TEST_PROJECT"
  redcap_base <- "https://redcap.miami.edu/"
  #test_short_names
  expect_error(validate_env_name("A project"))
  expect_error(validate_env_name("project$]"))
  expect_error(validate_env_name("1"))
  expect_error(validate_env_name(1))
  expect_error(validate_env_name(another_name))
  expect_no_error(validate_env_name("expected_name"))
  expect_no_error(validate_env_name("expected_name2"))
  expect_no_error(validate_env_name("EXPECTED_NAME"))
  #test_redcap_base
  expect_error(validate_web_link("https://redcap.blah"))
  expect_no_error(validate_web_link("https://redcap.miami.edu/"))
  expect_no_error(validate_web_link("https://redcap.miami.edu"))
  expect_no_error(validate_web_link("https://redcap.edu"))
  #test db
  expect_error(validate_DB(internal_blank_DB))
  expect_error(get_dir(DB))
  # Run setup_DB
  DB <- setup_DB(
    short_name = short_name,
    dir_path = test_dir,
    redcap_base = redcap_base,
    force = T,
    auto_check_token = F
  )
  expect_no_error(validate_dir(dir_path = test_dir))
  expect_no_error(validate_DB(DB = DB))
  expect_no_error(get_dir(DB))
  check_dir <- get_dir(DB)
  expect_identical(test_dir,check_dir)
  expect_true(is.list(DB))
  expect_named(DB)
  expect_true("short_name" %in% names(DB))
  expect_true("dir_path" %in% names(DB))
  expect_false(DB$internals$is_blank)
  expect_false(DB$internals$is_test)
  expect_true(file.exists(DB$dir_path))
  expect_equal(DB$short_name, short_name)
  test_dir_files <- list.files(test_dir)
  expect_true(all(internal_dir_folders %in% test_dir_files))
})
##test-load_test_DB ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-save_DB ----------------
test_that("save_DB doesn't save if it's blank but will save and cache if valid, also loads", {
  test_dir <- local_tempdir() %>% normalizePath()
  fake_cache_location <- file.path(test_dir,"fake_cache")
  local_mocked_bindings(
    get_cache = function(...){
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      return(fake_cache)
    }
  )
  short_name <- "TEST_PROJECT"
  redcap_base <- "https://redcap.miami.edu/"
  DB <- setup_DB(
    short_name = short_name,
    dir_path = test_dir,
    redcap_base = redcap_base,
    auto_check_token = F
  )
  save_DB(DB)
  expect_false(file.exists(file.path(DB$dir_path,"R_objects",paste0(short_name,"_RosyREDCap.rdata"))))
  #fakeDB
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
  #saving
  save_DB(DB)
  expected_save_location <- file.path(DB$dir_path,"R_objects",paste0(short_name,"_RosyREDCap.rdata"))
  expect_true(file.exists(expected_save_location))
  #check cached proj
  projects <- get_projects()
  expect_equal(nrow(get_projects()),1)
  expect_equal(projects$short_name, short_name)
  expect_equal(projects$redcap_base, redcap_base)
  expect_equal(projects$dir_path, test_dir)
  #loading tests
  expect_error(load_DB("a_project")) # wont load unknown project
  DB2 <- load_DB(short_name = short_name)#loads what we saved
  DB3 <- load_DB_from_path(DB_path = expected_save_location)#loads what we saved
  expect_identical(DB,DB2)
  expect_identical(DB,DB3)
  #delete_DB works...
  expect_no_warning(delete_DB(DB))
  expect_warning(delete_DB(DB))#warning for deleting twice
  expect_error(load_DB(short_name = short_name)) # wont load deleted project
})
##test-add_default_forms_transformation ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-add_field_transformation ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-transform_DB ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-untransform_DB ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-clean_DB ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-filter_DB ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-rmarkdown_DB ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-labelled_to_raw_form ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-raw_to_labelled_form ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-set_REDCap_token ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-view_REDCap_token ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-test_REDCap_token ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-get_REDCap_report ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-delete_REDCap_records ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-upload_file_to_REDCap_file_repository ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-add_REDCap_folder ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-delete_REDCap_file ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-upload_file_to_REDCap ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-delete_file_from_REDCap ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-add_ID_to_DF ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-deidentify_DB ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-drop_REDCap_to_directory ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-read_from_REDCap_upload ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-upload_form_to_REDCap ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-upload_DB_to_REDCap ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-find_upload_diff ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-edit_REDCap_while_viewing ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-update_DB ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-generate_horizontal_transform ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-upload_transform_to_DB ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-add_DB_subset ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-generate_summary_from_subset_name ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-summarize_DB ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-run_quality_checks ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-link_API_token ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-link_API_playground ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-link_REDCap_home ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-link_REDCap_project ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-link_REDCap_record ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-REDCap_diagram ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})
##test-run_RosyREDCap ----------------
test_that("works",{
  test_dir <- local_tempdir() %>% normalizePath()
})

# unlink(test_dir, recursive = TRUE)
#
#
# test_that("load_DB loads an existing DB object", {
#   # Temporary directory and parameters
#   test_dir <- tempdir()
#   short_name <- "TEST_PROJECT"
#   redcap_base <- "https://redcap.example.com/"
#
#   # Create DB object with setup_DB
#   setup_DB(
#     short_name = short_name,
#     dir_path = test_dir,
#     redcap_base = redcap_base
#   )
#
#   # Run load_DB
#   DB <- load_DB(short_name = short_name, dir_path = test_dir)
#
#   # Check DB object structure
#   expect_true(is.list(DB))
#   expect_equal(DB$short_name, short_name)
# })
#
# test_that("setup_DB respects the `force` parameter", {
#   # Temporary directory and parameters
#   test_dir <- tempdir()
#   short_name <- "TEST_PROJECT"
#   redcap_base <- "https://redcap.example.com/"
#
#   # Create initial DB object
#   DB1 <- setup_DB(
#     short_name = short_name,
#     dir_path = test_dir,
#     redcap_base = redcap_base
#   )
#
#   # Attempt to overwrite without force
#   DB2 <- setup_DB(
#     short_name = short_name,
#     dir_path = test_dir,
#     redcap_base = redcap_base,
#     force = FALSE
#   )
#
#   expect_identical(DB1, DB2) # Should not overwrite
#
#   # Overwrite with force = TRUE
#   DB3 <- setup_DB(
#     short_name = short_name,
#     dir_path = test_dir,
#     redcap_base = redcap_base,
#     force = TRUE
#   )
#
#   expect_false(identical(DB1, DB3)) # Should overwrite
# })
