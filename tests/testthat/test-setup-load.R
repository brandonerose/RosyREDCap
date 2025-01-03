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
##test-cache_clear ----------------
test_that("cache_clear works",{
  # test_dir <- local_tempdir()
  # cache_clear() # i dont know how to sim this
})
##test-get_projects ----------------
test_that("get_projects is df and has appropriate columns",{
  df <- get_projects()
  expect_s3_class(df,"data.frame")
  expect_true(all(colnames(df)%in%internal_blank_project_cols))
})
##test-project_health_check ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-get_dir ----------------
test_that("get_dir returns dir or throws error",{
  test_dir <- local_tempdir()
  expect_error(get_dir(internal_blank_DB))
  DB <- internal_blank_DB
  dir.create(file.path(test_dir,"fake_folder"),showWarnings = F)
  DB$dir_path <- test_dir
  # #must setup DB first
  # expect_true(file.exists(get_dir(DB)))
})
##test-nav_to_dir ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-check_folder_for_projects ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-setup_DB ----------------
test_that("setup_DB creates a valid DB object and valid directory", {
  # Temporary directory for testing
  # Parameters for setup_DB
  test_dir <- local_tempdir()
  short_name <- "TEST_PROJECT"
  redcap_base <- "https://redcap.miami.edu/"
  # Run setup_DB
  DB <- setup_DB(
    short_name = short_name,
    dir_path = test_dir,
    redcap_base = redcap_base,
    auto_check_token = F
  )
  expect_true(is.list(DB))
  expect_named(DB)
  expect_true("short_name" %in% names(DB))
  expect_true("dir_path" %in% names(DB))
  expect_true(file.exists(DB$dir_path))
  expect_equal(DB$short_name, short_name)
  test_dir_files <- list.files(test_dir)
  expect_true(all(internal_dir_folders %in% test_dir_files))
  cache_clear()
})
##test-load_DB ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-load_DB_from_path ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-load_test_DB ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-save_DB ----------------
test_that("save_DB doesn't save if it's blank", {
  test_dir <- local_tempdir()
  short_name <- "TEST_PROJECT"
  redcap_base <- "https://redcap.miami.edu/"
  DB <- setup_DB(
    short_name = short_name,
    dir_path = test_dir,
    redcap_base = redcap_base,
    auto_check_token = F
  )
  save_DB(DB)
  expect_true(!file.exists(file.path(DB$dir_path,"R_objects",paste0(short_name,"_RosyREDCap.rdata"))))
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
  save_DB(DB)
  expect_true(file.exists(file.path(DB$dir_path,"R_objects",paste0(short_name,"_RosyREDCap.rdata"))))
  cache_clear()
})
##test-delete_DB ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-add_default_forms_transformation ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-add_field_transformation ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-transform_DB ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-untransform_DB ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-clean_DB ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-filter_DB ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-rmarkdown_DB ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-labelled_to_raw_form ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-raw_to_labelled_form ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-set_REDCap_token ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-view_REDCap_token ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-test_REDCap_token ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-get_REDCap_report ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-delete_REDCap_records ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-upload_file_to_REDCap_file_repository ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-add_REDCap_folder ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-delete_REDCap_file ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-upload_file_to_REDCap ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-delete_file_from_REDCap ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-add_ID_to_DF ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-deidentify_DB ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-drop_REDCap_to_directory ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-read_from_REDCap_upload ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-upload_form_to_REDCap ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-upload_DB_to_REDCap ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-find_upload_diff ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-edit_REDCap_while_viewing ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-update_DB ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-generate_horizontal_transform ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-upload_transform_to_DB ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-add_DB_subset ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-generate_summary_from_subset_name ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-summarize_DB ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-run_quality_checks ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-link_API_token ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-link_API_playground ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-link_REDCap_home ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-link_REDCap_project ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-link_REDCap_record ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-REDCap_diagram ----------------
test_that("works",{
  test_dir <- local_tempdir()
})
##test-run_RosyREDCap ----------------
test_that("works",{
  test_dir <- local_tempdir()
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
