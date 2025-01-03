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

