test_that("test_dir works",{
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  expect_true(file.exists(test_dir))
  test_file <- file.path(test_dir, "projects.rds")
  expect_false(file.exists(test_file))
  file.create(test_file)
  expect_true(file.exists(test_file))
})
##test-setup_DB
test_that("setup_DB creates a valid DB object and valid directory", {
  test_dir <- withr::local_tempdir() %>% sanitize_path()
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
  expect_error(validate_DB(1))
  expect_error(validate_DB(data.frame()))
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
  DB$dir_path <- file.path(test_dir,"another_fake_folder") %>% sanitize_path()
  expect_error(get_dir(DB))
})
##test-load_test_DB
test_that("works",{
  test_dir <- withr::local_tempdir() %>% sanitize_path()
})
##test-save_DB
test_that("save_DB doesn't save if it's blank but will save and cache if valid, also loads", {
  test_dir <- withr::local_tempdir() %>% sanitize_path()
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
test_that("set_dir creates a new directory if it does not exist", {
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  dir_path <- file.path(test_dir, "new_dir")
  # Mock user input to create the directory
  mockery::stub(set_dir, "utils::menu", 1)
  expect_message(set_dir(dir_path), "Directory is Valid!")
  expect_true(file.exists(dir_path))
  expect_true(all(internal_dir_folders %in% list.files(dir_path)))
})
test_that("set_dir handles existing directory correctly", {
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  dir_path <- file.path(test_dir, "existing_dir")
  dir.create(dir_path)
  expect_message(set_dir(dir_path), "Directory is Valid!")
  expect_true(file.exists(dir_path))
  expect_true(all(internal_dir_folders %in% list.files(dir_path)))
})
test_that("set_dir throws an error for invalid directory path", {
  expect_error(set_dir(123), "dir must be a character string")
})
test_that("set_dir creates missing internal directories", {
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  dir_path <- file.path(test_dir, "partial_dir")
  dir.create(dir_path)
  dir.create(file.path(dir_path, "R_objects"))
  expect_message(set_dir(dir_path), "Directory is Valid!")
  expect_true(file.exists(dir_path))
  expect_true(all(internal_dir_folders %in% list.files(dir_path)))
})
test_that("set_dir stops if user chooses not to create directory", {
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  dir_path <- file.path(test_dir, "no_create_dir")
  # Mock user input to not create the directory
  mockery::stub(set_dir, "utils::menu", 2)
  expect_error(set_dir(dir_path), "Path not found. Use absolute path or choose one within R project working directory.")
  expect_false(file.exists(dir_path))
})
test_that("set_dir validates the directory structure", {
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  dir_path <- file.path(test_dir, "valid_dir")
  dir.create(dir_path)
  for (folder in internal_dir_folders) {
    dir.create(file.path(dir_path, folder))
  }
  expect_message(set_dir(dir_path), "Directory is Valid!")
  expect_true(file.exists(dir_path))
  expect_true(all(internal_dir_folders %in% list.files(dir_path)))
})
