##test-get_projects
test_that("get_projects is df and has appropriate columns",{
  df <- get_projects()
  expect_s3_class(df,"data.frame")
  expect_true(all(colnames(df)%in%internal_blank_project_cols))
  df <- blank_project()
  expect_s3_class(df,"data.frame")
  expect_true(all(colnames(df)%in%internal_blank_project_cols))
})
test_that("check_folder_for_projects works",{
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  test_dir_folder <- file.path(test_dir, "R_objects")
  test_file1 <- file.path(test_dir, "R_objects","not_a_proj.rdata")
  test_file2 <- file.path(test_dir, "R_objects","PROJ_RosyREDCap.rdata")
  test_file3 <- file.path(test_dir,"ANOTHER_PROJ_RosyREDCap.rdata")
  test_file4 <- file.path(test_dir,"ANOTHER_PROJ_wrong_suffix.rdata")
  dir.create(test_dir_folder)
  file.create(test_file1)
  file.create(test_file2)
  file.create(test_file3)
  file.create(test_file4)
  local_mocked_bindings(
    validate_dir = function(...) test_dir
  )
  #check without validation
  expect_false(test_file1%in%check_folder_for_projects(file_path = test_dir,validate = F))
  expect_contains(check_folder_for_projects(file_path = test_dir,validate = F),test_file2)
  expect_contains(check_folder_for_projects(file_path = test_dir,validate = F),test_file3)
  expect_false(test_file4%in%check_folder_for_projects(file_path = test_dir,validate = F))
  #check validation
  expect_false(test_file1%in%check_folder_for_projects(file_path = test_dir,validate = T))
  expect_contains(check_folder_for_projects(file_path = test_dir,validate = T),test_file2)
  expect_false(test_file3%in%check_folder_for_projects(file_path = test_dir,validate = T))
  expect_false(test_file4%in%check_folder_for_projects(file_path = test_dir,validate = T))
})
