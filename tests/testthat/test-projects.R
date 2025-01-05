##test-get_projects
test_that("get_projects is df and has appropriate columns",{
  df <- get_projects()
  expect_s3_class(df,"data.frame")
  expect_true(all(colnames(df)%in%internal_blank_project_cols))
  df <- blank_project()
  expect_s3_class(df,"data.frame")
  expect_true(all(colnames(df)%in%internal_blank_project_cols))
})
