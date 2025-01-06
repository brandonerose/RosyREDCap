test_that("internal constants are correct", {
  expect_equal(internal_RosyREDCap_token_prefix, "RosyREDCap_token_")
  expect_equal(internal_TEST_classic_token, "FAKE32TESTTOKENCLASSIC1111111111")
  expect_equal(internal_TEST_repeating_token, "FAKE32TESTTOKENREPEATING22222222")
  expect_equal(internal_TEST_longitudinal_token, "FAKE32TESTTOKENLONGITUDINAL33333")
  expect_equal(internal_TEST_multiarm_token, "FAKE32TESTTOKENMULTIARM444444444")
})
test_that("get_test_token works correctly", {
  expect_equal(get_test_token("TEST_classic"), internal_TEST_classic_token)
  expect_equal(get_test_token("TEST_repeating"), internal_TEST_repeating_token)
  expect_equal(get_test_token("TEST_longitudinal"), internal_TEST_longitudinal_token)
  expect_equal(get_test_token("TEST_multiarm"), internal_TEST_multiarm_token)
  expect_error(get_test_token("INVALID_SHORT_NAME"))
  expect_error(get_test_token(1213123))
  expect_error(get_test_token(c("TEST_classic","TEST_repeating")))
})
test_that("is_valid_REDCap_token respects the rules of 32L hexidecimal", {
  expect_true(is_valid_REDCap_token(generate_hex(32)))
  expect_false(is_valid_REDCap_token(NA))
  expect_false(is_valid_REDCap_token(NULL))
  expect_false(is_valid_REDCap_token(generate_hex(31)))
  expect_false(is_valid_REDCap_token(generate_hex(33)))
  expect_false(is_valid_REDCap_token(paste0(" ",generate_hex(31))))
  expect_false(is_valid_REDCap_token(paste0("J",generate_hex(31))))
  expect_false(is_valid_REDCap_token(paste0("_",generate_hex(31))))
  expect_false(is_valid_REDCap_token(internal_TEST_classic_token))
  expect_true(is_valid_REDCap_token(internal_TEST_classic_token,is_a_test = T))
  expect_true(is_valid_REDCap_token(internal_TEST_repeating_token,is_a_test = T))
  expect_true(is_valid_REDCap_token(internal_TEST_longitudinal_token,is_a_test = T))
  expect_true(is_valid_REDCap_token(internal_TEST_multiarm_token,is_a_test = T))
  expect_false(is_valid_REDCap_token(generate_hex(32),is_a_test = T))
})
test_that("validate_REDCap_token checks_env", {
  DB <- mock_DB()
  token_name <- get_REDCap_token_name(DB)
  token <- generate_hex(32)
  withr::with_envvar(c(RosyREDCap_token_TEST_PROJECT=token),{
    expect_equal(validate_REDCap_token(DB),token)
    expect_no_error(validate_REDCap_token(DB))
  })
  token <- generate_hex(2)
  withr::with_envvar(c(RosyREDCap_token_TEST_PROJECT=token),{
    expect_equal(validate_REDCap_token(DB),token)
  })
  withr::with_envvar(c(RosyREDCap_token_TEST_PROJECT=NULL),{
    expect_equal(validate_REDCap_token(DB),"")
  })
})
test_that("get_REDCap_token_name works", {
  DB <- mock_DB()
  expect_equal(get_REDCap_token_name(DB),"RosyREDCap_token_TEST_PROJECT")
})
# test_that("set_REDCap_token sets a new token", {
#   DB <- mock_DB()
#   mockery::stub(set_REDCap_token, "readline", internal_TEST_classic_token)
#   set_REDCap_token(DB, ask = FALSE)
#   token_name <- get_REDCap_token_name(DB)
#   expect_equal(Sys.getenv(token_name), internal_TEST_classic_token)
# })

# test_that("set_REDCap_token handles existing valid token", {
#   DB <- mock_DB("TEST_classic")
#
#   # Set an existing valid token
#   Sys.setenv(RosyREDCap_token_TEST_classic = internal_TEST_classic_token)
#
#   # Mock user input to not change the token
#   stub(set_REDCap_token, "utils::menu", 2)
#
#   expect_message(set_REDCap_token(DB, ask = TRUE), "You already have a valid token in your R session")
#   token_name <- get_REDCap_token_name(DB)
#   expect_equal(Sys.getenv(token_name), internal_TEST_classic_token)
# })

# test_that("set_REDCap_token changes existing token when user confirms", {
#   DB <- mock_DB("TEST_classic")
#
#   # Set an existing valid token
#   Sys.setenv(RosyREDCap_token_TEST_classic = internal_TEST_classic_token)
#
#   # Mock user input to change the token
#   stub(set_REDCap_token, "utils::menu", 1)
#   stub(set_REDCap_token, "readline", "NEW_FAKE32TESTTOKENCLASSIC1111111111")
#
#   set_REDCap_token(DB, ask = TRUE)
#   token_name <- get_REDCap_token_name(DB)
#   expect_equal(Sys.getenv(token_name), "NEW_FAKE32TESTTOKENCLASSIC1111111111")
# })

# test_that("set_REDCap_token validates the token", {
#   DB <- mock_DB("TEST_classic")
#
#   # Mock user input for an invalid token
#   stub(set_REDCap_token, "readline", "INVALID_TOKEN")
#
#   expect_error(set_REDCap_token(DB, ask = FALSE), "The token is not a valid test token.")
# })
#
# test_that("set_REDCap_token handles test tokens", {
#   DB <- mock_DB("TEST_classic")
#
#   # Mock user input for a test token
#   stub(set_REDCap_token, "readline", internal_TEST_classic_token)
#
#   set_REDCap_token(DB, ask = FALSE)
#   token_name <- get_REDCap_token_name(DB)
#   expect_equal(Sys.getenv(token_name), internal_TEST_classic_token)
# })
#
# test_that("set_REDCap_token handles invalid DB object", {
#   expect_error(set_REDCap_token(NULL), "DB must be a valid REDCap database object")
# })
#
# test_that("set_REDCap_token handles missing API link", {
#   DB <- mock_DB("TEST_classic")
#   DB$links$redcap_API <- NULL
#
#   expect_error(set_REDCap_token(DB, ask = FALSE), "REDCap API link is missing in the DB object")
# })
