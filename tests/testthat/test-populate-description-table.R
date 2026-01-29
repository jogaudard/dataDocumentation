#### Tests for populate_description_table ####

test_that("populate_description_table requires gemini.R package", {
  skip_if_not_installed("gemini.R")

  test_data <- tibble(
    year = c(2020, 2021),
    value = c(1.5, 2.5)
  )

  description <- tibble(
    TableID = "test",
    `Variable name` = c("year", "value"),
    Description = NA_character_,
    Units = NA_character_,
    `How measured` = NA_character_
  )

  # Save current API key and unset it temporarily
  old_key <- Sys.getenv("GEMINI_API_KEY")
  Sys.unsetenv("GEMINI_API_KEY")

  # This test checks error handling when API key is missing
  expect_error(
    populate_description_table(
      data = test_data,
      description_table = description
    ),
    "GEMINI_API_KEY"
  )

  # Restore original API key
  if (old_key != "") {
    Sys.setenv(GEMINI_API_KEY = old_key)
  }
})


test_that("populate_description_table validates description_table structure", {
  skip_if_not_installed("gemini.R")

  test_data <- tibble(
    year = c(2020, 2021),
    value = c(1.5, 2.5)
  )

  # Missing required columns
  bad_description <- tibble(
    `Variable name` = c("year", "value")
  )

  # Set a dummy API key to get past that check
  old_key <- Sys.getenv("GEMINI_API_KEY")
  Sys.setenv(GEMINI_API_KEY = "dummy_key_for_testing")

  expect_error(
    populate_description_table(
      data = test_data,
      description_table = bad_description
    ),
    "description_table must have columns"
  )

  # Restore original API key
  Sys.setenv(GEMINI_API_KEY = old_key)
})


test_that("populate_description_table warns about missing variables", {
  skip_if_not_installed("gemini.R")

  test_data <- tibble(
    year = c(2020, 2021)
  )

  description <- tibble(
    TableID = "test",
    `Variable name` = c("year", "nonexistent_var"),
    Description = NA_character_,
    Units = NA_character_,
    `How measured` = NA_character_
  )

  old_key <- Sys.getenv("GEMINI_API_KEY")
  Sys.setenv(GEMINI_API_KEY = "dummy_key_for_testing")

  expect_warning(
    populate_description_table(
      data = test_data,
      description_table = description,
      verbose = FALSE
    ),
    "not found in data"
  )

  Sys.setenv(GEMINI_API_KEY = old_key)
})


test_that("populate_description_table skips completed variables", {
  skip_if_not_installed("gemini.R")

  test_data <- tibble(
    year = c(2020, 2021),
    value = c(1.5, 2.5)
  )

  # All variables already have descriptions
  description <- tibble(
    TableID = "test",
    `Variable name` = c("year", "value"),
    Description = c("Year of measurement", "Measurement value"),
    Units = c("yyyy", "g"),
    `How measured` = c("defined", "measured")
  )

  old_key <- Sys.getenv("GEMINI_API_KEY")
  Sys.setenv(GEMINI_API_KEY = "dummy_key_for_testing")

  # Should return immediately without calling API
  result <- populate_description_table(
    data = test_data,
    description_table = description,
    verbose = FALSE
  )

  expect_equal(result, description)

  Sys.setenv(GEMINI_API_KEY = old_key)
})


test_that("populate_description_table identifies variables needing completion", {
  skip_if_not_installed("gemini.R")

  test_data <- tibble(
    year = c(2020, 2021),
    value = c(1.5, 2.5),
    site = c("A", "B")
  )

  # Mixed: some filled, some not
  description <- tibble(
    TableID = "test",
    `Variable name` = c("year", "value", "site"),
    Description = c("Year of measurement", NA_character_, NA_character_),
    Units = c("yyyy", "g", NA_character_),
    `How measured` = c("defined", "measured", NA_character_)
  )

  old_key <- Sys.getenv("GEMINI_API_KEY")
  Sys.setenv(GEMINI_API_KEY = "dummy_key_for_testing")

  # Should identify that value and site need work
  # (value has Description=NA, site has all NA)
  # We can't test the actual API call without a real key,
  # but we can verify the function runs
  expect_no_error({
    result <- populate_description_table(
      data = test_data,
      description_table = description,
      verbose = FALSE
    )
  })

  Sys.setenv(GEMINI_API_KEY = old_key)
})


#### Tests for parse_gemini_response helper function ####

test_that("parse_gemini_response extracts correct fields", {
  response <- "Description: Year of sampling
Units: yyyy
How measured: defined"

  result <- dataDocumentation:::parse_gemini_response(response)

  expect_equal(result$description, "Year of sampling")
  expect_equal(result$units, "yyyy")
  expect_equal(result$how_measured, "defined")
})


test_that("parse_gemini_response handles NA units", {
  response <- "Description: Site identifier code
Units: NA
How measured: defined"

  result <- dataDocumentation:::parse_gemini_response(response)

  expect_equal(result$description, "Site identifier code")
  expect_true(is.na(result$units))
  expect_equal(result$how_measured, "defined")
})


test_that("parse_gemini_response handles different how_measured values", {
  response1 <- "Description: Test var
Units: g
How measured: measured"

  response2 <- "Description: Test var
Units: NA
How measured: recorded"

  response3 <- "Description: Test var
Units: NA
How measured: defined"

  result1 <- dataDocumentation:::parse_gemini_response(response1)
  result2 <- dataDocumentation:::parse_gemini_response(response2)
  result3 <- dataDocumentation:::parse_gemini_response(response3)

  expect_equal(result1$how_measured, "measured")
  expect_equal(result2$how_measured, "recorded")
  expect_equal(result3$how_measured, "defined")
})


test_that("parse_gemini_response returns NULL for invalid format", {
  bad_response <- "This is not the expected format"

  result <- dataDocumentation:::parse_gemini_response(bad_response)

  expect_null(result)
})


test_that("parse_gemini_response handles extra whitespace", {
  response <- "Description:   Year of sampling  
Units:  yyyy  
How measured:  defined  "

  result <- dataDocumentation:::parse_gemini_response(response)

  expect_equal(result$description, "Year of sampling")
  expect_equal(result$units, "yyyy")
  expect_equal(result$how_measured, "defined")
})


test_that("parse_gemini_response normalizes how_measured case", {
  response <- "Description: Test var
Units: NA
How measured: MEASURED"

  result <- dataDocumentation:::parse_gemini_response(response)

  expect_equal(result$how_measured, "measured")
})


#### Integration tests (require actual API key) ####

test_that("populate_description_table works end-to-end with real API", {
  skip_if_not_installed("gemini.R")
  skip_if(Sys.getenv("GEMINI_API_KEY") == "", "GEMINI_API_KEY not set")

  # Small test dataset
  test_data <- tibble(
    year = c(2020, 2021, 2022),
    temperature = c(15.5, 16.2, 14.8)
  )

  description <- tibble(
    TableID = "test",
    `Variable name` = c("year", "temperature"),
    Description = NA_character_,
    Units = NA_character_,
    `How measured` = NA_character_
  )

  result <- populate_description_table(
    data = test_data,
    description_table = description,
    verbose = FALSE
  )

  # Check that descriptions were filled
  expect_false(is.na(result$Description[1]))
  expect_false(is.na(result$Description[2]))

  # Check structure is preserved
  expect_equal(nrow(result), 2)
  expect_equal(names(result), names(description))
})
