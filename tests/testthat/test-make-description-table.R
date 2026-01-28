test_that("make_description_table creates correct structure", {

  # Create test data
  test_data <- tibble(
    year = c(2020, 2021),
    siteID = c("A", "B"),
    value = c(1.5, 2.5)
  )

  # Test basic functionality

  result <- make_description_table(data = test_data, table_ID = "test")

  # Check that result is a tibble

  expect_s3_class(result, "tbl_df")

  # Check correct number of rows (one per variable)
  expect_equal(nrow(result), 3)

  # Check correct column names
  expect_equal(
    names(result),
    c("TableID", "Variable name", "Description", "Units", "How measured")
  )

  # Check variable names are extracted correctly
  expect_equal(result$`Variable name`, c("year", "siteID", "value"))

  # Check TableID is set correctly
  expect_true(all(result$TableID == "test"))

})


test_that("make_description_table returns NA when no previous table", {

  test_data <- tibble(
    year = c(2020, 2021),
    siteID = c("A", "B")
  )

  result <- make_description_table(data = test_data, table_ID = "test")

  # All Description, Units, How measured should be NA
  expect_true(all(is.na(result$Description)))
  expect_true(all(is.na(result$Units)))
  expect_true(all(is.na(result$`How measured`)))

})


test_that("make_description_table fills in from previous table", {

  # Create test data with 3 variables
  test_data <- tibble(
    year = c(2020, 2021),
    siteID = c("A", "B"),
    new_var = c(1, 2)
  )

  # Create previous description table with only year and siteID
  prev_table <- tibble(
    TableID = NA_character_,
    `Variable name` = c("year", "siteID"),
    Description = c("year of sampling", "unique site code"),
    Units = c("yyyy", NA_character_),
    `How measured` = c("defined", "defined")
  )

  result <- make_description_table(
    data = test_data,
    table_ID = "test",
    previous_description_table = prev_table
  )

  # Check that year is filled in
  year_row <- result[result$`Variable name` == "year", ]
  expect_equal(year_row$Description, "year of sampling")
  expect_equal(year_row$Units, "yyyy")
  expect_equal(year_row$`How measured`, "defined")

  # Check that siteID is filled in
  site_row <- result[result$`Variable name` == "siteID", ]
  expect_equal(site_row$Description, "unique site code")
  expect_true(is.na(site_row$Units))
  expect_equal(site_row$`How measured`, "defined")

  # Check that new_var remains NA (not in previous table)
  new_row <- result[result$`Variable name` == "new_var", ]
  expect_true(is.na(new_row$Description))
  expect_true(is.na(new_row$Units))
  expect_true(is.na(new_row$`How measured`))

})


test_that("make_description_table handles empty previous table", {

  test_data <- tibble(
    year = c(2020, 2021),
    value = c(1.5, 2.5)
  )

  # Empty previous table (0 rows)
  empty_prev <- tibble(
    TableID = character(),
    `Variable name` = character(),
    Description = character(),
    Units = character(),
    `How measured` = character()
  )

  result <- make_description_table(
    data = test_data,
    table_ID = "test",
    previous_description_table = empty_prev
  )

  # Should still work, all values should be NA
  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$Description)))

})


test_that("make_description_table preserves variable order from data", {

  # Create test data with specific column order
  test_data <- tibble(
    z_var = 1,
    a_var = 2,
    m_var = 3
  )

  result <- make_description_table(data = test_data, table_ID = "test")

  # Variable order should match original data, not alphabetical
  expect_equal(result$`Variable name`, c("z_var", "a_var", "m_var"))

})


test_that("make_description_table writes CSV when path is provided", {

  test_data <- tibble(
    year = c(2020, 2021),
    siteID = c("A", "B")
  )

  # Create temporary file path
  temp_file <- tempfile(fileext = ".csv")

  # Run function with path
  result <- make_description_table(
    data = test_data,
    table_ID = "test",
    path = temp_file
  )

  # Check that file was created
  expect_true(file.exists(temp_file))

  # Read the file back and compare
  written_data <- readr::read_csv(temp_file, show_col_types = FALSE)
  expect_equal(nrow(written_data), 2)
  expect_equal(written_data$`Variable name`, c("year", "siteID"))

  # Function should still return the tibble
  expect_s3_class(result, "tbl_df")

  # Clean up

  unlink(temp_file)

})
