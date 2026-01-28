#### Setup: create test data and description tables ####

# make numeric data
num <- tibble(num = c(2.3, 5.2, 4.8))

# make character
char <- tibble(char = c("a", "b", "c"))

# make data with date
date <- tibble(date = ymd("2014-04-06", "2015-12-06", "1950-04-09"))

# two variables
var2 <- tibble(var = "Gudmedalen",
               num = c(2.3, 5.2, 4.8))

# make description table
description_table <- tibble("TableID" = c(NA_character_, "num_ID", "char_ID", "date_ID"),
                            "Variable name" = c("var", "num", "char", "date"),
                            Description = c("Description of variable",
                                            "Description of numeric variable",
                                            "Description of character variable",
                                            "Sampling date"),
                            "Units" = c(NA_character_, "g", NA_character_, "yyy-mm-dd"),
                            "How measured" = c("recorded", "measured", "defined", "recorded"))


#### Tests for get_range helper function ####

test_that("get_range works for numeric data", {
  expected <- tibble("Variable name" = "num",
                     "Variable range or levels" = "2.3 - 5.2")
  expect_equal(get_range(num), expected)
})

test_that("get_range works for character data", {
  expected <- tibble("Variable name" = "char",
                     "Variable range or levels" = "a - c")
  expect_equal(get_range(char), expected)
})

test_that("get_range works for date data", {
  expected <- tibble("Variable name" = "date",
                     "Variable range or levels" = "1950-04-09 - 2015-12-06")
  expect_equal(get_range(date), expected)
})

test_that("get_range works for multiple variables", {
  expected <- tibble("Variable name" = c("var", "num"),
                     "Variable range or levels" = c("Gudmedalen - Gudmedalen", "2.3 - 5.2"))
  expect_equal(get_range(var2), expected)
})

test_that("get_range handles integer data", {
  int_data <- tibble(int_var = c(1L, 5L, 10L))
  expected <- tibble("Variable name" = "int_var",
                     "Variable range or levels" = "1 - 10")
  expect_equal(get_range(int_data), expected)
})


#### Tests for get_class helper function ####

test_that("get_class returns numeric for numeric data", {
  expected <- tibble("Variable name" = "num",
                     "Variable type" = "numeric")
  expect_equal(get_class(num), expected)
})

test_that("get_class returns categorical for character data", {
  expected <- tibble("Variable name" = "char",
                     "Variable type" = "categorical")
  expect_equal(get_class(char), expected)
})

test_that("get_class returns date for Date data", {
  expected <- tibble("Variable name" = "date",
                     "Variable type" = "date")
  expect_equal(get_class(date), expected)
})

test_that("get_class returns numeric for integer data", {
  int_data <- tibble(int_var = c(1L, 5L, 10L))
  expected <- tibble("Variable name" = "int_var",
                     "Variable type" = "numeric")
  expect_equal(get_class(int_data), expected)
})

test_that("get_class returns categorical for logical data", {
  logical_data <- tibble(log_var = c(TRUE, FALSE, TRUE))
  expected <- tibble("Variable name" = "log_var",
                     "Variable type" = "categorical")
  expect_equal(get_class(logical_data), expected)
})

test_that("get_class works for multiple variables", {
  expected <- tibble("Variable name" = c("var", "num"),
                     "Variable type" = c("categorical", "numeric"))
  expect_equal(get_class(var2), expected)
})


#### Tests for make_data_dictionary ####

test_that("make_data_dictionary works for numeric data", {
  result <- make_data_dictionary(data = num,
                                 description_table = description_table,
                                 table_ID = "num_ID",
                                 keep_table_ID = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$`Variable name`, "num")
  expect_equal(result$`Variable type`, "numeric")
  expect_equal(result$Description, "Description of numeric variable")
  expect_equal(result$Units, "g")
})

test_that("make_data_dictionary works for character data", {
result <- make_data_dictionary(data = char,
                                 description_table = description_table,
                                 table_ID = "char_ID",
                                 keep_table_ID = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$`Variable name`, "char")
  expect_equal(result$`Variable type`, "categorical")
  expect_equal(result$Description, "Description of character variable")
})

test_that("make_data_dictionary works for date data", {
  result <- make_data_dictionary(data = date,
                                 description_table = description_table,
                                 table_ID = "date_ID",
                                 keep_table_ID = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$`Variable name`, "date")
  expect_equal(result$`Variable type`, "date")
  expect_equal(result$`Variable range or levels`, "1950-04-09 - 2015-12-06")
})

test_that("make_data_dictionary works with multiple variables", {
  result <- make_data_dictionary(data = var2,
                                 description_table = description_table,
                                 table_ID = "num_ID",
                                 keep_table_ID = TRUE)

  expect_equal(nrow(result), 2)
  expect_equal(result$`Variable name`, c("var", "num"))
})

test_that("make_data_dictionary returns correct columns", {
  result <- make_data_dictionary(data = num,
                                 description_table = description_table,
                                 table_ID = "num_ID",
                                 keep_table_ID = TRUE)

  expected_cols <- c("TableID", "Variable name", "Description",
                     "Variable type", "Variable range or levels",
                     "Units", "How measured")
  expect_equal(names(result), expected_cols)
})

test_that("make_data_dictionary removes TableID when keep_table_ID = FALSE", {
  result <- make_data_dictionary(data = num,
                                 description_table = description_table,
                                 table_ID = "num_ID",
                                 keep_table_ID = FALSE)

  expect_false("TableID" %in% names(result))

  expected_cols <- c("Variable name", "Description",
                     "Variable type", "Variable range or levels",
                     "Units", "How measured")
  expect_equal(names(result), expected_cols)
})

test_that("make_data_dictionary handles NA TableID for shared variables", {
  # var has NA TableID in description_table (shared across datasets)
  result <- make_data_dictionary(data = var2,
                                 description_table = description_table,
                                 table_ID = "num_ID",
                                 keep_table_ID = TRUE)

  # Both variables should have the table_ID assigned
  expect_true(all(result$TableID == "num_ID"))
})
