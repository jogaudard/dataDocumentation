## code to prepare `previous_description_table` dataset goes here

library(dplyr)

# Create a minimal description table with only year and siteID
# This is used to demonstrate pre-filling in make_description_table()

previous_description_table <- tibble(
  TableID = NA_character_,
  `Variable name` = c("year", "siteID"),
  Description = c("year of sampling", "unique site code"),
  Units = c("yyyy", NA_character_),
  `How measured` = c("defined", "defined")
)

usethis::use_data(previous_description_table, overwrite = TRUE)
