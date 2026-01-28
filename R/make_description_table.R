#' Make description table
#' @description Function to create a description table template from any dataset.
#' This table can then be used as input for make_data_dictionary().
#' @param data dataset for extracting variable names
#' @param table_ID identifier for the dataset, used to distinguish variables with
#' the same name across different datasets.
#' @param previous_description_table optional; a previous description table to
#' pre-fill descriptions for variables that already exist. If NULL (default),
#' all Description, Units, and How measured columns will be NA.
#' @param path directory path to write the description table as a CSV file.
#' Default is "data_dic". The directory will be created if it does not exist.
#' The file will be named "description_table.csv". Set to NULL to skip writing.
#'
#' @return a tibble with columns: TableID, Variable name, Description, Units, How measured
#'
#' @importFrom dplyr tibble left_join coalesce select mutate all_of
#' @importFrom tibble as_tibble
#' @importFrom readr write_csv
#' @importFrom rlang .data
#' @examples
#' data(biomass)
#' description <- make_description_table(data = biomass,
#'                                        table_ID = "biomass",
#'                                        path = NULL)
#'
#' # Using a previous description table to pre-fill known variables
#' data(previous_description_table)
#' description <- make_description_table(data = biomass,
#'                                        table_ID = "biomass",
#'                                        previous_description_table = previous_description_table,
#'                                        path = NULL)
#'
#' # Write to CSV file in data_dic folder (default)
#' \dontrun{
#' make_description_table(data = biomass,
#'                        table_ID = "biomass")
#' }
#' @export

make_description_table <- function(data, table_ID, previous_description_table = NULL, path = "data_dic") {

  # Get all variable names from the data

  variable_names <- names(as_tibble(data))

  # Create description table template with all required columns

  description_table <- tibble(
    TableID = table_ID,
    `Variable name` = variable_names,
    Description = NA_character_,
    Units = NA_character_,
    `How measured` = NA_character_
  )

  # If previous description table is provided, fill in matching variables

  if (!is.null(previous_description_table)) {

    # Join with previous table on Variable name to get existing descriptions
    description_table <- description_table |>
      left_join(
        previous_description_table |>
          select(all_of(c("Variable name")),
                 Description_prev = all_of("Description"),
                 Units_prev = all_of("Units"),
                 `How measured_prev` = all_of("How measured")),
        by = "Variable name"
      ) |>
      # Use previous values if available, otherwise keep NA
      mutate(
        Description = coalesce(.data$Description_prev, .data$Description),
        Units = coalesce(.data$Units_prev, .data$Units),
        `How measured` = coalesce(.data$`How measured_prev`, .data$`How measured`)
      ) |>
      # Remove temporary columns
      select(all_of(c("TableID", "Variable name", "Description", "Units", "How measured")))

  }

  # Write to CSV if path is provided
  if (!is.null(path)) {
    # Create directory if it doesn't exist
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
    # Write to description_table.csv in the specified directory
    file_path <- file.path(path, "description_table.csv")
    write_csv(description_table, file_path)
  }

  description_table

}
