#' Make description table
#' @description Function to create a description table template from any dataset.
#' This table can then be used as input for make_data_dictionary().
#' @param data dataset for extracting variable names
#' @param table_ID identifier for the dataset, used to distinguish variables with
#' the same name across different datasets.
#' @param previous_description_table optional; a previous description table to
#' pre-fill descriptions for variables that already exist. If NULL (default),
#' all Description, Units, and How measured columns will be NA.
#'
#' @return a tibble with columns: TableID, Variable name, Description, Units, How measured
#'
#' @importFrom dplyr tibble left_join coalesce select mutate
#' @importFrom tibble as_tibble
#' @examples
#' data(biomass)
#' description <- make_description_table(data = biomass,
#'                                        table_ID = "biomass")
#'
#' # Using a previous description table to pre-fill known variables
#' data(previous_description_table)
#' description <- make_description_table(data = biomass,
#'                                        table_ID = "biomass",
#'                                        previous_description_table = previous_description_table)
#' @export

make_description_table <- function(data, table_ID, previous_description_table = NULL) {

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
          select(`Variable name`, Description_prev = Description,
                 Units_prev = Units, `How measured_prev` = `How measured`),
        by = "Variable name"
      ) |>
      # Use previous values if available, otherwise keep NA
      mutate(
        Description = coalesce(Description_prev, Description),
        Units = coalesce(Units_prev, Units),
        `How measured` = coalesce(`How measured_prev`, `How measured`)
      ) |>
      # Remove temporary columns
      select(TableID, `Variable name`, Description, Units, `How measured`)

  }

  description_table

}
