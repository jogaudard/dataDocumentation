#' Make description table
#' @description Function to create a description table template from any dataset.
#' This table can then be used as input for make_data_dictionary().
#' @param data dataset for extracting variable names
#' @param table_ID identifier for the dataset, used to distinguish variables with
#' the same name across different datasets.
#'
#' @return a tibble with columns: TableID, Variable name, Description, Units, How measured
#'
#' @importFrom dplyr tibble
#' @importFrom tibble as_tibble
#' @examples
#' data(biomass)
#' description <- make_description_table(data = biomass,
#'                                        table_ID = "biomass")
#' @export

make_description_table <- function(data, table_ID) {

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

  description_table

}
