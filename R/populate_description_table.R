#' Populate description table using Gemini AI
#' @description Function to automatically fill in Description, Units, and How measured
#' columns in a description table by analyzing the dataset with Google's Gemini AI.
#' @param data dataset to analyze
#' @param description_table description table from make_description_table() with columns:
#' TableID, Variable name, Description, Units, How measured
#' @param dataset_context optional; character string with additional context about
#' data collection to help the AI generate better descriptions. Default is NULL.
#' @param n_rows number of sample rows to send to Gemini for context. Default is 100.
#' @param model Gemini model to use. Default is "2.5-flash".
#' Other options: "2.5-pro", "2.0-flash", "1.5-pro".
#' @param verbose logical; if TRUE, shows progress messages. Default is TRUE.
#'
#' @return an updated description table with AI-generated descriptions, units, and
#' measurement methods
#'
#' @details
#' This function requires a Gemini API key set as an environment variable.
#' Set your API key with: \code{Sys.setenv(GEMINI_API_KEY = "your-key")}
#'
#' Get a free API key at: https://aistudio.google.com/apikey
#'
#' The function only processes variables where Description, Units, or How measured
#' are NA, allowing you to run it multiple times and manually edit results.
#'
#' @importFrom dplyr filter mutate select all_of pull
#' @importFrom tibble as_tibble
#' @importFrom rlang .data
#' @importFrom utils head
#'
#' @examples
#' \dontrun{
#' # Set your Gemini API key first
#' Sys.setenv(GEMINI_API_KEY = "your-api-key-here")
#'
#' # Create empty description table
#' data(biomass)
#' description <- make_description_table(data = biomass,
#'                                        table_ID = "biomass",
#'                                        path = NULL)
#'
#' # Populate with AI
#' description_filled <- populate_description_table(
#'   data = biomass,
#'   description_table = description
#' )
#'
#' # With additional context
#' description_filled <- populate_description_table(
#'   data = biomass,
#'   description_table = description,
#'   dataset_context = "Biomass data from functional group removal experiment in alpine sites"
#' )
#' }
#' @export

populate_description_table <- function(data,
                                      description_table,
                                      dataset_context = NULL,
                                      n_rows = 100,
                                      model = "2.5-flash",
                                      verbose = TRUE) {

  # Check if gemini.R is available
  if (!requireNamespace("gemini.R", quietly = TRUE)) {
    stop("Package 'gemini.R' is required. Install it with: install.packages('gemini.R')")
  }

  # Validate inputs
  required_cols <- c("TableID", "Variable name", "Description", "Units", "How measured")
  if (!all(required_cols %in% names(description_table))) {
    stop("description_table must have columns: ", paste(required_cols, collapse = ", "))
  }

  # Check for API key
  api_key <- Sys.getenv("GEMINI_API_KEY")
  if (api_key == "") {
    stop("GEMINI_API_KEY environment variable not set. Set it with: Sys.setenv(GEMINI_API_KEY = 'your-key')\n",
         "Get a free API key at: https://aistudio.google.com/apikey")
  }

  # Set API key for gemini.R
  gemini.R::setAPI(api_key)

  # Convert data to tibble
  data <- as_tibble(data)

  # Check that variables exist in data
  vars_in_table <- description_table$`Variable name`
  vars_in_data <- names(data)
  missing_vars <- setdiff(vars_in_table, vars_in_data)
  if (length(missing_vars) > 0) {
    warning("Variables in description_table not found in data: ",
            paste(missing_vars, collapse = ", "))
  }

  # Identify variables that need completion (any NA in Description, Units, or How measured)
  needs_completion <- description_table |>
    mutate(needs_work = is.na(.data$Description) | is.na(.data$Units) | is.na(.data$`How measured`)) |>
    filter(.data$needs_work) |>
    select(all_of("Variable name")) |>
    pull(.data$`Variable name`)

  if (length(needs_completion) == 0) {
    if (verbose) message("All variables already have descriptions. Nothing to do.")
    return(description_table)
  }

  if (verbose) {
    message("Processing ", length(needs_completion), " variable(s): ",
            paste(needs_completion, collapse = ", "))
  }

  # Prepare data sample
  data_sample <- head(data, n_rows)

  # Get all variable names for context
  all_var_names <- paste(names(data), collapse = ", ")

  # Process each variable
  for (i in seq_along(needs_completion)) {
    var_name <- needs_completion[i]

    if (verbose) {
      message("\n[", i, "/", length(needs_completion), "] Processing: ", var_name)
    }

    # Skip if variable not in data
    if (!var_name %in% vars_in_data) {
      if (verbose) message("  Skipping - variable not found in data")
      next
    }

    # Get sample of this variable
    var_sample <- data_sample[[var_name]]

    # Prepare sample text for current variable
    sample_text <- paste(
      "First", min(n_rows, length(var_sample)), "values:",
      paste(head(var_sample, 20), collapse = ", ")
    )

    # Add summary statistics for numeric variables
    if (is.numeric(var_sample)) {
      sample_text <- paste0(
        sample_text,
        "\nSummary: min=", round(min(var_sample, na.rm = TRUE), 3),
        ", max=", round(max(var_sample, na.rm = TRUE), 3),
        ", mean=", round(mean(var_sample, na.rm = TRUE), 3)
      )
    }

    # Add sample data from related variables for context (first 5 rows)
    related_vars <- setdiff(names(data), var_name)
    if (length(related_vars) > 0 && nrow(data_sample) > 0) {
      related_sample <- data_sample[1:min(5, nrow(data_sample)), related_vars, drop = FALSE]
      related_text <- paste("\nRelated variables (first 5 rows for context):\n",
                           paste(capture.output(print(related_sample, n = 5)), collapse = "\n"))
    } else {
      related_text <- ""
    }

    # Build prompt
    prompt <- paste0(
      "You are helping document a scientific dataset.\n\n",
      if (!is.null(dataset_context)) paste0("Dataset context: ", dataset_context, "\n\n"),
      "All variables in dataset: ", all_var_names, "\n\n",
      "Variable to document: ", var_name, "\n\n",
      "Sample data for this variable:\n", sample_text, "\n",
      related_text, "\n\n",
      "Please provide:\n",
      "1. Description: A concise description of this variable (1-3 sentences). ",
      "If this variable is related to or derived from other variables (e.g., plotID combines siteID, blockID, and treatment), ",
      "mention these relationships. If the variable contains abbreviations or codes, define them in the description ",
      "(e.g., 'F = forbs, G = graminoids, B = bryophytes').\n",
      "2. Units: The unit of measurement using SI standard abbreviations (e.g., 'g', 'cm', 'm^2', 'kg/ha'). ",
      "If no units apply, write 'NA'.\n",
      "3. How measured: How this variable was obtained (choose exactly one: 'defined', 'recorded', or 'measured').\n\n",
      "Format your response EXACTLY as follows (keep the labels):\n",
      "Description: [your description here]\n",
      "Units: [SI units or NA]\n",
      "How measured: [defined/recorded/measured]"
    )

    # Call Gemini API
    tryCatch({
      if (verbose) message("  Calling Gemini with model: ", model)
      response <- gemini.R::gemini(
        prompt = prompt,
        model = model,
        temperature = 0.3  # Lower temperature for more consistent output
      )

      # Parse response
      parsed <- parse_gemini_response(response)

      if (!is.null(parsed)) {
        # Update description_table
        row_idx <- which(description_table$`Variable name` == var_name)
        description_table$Description[row_idx] <- parsed$description
        description_table$Units[row_idx] <- parsed$units
        description_table$`How measured`[row_idx] <- parsed$how_measured

        if (verbose) {
          message("  Description: ", parsed$description)
          message("  Units: ", parsed$units)
          message("  How measured: ", parsed$how_measured)
        }
      } else {
        if (verbose) message("  Failed to parse response")
      }

    }, error = function(e) {
      if (verbose) message("  Error calling Gemini API: ", e$message)
    })

    # Small delay to avoid rate limiting
    Sys.sleep(0.5)
  }

  if (verbose) message("\nCompleted processing all variables.")

  return(description_table)
}


#' Parse Gemini response to extract description, units, and how measured
#' @param response character string from Gemini API
#' @return list with description, units, how_measured, or NULL if parsing fails
#' @keywords internal
parse_gemini_response <- function(response) {
  # Try to extract the three fields using regex
  desc_match <- regmatches(response, regexpr("(?<=Description:\\s).*?(?=\\n)", response, perl = TRUE))
  units_match <- regmatches(response, regexpr("(?<=Units:\\s).*?(?=\\n)", response, perl = TRUE))
  how_match <- regmatches(response, regexpr("(?<=How measured:\\s).*?(?=\\n|$)", response, perl = TRUE))

  # Check if all extractions succeeded
  if (length(desc_match) == 0 || length(units_match) == 0 || length(how_match) == 0) {
    return(NULL)
  }

  # Clean up the extracted text
  description <- trimws(desc_match)
  units <- trimws(units_match)
  how_measured <- trimws(how_match)

  # Validate how_measured is one of the expected values
  valid_how <- c("defined", "recorded", "measured")
  if (!tolower(how_measured) %in% valid_how) {
    # Try to find a valid value in the response
    for (val in valid_how) {
      if (grepl(val, tolower(how_measured))) {
        how_measured <- val
        break
      }
    }
  } else {
    how_measured <- tolower(how_measured)
  }

  # Return NULL if units is NA string
  if (units == "NA" || tolower(units) == "na") {
    units <- NA_character_
  }

  return(list(
    description = description,
    units = units,
    how_measured = how_measured
  ))
}
