#' Convert Funder terminology to FunCaB terminology and vice versa
#' @description Function to Convert Funder terminology to FunCaB terminology and vice versa.
#' @param dat data input file to convert
#' @param convert_to Conversion to desired terminology. FunCaB means that Funder terminology is converted to FunCaB, and else it is converted to Funder.
#'
#' @return a tibble
#'
#' @importFrom dplyr left_join mutate select coalesce
#' @importFrom readr read_csv
#' @importFrom rlang .data
#' @importFrom utils data
#'
#' @examples
#' data(funder)
#' funcabization(dat = funder, convert_to = "FunCaB")
#' data(funcab)
#' funcabization(dat = funcab, convert_to = "Funder")
#'
#' @export

funcabization <- function(dat, convert_to = "FunCaB"){

  data("dic", package = "dataDocumentation", envir = environment())
  dic <- get("dic", envir = environment())

  # convert to FunCaB
  if(convert_to == "FunCaB"){
    dat |>
      # join with dicionary by funder blockID
      left_join(dic, by = c("blockID" = "funder_blockID")) |>
      mutate(blockID = coalesce(.data$funcab_blockID, .data$blockID),
             plotID = paste0(.data$blockID, .data$treatment)) |>
      select(-.data$funcab_blockID)
    # convert to Funder
  } else if(convert_to == "Funder") {
    dat |>
      # join with dictionary by funcab blockID
      left_join(dic, c("blockID" = "funcab_blockID")) |>
      mutate(blockID = coalesce(.data$funder_blockID, .data$blockID),
             plotID = paste0(.data$blockID, .data$treatment)) |>
      select(-.data$funder_blockID)
  }

}
