#' Create durin meta data
#' @description Function to make durin meta data.
#' @param study select study for which metadata will be created. 4Corners: study on the 4 main Durin sites; DroughtNet: droughtNet experiment at Lygra and Tjotta; Nutrient: nutrient experiment at Lygra
#' @param csv_output logical; argument csv_output or not.
#' The default is csv_output = FALSE. If csv_output = TRUE a csv file is produced and saved in the project directory.
#' @param filename file name for storing csv output. Default is NULL. The path is only needed i csv_output = TRUE. The file name contains Durin_study_filename.csv, where the file name is defined by the user.
#'
#' @return a tibble and optionally a csv file
#'
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr case_when
#' @importFrom tidyr crossing
#' @importFrom readr write_csv
#' @importFrom stringr str_sub
#' @importFrom rlang .data
#' @importFrom utils data
#'
#' @examples
#' create_durin_meta_data()
#'
#' @export


create_durin_meta_data <- function(study = NULL, csv_output = FALSE, filename = NULL){

  if(is.null(study)){
    print("Warning: unknown study type. Choose 4Corners, DroughtNet or Nutrient")
    durin_metadata <- NULL

    # meta data 4Corners study
  } else if (study == "4Corners"){
  site <- tibble(site_name = c("Lygra", "Sogndal", "Senja", "Kautokeino"),
         siteID = c("LY", "SO", "SE", "KA"))
  habitat = c("Open", "Forested")
  sp = tibble(species = c("Vaccinium myrtillus", "Vaccinium vitis-idaea", "Calluna vulgaris", "Empetrum nigrum"),
              speciesID = c("VM", "VV", "CV", "EN"))
  plot_nr = c(1, 2, 3, 4, 5)

  durin_metadata <- crossing(site, habitat, sp, plot_nr) |>
    filter(!c(.data$species == "Calluna vulgaris" & .data$siteID == "KA")) |>
    mutate(biogeography = if_else(.data$siteID %in% c("LY", "SO"), "Boreal", "Sub-arctic"),
           oceantiy = if_else(.data$siteID %in% c("LY", "SE"), "Coast", "Inland"),
           plotID = paste(.data$siteID, str_sub(.data$habitat, 1, 1), .data$speciesID, plot_nr, sep = "_")) |>
    select("site_name", "siteID", "biogeography", "oceantiy", "habitat", "species", "speciesID", "plot_nr", "plotID")

  # meta data DroughtNet study
  } else if(study == "DroughtNet"){

    data("droughNet_meta", package = "dataDocumentation", envir = environment())
    droughNet_meta <- get("droughNet_meta", envir = environment())

    meta <- droughNet_meta |>
      mutate(plot_nr = str_sub(.data$plot, 1,3)) |>
      mutate(siteID = if_else(.data$site_name == "Lygra", "LY", "TJ"),
             age_classID = case_when(.data$age_class == "Pioneer" ~ "PIO",
                                     .data$age_class == "Building" ~ "BUI",
                                     .data$age_class == "Mature" ~ "MAT"),
             drought_treatmentID = toupper(str_sub(.data$drought_treatment, 1, 3)),
             habitat = "Open")
    sp = tibble(species = c("Vaccinium myrtillus", "Vaccinium vitis-idaea", "Calluna vulgaris", "Empetrum nigrum"),
                speciesID = c("VM", "VV", "CV", "EN"))

    durin_metadata <- crossing(meta, sp) |>
      mutate(plotID = paste(.data$siteID, .data$age_classID, .data$drought_treatmentID, .data$plot_nr, sep = "_")) |>
      select("site_name", "siteID", "geography", "habitat", "age_class", "age_classID", "drought_treatment", "drought_treatmentID", "plot_nr", "plotID", "species", "speciesID", "landpress_name")

  # meta data Nutrient study
  } else if(study == "Nutrient"){

    durin_metadata <- tibble(site_name = c("Lygra"),
           siteID = c("LY"),
           habitat = c("Open"),
           age_class = c("Building"),
           age_classID = "BUI") |>
      crossing(nitrogen_addition = c(0, 1, 5, 10, 25),
               block_nr = c("N1", "N2", "N3", "N4", "N5")) |>
      mutate(plotID = paste(.data$block_nr, .data$nitrogen_addition, sep = "-"))

  } else {
    print("Warning: unknown study type. Choose 4Corners, DroughtNet or Nutrient")
      durin_metadata <- NULL
  }

  # create csv output
  if(csv_output){

    if(is.null(filename)){

      filepath <- paste0("Durin_", study, ".csv")
      write_csv(x = durin_metadata,
                file = filepath)

    } else if(!is.null(filename)){

      filepath <- paste0("Durin_", study, "_", filename, ".csv")

      write_csv(x = durin_metadata,
                file = filepath)

    }
  }

  return(durin_metadata)

}
