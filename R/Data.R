#' Biomass data
#'
#' A dataset containing one year of biomass data from functional group removal experiment from four alpine sites in western Norway.
#'
#' @format A tibble with 192 rows and 7 variables:
#' \describe{
#'   \item{year}{year of sampling}
#'   \item{siteID}{unique site code}
#'   \item{blockID}{unique block code}
#'   \item{plotID}{unique plot code}
#'   \item{treatment}{functional group removal treatment}
#'   \item{removed_fg}{functional group removed}
#'   \item{biomass}{biomass value in g}
#'   ...
#' }
#' @source \url{https://osf.io/4c5v2/}
"biomass"
#'
#'
#' Description table
#'
#' A table describing the dataset.
#'
#' @format A tibble with 192 rows and 7 variables:
#' \describe{
#'   \item{TableID}{unique code for each dataset}
#'   \item{Variable name}{variable name}
#'   \item{Description}{variable description}
#'   \item{Units}{units for variables}
#'   \item{How measured}{how a variable was measured}
#'   ...
#' }
#' @source \url{https://osf.io/4c5v2/}
"description_table"

#' Previous description table
#'
#' A minimal description table containing only year and siteID variables.
#' Used to demonstrate pre-filling in make_description_table().
#'
#' @format A tibble with 2 rows and 5 variables:
#' \describe{
#'   \item{TableID}{unique code for each dataset}
#'   \item{Variable name}{variable name}
#'   \item{Description}{variable description}
#'   \item{Units}{units for variables}
#'   \item{How measured}{how a variable was measured}
#'   ...
#' }
"previous_description_table"


#' Block dictionary
#'
#' A table block_dictionary.
#'
#' @format A tibble with 48 rows and 2 variables:
#' \describe{
#'   \item{funder_blockID}{unique code for funder blockID}
#'   \item{funcab_blockID}{unique code for funcab blockID}
#'   ...
#' }
#' @source \url{https://osf.io/tx9r2/}
"dic"


#' Funder
#'
#' A random Funder dataset.
#'
#' @format A tibble with 4 rows and 3 variables:
#' \describe{
#'   \item{funder_blockID}{unique code for funder blockID}
#'   \item{var}{random variable}
#'   \item{treatment}{unique code for treatement}
#'   ...
#' }
"funder"

#' FunCaB
#'
#' A random FunCaB dataset.
#'
#' @format A tibble with 4 rows and 3 variables:
#' \describe{
#'   \item{funder_blockID}{unique code for funder blockID}
#'   \item{var}{random variable}
#'   \item{treatment}{unique code for treatement}
#'   ...
#' }
"funcab"

#' droughNet_meta
#'
#' DroughtNet meta data.
#'
#' @format A tibble with 54 rows and 6 variables:
#' \describe{
#'   \item{site_name}{Unique site name}
#'   \item{landpress_name}{Site names used in Landpress project}
#'   \item{geography}{}
#'   \item{drought_treatment}{DroughtNet treatment which is ambient, moderate or extreme}
#'   \item{age_class}{Age class of the vegetation representing post-fire successional stages}
#'   \item{plot}{Unique plot number, anumeric value}
#'   ...
#' }
"droughNet_meta"
NULL
