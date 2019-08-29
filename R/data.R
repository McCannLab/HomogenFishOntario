#' Lake data for AHI and BSM survey
#'
#' Lake data including species and presence absence and abiotic variables
#' both climatic variables and lake descriptors.
#'
#' @docType data
#' @keywords datasets
#' @name sf_bsm_ahi
#' @usage sf_bsm_ahi
#' @format A `sf` object
#' @references
#' * Dodge, D. P., et a. (1987). Manual of instructions: Aquatic habitat inventory surveys. Ontario Ministry of Natural Resources.
#' * Sandstrom, S., Rawson, M., & Lester, N. P. (2013). Manual of instructions for broad-scale fish community monitoring using North American (NA1) and Ontario small mesh (ON2) gillnets. Ontario Ministry of Natural Resources.
#' * McKenney, D. et al. (2013). The Forestry Chronicle. DOI: 10.5558/tfc2013-118.
"sf_bsm_ahi"
#


#' Ontario boundaries
#'
#' Ontario boundaries etrieved from https://gadm.org/ using [raster::getData()] and convert to a `sf` object.
#'
#' @docType data
#' @keywords datasets
#' @name gadm_ontario
#' @usage gadm_ontario
#' @format A `sf` object including the border of Ontario.
"gadm_ontario"


#' Information about commercial aspects of fish species
#'
#' Information about commercial aspects of fish species
#'
#' @docType data
#' @keywords datasets
#' @name df_fished
#' @usage df_fished
#' @format A data frame of 8 columns and 39 rows.
"df_fished"


#' Characteristics of Ontarian freshwater fish species
#'
#' Characteristics of freshwater fish species included in the study retrieved
#' from the literature as explained in Cazelles (2019).
#'
#' @docType data
#' @keywords datasets
#' @author Kevin Cazelles
#' @name df_species_info
#' @usage df_species_info
#' @references
#' Cazelles (2019)
#' @format A data frame of 34 columns and 169 rows.
"df_species_info"

