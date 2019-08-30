#' Pipeline
#'
#' This function performs the analysis and creates all figures included
#' in Cazelles (2019).
#'
#' @param tnr a logical. Should Times New Roman be used as a typeface?
#'
#' @export

pipeline <- function(tnr = TRUE) {
  
  if (tnr) {
    showtext_auto()
    font_add(family = 'TNM', regular = system.file("extdata", 
      "times-new-roman.ttf", package = "HomogenFishOntario"))
  }
  # As we were not allowed to share lake location `sf_bsm_ahi` is not included
  # res0 <- an1_gain_loss(sf_ahi_bsm0)
  res0 <- an1_gain_loss(sf_bsm_ahi)
  res1 <- an2_occurrence_species(res0)
  mods <- an3_regressions(res0, res1)
  res2 <- an4_contributions_beta(res0, res1)
  # number of lakes
  nlk <- nrow(res0[[1]])

  ## Figure Main
  # Fig 1 - Map not included
  fig2_homogenization(res0)
  # fig2_homogenization(res0, asfinal = FALSE)
  fig3_regressions(res0, mods)
  # fig3_regressions(res0, mods, asfinal = FALSE)
  fig4_table(res1, nlk)
  fig5_piecharts(res1, nlk)
  fig6_contributions_beta(res2)

  ## Figure SI
  figS1_outliers(an1_gain_loss(sf_bsm_ahi, rm_outlier = FALSE))
  figS2_abiotic(res0)
  # Cannot be run as exact lake locations are required
  # figS3_lossgain(res0)
  # figS4_homogenization_panels()
  figS5_saturation(res0)
  # Cannot be run as exact lake latitudes are required
  # figS6_northward(res1)
  figS7_occupancy_trends(res1)
  figS8_boats(res0)

  ## Figures that did not make the final version
  # figZ1_fnf_regressions(res0, res2)
  # figZ2_coldest(res0, res1)
}
