#' Compute gains and losses
#'
#' Filter data and compute gains and losses
#'
#' @param x data set of observation see `?df_bsm_ahi`.
#' @param min_year_AHI First year for AHI (see `details` below).
#' @param max_year_AHI Last year for AHI (see `details` below).
#' @param nocc Only species that have more than `nocc` occurrence are considered .
#' @param rm_outlier a logical. Should outlier be removed? Default is set to `TRUE`.
#' @param verbose a logical. Should extra information be reported on progress? Default is set to `TRUE`.
#'
#' @details
#' `min_year_AHI` and `max_year_AHI` are used to reproduce the analysis with
#' a subset of the dataset as used in Fig. S3.
#'
#' @return
#' A list of six elements:
#' - `sf_bsm_ahi`: a sf object including filtered data,
#' - `gain_loss`: gains and losses for all lakes,
#' - `beta_AHI`: beta diversity measure in AHI (historical survey),
#' - `beta_BSM`: beta diversity measure in BSM (contempory survey),
#' - `nm_ahi`: column names of ahi presence/absence columns in AHI,
#' - `nm_bsm`: column names of bsm presence/absence columns in BSM.
#'
#' @export
#'
#' @examples
#' res0 <- an1_gain_loss(sf_bsm_ahi)


an1_gain_loss <- function(x, nocc = 10, min_year_AHI = 1965,
  max_year_AHI = 1982, rm_outlier = TRUE, verbose = FALSE) {

  # NB
  x <- st_drop_geometry(x)
  nmsp <- names(x)
  # # lakes
  nlk <- nrow(x)
  pa_bsm <- grepl("PA_[0-9]{3}$", nmsp)
  pa_ahi <- grepl("PA_.*AHI$", nmsp)
  #
  sum(getIdsNum(nmsp[pa_bsm]) %in% getIdsNum(nmsp[pa_ahi]))
  occBSM <- apply(x[pa_bsm], 2, sum)
  occAHI <- apply(x[pa_ahi], 2, sum)

  ## Set the minimum number of lakes species selected occur
  set_bsm <- getIdsNum(names(occBSM)[occBSM > nocc])
  set_ahi <- getIdsNum(names(occAHI)[occAHI > nocc])
  # length(unique(set_bsm, set_ahi))
  id_sp <- set_bsm[which(set_bsm %in% set_ahi)]

  # outliers included (see fig S4)
  pa <- paste0("PA_", sprintf('%03d', id_sp))
  idgbsm_wo <- which(nmsp %in% pa)
  idgahi_wo <- which(nmsp %in% paste0(pa, '_AHI'))

  # Remove outliers (see Fig. S1):
  ## 1- Emerald shiner – Cool – 196
  ## 2- Trout-perch – Cold – 291 - species commonly used as bait!
  if (rm_outlier) id_sp <- id_sp[!id_sp %in% c(196, 291)]
  nm_bsm <- paste0("PA_", sprintf('%03d', id_sp))
  nm_ahi <- paste0(nm_bsm, "_AHI")
  ##
  idgbsm <- which(nmsp %in% nm_bsm)
  idgahi <- which(nmsp %in% nm_ahi)

  ## # species is based on the same set of species
  ## Identify columns recording AHI/BSM presence/absence data
  x <- x[x$Year_Updated_AHI >= min_year_AHI &
    x$Year_Updated_AHI<=max_year_AHI,]

  # Gain / Lost per lake
  df_gain_loss <- ec_temporal_betadiversity(x[, nm_ahi], x[, nm_bsm], methods = c("ra", "wi", "bc", "ja"), site_names = x$idLake)
  df_gain_loss$tot_ahi <- df_gain_loss[, 2] + df_gain_loss[, 4]
  df_gain_loss$tot_bsm <- df_gain_loss[, 3] + df_gain_loss[, 4]

  # Beta div
  df_beta_diff_ahi <- ec_betadiversity(x[, nm_ahi],
      methods = c("ra", "wi", "bc", "ja"))
  df_beta_diff_bsm <- ec_betadiversity(x[,nm_bsm],
      methods = c("ra", "wi", "bc", "ja"))

  if (verbose)
    cat(blue("Number of species included in the analysis:",
      length(nm_ahi), "\n"))

  list(
    sf_bsm_ahi = x,
    gain_loss = df_gain_loss,
    beta_AHI = df_beta_diff_ahi,
    beta_BSM = df_beta_diff_bsm,
    nm_ahi = nm_ahi,
    nm_bsm = nm_bsm
  )
}
