#' Contribution to beta diversity metrics
#'
#' Contribution to beta (spatial and temporal) of all species.
#'
#' @param x output of [an1_gain_loss()].
#' @param y output of [an2_occurrence_species()].
#'
#' @return
#' A list of five elements:
#' * `contrb`: contribution to beta diversity metrics;
#' * `fsh_ahi`: number of baitfish species in AHI for every lake;
#' * `fsh_bsm`: number of baitfish species in BSM for every lake;
#' * `nofsh_ahi`: number of baitfish species in AHI for every lake;
#' * `nofsh_bsm`: number of baitfish species in BSM for every lake.
#'
#' @export
#'
#' @examples
#' res0 <- an1_gain_loss(sf_bsm_ahi)
#' res1 <- an2_occurrence_species(res0)
#' res <- an4_contributions_beta(res0, res1)

an4_contributions_beta <- function(x, y) {

  ## x[[5]] and x[[6]] sorted according to idOnt
  out <- y[order(y$idOnt),]
  out$ctrb_spasim <- out$ctrb_tmpsim <- NA

  mat_fsh <- matrix((out$GameFish != 0) * 1, ncol = 1)
  mat_nofsh <- matrix((out$GameFish == 0) * 1, ncol = 1)
  #
  fsh_ahi <- as.matrix(x[[1]][, x[[5]]]) %*% mat_fsh
  fsh_bsm <- as.matrix(x[[1]][, x[[6]]]) %*% mat_fsh
  ##
  nofsh_ahi <- as.matrix(x[[1]][, x[[5]]]) %*% mat_nofsh
  nofsh_bsm <- as.matrix(x[[1]][, x[[6]]]) %*% mat_nofsh

  ######## Contribution
  for (i in seq_nrow(out)) {
    # remove species
    tmp <- out$idOnt[-i]
    matahi <- x[[1]][ ,paste0("PA_", sprintf("%03d", tmp), "_AHI")]
    matbsm <- x[[1]][ ,paste0("PA_", sprintf("%03d", tmp))]
    # spatial similarity analysis
    tmp_bd_ahi <- ec_betadiversity(matahi, methods = "ja")
    tmp_bd_bsm <- ec_betadiversity(matbsm, methods = "ja")
    id <- is.nan(tmp_bd_ahi$ja) | is.nan(tmp_bd_bsm$ja)
    out$ctrb_spasim[i] <- mean(tmp_bd_ahi$ja[!id]) - mean(tmp_bd_bsm$ja[!id])
    # temporal similarity analysis
    tmp_tbd <- ec_temporal_betadiversity(matahi, matbsm, methods = "ja")
    out$ctrb_tmpsim[i] <- mean(tmp_tbd$ja)
  }

  list(
    contrib = out,
    fsh_ahi = fsh_ahi,
    fsh_bsm = fsh_bsm,
    nofsh_ahi = nofsh_ahi,
    nofsh_bsm = nofsh_bsm
  )
}
