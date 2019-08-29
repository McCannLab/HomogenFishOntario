#' Occurrence analysis
#'
#' Occurrence analysis for individual species.
#'
#' @param x output of [an1_gain_loss()].
#'
#' @return
#' A data frame with info about species
#'
#' @export
# res <- an2_occurrence_species(an1_gain_loss(sf_bsm_ahi))

an2_occurrence_species <- function(x) {

  ## Occurrence of selected species
  occAHI <- apply(x[[1]][x[[5]]], 2, sum)
  occBSM <- apply(x[[1]][x[[6]]], 2, sum)

  # tmp data frame
  tmp_occ <- data.frame(idOnt = getIdsNum(names(occBSM)),
    occAHI = occAHI, occBSM = occBSM)

  # gain / lost per species
  df_gain_loss_sp <- ec_temporal_betadiversity(t(x[[1]][x[[5]]]),
    t(x[[1]][x[[6]]]), methods = c("ra", "wi", "bc", "ja"),
    site_names = getIdsNum(x[[5]]))
  names(df_gain_loss_sp)[1:3] <- c("idOnt", "lost", "gain")

  ##
  tmp_occ <- merge(tmp_occ, df_gain_loss_sp[, 1:4], by = "idOnt") %>%
    merge(df_species_info[, c("idOnt", "order", "family",
     "species", "commonName", "GameFish", "baitOntario", "FTP", "ULIT",
     "log10BS_bsm", "thermalGuild_bsm", "meanDepth_bsm",
     "Updated_Trophic_Level", "LongevityWild", "stocked")],  by = "idOnt")

  tmp_occ <- tmp_occ[order(tmp_occ$occAHI), ]
  tmp_occ$id_graph <- rev(seq_nrow(tmp_occ))

  tmp_occ
}
