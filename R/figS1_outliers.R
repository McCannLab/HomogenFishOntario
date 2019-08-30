#' Draw Figure S1
#'
#' @param x output of [an1_gain_loss()].
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#'
#' @export

figS1_outliers <- function(x, cex_bas = 5) {

  df <- x[[1]]
  ahi <- x[[5]]
  bsm <- x[[6]]

  ##
  occ_AHI <- apply(df[, ahi], 2, sum)
  occ_BSM <- apply(df[, bsm], 2, sum)

  ##
  abs_chg <- apply(df[, bsm], 2, sum) - apply(df[, ahi], 2, sum)
  rel_chg <- abs_chg/apply(df[, ahi], 2, sum)
  ids <- rel_chg > 2.5
  outlier <- names(rel_chg)[ids] %>% getIdsNum %>% getNamesSp

  tmp <- names(rel_chg) %>% getIdsNum %>% as.data.frame %>%
    cbind(occ_BSM - occ_AHI, (occ_BSM - occ_AHI)/occ_AHI)
  names(tmp) <- c("idOnt", "absol", "rela")
  tmp %<>% merge(df_species_info[, c("idOnt", "scientificName")], by = "idOnt")
  tmp %<>% merge(df_fished[, c("scientificName", "GameFish")],
    by = "scientificName")
  ## 1 -> fished; 2 -> non fished
  tmp["GameFish"] <- tmp["GameFish"] + 2

  val <- occ_BSM - occ_AHI

  checkOutputFolder()
  png("output/figS1_outliers.png", res = 900, units = "mm",
    width = 80, height = 65)
  par(mar = c(2.5, 2.5, 1, 1), bty = "l", las = 1, mgp = c(1.4, .5, 0),
   cex.axis = cex_bas, cex.lab = 1.25*cex_bas, tcl = -.4)
  ##
  plot(abs_chg, rel_chg, type = "n", xlab = TeX("Absolute $\\Delta$ occupancy"), ylab = TeX("Relative $\\Delta$ occupancy"))
  abline(v = 0, h = 0, lty = 2, col = "grey75")
  ##
  points(val[ids], val[ids]/occ_AHI[ids], pch = 19, col = myred, cex = .8)
  text(abs_chg[ids], rel_chg[ids], labels = outlier, pos = 2, col = myred,
      cex = cex_bas)
  points(tmp$absol, tmp$rela, pch = 19, col = pal[tmp[, c("GameFish")]])
  ##
  dev.off()
}
