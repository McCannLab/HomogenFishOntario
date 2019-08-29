#' Draw Figure S6
#'
#' @param x output of [an2_occurrence_species()].
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#'
#' @export


figS6_northward <- function(x, cex_bas = 4.5) {

  vc_gf <- (x$GameFish == 0) + 1
  val <- x$gain - x$lost

  ## Not the fastest way, just the easiest way to remember
  lat_AHI <- lat_BSM <- val * 0
  lat_AHIr <- lat_BSMr <- val * 0
  coords <- st_coordinates(sf_bsm_ahi)
  tmp <- st_drop_geometry(sf_bsm_ahi)
  for (i in seq_len(nrow(x))) {
    nmB <- paste0("PA_", sprintf("%03d", x$idOnt[i]))
    nmA <- paste0(nmB, "_AHI")
    ##
    valA <- tmp[, nmA] * coords[, 2]
    valB <- tmp[, nmB] * coords[, 2]
    ##
    lat_AHI[i] <- sum(valA)/sum(tmp[, nmA])
    lat_BSM[i] <- sum(valB)/sum(tmp[, nmB])
    ##
    lat_AHIr[i] <- quantile(valA[valA != 0], 0.8)
    lat_BSMr[i] <- quantile(valB[valB != 0], 0.8)
  }

  checkOutputFolder()
  png("output/figS6_northward.png", height = 80, width = 90, units = "mm", res = 900)

  idt <- as.numeric(as.factor(x$thermalGuild_bsm))
  vc_pch <- c(21, 22, 24)
  myblue <- gpuPalette("insileco")[2]
  val2 <- lat_BSMr - lat_AHIr
  vc_pos <- rep(3, nrow(x))
  vc_pos[x$id_graph %in% c(4, 8, 15, 27, 32, 34, 37:38)] <- 1
  vc_pos[x$id_graph %in% c(1)] <- 2
  vc_pos[x$id_graph %in% c(17, 18, 20)] <- 4
  palh <- pal[vc_gf]
  palh[x$id_graph %in% c(9, 11, 16, 20)] <- myred
  palh[x$id_graph %in% c(12, 14, 18, 22, 30, 33)] <- myblue
  ##
  par(xaxs = "i", yaxs = "i", las = 1, cex.lab = cex_bas*1.25,
    cex.axis =cex_bas, mar = c(3, 3, 1, 1), mgp = c(2, .6, 0))
  plot0(c(-109, 106), c(-2, 3.4))
  abline(h = 0, v = 0, lty = 2, col = "grey85")
  points(val, val2, bg = palh, col = NA, cex = 0.8, pch = vc_pch[idt])
  text(val, val2, labels = x$id_graph, col = palh, pos = vc_pos, cex = 3.24, offset = 0.24)
  # mtext("A", 3, adj=.02, line = -1.1, cex = 9)
  #
  points(rep(76.5, 3), c(3.2, 2.9, 2.6), pch = vc_pch, cex = .9)
  text(rep(77, 3), c(3.2, 2.9, 2.6), labels = c("Cold", "Cool", "Warm"),
   pos = 4, cex = 4.5)
  #
  axis(1)
  axis(2)
  box2(1:2)
  title(ylab = c("Change in mean latitude (degree)", "of the most 20% northern lakes of occurrence"))
  par(mgp = c(1.3, 0, 0))
  title(xlab = "Change in overall occupancy")

  dev.off()
}
