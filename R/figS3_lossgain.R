#' Draw Figure S3
#'
#' @param x output of [an1_gain_loss()].
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#'
#' @export

figS3_lossgain <- function(x, cex_bas = 6) {

  lost <- x[[2]]$site_t1_only
  gain <- x[[2]]$site_t2_only
  bil <- gain - lost

  ## Colors palettes
  pal0 <- c("#0b2d40", "#045579", "#9eb625", "#f5de81")
  pal <- colorRampPalette(pal0)(13)
  pal2 <- colorRampPalette(pal0)(17)
  pal3 <- c(
    colorRampPalette(pal0[1:2])(11),
    colorRampPalette(pal0[3:4])(16)
  )

  checkOutputFolder()
  png("output/figS5_lossgain.png", res = 900, width = 180, height = 90,
   units = "mm")
  ##
  mat_lay <- matrix(1:6, 2, byrow = TRUE)
  layout(mat_lay, heights = c(1, .18))
  par(mar = c(0,0,0,0))

  ls_col <- list(pal[lost+1], pal2[gain+1], pal3[bil - min(bil) +1])
  ##
  for (i in 1:3) {
    plot(st_geometry(gadm_ontario), col = "grey80", border = "grey10",
      lwd = 1.1)
    plot(st_geometry(sf_bsm_ahi[sf_bsm_ahi$idLake %in% x[[1]]$idLake,]),
    col = ls_col[[i]], add = TRUE, pch = 19, cex = .5)
  }

  ##
  par(mar = c(3.2, 1, .2, 1))
  image(matrix(1:13, 13), col = pal, axes = FALSE)
  axis(1, at = seq(0, 1, length.out = 7), labels = 0:6*2, lwd = 0, lwd.ticks = .5, tcl = -.25, mgp = c(2.2, .1, 0), cex.axis = cex_bas)
  mtext("Losses", 1, cex = cex_bas, line = 1.2)
  ##
  image(matrix(1:17, 17), col = pal2, axes = FALSE)
  axis(1, at = seq(0, 1, length.out = 9), labels = 0:8*2, lwd = 0, lwd.ticks = .5, tcl = -.25, mgp = c(2.2, .1, 0), cex.axis = cex_bas)
  mtext("Gains", 1, cex = cex_bas, line = 1.32)
  ##
  image(matrix(1:27, 27), col = pal2, axes = FALSE)
  axis(1, at = seq(0, 1, length.out = 14), labels = (0:13*2 - 10), lwd = 0, lwd.ticks = .5, tcl = -.25, mgp = c(2.2, .1, 0), cex.axis = cex_bas)
  mtext("Gains - Losses", 1, cex = cex_bas, line = 1.32)

  dev.off()
}
