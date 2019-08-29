#' Draw Figure Z1
#'
#' @param x output of [an1_gain_loss()].
#' @param y output of [an4_contributions_beta()].
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#'
#' @details
#' Figure Z1 was found in a previous version of the SI.

figZ1_fnf_regressions <- function(x, y, cex_bas = 6) {

  gain <- x[[2]]$site_t1_only
  lost <- x[[2]]$site_t2_only

  palgg <- colorRampPalette(c("grey90", "grey10"))(20)

  checkOutputFolder()
  png("output/figZ1_regression.png", width = 120, height = 120, res = 900, units = "mm")
  layout(rbind(1:2, 3:4, 5), heights = c(1, 1, 0.3))
  par(las = 1, bty = "l", cex.lab = 1.25*cex_bas, cex.axis = cex_bas,
    mgp = c(1.7, .7, 0))
  ##
  par(mar = c(3, 3, 1, 0.25))
  densPlot(x = y$nofsh_ahi, y = gain, xlab = "", ylab = "Gain", pal = palgg)
  par(mar = c(3, 3, 1, 1))
  densPlot(y$fsh_ahi, gain, xlab = "", pal = palgg)
  ##
  par(mar = c(3, 3, 1, 0.25))
  densPlot(y$nofsh_ahi, lost, xlab = "Number of baitfish species in AHI",
    ylab = "Loss", pal = palgg)
  par(mar = c(3, 3, 1, 1))
  densPlot(y$fsh_ahi, lost, xlab = "Number of gamefish species in AHI",
    ylab = "", pal = palgg)
  ##
  par(mar = c(2.8, 4, .5, 4), mgp = c(1.4, 0.1, 0))
  image(cbind(1:20), col = palgg, axes = FALSE)
  lab <- c(paste0(1:19), "20+")
  axis(1, at = seq(0, 1, length = 20), labels = lab, lwd = 0)
  title(xlab = "Number of lakes")
  ##
  dev.off()
}



densPlot <- function(x, y, mn = 1, mx = 20, xlab = "", ylab = "", pal) {
  plot0(x, y)
  val <- as.matrix(table(x, y))
  nua <- scaleWithin(val, length(pal), mn, mx)
  for (i in seq_len(nrow(val))) {
    for (j in seq_len(ncol(val))) {
      if (val[i, j] > 0)
        points(as.numeric(rownames(val))[i], as.numeric(colnames(val))[j], col = pal[nua[i, j]], pch = 19, cex = 1.4)
    }
  }
  box2(1:2)
  axis(1)
  axis(2)
  title(xlab = xlab, ylab = ylab)
}
