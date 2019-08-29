#' Draw Figure S5
#'
#' Perform model II linear regression and drawFigure S5.
#'
#' @param x output of [an1_gain_loss()].
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#'
#' @export

figS5_saturation <- function(x, cex_bas = 6) {

  lm(x[[2]]$tot_bsm ~ x[[2]]$tot_ahi) %>% summary
  id <- x[[2]]$tot_ahi <= quantile(x[[2]]$tot_ahi, .5)
  mean(x[[2]]$tot_bsm[id] - x[[2]]$tot_ahi[id])
  mean(x[[2]]$tot_bsm[!id] - x[[2]]$tot_ahi[!id])

  ## LOST
  checkOutputFolder()
  png("output/figS4_saturation.png", width = 180, height = 90,
    units = "mm", res = 900)
  layout(rbind(c(3,0,6,0), c(1,2,4,5)), widths = c(1, .34, 1, .34), heights = c(.34, 1))
  ##
  par(las = 1, mar = c(3.5, 4, 0, 0), cex.lab = 1.25*cex_bas, cex.axis = cex_bas, mgp = c(2, .7, 0), bty = "l", xaxs = "i", yaxs = "i")
  ##
  plot(c(-.8,25), c(-.8,25), type = "n", xlab = "Historical species richness", ylab = "Contemporary species richness")
  points(x[[2]]$tot_ahi, x[[2]]$tot_bsm, pch = 19, col = "#66666677")
  abline(a = 0, b = 1, lty = 2)
  lmod2 <- lmodel2(x[[2]]$tot_bsm ~ x[[2]]$tot_ahi)
  abline(a = lmod2$regression.results$Intercept[2],
    b = lmod2$regression.results$Slope[2],  col = myred)

  # 0.6638243   0.8404635
  abline(lmodel2(x[[2]]$tot_bsm ~ x[[2]]$tot_ahi))
  mtext("A", 3, adj = 0.02, cex = 3.6*cex_let, line = -2)
  ##
  plotdens(x[[2]]$tot_ahi, rgv = c(-.8,25))
  title(xlab = "Density")
  ##
  plotdens(x[[2]]$tot_ahi, verti = FALSE, rgv = c(-.8,25))
  title(ylab = "Density")


  ## RICHNESS - AREA
  par(mar = c(3.5, 4, 0, 0), xaxs = "i", yaxs = "i")
  valtot <- apply(x[[2]][,2:4], 1, sum)
  val <- 100*(x[[2]]$site_t2_only - x[[2]]$site_t1_only)/valtot
  plot(log10(x[[1]]$Area_km2), val, xlim = c(-.8, 2.8), ylim = c(-55, 102), xlab = "log10(Area)", ylab = "Relative gain of species",
  pch = 19, col = "#66666677")
  abline(h = 0, lty = 2)
  lm(val~log(x[[1]]$Area_km2)) %>% abline(col = myred)
  mtext("B", 3, adj = 0.02, cex = 3.6*cex_let, line = -2)
  #
  ##
  plotdens(val, rgv = c(-55, 102))
  abline(h = 0, lty = 2)
  title(xlab = "Density")
  ##
  plotdens(log10(x[[1]]$Area_km2), verti = FALSE, rgv = c(-.8, 2.8))

  ##
  dev.off()
}



plotdens <- function(val, rgv = range(val), verti = TRUE) {
  den <- density(val)
  if (verti) {
    par(mar = c(3.5, 1, 0, 1), xaxs = "i", yaxs = "i")
    plot0(range(den$y)*c(0,1.02), rgv)
    envelop(den$y, den$x, col = "#66666677", border = NA)
    lines(den$y, den$x, lwd = 1.6)
    lines(c(0, max(den$y)), rep(mean(val),2), col = myred)
    axis(1)
  } else {
    par(mar = c(1, 4, 1, 0))
    plot0(rgv, range(den$y)*c(0,1.02), xaxs = "i", yaxs = "i")
    lines(den$x, den$y, lwd = 1.6)
    envelop(den$x, den$y, col = "#66666677", border = NA)
    lines(rep(mean(val),2), c(0, max(den$y)), col = myred)
    axis(2)
  }
}
