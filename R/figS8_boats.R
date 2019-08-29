#' Draw Figure S8
#'
#' @param x output of [an1_gain_loss()].
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#'
#' @export

figS8_boats <- function(x, cex_bas = 5) {

  names(x[[1]])[1] <- "site"
  gl_boat <- merge(x[[1]][c("site", "Angling_Pressure_Min")], x[[2]])

  checkOutputFolder()
  png("output/figS8_boats_pressure.png", res = 900,
    width = 80, height = 75, units = "mm")
    val <- gl_boat[,"site_t2_only"]-gl_boat[,"site_t1_only"]
    boa <- log(gl_boat[,"Angling_Pressure_Min"] + 1)
    # there are 15 NA => 464-15 = 449
    par(bty = "l", las = 1, mar = c(2.9, 2.6, 1, 1), cex.lab = 1.25*cex_bas,
      cex.axis = cex_bas, mgp  = c(1.6, .7, 0))
    plot(boa, val, pch = 19, cex = .5, xlab = "Minimal angling pressure", ylab =  "Gains - Losses")
    # abline(h = mean(val), lty = 2, lwd = 1.2)
    lm(val~boa) %>% abline(col = pal[2], lwd = 1.6)
  dev.off()
}
