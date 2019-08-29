#' Draw Figure 6
#'
#' Use the output of [an4_contributions_beta()] and draw Figure 6.
#'
#' @param x output of [an4_contributions_beta()].
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#'
#' @export
#'
#' @examples
#' res0 <- an1_gain_loss(sf_bsm_ahi)
#' res1 <- an2_occurrence_species(res0)
#' res2 <- an4_contributions_beta(res0, res1)
#' fig6_contributions_beta(res2)

fig6_contributions_beta <- function(x, cex_bas = 6) {

  nsp <- nrow(x[[1]])
  val_x <- scale(x[[1]]$ctrb_tmpsim)
  val_y <- scale(x[[1]]$ctrb_spasim)
  idf <- (x[[1]]$GameFish == 0) + 1
  idt <- as.numeric(as.factor(x[[1]]$thermalGuild_bsm))
  vc_pch <- c(21, 22, 24)

  checkOutputFolder()
  png("output/fig6_contributions.png", width = 180, height = 110, units = "mm",
   res = 900)
  # letters positions
  vc_pos <- rep(3, nsp)
  vc_pos[x[[1]]$id_graph %in% c(2, 11, 25, 31, 33, 36, 37, 39)] <- 1
  vc_pos[x[[1]]$id_graph %in% c(12, 16, 20, 23, 30)] <- 4
  vc_pos[x[[1]]$id_graph %in% c(1, 11, 14:15, 17:18, 24, 27, 29, 35)] <- 2
  vc_lab <- x[[1]]$id_graph
  id <- which(vc_lab %in% c(39, 37))
  val_x_txt <- -val_x
  val_x_txt[id] <- val_x_txt[id] + c(0.025, -0.025)  #c(0.018, -0.018)
  ####
  layout(cbind(1, 4, c(2, 3)), widths = c(1, .1, .55))

  ### LEFT PANEL
  par(las = 1, fg = pal[1L], mar = c(3.25, 3, 1, 1), mgp = c(1.8, .7, 0),
    cex.axis = 1.2*cex_bas, cex.lab = 1.5*cex_bas, xaxs = "i", yaxs = "i")
  plot0(ext_rg(-val_x, 2), ext_rg(val_y, 2))
  abline(h = 0, v = 0, lty = 2, lwd = 0.8, col = "grey65")
  points(-val_x, val_y, col = pal[idf], pch = vc_pch[idt], bg = pal[idf], cex = 1.05)
  text(val_x_txt, val_y, labels = vc_lab, col = pal[idf], pos = vc_pos, offset = 0.34, cex = cex_bas)
  axis(1)
  axis(2)
  box()
  # HOME MADE CAPTION
  points(-0.52 + c(2.55, 2.55), c(-1.15, -1.4), pch = 19, col = pal, cex = 1.4)
  text(-0.52 + c(2.55, 2.55), c(-1.15, -1.4), labels = c("Gamefish", "Baitfish"), pos = 4, cex = 1.3 * cex_bas)
  points(rep(2.85, 3), c(-0.9, -1.15, -1.4), pch = vc_pch, cex = 1.4)
  text(rep(2.89, 3), c(-0.9, -1.15, -1.4), labels = c("Cold", "Cool", "Warm"), pos = 4, cex = 1.3 * cex_bas)
  #
  title(ylab = "Contribution to changes in spatial similarity")
  par(xaxs = "r", yaxs = "r", lend = 1)
  title(xlab = "Contribution to temporal similarity")
  mtext("A", 3, at = -1.25, cex = 2*cex_bas, line = -1.4)

  ##
  par(mar = c(3.25, .25, 1, 0.5), xaxs = "i", yaxs = "i")
  cust_hist(seq(0, 16, length = 9), x[[2]], x[[3]], ylim = c(0, 0.35), di = 0.1, text_x = "")
  pval <- wilcox.test(x[[2]], x[[3]], paired = TRUE)$p.value
  mtot <- round(mean(x[[3]]) - mean(x[[2]]), 3)
  if (mtot > 0)
    mtot <- paste0("+", mtot)
  text(6.7, 0.324, labels = mtot, cex = cex_bas * 1.2, col = pal, pos = 4)
  text(6.5, 0.325, labels = TeX("$\\rightarrow}$"), cex = cex_bas * 3, col = pal[1L])
  text(6.45, 0.345, signifSymbols(pval), col = pal[1L], cex = cex_bas * 1.5)
  mtext("B", 3, at = 15, cex = 2*cex_bas, line = -1.4)
  title(xlab = "Number of gamefish species")
  # title(ylab = "Frequency")

  ##
  par(mar = c(3.25, .25, 1, 1.5))
  cust_hist(seq(0, 16, length = 9), x[[4]], x[[5]], ylim = c(0, 0.44), di = 0.1, text_x = "")
  pval <- wilcox.test(x[[4]], x[[5]], paired = TRUE)$p.value
  mtot <- round(mean(x[[5]]) - mean(x[[4]]), 3)
  if (mtot > 0)
    mtot <- paste0("+", mtot)
  text(2.7, 0.404, labels = mtot, cex = cex_bas * 1.2, col = pal, pos = 4)
  text(2.5, 0.405, labels = TeX("$\\leftarrow}$"), cex = cex_bas * 3, col = pal[1L])
  text(3, 0.438, signifSymbols(pval), col = pal[1L], cex = cex_bas * 1.5)
  ##
  title(xlab = "Number of baitfish species")
  mtext(3, at = 15, 2*cex_bas, line = -1.2, text = "C")
  ##
  mtext("C", 3, at = 15, cex = 2*cex_bas, line = -1.4)
  points(c(11.7, 11.7), c(0.31, 0.35), pch = 15, col = pal, cex = 1.5)
  text(c(12, 12), c(0.307, 0.346), labels = c("Historical", "Recent"), pos = 4, cex = 1.5 * cex_bas)

  ##
  plot0()
  text(-.2, 0, "Frequency", cex = 1.5*cex_bas, srt = 90)

  ##
  dev.off()
}
