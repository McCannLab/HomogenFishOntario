#' Draw Figure 2
#'
#' Perform rarefaction curves, test for homogenization and draw Figure 2 - homogenization and rarefaction curves.
#'
#' @param x output of [an1_gain_loss()].
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#' @param add_let logical. Should a letter for the panel be added? Default is set to `TRUE`.
#' @param addtitle a logical. Should a title be added. Default is set to `FALSE`?
#' @param asfinal logical. Should the figure be formatted for publication and save as a separate file? Note that in this case `cex_bas` is ignored.
#'
#' @export
#'
#' @examples
#' fig2_homogenization(an1_gain_loss(sf_bsm_ahi))

fig2_homogenization <- function(x, cex_bas = 1, add_let = TRUE,
    addtitle = FALSE, asfinal = TRUE) {

  ## Rarefaction curves (from ecoocc)
  nrep <- 20000
  rare_ahi <- ec_rarefaction(x[[1]][, x[[5]]], nrep) %>% apply(1, mean)
  rare_bsm <- ec_rarefaction(x[[1]][, x[[6]]], nrep) %>% apply(1, mean)

  if (asfinal) {
    checkOutputFolder()
    cex_bas = 6.5
    png("output/fig2_homogenization.png", width = 180, height = 65,
      units = "mm", res = 900)
  }
  mat_lay <- rbind(c(4,1,2,3))
  layout(mat_lay, heights = c(1), widths = c(.12, 1, 1, 1.1))
  nl <- nrow(x[[2]])
  cex_tx <- cex_bas*1.5
  lwd_b <- 5.1
  seqb <- seq(0, 30, length.out = 11)
  seqc <- seq(0, 1, length.out = 11)

  par(lend = 1, mar =  c(3.5, 2.1, 1, .5), mgp = c(2, .6, 0),
    cex.lab = cex_bas, cex.axis = cex_bas, fg = pal[1L],
    xaxs = "i", yaxs = "i")

  #### 1- FIRST BAR PLOT
  cust_hist(seqb, x[[2]]$tot_ahi, x[[2]]$tot_bsm, ylim = c(0, .36),
  cex_bas = cex_bas)
  # species richness
  custom_ticks(mean(x[[2]]$tot_ahi), mean(x[[2]]$tot_bsm), pal)

  ## Annotations
  pval <- wilcox.test(x[[2]]$tot_ahi, x[[2]]$tot_bsm, paired = TRUE)$p.value
  text(11.6, .335, labels = TeX("$\\rightarrow}$"), cex = cex_bas*2.8,
    col = pal, pos = 2)
  mtot <- signif(mean(x[[2]]$tot_bsm) - mean(x[[2]]$tot_ahi), 3)
  if (mtot > 0) mtot <- paste0("+", mtot)
  text(8.4, .336, labels = mtot, cex = cex_bas*1.2, col = pal, pos = 4)
  text(9, .36, signifSymbols(pval), col = pal[1L], cex = cex_bas*1.6)
  add_letter(add_let, cex = cex_bas*cex_let)

  #### 2- SECOND BAR PLOT
  cust_hist(seqc, x[[3]]$ja, x[[4]]$ja, ylim = c(0, .25), di = .006,
    text_x = "Similarity (Jaccard index)", cex_bas = cex_bas)

  #
  mbahi <- mean(x[[3]]$ja[!is.nan(x[[3]]$ja)])
  mbbsm <- mean(x[[4]]$ja)
  custom_ticks(mbahi, mbbsm, pal)
  ## Annotations
  pval <- wilcox.test(x[[3]]$ja, x[[4]]$ja, paired = TRUE)$p.value
  mtot <- signif(mbbsm - mbahi, 3)
  if (mtot > 0) mtot <- paste0("+", mtot)
  text(.33, .234, labels = mtot, cex = cex_bas*1.2, col = pal, pos = 4)
  text(.31, .235, labels = TeX("$\\rightarrow$"), cex = cex_bas*2.8,
    col = pal[1L])
  text(.325, .25, signifSymbols(pval), col = pal[1L], cex = cex_bas*1.6)
  add_letter(add_let, "B", cex_bas*cex_let)
  if (addtitle) {
    rgy <- range(x$Year_Updated_AHI)
    mtext(3, line = 1, text =
      paste0("Period ", rgy[1], "-", rgy[2], "  ", nrow(x), "lakes"))
  }

  ## 3- Rarefaction curves
  par(xaxs = "r", yaxs = "r", mar = c(3.5, 3, 1, 1))

  plot0(seq(0, nl), c(0, rare_ahi+1))
  lines(seq(0, nl), c(0, rare_ahi), col = pal[1L], lwd = 2.6)
  lines(seq(0, nl), c(0, rare_bsm), col = pal[2L], lwd = 2.6, lty=2)
  axis(1, las = 1)
  axis(2, las = 1)
  add_letter(add_let, "C", cex_bas*cex_let)
  box2(1:2, lwd = 2)
  mtext(1, line = 2, text = "Cumulative number of lakes", cex = cex_bas)
  mtext(2, line = 2, text = "Species richness", cex = cex_bas)

  ## legend
  points(rep(300, 2), c(6, 9), pch = 15, col = pal, cex = 2)
  text(rep(320, 2), c(6, 9) - c(.4, .4), labels = c("Historical", "Recent"), pos = 4, cex = cex_tx)

  ## yaxis
  par(mar = c(3.5, 0, 0, 0))
  plot0()
  text(0, 0, "Frequency", srt = 90, cex = cex_tx)

  if (asfinal) dev.off()
}

add_letter <- function(add_let, txt = "A", cex = 1) {
    if (add_let)
      mtext(3, line = -1, adj = .04, text = txt, cex = cex)
}



cust_hist <- function(sqx, y1, y2, xlim = range(sqx), di = .1, ylim = c(0,1), lwd_b = 3, text_x = "Species richness", cex_bas = 1) {
  plot0(xlim + diff(xlim)*0.02*c(-1, 1), ylim + diff(ylim)*0.02*c(-1, 1))
  vala <- myfreq(hist(y1, sqx, plot = FALSE)$counts)
  valb <- myfreq(hist(y2, sqx, plot = FALSE)$counts)
  ##
  envelop(meanAlong(sqx, 2)-di, upper = vala, col =  paste0(col2Hex(pal[1]), "44"), border = pal[1L])
  envelop(meanAlong(sqx, 2)+di, upper = valb, col =  paste0(col2Hex(pal[2]), "44"), border = pal[2L])

  #
  lines(meanAlong(sqx, 2), vala, col = pal[1L], lwd = 2, lend = 2)
  lines(meanAlong(sqx, 2), valb, col = pal[2L], lwd = 2, lend = 2)

  points(meanAlong(sqx, 2)-di, vala, pch = 19, lwd = lwd_b, col = pal[1L])
  points(meanAlong(sqx, 2)+di, valb, pch = 19, lwd = lwd_b, col = pal[2L])
  #
  box2(1:2, lwd = 1.2)
  axis(1L, at = sqx, labels = sqx, las = 1)
  axis(2L, las = 1)
  mtext(1, line = 2, text = text_x, cex = cex_bas)
}
