#' Draw Figure S4
#'
#' Create rarefaction curves, test for homogenization and draw the
#' three figures, each includes the four panels for one of the three different
#' time periods. Note that the figures are combined afterward with ImageMagick
#' <https://imagemagick.org/script/develop.php> (see Makefile included in the
#' GitHub repository of this package). Also, all the details to draw the panels
#' `figS4_panel` that is not exported.
#'
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#'
#' @export

figS4_homogenization_panels <- function(cex_bas = 4) {
  x <- an1_gain_loss(sf_bsm_ahi, min_year_AHI = 1965, max_year_AHI = 1971)
  png("output/fig1P1.png", width = 210, height = 60, units = "mm", res = 900)
  figS4_panel(x, cex_bas = cex_bas, addtitle = TRUE, ylim1 = c(0, 0.4),
    text_xlab = FALSE, legend = TRUE)
  dev.off()
  ##
  x <- an1_gain_loss(sf_bsm_ahi, min_year_AHI = 1972, max_year_AHI = 1976)
  png("output/fig1P2.png", width = 210, height = 60, units = "mm", res = 900)
  figS4_panel(x, cex_bas = cex_bas, addtitle = TRUE, ylim1 = c(0, 0.4),
    ylim2 = c(0, 0.27), st_let = 4, text_xlab = FALSE)
  dev.off()
  ##
  x <- an1_gain_loss(sf_bsm_ahi, min_year_AHI = 1977, max_year_AHI = 1982)
  png("output/fig1P3.png", width = 210, height = 60, units = "mm", res = 900)
  figS4_panel(x, cex_bas = cex_bas, addtitle = TRUE, ylim1 = c(0, 0.46),
    ylim2 = c(0, 0.27), st_let = 8)
  dev.off()
}



figS4_panel <- function(x, cex_bas = 1, lett = TRUE, addtitle = FALSE,
    ylim1 = c(0, .36), ylim2 = c(0, .25), st_let = 0, text_xlab = TRUE,
    legend = FALSE) {

  ##
  nl <- nrow(x[[2]])
  cex_txt <- cex_bas*3.2
  cex_let <- 2*cex_bas
  cex_lab <- 1.5*cex_bas
  seqb <- seq(0, 30, length.out = 11)
  seqc <- seq(0, 1, length.out = 11)

  ## Rarefaction curves
  rare_ahi <- ec_rarefaction(x[[1]][, x[[5]]], 20000) %>% apply(1, mean)
  rare_bsm <- ec_rarefaction(x[[1]][, x[[6]]], 20000) %>% apply(1, mean)

  ## Layout
  mat_lay <- rbind(c(4,1,2,3,5))
  layout(mat_lay, heights = c(1), widths = c(.1, 1, 1, 1, .8))


  par(lend = 1, mar =  c(3.2, 2, 2.5, .5), mgp = c(1.7, .7, 0),
    cex.lab = cex_lab, cex.axis = 1.6*cex_bas, fg = pal[1L],
    xaxs = "i", yaxs = "i")


  #### FIRST BAR PLOT
  cust_hist(seqb, x[[2]]$tot_ahi, x[[2]]$tot_bsm, ylim = ylim1,
    cex_bas = cex_bas, text_x = "")
  if (text_xlab)
    mtext(1, line = 1.7, text = "Species richness", cex = cex_lab)


  custom_ticks(mean(x[[2]]$tot_ahi), mean(x[[2]]$tot_bsm), pal)
  ## Annotations
  pval <- wilcox.test(x[[2]]$tot_ahi, x[[2]]$tot_bsm, paired = TRUE)$p.value
  text(11, ylim1[2]-.035, labels = TeX("$\\rightarrow}$"), cex = cex_txt,
    col = pal, pos = 2)
  mtot <- signif(mean(x[[2]]$tot_bsm) - mean(x[[2]]$tot_ahi), 3)
  if (mtot > 0) mtot <- paste0("+", mtot)
  text(8.4, ylim1[2]-.035, labels = mtot, cex = .7*cex_txt, col = pal,
      pos = 4)
  text(9, ylim1[2]-0.005, signifSymbols(pval), col = pal[1L], cex = cex_txt)
  if (lett)
    mtext(3, line = -1, adj = .04, text = LETTERS[st_let+1], cex = cex_let)


  #### SECOND BAR PLOT
  cust_hist(seqc, x[[3]]$ja, x[[4]]$ja, ylim = ylim2, di = .006, text_x = "",
    cex_bas = cex_bas)
  if (text_xlab)
    mtext(1, line = 1.7, text = "Similarity (Jaccard index)", cex = cex_lab)

  #
  mbahi <- mean(x[[3]]$ja[!is.nan(x[[3]]$ja)])
  mbbsm <- mean(x[[4]]$ja)
  custom_ticks(mbahi, mbbsm, pal)
  ## Annotations
  pval <- wilcox.test(x[[3]]$ja, x[[4]]$ja, paired = TRUE)$p.value
  mtot <- signif(mbbsm - mbahi, 3)
  if (mtot > 0) mtot <- paste0("+", mtot)
  text(.32, ylim2[2] - .018, labels = mtot, cex = .7*cex_txt, col = pal,
    pos = 4)
  text(.30, ylim2[2] - .018, labels = TeX("$\\rightarrow$"), cex = cex_txt,
  col = pal[1L])
  text(.34, ylim2[2], signifSymbols(pval), col = pal[1L], cex = cex_txt)
  if (lett)
    mtext(3, line = -1, adj = .04, text = LETTERS[st_let+2], cex = cex_let)
  if (addtitle) {
    rgy <- range(x[[1]]$Year_Updated_AHI)
    mtext(3, line = 1.1, text =
      paste0("Period ", rgy[1], "-", rgy[2]),  cex = cex_lab)
  }

  ## RAREFACTION CURVES
  par(xaxs = "r", yaxs = "r", mar = c(3.6, 2, 2.5, .5))

  plot0(0:nl, c(0, rare_ahi+1))
  lines(0:nl, c(0, rare_ahi), col = pal[1L], lwd = 2)
  lines(0:nl, c(0, rare_bsm), col = pal[2L], lwd = 2, lty = 2)
  axis(1, las = 1)
  axis(2, las = 1)
  if (lett)
    mtext(3, line = -1, adj = .04, text = LETTERS[st_let+3],
      cex = cex_let)
  box2(1:2, lwd = 1.2)
  mtext(2, line = 1.7, text = "Species richness", cex = cex_lab)

  if (text_xlab)
    mtext(1, line = 1.7, text = "Cumulative number of lakes", cex = cex_lab)

  if (legend) {
    points(rep(100, 2), c(5, 10), pch = 15, col = pal, cex = .5*cex_bas)
    text(rep(105, 2), c(5, 10) - c(.4, .4), labels = c("Historical", "Recent"), pos = 4, cex = cex_lab*1.1)
  }

  ##
  par(mar =  c(3.2, .5, 2.5, .5))
  plot0()
  mtext(2, line = -1.3, text = "Frequency", cex = cex_lab)

  ## MAP
  par(mar =  c(3, .6, 2.5, .5))
  plot(st_geometry(gadm_ontario), col = "grey90", axes = FALSE, ann = FALSE, lwd = .8)
  plot(st_geometry(sf_bsm_ahi[sf_bsm_ahi$idLake %in% x[[1]]$idLake,]),
    pch = 19, add = TRUE, col = "grey10", cex = .4)
  mtext(3, line = 0, text = paste0(nrow(x[[1]]), " lakes"), cex = cex_lab)
  if (lett)
    mtext(3, line = -1, adj = .04, text = LETTERS[st_let+4], cex = cex_let)
}
