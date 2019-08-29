#' Draw Figure S7
#'
#' @param x output of [an2_occurrence_species()].
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#'
#' @export

figS7_occupancy_trends <- function(x, cex_bas = 5) {

  vc_gf <- (x$GameFish == 0) + 1
  val <- x$gain - x$lost
  #
  vc_pos <- rep(1, nrow(x))
  vc_pos[x$id_graph %in% c(1, 4, 9, 12, 22, 24:25, 32:33, 37:38)] <- 3
  vc_pos[x$id_graph %in% c(3, 5, 7, 26, 39)] <- 4
  vc_pos[x$id_graph %in% c(13, 20, 22, 27, 35)] <- 2

  checkOutputFolder()
  png("output/figS7_occupancy_trends.png", height = 70, width = 180,
    units = "mm", res = 600)

  layout(cbind(4, 1, 2, 3), widths = c(0.1, 1, 1, 1))

  par(mar = c(3.8, 1.6, 1.2, 1), bty = "l", cex.axis = cex_bas, cex.lab = cex_bas * cex_txt, las = 1, mgp = c(2, .7, 0), xaxs = "i")

  #
  graph_netocc(x, x$log10BS_bsm, val, vc_pos, cex_bas = cex_bas, xlim = c(1.6, 3), vc_gf = vc_gf)


  vc_pos <- rep(1, nrow(x))
  vc_pos[x$id_graph %in% c(3:4, 15, 18:20, 28, 33, 36)] <- 3
  vc_pos[x$id_graph %in% c(5:7, 9, 32, 38)] <- 4
  vc_pos[x$id_graph %in% c(13:14, 21:22)] <- 2
  graph_netocc(x, x$FTP, val, vc_pos, cex_bas = cex_bas, xlab = "FTP", xlim = c(10, 32), let = 2, vc_gf = vc_gf)


  vc_pos <- rep(1, nrow(x))
  vc_pos[x$id_graph %in% c(1:2, 4:7, 14:16, 28:29, 33, 35:39)] <- 3
  vc_pos[x$id_graph %in% c(9, 19, 21:22, 27, 32)] <- 4
  vc_pos[x$id_graph %in% c(12, 13, 15, 17, 20, 30)] <- 2
  graph_netocc(x, log10(x$meanDepth_bsm), val, vc_pos, cex_bas = cex_bas, xlab = "log10(Average Depth of Catch)", xlim = c(0.3, 1.2), let = 3,
    vc_gf = vc_gf)

  ##
  par(mar = c(3.5, 0.5, 1.5, 0.5))
  plot0()
  text(0, 0, labels = "Net occupancy difference", srt = 90, cex = cex_bas * cex_txt)

  dev.off()
}


graph_netocc <- function(tmp_occ, x, y, vc_pos, vc_gf,
  xlab= "log10(Body Size)", let = 1, xlim = range(x), ylim = range(y),
  cex_bas = 1) {
  plot(x, y, col = pal[vc_gf], pch = 19, ylab = "", xlab = xlab, xlim = xlim, ylim = ylim, cex = .87)
  abline(h = 0, lty = 2, lwd = .6)
  mod <- lm(y ~ vc_gf*x)
  sm <- summary(mod)
  vp <-  predict(mod)
  #
  fst <- sm$fstatistic
  pv <- 1-pf(fst[1],fst[2],fst[3])
  if (pv < .05) {
    lines(x[vc_gf == 1], vp[vc_gf == 1], col = pal[1])
    lines(x[vc_gf == 2], vp[vc_gf == 2], col = pal[2])
  }

  text(x, y, labels = tmp_occ$id_graph, col = pal[vc_gf], pos = vc_pos,
    cex = .77*cex_bas, offset = .3)

  mtext(3, line = -1.4, at = xlim[1]+.92*diff(xlim), text = LETTERS[let],
    cex = cex_bas*cex_let)
  #
  cx <- xlim[1] + .83*diff(xlim)
  cex_txt <- .88*cex_bas
  text(cx, -72, expression("Adjusted R"^2), cex = cex_txt, pos = 2)
  text(cx, -72, ":", cex = cex_bas)
  text(cx, -72, paste0(100*round(sm$adj.r.squared, 3), "%"), cex = cex_txt,
    pos = 4)
  ##
  text(cx, -82, "p-value", cex = cex_txt, pos = 2)
  text(cx, -82, ":", cex = cex_txt)
  text(cx, -82, signif(pv, 3), cex = cex_txt, pos = 4)

}
