#' Draw Figure Z2
#'
#' @param x output of [an1_gain_loss()].
#' @param y output of [an4_contributions_beta()].
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#'
#' @details
#' Figure Z2 was found in a previous version of the SI.

figZ2_coldest <- function(x, y, cex_bas = 4) {

  ## Presence Absence matrix
  pres_AHI <- x[[1]][, c("idLake", x[[5]])]
  pres_BSM <- x[[1]][, c("idLake", x[[6]])]
  ##
  idt <- as.numeric(as.factor(x$thermalGuild_bsm))
  vc_pch <- c(21, 22, 24)

  ## temperature data BSM and AHI
  df_temp <- x[[1]][c(1, which(grepl(pattern = "temperature", names(x[[1]]))))]
  ##
  df_temp$max_temperature_july_BSM <- df_temp$max_temperature_july_delta + df_temp$max_temperature_july_AHI
  ##
  df_temp$min_temperature_july_BSM <- df_temp$min_temperature_july_delta + df_temp$min_temperature_july_AHI

  df_temp$annual_mean_temperature_BSM <- df_temp$annual_mean_temperature_delta + df_temp$annual_mean_temperature_AHI

  df_temp$mean_temperature_gg_BSM <- df_temp$mean_temperature_gg_delta + df_temp$mean_temperature_gg_AHI

  pres_AHI <- merge(pres_AHI, df_temp[c(1, which(grepl("_AHI$", names(df_temp))))])
  pres_BSM <- merge(pres_BSM, df_temp[c(1, which(grepl("_BSM$", names(df_temp))))])

  nm <- c("max_temperature_july", "min_temperature_july", "annual_mean_temperature", "mean_temperature_gg")

  y[paste0("min_", nm, rep(c("_AHI", "_BSM"), each = 4))] <- NA

  for (i in seq_len(nrow(y))) {
    idsp <- sprintf("PA_%03d", y$idOnt[i])
    for (j in seq_along(nm)) {
      val_ahi <- pres_AHI[, paste0(nm[j], "_AHI")][pres_AHI[, paste0(idsp, "_AHI")] > 0]
      val_bsm <- pres_BSM[, paste0(nm[j], "_BSM")][pres_BSM[, idsp] > 0]
      y[i, paste0("min_", nm[j], "_AHI")] <- min(val_ahi)
      y[i, paste0("min_", nm[j], "_BSM")] <- min(val_bsm)
    }
  }

  val1 <- y$min_max_temperature_july_BSM - y$min_max_temperature_july_AHI
  val2 <- y$min_min_temperature_july_BSM - y$min_min_temperature_july_AHI
  val3 <- y$min_annual_mean_temperature_BSM - y$min_annual_mean_temperature_AHI
  val4 <- y$min_mean_temperature_gg_BSM - y$min_mean_temperature_gg_AHI

  vc_gf <- (y$GameFish == 0) + 1

  checkOutputFolder()
  png("output/figZ2_coldest.png", height = 180, width = 90, units = "mm",
    res = 900)
  mat_lay <- cbind(5, c(1, 2), c(3, 4))
  layout(rbind(mat_lay, c(0, 6, 0)), widths = c(0.08, 1, 0.3),
    heights = c(1, 1, 0.125))

  par(las = 1, xaxs = "i", yaxs = "i", cex.lab = 4.8, cex.axis = 4.6, mar = c(1, 2, 1.5, 1))

  vc_pos <- rep(3, nrow(y))
  vc_pos[which(y$id_graph %in% c(10, 14, 17, 20, 25, 32, 36))] <- 1
  vc_pos[which(y$id_graph %in% c(11, 31))] <- 4
  vc_pos[which(y$id_graph %in% c(9, 30))] <- 2

  plot0(c(10, 31), c(-2, 4))
  points(y$FTP, val1, col = NA, bg = pal[vc_gf], pch = vc_pch, cex = 1)
  text(y$FTP, val1, labels = y$id_graph, pos = vc_pos, cex = cex_bas, offset = 0.28)
  abline(h = 0, v = c(19, 25), lty = 2)
  axis(1, mgp = c(3, 0.3, 0))
  axis(2)
  points(c(26, 26), c(3, 3.5), pch = 19, col = pal, cex = 1.4)
  text(c(26, 26), c(3, 3.5), pos = 4, labels = c("gamefish", "baitfish"), cex = 5.6)
  mtext("Maximum Temperature in July", 3, cex = 3.8, line = .2, adj=0)
  mtext("B", 3, line = -1.1, cex = 1.25*cex_bas, adj = 0.02)

  vc_pos[which(y$id_graph %in% c(29))] <- 1
  vc_pos[which(y$id_graph %in% c(17))] <- 4
  plot0(c(10, 31), c(-2, 4))
  points(y$FTP, val2, col = NA, bg = pal[vc_gf], pch = vc_pch, cex = 1)
  text(y$FTP, val2, labels = y$id_graph, pos = vc_pos, cex = cex_bas, offset = 0.28)
  abline(h = 0, v = c(19, 25), lty = 2)
  axis(1, mgp = c(3, 0.3, 0))
  axis(2)
  mtext("Minimum Temperature in July", 3, cex = cex_bas, line = .2, adj=0)
  mtext("D", 3, line = -1.1, cex = 1.25*cex_bas, adj = 0.02)

  ##
  par(mar = c(1, 0, 1, 1))
  extr <- list(boxwex = 0.5, staplewex = 0.35, outwex = 0.4)
  plot0(c(0.8, 2.2), c(-2, 4))
  boxplot(val1 ~ y$GameFish, col = c("grey25", "grey65"), pars = extr, axes = FALSE, at = c(1, 1.75), add = TRUE,  lwd = .9)
  mtext("C", 3, line = -1.6, cex = 1.25*cex_bas, adj = 0.02)
  plot0(c(0.8, 2.2), c(-2, 4))
  boxplot(val2 ~ y$GameFish, col = c("grey25", "grey65"), pars = extr, axes = FALSE, at = c(1, 1.75), add = TRUE, lwd = .9)
  mtext("E", 3, line = -1.6, cex = 1.25*cex_bas, adj = 0.02)

  ##v
  par(mar = c(0, 0, 0, 0))
  plot0()
  text(0, 0, labels = "Difference of temperature of the coldest lakes (\u00b0C)", srt = 90, cex = 1.25*cex_bas)

  ##
  plot0()
  text(0, 0, labels = "Final Preferundum Temperature", cex = 1.25*cex_bas)

  dev.off()
}
