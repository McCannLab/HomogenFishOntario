#' Draw Figure 5
#'
#' Compute occupancy changes for every grouping and draw Figure 5.
#'
#' @param x output of [an2_occurrence_species()].
#' @param nlk number of lakes.
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#'
#' @export
#'
#' @examples
#' res0 <- an1_gain_loss(sf_bsm_ahi)
#' res1 <- an2_occurrence_species(res0)
#  fig5_piecharts (res1, nrow(res0[[1]]))

fig5_piecharts  <- function(x, nlk, cex_bas = 3.3) {

  ## val
  occAHI_sc <- x$occAHI/nlk
  occBSM_sc <- x$occBSM/nlk

  ###
  tot <- x$lost + x$gain + x$both
  stab <- x$both/tot
  vari <- 1 - stab
  gain <- x$gain/tot
  lost <- x$lost/tot
  ###
  chg_lk <- nlk * (occBSM_sc - occAHI_sc)

  ## Graphical values
  palg <- c("grey15", "grey45", "grey75")
  palg2 <- c("grey20", "grey35", "grey50")
  ##
  vc_tg <- x$thermalGuild_bsm %>% as.factor %>% as.numeric
  vc_tl <- x$Updated_Trophic_Level %>% as.factor %>% as.numeric
  spc_nm <- x$species
  spc_nm[grepl(x = spc_nm, "Osmer")] <- "Osmerus mordax*"
  vc_bodysize <- as.character(signif(10^x$log10BS_bsm, 3))

  tpm_df <- cbind(stab, gain, lost)
  quant_bs <- quantile(x$log10BS_bsm, c(0.33, 0.66))

  df_log <- data.frame(x$GameFish != 0, x$GameFish == 0, x$Updated_Trophic_Level == "Predator", x$Updated_Trophic_Level ==
    "Mesopredator", x$Updated_Trophic_Level == "Consumer", x$log10BS_bsm >= quant_bs[2L], x$log10BS_bsm > quant_bs[1L] &
    x$log10BS_bsm < quant_bs[2L], x$log10BS_bsm <= quant_bs[1L], x$thermalGuild_bsm == "Cold", x$thermalGuild_bsm ==
    "Cool", x$thermalGuild_bsm == "Warm")


  ls_pie <- list()
  for (i in seq_ncol(df_log)) {
    ls_pie[[i]] <- c(mean(stab[df_log[, i]]), mean(lost[df_log[, i]]), mean(gain[df_log[, i]]), mean(occAHI_sc[df_log[, i]]), se(occAHI_sc[df_log[,
      i]]), mean(occBSM_sc[df_log[, i]]), se(occBSM_sc[df_log[, i]]))
  }

  ## Categories' labels
  vc_lab <- c("Gamefish", "Baitfish", "Predator", "Mesopredator", "Consumer", "Large", "Medium", "Small", "Cold", "Cool", "Warm")

  ## Number per categories
  vc_num <- paste0(apply(df_log, 2, sum))

  for (i in 3:11) {
    vc_num[i] <- paste0(vc_num[i], " (", sum(df_log[, 1] & df_log[, i]), ")")
  }


  ##
  vc_ylab <- c("Trophic level", "Body size", "Thermal Guild")
  ccx <- c(1.5, 4.5, rep(1.5 + 3 * (0:2), 3))
  ccy <- rep(rev(1.5 + 3 * (0:3)), c(2, 3, 3, 3))
  ccy2 <- ccy - 0.65
  ccy3 <- ccy - 0.85
  mag <- 2.5 * 2


  #############
  checkOutputFolder()
  png("output/fig5_piecharts .png", res = 900, width = 80,
   height = 100, units = "mm")

  par(mar = rep(0.4, 4), xaxs = "i", yaxs = "i")

  plot0(c(-0.8, 9.25), c(-1, 12.5))

  # Text describing categories
  text(ccx - 1.6, ccy + 1.4, labels = vc_lab, pos = 4, cex = cex_bas * 1.5)
  # How many species per categories
  text(ccx + 0.34, ccy + 1.4, labels = vc_num, pos = 4, cex = cex_bas * c(1.4, 1.4, rep(1.2, 9)))
  # Y labels
  text(rep(-0.5, 3), rev(1.5 + 3 * 0:2), srt = 90, labels = vc_ylab, cex = cex_bas * 1.6)

  # Top right legend
  points(0.3 + rep(6.5, 3), 3 + c(8.75, 8.1, 7.45), pch = 19, cex = 1.8, col = palb)
  text(0.4 + rep(6.5, 3), -0.05 + 3 + c(8.75, 8.1, 7.45), labels = c("Unchanged", "Gains", "Losses"), pos = 4, cex = 1.7 * cex_bas)

  ## bottom legend
  text(-.3, -.6, cex = 1.2 * cex_bas, labels = "Raw occupancy as percentage of the total number of lakes:", pos = 4)
  text(c(6.1, 6.1), -0.6 + c(.2, -.2), cex = 1.2 * cex_bas, labels = c("Historical", "Recent"), pos = 4, col = palg[c(1, 3)])
  lines(c(7.5, 8.2), rep(-0.4, 2), lwd = 4, col = palg[1], lend = 1)
  lines(c(8.2, 8.5), rep(-0.4, 2), lwd = 1, col = palg[1], lend = 1)
  lines(c(7.5, 8.2), rep(-0.8, 2), lwd = 4, col = palg[3], lend = 1)
  lines(c(8.2, 8.5), rep(-0.8, 2), lwd = 1, col = palg[3], lend = 1)

  ## custom pie charts
  lwb <- 5.2
  lwbe <- 1.4
  for (i in seq_along(ccx)) {
    ss <- sum(ls_pie[[i]][1:2])
    v1 <- 0.5 * pi + 2 * pi * ls_pie[[i]][1L]
    v2 <- 0.5 * pi + 2 * pi * ss

    ## HOME MADE PIE CHARTS
    circles(ccx[i], 0.4 + ccy[i], from = pi/2, radi = 0.8, to = v1, pie = T, col = palb[1], clockwise = TRUE, border = "white", lwd = 0.8)
    circles(ccx[i], 0.4 + ccy[i], from = pi - v1, radi = 0.8, to = pi - 2 * v1 + v2, pie = T, col = palb[3], clockwise = TRUE, border = "white",
      lwd = 0.8)
    circles(ccx[i], 0.4 + ccy[i], from = pi/2, radi = 0.8, to = pi/2 + 2 * pi * (1 - ss), pie = T, col = palb[2], border = "white", lwd = 0.8)

    ## BARPLOTS
    lines(-1.25 + ccx[i] + c(0, mag * ls_pie[[i]][4L]),
     rep(ccy2[i], 2), lwd = lwb, col = palg[1], lend = 1)
    lines(-1.25 + ccx[i] + mag * c(ls_pie[[i]][4L], ls_pie[[i]][4L] + ls_pie[[i]][5L]),
      rep(ccy2[i], 2), lwd = lwbe, col = palg[1], lend = 1)
    ###
    lines(-1.25 + ccx[i] + c(0, mag * ls_pie[[i]][6L]), rep(ccy3[i], 2), lwd = lwb, col = palg[3], lend = 1)
    lines(-1.25 + ccx[i] + mag * c(ls_pie[[i]][6L], ls_pie[[i]][6L] + ls_pie[[i]][7L]), rep(ccy3[i], 2), lwd = lwbe, col = palg[3], lend = 1)
  }

  ## Legend barplots
  for (i in 1:3) {
    vx <- 0.25 + (i - 1) * 3 + c(0, 2.5)
    lines(vx, c(0.4, 0.4), lwd = 1.2)
    text(mean(vx) + c(-1.15, 0, 1.15), rep(0.25, 3), labels = c("0%", "25%", "50%"), lwd = 1.2, cex = cex_bas * 0.9)
  }

  dev.off()

}
