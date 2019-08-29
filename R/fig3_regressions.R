#' Create figure 2 - Regressions
#'
#' @param x output of [an1_gain_loss()].
#' @param mods output of [an3_regressions()].
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#' @param asfinal logical. Should the figure be formatted for publication and save as a separate file? Note that in this case `cex_bas` is ignored.
#'
#' @export
#'
#' @examples
#' res0 <- an1_gain_loss(sf_bsm_ahi)
#' res1 <- an2_occurrence_species(res0)
#' mods <- an3_regressions(res0, res1)
#' fig3_regressions(res0, mods)

fig3_regressions <- function(x, mods, cex_bas = 1, asfinal = TRUE) {

  mods[[1]]$gain <- x[[2]]$site_t1_only
  mods[[1]]$lost <- x[[2]]$site_t2_only
  ## CIT
  # best model
  mod_cti <- mods[[2]][[2]]
  #  summary
  mod_ctiS <- summary(mod_cti)
  # fish/nofish only
  mod_ctif  <- mods[[2]][[3]]
  ## GAIN
  mod_gai <- mods[[3]][[2]]
  mod_gaiS <- summary(mod_gai)
  mod_gaif  <- mods[[3]][[3]]
  ## LOSS
  mod_los <- mods[[4]][[2]]
  mod_losS <- summary(mods[[4]][[2]])
  mod_losf <- mods[[4]][[3]]
  ## CENTROID
  mod_cen <- mods[[5]][[2]]
  mod_cen <- summary(mod_cen)
  mod_cenf  <- mods[[5]][[3]]


  ## MCFADDEN RSQ mcfa <- function(x) ###
  prsq_gai <- 1 - (as.numeric(logLik(mod_gai)) - (nrow(mod_gaiS$coefficients)) - 1)/as.numeric(logLik(glm.nb(gain ~ 1, data = mods[[1]])))
  prsq_gaif <- 1 - (as.numeric(logLik(mod_gaif)) - 1)/as.numeric(logLik(glm.nb(gain ~ 1, data = mods[[1]])))

  prsq_los <- 1 - (as.numeric(logLik(mod_los)) - (nrow(mod_losS$coefficients)) - 1)/as.numeric(logLik(glm.nb(lost ~ 1, data = mods[[1]])))
  prsq_losf <- 1 - (as.numeric(logLik(mod_losf)) - 1)/as.numeric(logLik(glm.nb(lost ~ 1, data = mods[[1]])))


  vc_nm2 <- names(mods[[1]])[7:20]
  vc_lab <- c("Area", "Mean depth", "Elevation", "Total phosphorus", "Watershed area", "Watershed mean slope", "Forest cover", "Distance lake to highway", "Precipitation July (AHI)", "Maximun temperature July (AHI)", "Annual precipitation (AHI)", "Maximun temperature July (\u0394)", "Annual mean temperature (\u0394)", "Minmum temperature July (\u0394)")

  nvar <- 14
  lrsq <- 3.7

  ## PLOT
  if (asfinal) {
    checkOutputFolder()
    png("output/fig3_regressions.png", res = 900, height = 100,
    width = 80, units = "mm")
    cex_bas = 3.8
  }

  cex_mt <- cex_bas
  cex_tx <- cex_bas*1.6

  mat0 <- matrix(1:10, ncol = 2)
  mat <- cbind(c(0,rep(11, 4), 0), rbind(0, mat0))
  layout(mat, heights = c(0.08, 1, 1, 1, 1, 1.5), widths = c(.12, 1, .3))

  par(mar = c(0.75, 1, 0.1, 5.5), xaxs = "i", yaxs = "i",
    cex.lab = 1.25*cex_bas, cex.axis = 1.4*cex_bas, las = 2)

  if (!asfinal) par(mar = c(.6, 1, 0.15, 11))

  ## CTI
  rgy1 <- c(-.4, .45)
  seq_val1 <- seq(-.4, .4, .2)
  plot_bg(nvar, rgy = rgy1)
  add_coef(mod_ctiS$coefficients, vc_nm2)
  add_marks(seq_val1)
  add_text(expression(Delta*FTP), signif(mod_ctiS$adj.r.squared * 100, 3),
    cex_bas)

  ## GAINS
  rgy2 <- c(-.7, .4)
  seq_val2 <- seq(-.6, .4, .2) %>% round(4)
  plot_bg(nvar, rgy = rgy2)
  add_coef(mod_gaiS$coefficients, vc_nm2)
  add_marks(seq_val2)
  add_text("Gains", signif(100 * prsq_gai, 3), cex_bas)

  ## LOSSES
  rgy3 <- c(-.2, .5)
  seq_val3 <- seq(-.2, .5, .1) %>% round(4)
  plot_bg(nvar, rgy = rgy3)
  add_coef(mod_losS$coefficients, vc_nm2)
  add_marks(seq_val3)
  add_text("Losses", signif(100 * prsq_los, 3), cex_bas)


  ## CENTROIDS
  rgy4 <- 0.01*c(-3, 2)
  seq_val4 <- 0.01*seq(-3, 2, 1)

  plot_bg(nvar, rgy4)
  add_coef(mod_cen$coefficient, vc_nm2)
  add_marks(seq_val4)
  add_text(expression(Delta*d[cen]), signif(100 * mod_cen$adj.r.squared, 3),
    cex_bas)

  ## VARIABLES NAMESc(0.75, 1, 0.2, 6)
  par(mar = c(0, 1, 0, 5.5), xpd = TRUE)
  if (!asfinal) par(mar = c(0, 1, 0, 10))
  plot0(c(0, nvar), c(-1, 1))
  text(seq_len(nvar)+.15, rep(1, nvar), labels = vc_lab, pos = 2, srt = 90, cex = cex_tx)



  ## SECOND COLUMN
  vc_nm <- c("scale(dnfsh)", "scale(dfsh)")
  lrsq <- .6
  par(mar = c(0.6, 1, 0.2, 2.2), xpd = FALSE)
  if (!asfinal) par(mar = c(1, 1, 0.2, 7))

  plot_bg(2, rgy = rgy1)
  add_coef(summary(mod_ctif)$coefficients, vc_nm2 = c("scale(dnfsh[id_nn])", "scale(dfsh[id_nn])"))
  add_marks(seq_val1)
  mtext(paste0(signif(100 * summary(mod_ctif)$adj.r.squared, 2), "%"), 4,
  cex = cex_mt, line = lrsq, las = 1)

  plot_bg(2, rgy = rgy2)
  add_coef(summary(mod_gaif)$coefficients, vc_nm2 = vc_nm)
  add_marks(seq_val2)
  mtext(paste0(signif(100 * prsq_gaif, 3), "%"), 4, cex = cex_mt,
  line = lrsq, las = 1)

  plot_bg(2, rgy = rgy3)
  add_coef(summary(mod_losf)$coefficients, vc_nm2 = vc_nm)
  add_marks(seq_val3)
  mtext(paste0(signif(100 * prsq_losf, 3), "%"), 4, cex = cex_mt,
  line = lrsq, las = 1)

  plot_bg(2, 0.01*c(-8, 4))
  add_coef(summary(mod_cenf)$coefficients, vc_nm2 = vc_nm)
  add_marks(0.01*seq(-8, 8, 2))
  mtext(paste0(signif(100 * summary(mod_cenf)$adj.r.squared, 3), "%"), 4,
  cex = cex_mt, line = lrsq, las = 1)

  par(mar = c(0, 1, 0, 2.2), xpd = TRUE)
  plot0(c(0, 2), c(-1, 1))
  text((1:2) + .15, c(1, 1), labels = c("Number of baitfish (\u0394)", "Number of gamefish (\u0394)"), pos = 2, srt = 90, cex = cex_tx)


  ##
  par(mar = c(0, 0, 0, 0))
  plot0()
  text(-.5, 0, srt = 90, labels = "Scale effect", cex = 1.33*cex_tx)

  if (asfinal) dev.off()

}




## Figure regression main
plot_bg <- function(nvar = 12, rgy = c(-.8, .8)) {
  plot0(c(0,nvar), rgy)
}

## rect and background
add_coef <- function(coef, vc_nm2, mxy = 10, mny = -10) {
    for (i in seq_nrow(coef)) {
      if (rownames(coef)[i] != "(Intercept)") {
        id <- which(vc_nm2 == rownames(coef)[i])
        stopifnot(!is.null(id))
        if (coef[i, 4] <= .001) {
          rect(id -1, mny, id, mxy , col = "grey50", border = NA)
        } else {
          if (coef[i, 4] <= .01) {
            rect(id -1, mny, id, mxy, col = "grey70", border = NA)
          } else {
            if (coef[i, 4] <= .05) rect(id -1, mny, id, mxy, col = "grey90", border = NA)
          }
        }
        lines(rep(id-.5, 2), c(0, coef[i, 1]), lwd = 6.8, lend = 1)
        tmp <- ifelse(coef[i, 1] > 0,
          coef[i, 1] + coef[i, 2], coef[i, 1] - coef[i, 2])
        lines(rep(id-.5, 2), c(coef[i, 1], tmp), lwd = 1.4, lend = 1)
      }
    }
}

add_text <- function(title, rsq, cex_bas = 1, lrsq = 2.5) {
  mtext(4, text = title, cex = cex_bas*1.33, padj=-4, line = .65)
  mtext(4, text = TeX("R$^2$$_{adj}$ = "), cex = 1.15*cex_bas, line = .65)
  mtext(4, text = paste0(rsq, "%"), cex = 1.15*cex_bas, line = lrsq)
}

add_marks <- function(seq_val) {
  abline(h = 0, lty = 2, col = "grey75")
  axis(2, at = seq_val, labels = seq_val)
}
