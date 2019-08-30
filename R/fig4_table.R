#' Create figure 3
#'
#' Create Fig. 3 - Change in occupancy
#'
#' @param x output of [an2_occurrence_species()].
#' @param nlk number of lakes.
#' @param cex_bas magnification coefficient, controls the size of plot elements.
#'
#' @export
#'
#' @references
#' The image are available on line:
#' - Hook <https://www.shareicon.net/miscellaneous-fishing-hook-sport-tools-and-utensils-steel-788675>
#' - Lake Trout <http://phylopic.org/image/7f2cbb42-12b1-4481-8ac0-705eb7363c74/>
#' - White sucker <http://phylopic.org/image/c657b67e-db70-4f8d-a66f-e6b287b148a6/>
#' - Yellow perch <http://phylopic.org/image/5e431267-dc57-435d-a64f-b91d4c569677/>
#'
#' @examples
#' # res0 <- an1_gain_loss(sf_bsm_ahi)
#' # res1 <- an2_occurrence_species(res0)
#' # fig4_table(res1, nrow(res0[[1]]))

fig4_table <- function(x, nlk, cex_bas = 5.4) {

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
  lwb <- 2.95
  ##
  palg <- c("grey15", "grey45", "grey75")
  palg2 <- c("grey20", "grey35", "grey50")
  ##
  vc_tg <- x$thermalGuild_bsm %>% as.factor %>% as.numeric
  vc_tl <- x$Updated_Trophic_Level %>% as.factor %>% as.numeric
  spc_nm <- x$species
  spc_nm[grepl(x = spc_nm, "Osmer")] <- "Osmerus mordax*"
  vc_bodysize <- as.character(signif(10^x$log10BS_bsm, 3))

  ## files
  trout <- system.file("extdata/img", "lake_trout_reusable.png",
    package = "HomogenFishOntario")
  perch <- system.file("extdata/img", "yellow_perch_reusable.png",
    package = "HomogenFishOntario")
  sucker <- system.file("extdata/img", "white_sucker_reusable.png",
    package = "HomogenFishOntario")
  hook <- system.file("extdata/img", "hook.png", package = "HomogenFishOntario")

  checkOutputFolder()
  png("output/fig4_table.png", res = 900, width = 260, height = 180,
    units = "mm")
  par(mar = rep(0.6, 4), xaxs = "i", yaxs = "i", fg = pal[1], lend = 1,
    family = "TNM")
  plot0(c(-12.2, 12.2), c(-1.2, 43.2))
  # bg
  for (i in 0:19 * 2 + 1) {
    rect(-12, i - 0.5, 12, i + 0.5, col = "grey95", border = NA)
  }
  #
  for (i in seq_nrow(x)) {
    ###
    text(-11.5, i, labels = as.character(x$id_graph[i]), pos = 2, cex = cex_bas)
    text(-11.76, i, labels = spc_nm[i], pos = 4, cex = cex_bas)
    text(-8.8, i, labels = x$commonName[i], pos = 4, cex = cex_bas)
    text(-6.5, i, labels = x$family[i], pos = 4, cex = cex_bas)
    #
    ##-- Gamefish
    if (x$GameFish[i] != 0)
      text(-4.6, i, labels = fontawesome("fa-check"), cex = 1.1 * cex_bas, family = "fontawesome-webfont")
    #
    ##-- Bait
    if (!is.na(x$baitOntario[i]) & x$baitOntario[i] > 0) {
      pchImage(-4.15, i, file = hook, cex.x = 0.11, cex.y = 0.2)
    }
    ##-- Socked fish
    if (x$stocked[i])
      text(-3.75, i, labels = fontawesome("fa-check"), cex = 1.1 * cex_bas, family = "fontawesome-webfont")
    ##-- Life span
    if (!is.na(x$LongevityWild[i])) {
      text(-1.6, i, labels = x$LongevityWild[i], pos = 2, cex = cex_bas)
    }
    #
    ##-- Body size
    text(-2.9, i, labels = vc_bodysize[i], pos = 2, cex = cex_bas)


    if (vc_tl[i] > 1) {
      if (vc_tl[i] > 2) {
        pchImage(-2.6, i, file = trout, cex.x = 0.24, cex.y = 0.18,
          col = palg2[vc_tl[i]])
      } else {
        pchImage(-2.6, i, file = perch, cex.x = 0.24, cex.y = 0.18,
          col = palg2[vc_tl[i]])
      }
    } else {
      pchImage(-2.6, i, file = sucker, cex.x = 0.24, cex.y = 0.18,
        col = palg2[vc_tl[i]])
    }
    ##-- Mean depth
    dph <- x$meanDepth_bsm[i]
    if (!is.na(dph))
      points(-1.45, i, pch = 19, col = pal[1], cex = 0.55 * log(dph))
    #
    ##-- FTP
    if (!is.na(x$FTP[i]))
      text(-1.28, i, labels = x$FTP[i], pos = 4, cex = 0.9 * cex_bas)
    # Thermal Guild
    points(-0.35, i, pch = 15, col = palg[vc_tg[i]], cex = 1.6)


    ######### Occurrence
    lw_bar <- 12
    ext <- 5.2
    #### Raw history
    lines(c(0, ext * occAHI_sc[i]), c(i, i), lwd = lw_bar, col = pal[1])
    #### Net variation
    cx <- ext * c(occAHI_sc[i], occBSM_sc[i])
    chg_perc <- 100 * chg_lk[i]/(occAHI_sc[i] * nlk)
    if (chg_lk[i] > 0) {
      lines(cx, c(i, i), lwd = 2, col = pal[1])
      txt <- paste0("+", format(chg_perc, digit = 3), "%")
      text(cx[2], i, labels = txt, cex = 0.9 * cex_bas, pos = 4)
    } else {
      lines(cx, c(i, i), lwd = 2, col = "white")
      txt <- paste0("", format(chg_perc, digit = 3), "%")
      text(cx[1], i, labels = txt, cex = 0.9 * cex_bas, pos = 4)
    }

    #### Relative changes
    lines(9 + c(0, lwb * stab[i]), c(i, i), lwd = lw_bar, col = palb[1])
    lines(9 + c(0, -lwb * lost[i]), c(i, i), lwd = lw_bar, col = palb[3])
    lines(9 + c(-lwb * lost[i], -lwb * vari[i]), c(i, i), lwd = lw_bar, col = palb[2])

  }

  lab_txt <- c("Id", "Scientific name", "Common name", "Family")
  text(c(-11.52, -11.75, -8.85, -6.55), rep(40.4, 4), labels = lab_txt, pos = c(2, rep(4, 4)), cex = cex_bas * cex_txt)

  lab_mid <- c("G", "B", "S", "BS", "TL", "LS", "D", "FTP", "TG")
  text(c(-4.6, -4.2, -3.75, -3.3, -2.6, -1.95, -1.46, -0.9, -0.35), rep(40.4, length(lab_mid)), labels = lab_mid, cex = cex_bas)

  # ##
  di = 0.15
  lines(c(0, 5.2), c(0, 0) + di, lwd = 2)
  lines(c(0, 5.2), c(40, 40) - di, lwd = 2)
  for (i in c(2, 200, 400, 543)) {
    lines(rep(i/545 * ext, 2), c(0 + di, 0 - di), lwd = 1.2)
    lines(rep(i/545 * ext, 2), c(40 - di, 40 + di), lwd = 1.2)
  }
  #
  lines(9 + c(-lwb, lwb), c(0, 0) + di, lwd = 2)
  lines(9 + c(-lwb, lwb), c(40, 40) - di, lwd = 2)
  for (i in c(-98, -50, 0, 50, 98)) {
    lines(6 + rep((100 + i)/200 * 6, 2), c(0 + di, 0 - di), lwd = 1.2)
    lines(6 + rep((100 + i)/200 * 6, 2), c(40 - di, 40 + di), lwd = 1.2)
  }


  text(c(2.6, 9), c(42.5, 42.5), labels = c("Historical occupancy and changes", "Components of the changes"), cex = cex_txt * cex_bas)
  text(c(2.6, 7.5, 10.5), rep(41.5, 4), labels = c("Number of lakes", "Gains      |      Losses", "Unchanged"), cex = cex_bas)

  for (i in c(-0.5, 40.5)) text(6 + (100 + c(-98, -50, 0, 50, 98))/200 * 6, rep(i, 5), labels = c("100%", "50", "0", "50", "100%"), cex = cex_bas)

  for (i in c(-0.5, 40.5)) text(c(2, 200, 400, 543) * 5.2/545, rep(i, 4), labels = c(0, 200, 400, nlk), cex = cex_bas)

  ## Legend TL pchImage(-3.3, i, file = 'inst/extdata/img/hook.png', cex.x = .11, cex.y = .2)
  pchImage(-11.5, -0.8, file = sucker, cex.x = 0.25, cex.y = 0.75 * 0.25,
      col = palg2[1])
  pchImage(-11.5 + 2.3, -0.8, file = perch, cex.x = 0.25, cex.y = 0.75 * 0.25,
      col = palg2[2])
  pchImage(-11.5 + 2 * 2.3, -0.8, file = trout, cex.x = 0.25,
    cex.y = 0.75 * 0.25, col = palg2[3])
  text(-11.32 + 2.3 * (0:2), rep(-0.88, 3), labels = c("Consumer", "Mesopredator", "Top predator"), pos = 4, cex = 1.1 * cex_bas)

  ### TG
  points(-4.1 + 1.2 * (0:2), rep(-0.8, 3), pch = 15, col = palg, cex = 1.8)
  text(-4.05 + 1.2 * (0:2), rep(-0.88, 3), labels = c("Cold", "Cool", "Warm"), pos = 4, cex = 1.1 * cex_bas)
  cex_bas <- 4.2
  dev.off()

}
