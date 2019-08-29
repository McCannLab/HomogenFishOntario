#' Draw Figure S1
#'
#' @param x output of [an1_gain_loss()].
#'
#' @export

figS2_abiotic <- function(x) {

  ## Remove Presence Absence
  tmp <- names(x[[1]])
  tmp2 <- tmp[!grepl("PA_", tmp)]
  # selection (remove variables with NA)
  tmp3 <- tmp2[c(4:9, 11:19, 44:46, 48)]

  ttmp <- dudi.pca(apply(x[[1]][, tmp3], 2, scale), nf = length(tmp3),
    scannf = FALSE)
  # Number of axes needed to have at least 80% of inertia cum_inertia <- cumsum((ttmp$eig)/length(tmp3)) .8357162 pour 6 axes nx <-
  # min(which(round(cum_inertia, 2)>=.8)) nx <- min(which(cum_inertia>=.8))

  ################################# CLIMATIC DATA
  df_clim <- x[[1]][, 231:250]
  tmp_clim <- df_clim[, -c(1:4)]
  ttmp_clim <- dudi.pca(apply(tmp_clim, 2, scale), nf = ncol(tmp_clim),
    scannf = FALSE)
  # Number of axes needed to have at least 80% of inertia cum_inertia <- cumsum((ttmp_clim$eig)/ ncol(tmp_clim)) nx <-
  # min(which(cum_inertia>=.8)) 6 axes inertia = .9167119
  vc_lab <- c("Area (km2)", "Maximum depth (m)", "Mean depth (m)", "Shoreline (km)", "Perimeter (km)", "Elevation (m)", "Total phosphorus (\u03bcg/L)",
    "Watershed area (km2)", "Watershed mean elevation (m)", "Watershed max elevation (m)", "Watershed mean slope (%)", "Length of main channel (km)",
    "Max channel elevation (m)", "Min channel elevation (m)", "Slope of main channel (m/km)", "Forest cover (%)", "Human activity cover (%)",
    "Wetland cover (%)", "Distance lake to highway (m)")

  vc_lab_clim <- names(tmp_clim) %>%
    gsub(pattern = "_", replacement = " ") %>%
    gsub(pattern = "AHI$", replacement = "(AHI)") %>%
    gsub(pattern = "delta$", replacement = "(\u0394)") %>%
    gsub(pattern = " gg ", replacement = " growing season ") %>% applyString(toupper, pos = 1)

  checkOutputFolder()
  png("output/figS2_abiotic.png", res = 900, width = 180, height = 90,
    units = "mm")
  ##
  par(las = 1, mar = c(.5, 2.5, 2, 1), fg = "grey15", mfrow = c(1, 2), cex.lab = 5, cex.axis = 5, cex.main = 6.4)
  plot(hclust(dist(ttmp$co[, 1:8])), hang = -1, labels = vc_lab, ann = FALSE, cex = 3.8, lwd = 1.2)
  title(ylab = "Euclidean distance based on PCA coordinates")
  title(main = "A. Lake descriptors")
  points(c(2, 3, 5, 7, 10, 12, 14, 19), rep(-0.01, 8), pch = 25, bg = "grey15",
    cex = .6)

  ## Climate
  plot(hclust(dist(ttmp_clim$co[, 1:6])), hang = -1, labels = vc_lab_clim, ann = FALSE, cex = 3.8, lwd = 1.2)
  title(ylab = "Euclidean distance based on PCA coordinates")
  title(main = "B. Climatic data")
  points(c(2, 4, 6, 9, 13, 19), rep(-0.01, 6), pch = 25, bg = "grey15",
    cex = .6)
  ##
  dev.off()
}
