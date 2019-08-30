##' homogenOntario
#'
#' Set of functions to reproduce Cazelles (2019) DOI:XXX
#'
#' @docType package
#' @name homogenOntario
#' @keywords internal
#'
#'
#' @import emojifont
#'
#' @importFrom ape pcoa
#' @importFrom magrittr %>% %<>%
#' @importFrom ecoocc ec_betadiversity ec_temporal_betadiversity ec_rarefaction
#' @importFrom crayon blue
#' @importFrom graphics abline axis box boxplot hist image layout lines
#' @importFrom graphics mtext par plot points rect text title
#' @importFrom graphicsutils plot0 box2 envelop col2Hex pchImage circles
#' @importFrom graphicsutils gpuPalette lighten
#' @importFrom grDevices colorRampPalette dev.off png
#' @importFrom latex2exp TeX
#' @importFrom lmodel2 lmodel2
#' @importFrom inSilecoMisc signifSymbols meanAlong scaleWithin keepWords
#' @importFrom ade4 dudi.pca
#' @importFrom sf st_drop_geometry st_geometry st_coordinates
#' @importFrom MuMIn dredge
#' @importFrom MASS glm.nb
#' @importFrom showtext showtext_auto
#' @importFrom stats density dist glm hclust lm logLik pf predict quantile
#' @importFrom stats sd step wilcox.test as.dist
#' @importFrom sysfonts font_add
#' @importFrom taxize classification get_tsn

NULL


############### HELPERS Functions ###############


checkOutputFolder <- function() if (!dir.exists("output")) dir.create("output")

getNamesSp <- function(vec_idOnt, keepOrder = TRUE) {
  if (keepOrder) {
    tmp <- lapply(vec_idOnt, function(x) which(df_species_info$idOnt == x))
    df_species_info[unlist(tmp), "scientificName"]
  } else df_species_info[df_species_info$idOnt %in% vec_idOnt,
    "scientificName"]
}

getVec <- function(vec, df, fieldin, fieldout){
  tmp <- lapply(vec, function(x) which(df[,fieldin] == x)) %>% unlist
  df[tmp, fieldout]
}

# frequency
myfreq <- function(x) x/sum(x)

#
`%m%` <- function(a, b) {
   lapply(a, function(x) which(b == x)) %>% unlist
 }

# standard error
se <- function(x) sd(x)/sqrt(length(x))

# distance to centroid
get_dist_cent <- function(mat) {
  cent <- apply(mat, 2, mean)
  pres <- apply(mat, 2, sum)
  apply(mat, 1, function(x) sqrt(sum((x-cent)^2)))
}

to_dist <- function(m_ec) {
  n <- max(m_ec[,2])
  out <- matrix(0, n, n)
  out[(m_ec[,1]-1)*n + m_ec[,2]] <- m_ec[,3]
  as.dist(out)
}

#
seq_nrow <- function(nr) seq_len(nrow(nr))
seq_ncol <- function(nr) seq_len(ncol(nr))

get_dcen_jac <- function(mat) {
  val <- to_dist(ec_betadiversity(mat))
  # 1 na and difference set o 1
  val[which(is.na(val))] <- 1
  co_pco <- pcoa(val)$vectors
  cent <- apply(co_pco, 2, mean)
  apply(co_pco, 1, function(x) sqrt(sum((x-cent)^2)))
}
# val <- vegan::vegdist(rbind(df_pa_ahi, df_pa_bsm), "jaccard")
# hh = vegan::betadisper(val, rep(c("old", "new"), each = 524))

## retrieve ID from charcater strings
getIdsNum <- function(x) {
    gsub(x, pattern = "\\D", replacement = "") %>% as.numeric
}


# determine game fish and bait fish
fish_nofish <- function(x, y) {

  # NB x[[5]] and x[[6]] are ordered according to idOnt
  y <- y[order(y$idOnt), ]
  mat_fsh <- matrix((y$GameFish != 0) * 1, ncol = 1)
  mat_nofsh <- matrix((y$GameFish == 0) * 1, ncol = 1)

  data.frame(
    idLake = x[[1]][,"idLake"],
    fsh_ahi = as.matrix(x[[1]][, x[[5]]]) %*% mat_fsh,
    fsh_bsm =  as.matrix(x[[1]][, x[[6]]]) %*% mat_fsh,
    nofsh_ahi = as.matrix(x[[1]][, x[[5]]]) %*% mat_nofsh,
    nofsh_bsm = as.matrix(x[[1]][, x[[6]]]) %*% mat_nofsh
  )
}

utils::globalVariables(
  c("df_species_info", "gadm_ontario", "sf_bsm_ahi", "df_fished"),
  "HomogenFishOntario"
)

anonymize_data <- function(x) {
  tmp_crs <- sf::st_crs(x)
  tmp <- st_drop_geometry(x)
  nl <- nrow(tmp)
  tmp <- tmp[sample(nl), ]
  tmp$idLake <- sprintf("Lake_%03d", seq_len(nl))
  tmp$Waterbody_Name <- tmp$Waterbody_Location_ID <- 0
  rownames(tmp) <- NULL
  tmp$lat <- tmp$long <- 0
  sf::st_as_sf(tmp, coords = c("long", "lat"), crs = tmp_crs$epsg)
}
# sf_bsm_ahi <- anonymize_data(sf_ahi_bsm0)
# save(sf_bsm_ahi, file = "data/sf_bsm_ahi.rda")

############ Graph helpers

showtext_auto()
font_add(family = 'TNM', regular = "Times_New_Roman.ttf")


## Some graphical parameters and functions
myred <- "#d03757"
mybl <- "grey10"
cex_let <- 1.4
cex_txt <- 1.2
pal <- c('grey10', 'grey55')
palb <- lighten(c('#071b32'), c(0, 30, 60))


#
custom_ticks <- function(x, y, pal) {
  for (i in c(-1, 0.5)) {
    axis(1, at = x, labels = "", lwd = 0, lwd.ticks = 2.5, tcl = i,
      col = pal[1])
    axis(1, at = y, labels = "", lwd = 0, lwd.ticks = 2.5, tcl = i,
      col = pal[2])
  }
}

#
stat_envelop <- function(x, pred, col, lwd = 2, transp = "33") {
  lines(x, pred$fit, col=myred, lwd=3)
  envelop(x, pred$fit+pred$se.fit,  pred$fit-pred$se.fit,
    col = paste0(myred, transp), border = NA)
}

#
applyString <- function(s, pos = c(1,3), FUN = toupper) {
  tmp <- strsplit(s, split = "")
  tmp_fun <- function(x) {
    x[pos] %<>% FUN
    paste(x, collapse="")
  }
  lapply(tmp, tmp_fun) %>% unlist
}



ext_rg <- function(x, perc = 2) {
  tmp <- range(x)
  tmp + diff(tmp)*0.01*perc*c(-1, 1)
}

## Fig 2

cust_beta_temp <- function(x, y, let = "A", cex_base = 1) {
  plot(x, y, pch = 19, col = pal[1], xlab = "", ylab = "")
  mtext(3, at = min(x), text = let, cex = cex_base*cex_let)
  mod <- glm(y~x+I(x^2)+I(x^3)) %>% step(trace = 0)
  newdata <- data.frame(x = seq(min(x), max(x), length.out = 100))
  pred <- predict(mod, newdata = newdata, level=.95, interval="confidence", se.fit = T)
  stat_envelop(newdata$x, pred, col = myred)
}

## regressions val and pa should be ordered properly
getCWM <- function(val, pa) {
  nna <- !is.na(val)
  apply(pa, 1, function(x) sum(x[nna]*val[nna])/sum(x[nna]))
}
