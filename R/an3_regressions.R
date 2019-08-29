#' Regressions
#'
#' Perform the regressions described in the study.
#'
#' @param x output of [an1_gain_loss()].
#' @param y output of [an2_occurrence_species()].
#' @param trace a logical. If `TRUE`, information is printed during the running of [stats::step()] and [MuMIn::dredge()].
#' @param dredge a logical. Should dredging be performed? See `details`.
#'
#' @details
#' `dredge` was only used to respond to one reviewer comment, we nonetheless #' kept it to show how to use it (but results are not exported).
#'
#' @return
#' A list of models.
#'
#' @export
#'
#' @examples
#' x <- an1_gain_loss(sf_bsm_ahi)
#' y <- an2_occurrence_species(x)
#' an3_regressions(x, y)

an3_regressions <- function(x, y, trace = FALSE, dredge = FALSE) {

  ## Get data frame ready for regressions
  df_reg <- x[[2]][, c("site", "ja")]
  names(df_reg)[1] <- "idLake"
  df_reg$gain <- x[[2]]$site_t1_only
  df_reg$lost <- x[[2]]$site_t2_only
  # presence/absence data
  df_pa_ahi <- as.matrix(x[[1]][x[[5]]])
  df_pa_bsm <- as.matrix(x[[1]][x[[6]]])

  #### Response variables
  #### Remember FTP values missing for 5 specie
  ido <- order(y$idOnt)
  df_reg$cti <- getCWM(y$FTP[ido], df_pa_bsm) - getCWM(y$FTP[ido], df_pa_ahi)

  ## Distance to centroid see zzz.R
  df_reg$centr <- (get_dcen_jac(df_pa_bsm) - get_dcen_jac(df_pa_ahi))
  # below a t.test check
  # t.test(get_dcen_jac(df_pa_bsm), get_dcen_jac(df_pa_ahi), paired = TRUE)

  #### EXPLANATORY VARIABLES
  #### physical data
  tmp <- names(x[[1]])
  tmp2 <- tmp[!grepl("PA_", tmp)]
  # selection (remove variables with NA)
  phys_nm <- c("idLake", "Area_km2_", "Mean_Depth_m_", "Elevation_m_",
    "Total_P_ug/L_", "Watershed_Area_km2_", "Watershed_Mean_Slope_%_",
    "cover_forest", "lake_to_highway")
  tmp_phys <- x[[1]][, phys_nm]
  ## scale them
  tmp_phys[, -1] <- apply(tmp_phys[, -1], 2, scale)
  ##
  df_reg <- merge(df_reg, tmp_phys, by = "idLake")

  ## climatic data
  clim_nm <- c("idLake", "precipitation_july_AHI", "max_temperature_july_AHI", "annual_precipitation_AHI", "max_temperature_july_delta",
    "annual_mean_temperature_delta", "min_temperature_july_delta")
  tmp_clim <- x[[1]][, clim_nm]
  tmp_clim[, -1] <- apply(tmp_clim[, -1], 2, scale)
  # merge climate data
  df_reg <- merge(df_reg, tmp_clim, by = "idLake")

  # merge fish nofish data
  tmp_nf <- fish_nofish(x, y)
  df_reg <- merge(df_reg, tmp_nf, by = "idLake")

  ## tweak variables names
  names(df_reg)[grepl("Total_P_", names(df_reg))] <- "Total_Phosphorus"
  names(df_reg) <- gsub("_%_", "", names(df_reg))



  #### REGRESSIONS Y variables ==> loss gain / cti / centroid

  dfsh <- df_reg$fsh_bsm - df_reg$fsh_ahi
  dnfsh <- df_reg$nofsh_bsm - df_reg$nofsh_ahi


  ## MODELS CTI VS ABIOTIC VARIABLES
  id <- c(1:4, 6, 21:24)
  id_nn <- !is.nan(df_reg$cti)
  df_reg2 <- df_reg[id_nn, -id]
  # full model
  mod_ctiF <- lm(cti ~ ., data = df_reg2, na.action = "na.fail")
  # best model
  mod_cti <- step(mod_ctiF, trace = trace)
  if (dredge)
    drg_cti <- dredge(mod_ctiF, extra = c("adjR^2", "R^2"),
    trace = trace, rank = "AIC")
  # drg_cti[1:3,]
  # model gamefish/baitfish
  mod_ctif <- lm(df_reg$cti[id_nn] ~ scale(dfsh[id_nn]) +
    scale(dnfsh [id_nn])) %>% step(trace = trace)


  ## MODELS GAIN LOSS
  df_reg3 <- df_reg[, -c(1:2, 4:6, 21:24)]
  mod_gaiF <- glm.nb(gain ~ ., data = df_reg3, na.action = "na.fail")
  mod_gai <-  step(mod_gaiF, trace = trace)
  if (dredge)
    drg_gai <- dredge(mod_gaiF, extra = c("adjR^2", "R^2"), rank = "AIC")
  #
  mod_gaif <- glm.nb(df_reg$gain ~ scale(dfsh) + scale(dnfsh))

  df_reg3 <- df_reg[, -c(1:3, 5:6, 21:24)]
  mod_losF <- glm.nb(lost ~ ., data = df_reg3, na.action = "na.fail")
  mod_los <- step(mod_losF, trace = trace)
  if (dredge)
    drg_los <- dredge(mod_losF, extra = c("adjR^2", "R^2"), rank = "AIC")
  mod_losf <- glm.nb(df_reg$lost ~ scale(dfsh) + scale(dnfsh)) %>%
    step(trace = trace)


  ## DISTANCE TO CENTROID
  df_reg4 <- df_reg[, -c(1:5, 21:24)]
  mod_cenF <- lm(centr ~ ., data = df_reg4, na.action = "na.fail")
  mod_cen <- step(mod_cenF, trace = trace)
  if (dredge)
    drg_cen <- dredge(mod_cenF, extra = c("adjR^2", "R^2"), rank = "AIC")
  ##
  mod_cenf <- lm(df_reg$centr ~ scale(dfsh) + scale(dnfsh)) %>%
    step(trace = trace)

  list(df_reg,
    list(mod_ctiF, mod_cti, mod_ctif),
    list(mod_gaiF, mod_gai, mod_gaif),
    list(mod_losF, mod_los, mod_losf),
    list(mod_cenF, mod_cen, mod_cenf)
  )
}
