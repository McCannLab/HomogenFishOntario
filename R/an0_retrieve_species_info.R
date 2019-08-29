#' @title Retrieve taxonomic information.
#'
#' @description
#' Assign a unique identifiers to species and build a data frame including
#' their taxonomic hierarchy. Note that the data frame for the full set of
#' species is included in `fishList` (see `?fishList`) abd the code of thi
#' function shows how we retrieved the data.
#'
#' @param sciNames Vector of scientific names of species.
#' @param verbose Logical. Should extra information be reported on progress?
#'
#' @return
#' A data frame, each row contains the information for one species, NA #' are added for pieces of information missing.
#'
#' @examples
#' # Not run
#  # sps <- c('Carassius auratus complex',  'catostomus commersonii')
#' # an0_retrieve_species_info(sps)
#'
#' @export


an0_retrieve_species_info <- function(sciNames, verbose = FALSE) {
    ##
    sciNames <- as.character(sciNames)
    ##
    out <- data.frame(sciNameTsn = sciNames, hybrid = FALSE, tsn = NA,
      subphylum = "Vertebrata", stringsAsFactors = FALSE)
    ## using these lines in this order gives the desired order for columns
    cla <- c("superclass", "class", "superorder", "order", "superfamily",
    "family", "genus", "species", "subspecies")
    out[cla] <- NA
    ##
    for (i in seq_along(sciNames)) {
        Sys.sleep(.5)
        if (verbose)
            cat("==> scientific name: ", sciNames[i], "\n")
        stp <- FALSE
        if (sciNames[i] == "" | is.na(sciNames[i])) {
            if (verbose)
                cat("==> NO NAME\n")
            next
        }
        ##---- sp.
        if (grepl(sciNames[i], pattern = "sp\\.")) {
            if (verbose)
                cat("==> Unknown species, looking for the genus: ")
            val <- keepWords(sciNames[i], 1)[[1L]]
            cat(val, "\n")
            out$tsn[i] <- get_tsn(keepWords(sciNames[i], 1)[[1L]],
                accepted = TRUE, rows = 2)
        } else {
            if (grepl(sciNames[i], pattern = "[Hh]ybrid[s ]| x ")) {
                ##---- Hybrids
                if (verbose)
                  cat("==> Hybrid detected....\n")
                wrd1 <- keepWords(sciNames[i], 1)[[1L]]
                wrd2 <- keepWords(sciNames[i], 4)[[1L]]

                if (!is.na(wrd2) & wrd1 != wrd2)
                  stp <- TRUE  #order are not the same
                out$tsn[i] <- get_tsn(keepWords(wrd1))
                out$hybrid[i] <- TRUE
            } else {
                ##---- Other
                out$tsn[i] <- get_tsn(sciNames[i], verbose = verbose)
                if (is.na(out$tsn[i])) {
                  if (verbose)
                    cat("==> Trying to remove subspecies names....\n")
                  ## test if subspecies
                  out$tsn[i] <- get_tsn(keepWords(sciNames[i], 1:2,
                    collapse = " ")[[1L]],
                    verbose = verbose)
                  ## test if there is a tsn for genus/order - this is our last attempt.
                  if (verbose)
                    cat("==> Using Genus only....\n")
                  if (is.na(out$tsn[i])) {
                    out$tsn[i] <- get_tsn(keepWords(sciNames[i], 1)[[1L]], verbose = verbose)
                  } else out$subspecies[i] <- keepWords(sciNames[i], 3)[[1L]]
                }
            }
        }
        ##
        if (verbose)
            cat("==> tsn is: ", out$tsn[i], "\n")
        ##
        if (!is.na(out$tsn[i])) {
            tmp <- classification(out$tsn[i], db = "itis")[[1L]] %>% as.data.frame
            for (j in cla) {
                id <- which(tmp$rank == j)
                if (length(id))
                  out[i, j] <- tmp$name[id]
            }
            if (stp) out$genus[i] <- NA
        }
    }
    out
}
