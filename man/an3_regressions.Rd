% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/an3_regressions.R
\name{an3_regressions}
\alias{an3_regressions}
\title{Regressions}
\usage{
an3_regressions(x, y, trace = FALSE, dredge = FALSE)
}
\arguments{
\item{x}{output of \code{\link[=an1_gain_loss]{an1_gain_loss()}}.}

\item{y}{output of \code{\link[=an2_occurrence_species]{an2_occurrence_species()}}.}

\item{trace}{a logical. If \code{TRUE}, information is printed during the running of \code{\link[stats:step]{stats::step()}} and \code{\link[MuMIn:dredge]{MuMIn::dredge()}}.}

\item{dredge}{a logical. Should dredging be performed? See \code{details}.}
}
\value{
A list of models.
}
\description{
Perform the regressions described in the study.
}
\details{
\code{dredge} was only used to respond to one reviewer comment, we nonetheless #' kept it to show how to use it (but results are not exported).
}
\examples{
x <- an1_gain_loss(sf_bsm_ahi)
y <- an2_occurrence_species(x)
an3_regressions(x, y)
}
