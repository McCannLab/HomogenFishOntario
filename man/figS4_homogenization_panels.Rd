% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figS4_homogenization_panels.R
\name{figS4_homogenization_panels}
\alias{figS4_homogenization_panels}
\title{Draw Figure S4}
\usage{
figS4_homogenization_panels(cex_bas = 4)
}
\arguments{
\item{cex_bas}{magnification coefficient, controls the size of plot elements.}
}
\description{
Create rarefaction curves, test for homogenization and draw the
three figures, each includes the four panels for one of the three different
time periods. Note that the figures are combined afterward with ImageMagick
\url{https://imagemagick.org/script/develop.php} (see Makefile included in the
GitHub repository of this package). Also, all the details to draw the panels
\code{figS4_panel} that is not exported.
}
