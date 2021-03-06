% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Util.R
\name{factorLevels}
\alias{factorLevels}
\title{Retrieve the levels within a factor object}
\usage{
factorLevels(x)
}
\arguments{
\item{x}{the factor from which to extract the levels}
}
\value{
the levels; this will be numeric if possible, character otherwise
}
\description{
This function is essentially the same as the levels command in base, but it will type-cast the return to numeric when
that is valid.
}
