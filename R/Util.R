# Utility functions to simplify/assist in computation within the NPEL Classification package
# Created 9.Oct.2015 from prexisting code file started 6.Apr.2015

#' Sort the levels in a factor
#'
#' @param x the factor to process
#'
#' @return the same data with levels sorted
sort.levels <- function (x) {
  return ( factor(x,levels=sort(as.numeric(levels(x)))) )
}

#' Trim the levels so the only levels present are ones that actually have data
#'
#' @param x the factor to process
#'
#' @return the same data with the levels trimmed to reflect the factors that are present
trim.levels <- function (x) {
  return ( factor(x,levels=levels(factor(x))) )
}

#' Merge the levels of two factors; the factors of y not present in x will be added to the x levels.
#'
#' @param x the factor to process
#'
#' @return the factor x with the levels of y merged to the x levels
merge.levels <- function (x,y) {
  x <- factor(x,union(levels(x),levels(y)))
  return ( sort.levels(x) )
}

#' Merge the levels of two factors; the factors of y not present in x will be added to the x levels.
#'
#' @param x the factor to process
#'
#' @return the factor x with the levels of y merged to the x levels
merge.levels <- function (x,y) {
  x <- factor(x,union(levels(x),levels(y)))
  return ( sort.levels(x) )
}

#' Retrieve the values within a factor object
#'
#' This function retrieves the *values* within a factor object. There is a subtle but serious gotcha when using factors
#' that is outlined in the help for factor() under the Warning section. Notably, then using factors, what is returned
#' is the *indicies* of the factor, not the values stored in the factor. This is fairly easy to catch when factors are
#' type character, but can cause serious bugs when they are numeric. The purpose of this function is to provide an easy
#' way to return the values stored within a factor
factorValues <- function (x) {
  as.numeric(as.character(x))
}

#' Find the minimum extent of a collection of Raster* object
#'
#' @param ... the Raster* objects to compare
#'
#' @return the minimum extent of all Raster* objects
minExtent <- function (...) {
  inL <- eval(substitute(alist(...)))
  ex <- extent(eval(inL[[1]]))
  for (l in inL) {
    ex <- extent(c(max(ex@xmin,extent(eval(l))@xmin),
                   min(ex@xmax,extent(eval(l))@xmax),
                   max(ex@ymin,extent(eval(l))@ymin),
                   min(ex@ymax,extent(eval(l))@ymax)))
  }
  return (ex)
}
