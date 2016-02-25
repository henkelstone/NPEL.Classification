# Utility functions to simplify/assist in computation within the NPEL Classification package
# Created 9.Oct.2015 from pre-existing code file started 6.Apr.2015

##### sortLevels #####
#' Sort the levels in a factor
#' @param x the factor to process
#' @return the same factor variable with levels sorted
#' @export
sortLevels <- function (x) { factor(x,levels=sort(as.numeric(levels(x)))) }

##### trimLevels #####
#' Trim the levels so the only levels present are ones that actually have data
#' @param x the factor to process
#' @return the same data with the levels trimmed to reflect the factors that are present
#' @export
trimLevels <- function (x) { factor(x,levels=levels(factor(x))) }

##### mergeLevels #####
#' Merge the levels of two factors; the factors of y not present in x will be added to the x levels.
#' @param x the factor to which a the levels of...
#' @param y the factor for which to extract the added levels
#' @return the factor x with the levels of y merged to the x levels
#' @export
mergeLevels <- function (x,y) { sortLevels( factor(x,union(levels(x),levels(y))) ) }

##### factorValues #####
#' Retrieve the values within a factor object
#'
#' This function retrieves the *values* within a factor object. There is a subtle but serious gotcha when using factors
#' that is outlined in the help for factor() under the Warning section. Notably, then using factors, what is returned
#' is the *indices* of the factor, not the values stored in the factor. This is fairly easy to catch when factors are
#' type character, but can cause serious bugs when they are numeric. The purpose of this function is to provide an easy
#' way to return the values stored within a factor
#'
#' @param x the factor from which to extract (numeric) values
#' @return the (numeric) values stored in the factor
#' @export
factorValues <- function (x) { as.numeric(as.character(x)) }

##### deg2rad #####
#' Convert degrees to radians
#' @param x the input angle (in degrees)
#' @return The angle in radians
#' @export
deg2rad <- function (x) { x*pi/180 }

##### rad2deg #####
#' Covert radians to degrees
#' @param x the input angle (in radians)
#' @return The angle in degrees
#' @export
rad2deg <- function (x) { x*180/pi }

