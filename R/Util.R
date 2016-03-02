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

##### prob2class #####
#' Convert a matrix of probabilities to a vector or classes
#'
#' Some model types and packages, when asked to predict new values, return probabilite that a given class will occur.
#' This function converts this format into a vector of classes (type factor).
#'
#' @param prob a matrix of probabilities with each column representing a different class.
#' @param classes (optional) the classes represented by \code{prob}. Must match the number of columns in \code{prob}.
#' @param threshold (optional) if the probability is less than this value, output 0 for the class as we don't have
#'   confidence in the result.
#'
#' @return A factor variable containing the most probable class indicated by the probability table. Note that if this
#'   function is passed a matrix with only one column or a vector of values, it will return a vector of the values.
#'   Care should be taken as this function does not always return the same type (factor)!
#' @seealso \code{\link{isCat}}, \code{\link{isCont}}, and \code{\link{factorValue}} for help in handling the result.
#' @export
#' @examples
#' data ('siteData')
#' modelRun <- generateModels (data = siteData,
#'                             modelTypes = c ('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm'),
#'                             x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
#'                             y = 'ecoType',
#'                             grouping = ecoGroup[['domSpecies','transform']])
#' m <- modelRun$gbm
#' p <- buildPredict(m)(m, getData(m))
#' prob2class(p)
prob2class <- function(prob, classes=colnames(prob), threshold=0) {
  if (class(prob) != 'matrix') prob <- as.matrix(prob)
  if (ncol(prob) == 1) {
    warning('prob2class: expecting a matrix of probabilities, returning the found a vector unchanged')
    return (as.vector(prob[,1]))
  }
  if (ncol(prob) != length (classes)) stop("prob2class: number of classes must match the number of columns")

  as.factor(colnames(prob)[ apply(prob, 1, which.max) ])
}
