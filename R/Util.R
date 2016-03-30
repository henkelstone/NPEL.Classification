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
mergeLevels <- function (x,y) {
  levels(x) <- gsub("(?<![0-9])0+", "", levels(x), perl = TRUE)  # Union has a little bug that it automatically strips leading zeros from character strings; this means
  levels(y) <- gsub("(?<![0-9])0+", "", levels(y), perl = TRUE)  # if we call this successively we get class "2" and "02" which are 'different'. This is a work around.
  sortLevels( factor(x,union(levels(x),levels(y))) )
}

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
#' Convert degrees to radians and vice versa
#' @param x the input angle in degrees (or radians)
#' @return The angle in radians (or degrees)
#' @export
deg2rad <- function (x) { x*pi/180 }

##### rad2deg #####
#' @rdname deg2rad
#' @export
rad2deg <- function (x) { x*180/pi }

##### prob2class #####
#' Convert a matrix of probabilities to a vector of classes
#'
#' Some model types and packages, when asked to predict new values, return the probability that a given class will occur.
#' This function converts this format into a vector of classes (type factor).
#'
#' @param prob a matrix of probabilities with each column representing a different class.
#' @param classes (optional) the classes represented by \code{prob}. Must match the number of columns in \code{prob}.
#' @param threshold (optional) if the probability is less than this value, output 0 for that point as we don't have
#'   confidence in the result.
#'
#' @return A factor variable containing the most probable class indicated by the probability table. Note that if this
#'   function is passed a matrix with only one column or a vector of values, it will return a vector of the values.
#'   Care should be taken as this function does not always return the same type.
#'
#' @seealso \code{\link{isCat}}, \code{\link{isCont}}, and \code{\link{factorValue}} for help in handling the result.
#' @export
#' @examples
#' data ('siteData')
#' modelRun <- generateModels (data = siteData,
#'                             modelTypes = suppModels,
#'                             x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
#'                             y = 'ecoType',
#'                             grouping = ecoGroup[['domSpecies','transform']])
#' m <- modelRun$gbm
#' p <- buildPredict(m)(m, getData(m))
#' prob2class(p)
prob2class <- function(prob, classes=colnames(prob), threshold=0) {
  if (class(prob)[1] != 'matrix') {
    if (class(prob)[1] == 'factor') { warning('prob2class: expecting a matrix of probabilities, returning the found factor unchanged. Did you accidentally pass a collection of values?'); return (prob) }
    if (class(prob)[1] == 'vector') { warning('prob2class: expecting a matrix of probabilities, returning the found vector unchanged. Did you accidentally pass a collection of values?'); return (prob) }
    prob <- as.matrix(prob)
    if (ncol(prob) == 1) {
      warning('prob2class: expecting a matrix of probabilities, returning the found vector unchanged. Did you accidentally pass a collection of values?')
      return (as.vector(prob[,1]))
    }
  }
  if (ncol(prob) != length (classes)) stop("prob2class: number of classes must match the number of columns")
  sortLevels(as.factor(colnames(prob)[ apply(prob, 1, which.max) ]))
}

##### fx2vars #####
#' Convert formula object to x,y variables and vice versa
#'
#' This is a utility function that takes either a formula object, or two vectors of variable names and converts one to the other.
#' Alternatively, if \emph{neither} of these are supplied, then it will assume the first entry in \code{names} is the y value and the rest
#' are the x columns. If names is provided, the function will check that the results are consistent with the data frame from which the
#' names were extracted.
#'
#' @section Warning:
#' Note, use this function with caution as it changes your calling parameters! See Value for more information...
#'
#' @param fx (optional) a formula object from which to extract x and y variable names.
#' @param x (optional) a character vector of predictor variables.
#' @param y (optional) the name of a dependent variable.
#' @param names (optional) a complete list of names; it will only used to compute variable names if no other variables are supplied, but it
#'   will be used to check the validity of the variables if it is given; hence, providing all the names of the variables in the proposed
#'   data frame can be an easy and useful check that all is as expected.
#'
#' @return Because R does not allow passing by reference, this function reaches into the parent frame and changes the variables that were
#'   passed to it. So, for example, if fx was supplied and x and y were not, this function call will change the calling environment such
#'   that the variables passed to x and y now store the values encoded by fx. In a similar way, if fx is not specified, but x and y are,
#'   then it will set fx in the parent environment to a formula object representing the variables from x and y.
#'
#' @export
#'
#' @examples
#' fx <- x <- y <- NULL
#' fx2vars(formula('a~b+c'), x, y)
#' c (fx, x, y)  # fx is still NULL because it wasn't passed to the call
#'
#' fx <- x <- y <- NULL
#' fx2vars(fx, c('b','c'), 'a')
#' c (fx, x, y)  # likewise for a and b
#'
#' \dontrun{
#' fx <- x <- y <- NULL
#' fx2vars(formula('a~b+c'), x, y, names=c('a','b'))
#' c (fx, x, y)  # x has been trimmed to only contain 'b' as 'c' does not occur in names }
fx2vars <- function (fx=NULL, x=NULL, y=NULL, names=NULL) {
  fxRet <- substitute(fx)
  xRet <- substitute(x)
  yRet <- substitute(y)

  if (!is.null(fx)) {                   # fx is provided so pull out x and y
    x <- attr(terms(fx),'term.labels')
    y <- as.character(terms(fx)[[2]])
  } else {                              # fx is NOT provided
    if (is.null(y)) {                   # if y is NOT provided then see if we have default values
      if (!is.null(names)) {            # if we have defaults, use them
        y <- names[1]
      } else stop("fx2vars: needs some data; supply either a formula, x and y, and/or a list of names.")
    }
  }

  # Check y is legit
  if (length(y) != 1) stop ("fx2vars: multivariate analysis not supported; specify only a single variable for y.")
  if (!is.null(names))
    if (!y %in% names)  stop ("fx2vars: variable specified for y does not occur in the dataset; check the column names.")

  # Handle the x variable(s)
  if (is.null(x)) {                     # x is null so get default values if available
    if (!is.null(names)) {
      x <- names[-match(y,names)]
    } else stop("fx2vars: cannot find a suitable default for x variables; provide fx and/or names.")
  } else {                              # x is provided so check if it's legit (if there's information)
    if (!is.null(names)) {
      defaultCols <- names[-match(y,names)]
      if (any(!x %in% defaultCols)) {
        stop("fx2vars: a column specified for x does not occur in the dataset.")
        # x <- x[x %in% defaultCols]
      }
    } else if (y %in% x) stop("fx2vars: x cannot contain the y variable.")
  }

  if (class(fxRet)=='name') assign(deparse(fxRet), formula(paste0(y,'~',paste0(x,collapse='+'))),envir=parent.frame())
  if (class(xRet)=='name')  assign(deparse(xRet), x, envir=parent.frame())
  if (class(yRet)=='name')  assign(deparse(yRet), y, envir=parent.frame())
}
