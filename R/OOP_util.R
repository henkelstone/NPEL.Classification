# Functions oriented towards making NPEL.
# Classification more OOP This targets the idea of making more maintainable,
#   easier to test, and easier to add functionality.
# Created 9.Oct.2015 from pre-existing code file started 6.Apr.2015

########## Get Methods ##########

##### isCat #####
#' Is the object categorical or continuous
#'
#' Checks if an object is categorical (or continuous). Currently accepts either a vector, presumably from a data frame,
#' or a model object. In the case of the latter, it will check whether the model was built using categorical data.
#'
#' Note that it cannot be assume that if a model fails \code{isCat} it will pass \code{\link{isCont}}; both functions test for the
#' conditions which it expects to hold, and there may be cases in which neither set of conditions is fully met.
#'
#' @param x the object to test
#' @return TRUE if the object is categorical/continuous, or is a model built on categorical/continuous data, FALSE otherwise
#' @export
#' @examples
#' isCat(siteData$ecoType)
#' isCat(siteData$slope)
#' \dontrun{
#' isCat(model) }
#'
#' isCont(siteData$ecoType)
#' isCont(siteData$slope)
#' \dontrun{
#' isCont(model) }
isCat <- function(x) { UseMethod('isCat') }
#' @export
isCat.default <- function(x){
  if (class(x) %in% c('factor','character','logical')) return(TRUE)
  if (class(x) %in% c('numeric','integer')) return(FALSE)
  if (class(x) %in% c('array','matrix','data.frame')) {
    if (length(dim(x)) > 1) {
      warning ("isCat: expects only a single column; running test on the first column")
      x <- x[,1,drop=TRUE]
    }
    return( isCat(as.vector(as.matrix(x))) ) # Strip off data.frame, array, or matrix type
  }
  stop("isCat: unexpected class passed to isCat")
}
#' @export
isCat.randomForest <- function(x) {
  if (x$type == 'classification') return(TRUE)
  if (x$type == 'regression') return(FALSE)
  stop("isCat: unrecognized type of randomForest")
}
#' @export
isCat.rfsrc <- function(x) { x$family %in% c('class','class+') }
#' @export
isCat.fnn.FNN <- function(x) { TRUE }
#' @export
isCat.fnn.class <- function(x) { TRUE }
#' @export
isCat.kknn <- function(x) {
  if (x$response %in% c('ordinal','nominal')) return(TRUE)
  if (x$response == 'continuous') return(FALSE)
  stop("isCont: unrecognized type of kknn")
}
#' @export
isCat.gbm <- function(x) { x$num.classes > 1 }
#' @export
isCat.svm <- function(x) { any(x$type == 0:2) }

##### isCont #####
#' @rdname isCat
#' @export
isCont <- function(x) { UseMethod('isCont') }
#' @export
isCont.default <- function(x){
  if (class(x) %in% c('factor','character','logical')) return(FALSE)
  if (class(x) %in% c('numeric','integer')) return(TRUE)
  if (class(x) %in% c('array','matrix','data.frame')) {
    if (length(dim(x)) > 1) {
      warning ("isCont: expects only a single column; running test on the first column")
      x <- x[,1,drop=TRUE]
    }
    return (isCont(as.vector(as.matrix(x))) ) # Strip off data.frame, array, or matrix type
  }
  stop("isCont: unexpected class passed to isCont")
}
#' @export
isCont.randomForest <- function(x) {
  if (x$type == 'regression') return(TRUE)
  if (x$type == 'classification') return(FALSE)
  stop("isCont: unrecognized type of randomForest")
}
#' @export
isCont.rfsrc <- function(x) { x$family %in% c('regr','regr+') }
#' @export
isCont.fnn.FNN <- function(x) { FALSE }
#' @export
isCont.fnn.class <- function(x) { FALSE }
#' @export
isCont.kknn <- function(x) {
  if (x$response == 'continuous') return(TRUE)
  if (x$response %in% c('ordinal','nominal')) return(FALSE)
  stop("isCont: unrecognized type of kknn")
}
#' @export
isCont.gbm <- function(x) { x$num.classes == 1 }
#' @export
isCont.svm <- function(x) { any(x$type == 3:4) }

##### getData #####
#' Extract the original data from an object
#'
#' This function recovers the original data from a model object. Since some of the modelling objects that this package
#' uses do not store the data, this function cannot reliably extract the data from an model object not generated using
#' generateModel. If the data is not found in the model, an error will be thrown.
#'
#' @note The nearest neighbour models in package:FNN and package:class do not enclose their results in a class; when
#'   NPEL.Classification builds objects of these types it wraps them in a class so they are recognizable by S3 methods,
#'   and attaches the formula and data. Hence, if a model was built directly using these packages the result will not
#'   run this function.
#' @param model is the model for which to extract the data.
#' @section Warning:
#'   If you are getting very strange errors from getData, consider this\ldots The raster package also has a getData
#'   function that is not overloaded. Depending on where the two packages fall in the search path, it may be that
#'   raster::getData is getting called instead of NPEL.Classification::getData. See help on \code{\link{search}} for a
#'   starting point on search paths. The following hack may help.\cr\cr
#'   \code{detach (package:NPEL.Classification)}
#'   \code{library (NPEL.Classification)}
#' @return a data frame of the data used to generate the model.
#' @export
getData <- function(model) {
  UseMethod("getData")
}
#' @export
getData.randomForest <- function(model) {
  if (is.null(attr(model,'data'))) stop ("getData: the randomForest object does not have data attached; did you generate this model using generateModels?")
  as.data.frame(attr(model,'data'))
}
#' @export
getData.rfsrc <- function(model) {
  df <- cbind(model$yvar,model$xvar)
  names (df) <- c(model$yvar.names,model$xvar.names)
  as.data.frame(df)
}
#' @export
getData.fnn.FNN <- function(model) {
  df <- cbind(model$classes,model$train)
  names (df) <- c(model$formula[[2]], attr(terms(model$formula),'term.labels'))
  as.data.frame(df)
}
#' @export
getData.fnn.class <- function(model) { getData.fnn.FNN(model) }
#' @export
getData.kknn <- function(model) {
  as.data.frame(model$data)
}
#' @export
getData.gbm <- function(model) {
  gbm::reconstructGBMdata(model)
}
#' @export
getData.svm <- function(model) {
  warning('getData: SVM not supported yet')
}

##### getClasses #####
#' Recover the classes that a model uses
#'
#' This function recovers the classes present in this model That is, it returns the levels used in the original
#' predicted variable. Of course, this is only relevant if the model was built using categorical data; otherwise
#' an error will be returned.
#'
#' @note The nearest neighbour models in package:FNN and package:class do not enclose their results in a class; when
#'   NPEL.Classification builds objects of these types it wraps them in a class so they are recognizable by S3 methods,
#'   and attaches the formula and data. Hence, if a model was built directly using these packages the result will not
#'   run this function.
#' @param model is the model from which to recover the classes
#' @return a character vector of classes
#' @export
getClasses <- function(model) {
  if (!isCat(model)) stop("getClasses: required categorical data")
  UseMethod("getClasses")
}
#' @export
getClasses.randomForest <- function(model) { model$classes }
#' @export
getClasses.rfsrc <- function(model) { levels(model$class) }
#' @export
getClasses.fnn.FNN <- function(model) { levels(model$classes) }
#' @export
getClasses.fnn.class <- function(model) { levels(model$classes) }
#' @export
getClasses.kknn <- function(model) { levels(model$data[ ,as.character(model$terms[[2]]) ]) }
#' @export
getClasses.gbm <- function(model) { model$classes }
#' @export
getClasses.svm <- function(model) { warning('getClasses: SVM not supported yet') }

##### getProb #####
#' Extract the probabilities (confidence) from a model; the probabilities of class occurrence.
#'
#' This is a utility function to simplify and standardize access to model probabilities. The situation is a bit fiddly because not all
#' models return probabilities (e.g. kknn), and those that do are not all in the same format (e.g. fnn.* returns only a single vector of
#' probabilities for the most likely class). This function will return the same structure regardless of model type encountered.
#' \itemize{
#'   \item for models that compute probabilities by class, these probabilities will be returned;
#'   \item for models that compute only the probability of the most likely class, this function will put that probability in the correct
#'   column and will fill the remaining cells in the row with equal values such that the row total is 1. Note that these are \emph{not} the
#'   true probabilities, but merely inserted so the assumption that row totals are 1 is satisfied.
#'   \item for models that do not compute probabilities, 1 will be inserted in the correct column and the remainder will be set to zero.
#' }
#' Also note that this function is only useful for categorical data (\code{\link{isCat} returns \code{TRUE}}).
#'
#' @param model the model from which to recover the probability table
#' @return a matrix of probabilities in which columns are labelled with the class names
#'
#' @seealso \code{\link{isCat}}, and \code{\link{isCont}}.
#' @export
getProb <- function(model) {
  if (!isCat(model)) stop ("getProb: requires categorical data")
  UseMethod("getProb")
}
#' @export
getProb.randomForest <- function(model) { model$votes }
#' @export
getProb.rfsrc <- function(model) { model$predicted.oob }
#' @export
getProb.fnn.FNN <- function(model) {
  k <- attr(model,'args')$k
  n <- length(getFitted(model))
  m <- levels(sortLevels(getFitted(model)))
  prob <- attr(model$fnn,'prob')

  ret <- matrix(0, nrow=n, ncol=length(m))
  colnames(ret) <- m
  if (k > 1)
    ret[,1:length(m)] <- (1-prob)/(length(m)-1)

  # A bit of a hack... need to compensate for missing levels
  tmp <- factorValues(getFitted(model))
  dim(tmp) <- c(length(tmp),1)
  ret[cbind( 1:n,apply (tmp,1,function(x){which(x == m)}) )] <- prob
  ret
}
#' @export
getProb.fnn.class <- function(model) { getProb.fnn.FNN(model)}
#' @export
getProb.kknn <- function(model) {
  n <- length(getFitted(model))
  m <- levels(sortLevels(getFitted(model)))

  ret <- matrix(0, nrow=n, ncol=length(m))
  colnames(ret) <- m
  m <- length(m)
  ret[cbind(1:n,factorValues(getFitted(model)))] <- 1
  ret
}
#' @export
getProb.gbm <- function(model) {
  # I don't know how to transform gbm$fit into probability space... but this work around works.
  buildPredict (model)(model,getData(model))
}
#' @export
getProb.svm <- function(model) { warning('getProb: SVM not supported yet') }

##### getFormula #####
#' Extract the formula used to generate a model object
#'
#' This function will return a formula object from the specified model. The underlying code here uses only the model
#' object, and casts the result into a formula object even if the model does not natively store the formula in that
#' format. As a result this function should for all model types this package uses, even if it wasn't built by
#' generateModels.
#'
#' @note The nearest neighbour models in package:FNN and package:class do not enclose their results in a class; when
#'   NPEL.Classification builds objects of these types it wraps them in a class so they are recognizable by S3 methods,
#'   and attaches the formula and data. Hence, if a model was built directly using these packages, the result will not
#'   run this function.
#' @param model is the model for which to extract the formula.
#' @return a "formula" object representing the parameters used to generate the model.
#' @export
getFormula <- function(model) {
  UseMethod("getFormula")
}
#' @export
getFormula.randomForest <- function(model) { formula(model$terms) }
#' @export
getFormula.rfsrc <- function(model) { formula( paste(model$yvar.names,'~',paste(model$xvar.names,collapse='+')) ) }
#' @export
getFormula.fnn.FNN <- function(model) { model$formula }
#' @export
getFormula.fnn.class <- function(model) {  model$formula }
#' @export
getFormula.kknn <- function(model) { formula(model$terms) }
#' @export
getFormula.gbm <- function(model) { formula(model$Terms) }
#' @export
getFormula.svm <- function(model) { warning('getFormula: SVM not supported yet') }

##### getArgs #####
#' Extract the arguments used to generate a model object
#'
#' This function will recover the arguments used when generating this model object. It does not recover all arguments,
#' but merely those relating to this model object.
#'
#' @note The nearest neighbour models in package:FNN and package:class do not enclose their results in a class; when
#'   NPEL.Classification builds objects of these types it wraps them in a class so they are recognizable by S3 methods,
#'   and attaches the formula and data. Hence, if a model was built directly using these packages, the result will not
#'   run this function.
#' @param model is the model for which to extract the arguments
#' @return a list of arguments
#' @export
getArgs <- function(model) {
  attr(model,'args')
}

##### getFitted #####
#' Extract the fitted data from a model object
#'
#' This function returns the fitted data that the model was built on; that is, if we 'predict' this model using the same data we used to
#' generate the model, we would get this as a result.
#'
#' There is a subtle but significant point about getFitted: it cannot be assumed that \code{predict} called with the same data used to
#' generate the model will give the same results as getFitted. In general, when the original data is dropped down through a classifier, it
#' will return the original results. It makes sense: if this is a known data point, then why not return the known class. However, in order to
#' estimate accuracy, most packages will use some type of technique to give a dataset that shows what the model \emph{would} have return if
#' the datapoint had \emph{not} been included in the original data. In all cases, getFitted returns this dataset: the one that can be used
#' for error estimation. \code{\link{buildPredict}} was designed for classifying new data; what it returns when original data is used is
#' dependent on both the type of classifier and the implementation of the package.
#' \itemize{
#'   \item Random Forest: both packages will estimate the accuracy based on data that falls out-of-bag (OOB); that is, when a data point is
#'   not directly represented in a tree, that data point can be used to estimate the accuracy of that tree. The technique is akin to a
#'   leave-one-out validation method.
#'   \item Nearest Neighbour: the issue here is identical. getFitted is estimated using a leave-out-out cross-validation approach for all
#'   packages. The predict function will return the original data. In the case of the FNN package, the nearest-neighbour index is included in
#'   the model results; the \emph{second} nearest neighbour returned by predict matches the values returned by getFitted.
#'   \item GBM: in this case, the values of getFitted match those returned by predict.
#' }
#'
#' @note The nearest neighbour models in package:FNN and package:class do not enclose their results in a class; when NPEL.Classification
#'   builds objects of these types it wraps them in a class so they are recognizable by S3 methods, and attaches the formula and data.
#'   Hence, if a model was built directly using these packages the result will not run this function.
#' @section Warning:
#'   Note there is a bug in FNN (and possibly the \pkg{class} package) that means that the class reported, and the associated probability
#'   is incorrect. As would be expected it's a bit subtle, but it goes as follows:
#'   \itemize{
#'     \item the data is correct when it is returned from the C function;
#'     \item in cases where k > 1, i.e. we are looking for more than the first nearest neighbour, it uses \code{table} to figure out if any
#'       of the nearest neighbours are of the same class;
#'     \item if so, it returns that class as the most likely class with the probability of it's occurrence;
#'     \item if not, it returns the first class in the list.
#'   }
#'   However, table sorts the input data, so the first class in the list is \emph{not necessarily} the closest neighbour. Hence, in cases
#'   where there is no repetition of classes, that is, all the nearest \code{k} neighbours are of unique classes, the function returns the
#'   first alpha-numeric class, \emph{not} the class of the nearest neighbour. I have written getFitted to return the class of the nearest
#'   neighbour, but it does not check for repeat classes in the k nearest neighbours. I was not able to do the same for the \pkg{class}
#'   package as the C function does not return the indices of nearest neighbours.
#'
#' @param model is the model for which to extract the fitted data
#' @return a factor variable containing the fitted data
#' @export
getFitted <- function(model) {
  UseMethod("getFitted")
}
#' @export
getFitted.randomForest <- function(model) {
  pred <- model$predicted
  names(pred) <- NULL
  pred
}
#' @export
getFitted.rfsrc <- function(model) {
  if (isCat(model)) return(model$class.oob)
  return (as.numeric(model$predicted.oob))
}
#' @export
getFitted.fnn.FNN <- function(model) {
  y <- as.character(getFormula(model)[[2]])
  d <- getData(model)[,eval(y)]
  d[attr(model$fnn,'nn.index')[,1]]
}
#' @export
getFitted.fnn.class <- function(model) { model$fnn }
#' @export
getFitted.kknn <- function(model) { model$fitted.values[[ which(sapply(model$fitted.values,function(x){ attr(x,'kernel') == model$best.parameters$kernel && attr(x,'k') == model$best.parameters$k })) ]] }
#' @export
getFitted.gbm <- function(model) {
  pred <- predict(model, getData(model), (function(){ capture.output(suppressWarnings(tmp<-gbm::gbm.perf(model,plot.it=FALSE))); tmp })() )
  if (isCat(model)) return( factor(model$classes[apply(pred,1,which.max)]) )
  return(pred)
}
#' @export
getFitted.svm <- function(model) { warning('getFitted: SVM not supported yet') }

##### getVIMP #####
#' Extract the VIMP if it is present
#'
#' This function will extract the Variable Importance data if the model contains it, otherwise it attempts to compute it
#' using a leave-one-out algorithm.
#'
#' @note The nearest neighbour models in package:FNN and package:class do not enclose their results in a class; when
#'   NPEL.Classification builds objects of these types it wraps them in a class so they are recognizable by S3 methods,
#'   and attaches the formula and data. Hence, if a model was built directly using these packages the result will not
#'   run this function.
#' @param model is the model for which to extract the VIMP
#' @return a data frame with the VIMP data if it is present, otherwise NULL
#' @export
getVIMP <- function(model) {
  UseMethod("getVIMP")
}
#' @export
getVIMP.randomForest <- function(model) {
  if (getArgs(model)$importance == 'none') return (npelVIMP (model, calc=TRUE, echo=FALSE))
  if (is.null(model$importance)) {
    warning("getVIMP.randomForest: model does not have VIMP attached even though specified it does; generating")
    return(npelVIMP (model, calc=TRUE, echo=FALSE))
  }
  tmp <- as.data.frame(model$importance)
  cbind (MeanDecreaseAccuracy=tmp[,ncol(tmp)-1],tmp[,1:(ncol(tmp)-2)])
}
#' @export
getVIMP.rfsrc <- function(model) {
  if (getArgs(model)$importance == 'none') return (npelVIMP (model, calc=TRUE, echo=FALSE))
  if (is.null(model$importance)) {
    warning("getVIMP.rfsrc: model does not have VIMP attached even though specified it does; generating")
    return(npelVIMP (model, calc=TRUE, echo=FALSE))
  }
  as.data.frame(model$importance)
}
#' @export
getVIMP.fnn.FNN <- function(model) {
  npelVIMP (model, calc=TRUE, echo=FALSE)
}
#' @export
getVIMP.fnn.class <- function(model) {
  npelVIMP (model, calc=TRUE, echo=FALSE)
}
#' @export
getVIMP.kknn <- function(model) {
  npelVIMP (model, calc=TRUE, echo=FALSE)
}
#' @export
getVIMP.gbm <- function(model) {
  requireNamespace("gbm")
  tmp <- npelVIMP (model, calc=TRUE, echo=FALSE)                      # GBM doesn't compute by class VIMP so do it here; be sure this is calc=TRUE so we don't get an infinite loop!
  cbind (tmp, summary(model,order=FALSE,plotit=FALSE)[,2,drop=FALSE]) # But it has a special kind of VIMP: relative influence; append it
}
#' @export
getVIMP.svm <- function(model) { warning('getVIMP: SVM not supported yet') }


########## Higher level functions ##########

##### buildModel #####
#' Builds a model of the specified type
#'
#' This method builds a model of the requested type. It is passed data, a formula object, and whatever arguments that model type would
#' ordinarily use (see details).
#'
#' @details
#' Model building is such a primary part of this package, but each modelling package does not use the same set of calls or parameters to
#' build models. This function is a wrapper that builds models of using a standard interface. It converts from the normal suite of
#' parameters: a data frame, a formula object accessing variables within the data frame, and various arguments specifying the particulars
#' required by that model package. See \code{\link{generateModels}} for more information on arguments that can/should be passed.
#'
#' It also wraps results from packages that do not return an S3 class in a class object by attaching a class attribute. For for modelling
#' packages that return objects, this package uses the same class names for interoperability, however, in some cases it attaches data that
#' are not included by that class, but are required by NPEL.Classification.
#'
#' @seealso generateModels
#' @param type the type of model to build
#' @param data a data frame specifying data
#' @param fx a model formula object
#' @param args (optional) other arguments to pass to the model.
#' @return a model object of the type requested, with the class set to match the constants used in this package
#' @export
buildModel <- function(type,data,fx,args=NULL) {
  # This function can be called either from generateModels, or from other internal functions e.g. npelVIMP. As a result, it could be passed
  # a type that is one of the types listed above, i.e. internal types, or it could be passed a class name. These match in all but two cases,
  # so do a minor check on the type variable before calling the resulting function.
  class (data) <- c(type,class(data))
  .buildModel(data,fx,args)
}

#' @export
.buildModel <- function(data,fx,args=NULL) {
  UseMethod('.buildModel')
}
#' @export
.buildModel.randomForest <- function(data,fx,args=NULL) {
  if (is.null(args)) args$importance <- 'none'
  model <- do.call(randomForest::randomForest, c(list(formula=quote(fx), data=quote(data), importance=(args$importance!='none')), args[names(args)!='importance']))
  structure (.Data=model, data=data, args=args, class=rev(class(model)))
}
#' @export
.buildModel.rfsrc <- function(data,fx,args=NULL) {
  model <- do.call(randomForestSRC::rfsrc, c(list(formula=quote(fx), data=quote(data)), args))
  structure (.Data=model, args=args)
}
#' @export
.buildModel.fnn.FNN <- function(data,fx,args=NULL) {
  x <- attr(terms(fx),'term.labels')
  y <- as.character(terms(fx)[[2]])
  if (args$scale) data <- cbind (data[,match(y,names(data)),drop=FALSE],scale(data[,x]))
  model <- do.call(FNN::knn.cv, list(train=quote(data[,x]), cl=quote(data[,y]), prob=TRUE, k=args$k))
  structure (list(fnn=model, formula=fx, train=data[,x], classes=data[,y]), args=args[c('k','scale')], class='fnn.FNN')
}
#' @export
.buildModel.fnn.class <- function(data,fx,args=NULL) {
  x <- attr(terms(fx),'term.labels')
  y <- as.character(terms(fx)[[2]])
  if (args$scale) data <- cbind (data[,match(y,names(data)),drop=FALSE],scale(data[,x]))
  model <- do.call(class::knn.cv, list(train=quote(data[,x]), cl=quote(data[,y]), prob=TRUE, k=args$k))
  structure (list(fnn=model, formula=fx, train=data[,x], classes=data[,y]), args=args[c('k','scale')], class='fnn.class')
}
#' @export
.buildModel.kknn <- function(data,fx,args=NULL) {
  x <- attr(terms(fx),'term.labels')
  y <- as.character(terms(fx)[[2]])
  if (args$scale) data <- cbind (data[,match(y,names(data)),drop=FALSE],scale(data[,x]))
  model <- do.call(kknn::train.kknn, c(list(formula=quote(fx), data=quote(data)), args))
  structure (.Data=model, args=args[c('kmax','kernel','scale')], class=rev(class(model)))
}
#' @export
.buildModel.gbm <- function(data,fx,args=NULL) {
  capture.output(suppressWarnings(model <- do.call(gbm::gbm, c(list(formula=quote(fx), data=quote(data)), args))))
  attr(model,'args') <- args
  return (model)
}
#' @export
.buildModel.svm <- function(data,fx,args=NULL) {
  model <- do.call(e1071::svm, c(list(formula=quote(fx), data=quote(data)), args))
  structure (.Data=model, data=data, args=args, class=rev(class(model)))
}

##### buildPredict #####
#' A closure that generates a function call for predicting new data from a model
#'
#' This function returns a function that will predict data for the specified model type. This functionality is necessary because not all the
#' packages used by NPEL.Classification conform to the standard predict.* function model; hence this function standardizes the interface so
#' it is straightforward to predict data from any of the model types used, using the same interface.
#'
#' @note The nearest neighbour models in package:FNN and package:class do not enclose their results in a class; when NPEL.Classification
#'   builds objects of these types it wraps them in a class so they are recognizable by S3 methods, and attaches the formula and data.
#'   Hence, if a model was built directly using these packages, the result will not run this function.
#' @param model the model for which to build a predict function
#' @return a predict function for that model type.
#' @export
buildPredict <- function(model) {
  UseMethod("buildPredict")
}

#' @export
buildPredict.randomForest <- function(model) {
# RandomForest models have a built in predict function, but it cannot handle NA values so set these to 0.
  requireNamespace('randomForest')
  if (isCat(model)) {
    return ( function (model,data,...) {
      data[is.na(data)] <- 0
      return( predict(model,data,type='prob',...) )
    } )
  } else if(isCont(model)) {
    return ( function (model,data,...) {
      data[is.na(data)] <- 0
      return( predict(model,data,...) )
    } )
  } else stop("buildPredict.randomForest: model is neither categorical nor continuous")
}
#' @export
buildPredict.rfsrc <- function(model) {
# rfsrc returns probabilities when it's categorical and a vector when it's continuous data
  requireNamespace('randomForestSRC')
  return ( function (model,data,...) {
    return( randomForestSRC::predict.rfsrc(model,data,...)$predicted )
  } )
}
#' @export
buildPredict.fnn.FNN <- function(model) {
# Package::FNN doesn't have a predict function at all; manually strip off the NA values, then generate using FNN::knn
  requireNamespace('FNN')
  return ( function (model,data,...) {
    data[is.na(data)] <- 0
    if (getArgs(model)$scale) {
      x <- attr(terms(getFormula(model)),'term.labels')
      data <- scale(data[,x])
    }
    return ( FNN::knn(train=model$train, test=data[,names(model$train)], cl=model$classes, k=getArgs(model)$k,...) )
  } )
}
#' @export
buildPredict.fnn.class <- function(model) {
# Package::class also doesn't have a predict function; functionality is the same as that for package::FNN
  requireNamespace('class')
  return ( function (model,data,...) {
    data[is.na(data)] <- 0
    if (getArgs(model)$scale) {
      x <- attr(terms(getFormula(model)),'term.labels')
      data <- scale(data[,x])
    }
    return ( class::knn(model$train,data[,names(model$train)],model$classes,getArgs(model)$k,...) )
  } )
}
#' @export
buildPredict.kknn <- function(model) {
# kknn also doesn't have a predict function. Recover the parameters from the existing model
  requireNamespace('kknn')
  return ( function (model,data,...) {
    data[is.na(data)] <- 0
    if (getArgs(model)$scale) {
      x <- attr(terms(getFormula(model)),'term.labels')
      data <- as.data.frame(scale(data[,x]))
    }
    return( kknn::kknn(formula=model$terms,train=model$data,test=data,k=model$best.parameters[[2]],kernel=model$best.parameters[[1]],getArgs(model)[c('kmax','kernel')],...)$fitted.values)
  } )
}
#' @export
buildPredict.gbm <- function(model) {
# GBM requires that we recover the optimal n.tree; suppress the output from that call.
  requireNamespace('gbm')
  if (isCat(model)) {
    return ( function (model,data,...) {
      capture.output(suppressWarnings(n <- gbm::gbm.perf(model,plot.it=FALSE)))
      return( predict(model,data,n.trees=n,type='response',...)[,,1] )
    } )
  } else if (isCont(model)) {
    return ( function (model,data,...) {
      capture.output(suppressWarnings(n <- gbm::gbm.perf(model,plot.it=FALSE)))
      return( predict(model,data,n.trees=n,type='response',...) )
    } )
  } else stop("buildPredict.gbm: model is neither categorical nor continuous")
}
#' @export
buildPredict.svm <- function(model) {
# SVM returns the probabilities as an attribute.
  requireNamespace('e1071')
  return ( function (model,data,...) {
    return( attr(predict(model,data,probability=TRUE,na.action=na.pass,...),'probabilities') )
  } )
}

