# Functions used to generate and evaluate models
# Created 9.Oct.2015 from prexisting code file started 6.Apr.2015

#' An example of a transformation 'function'
#'
#' This dataset ia a list, cast in matrix format, that can be used to lump ecotypes into coarser groups for analysis and plotting.
#'   The current implementation includes vectors for: transformation, nameing, and plotting colours
#'
#' This default transformation 'function' maps several different grouping scenarios. Each is provided with labels for the groups
#'   as well as suggest colours, although in the case of identity this is merely the first \code{N} colours.
#' \tabular{lccc}{
#' \bold{Ecotype} \tab \bold{Dominant Species} \tab \bold{Dominant Group} \tab \bold{Maximum Granularity}\cr
#' BS 01 – Sand heather \tab 1 (barren) \tab 1 (barren) \tab 1 (sparse vegitation)\cr
#' BS 02 – Lichen felsenmeer \tab 1 \tab 1 \tab 1\cr
#' BS 03 – Jack pine, blueberry, lichen \tab 2 (pine) \tab 2 (conifer) \tab 2\cr
#' BS 04 – Jack pine, black spruce, feathermoss \tab 2 \tab 2 \tab 3\cr
#' BS 05 – Jack pine, birch, feathermoss \tab 2 \tab 2 \tab 4\cr
#' BS 06 – Jack pine, aspen, alder \tab 2 \tab 2 \tab 5\cr
#' BS 07 – Black spruce, blueberry, lichen \tab 3 (bs) \tab 2 \tab 6\cr
#' BS 08 – Black spruce, birch, lichen \tab 3 \tab 2 \tab 7\cr
#' BS 09 – Black spruce, pine, feathermoss \tab 3 \tab 2 \tab 8\cr
#' BS 10 – Black spruce, birch, feathermoss \tab 3 \tab 2 \tab 9\cr
#' BS 11 – White spruce, fir, feathermoss \tab 4 (ws) \tab 2 \tab 10\cr
#' BS 12 – White spruce, crowberry, feathermoss \tab 4 \tab 2 \tab 11\cr
#' BS 13 – Birch, black spruce, aspen \tab 5 (birch) \tab 3 (decid) \tab 12\cr
#' BS 14 – Birch, lingonberry, lab tea \tab 5 \tab 3 \tab 13\cr
#' BS 15 – Aspen, birch, alder \tab 6 (aspen) \tab 3 \tab 14\cr
#' BS 16 – Black spruce, poplar, alder swamp \tab 7 (swamp) \tab 4 (wetland) \tab 15 (group swamp in with treed bog)\cr
#' BS 17 – Black spruce bog \tab 8 (bog) \tab 4 \tab 15\cr
#' BS 18 – Lab tea shrubby bog \tab 8 \tab 4 \tab 16\cr
#' BS 19 – Graminoid bog \tab 8 \tab 4 \tab 17 (sparse bog)\cr
#' BS 20 – Open bog \tab 8 \tab 4 \tab 17\cr
#' BS 21 – Tamarack fen \tab 9 (fen) \tab 4 \tab 18 (upright fen)\cr
#' BS 22 – Leatherleaf fen \tab 9 \tab 4 \tab 18\cr
#' BS 23 – Willow shrubby fen \tab 9 \tab 4 \tab 19\cr
#' BS 24 – Graminoid fen \tab 9 \tab 4 \tab 20 (sparse fen)\cr
#' BS 25 – Open fen \tab 9 \tab 4 \tab 20\cr
#' BS 26 – Rush sandy shore \tab 10 (shore) \tab 4 \tab 21 (shoreline)\cr
#' BS 27 – Sedge rocky shore \tab 10 \tab 4 \tab 21\cr
#' }
#'
#' @section Warning:
#' The following expressions will not work as transformation functions. See \code{\link{factor}}, esp. the 'Warning' section.
#' \describe{
#'   \item{\code{vData$Ecotype}}{is a factor list, not a numeric list representing the factors}
#'   \item{\code{ecoGroup[['identity','transform']][vData$Ecotype]}}{will regroup based on the factor level \emph{indices}, NOT the factors}
#'   \item{\code{as.numeric(vData$Ecotype]}}{returns the factor \emph{indices}, not a numeric representation of the factors}
#'   \item{\code{(1:27)[vData$Ecotype]}}{returns the factor \emph{index} rather than the ecoGroup} }
#'
#' @usage ecoGroup[['which.grouping','type.of.data']]
#' @param which.grouping one of \code{c('identity','domSpecies','domGroup','maxGranularity'), where: \sQuote{identity} does no
#'   transformation, \sQuote{domSpecies} groups by the dominant species, \sQuote{domGroup} which groups based on
#'   coarse overstory groups, and \sQuote{maxGranularity} groups into what is deemed a maximum realistic granluarity
#'   given the the number of cases of each ecoType. See Details below for the specifics of each grouping. }
#' @param type.of.data one of \code{c('transform','labels','colours'), where: \sQuote{transform} is the lookup vector,
#'   \sQuote{labels} are meaningful names for each group, and \sQuote{colours} are suggest plotting colours. }
#'
#' @format A matrix list with 4 rows (scenarios) and 3 columns (possible data types for that scenario)
#' \describe{
#'   \item{\sQuote{transform}}{a transformation vector converting input factors (Ecotypes) to output factors (groups). Functionally
#'      it is a lookup table that maps input classes to output classes. See examples for how to use it.}
#'   \item{\sQuote{labels}}{names for the output groups}
#'   \item{\sQuote{colours}}{suggested plotting colours for the output groups}
#' }
#'
#' @examples
#' ecoGroup[['domSpecies','transform']][ as.numeric(as.character(vData$Ecotype)) ]
"ecoGroup"

#' Extracts the raster data corresponding to each point in the point dataset.
#'
#' @param rData the raster data brick
#' @param vData the point data
#' @param colNames (optional) a collection of column names. If unspecified the function will pull them from the layers in the brick.
#' @param na.omit remove NA points from the dataset?
#' @return a data frame with the extracted raster data columns appended
#'
extractPoints <- function (rData, vData, colNames=NULL, na.omit=T) {
  pointData <- data.frame()
  len <- nrow(vData)
  spPoints <- sp::SpatialPoints(vData[,c('long','lat')], proj4string=rData@crs)
  pb <- utils::txtProgressBar (0,len,style=3)
  for (i in 1:len) {
    pixel <- raster::extract (rData, spPoints[i])
    valid <- sum(pixel!=0, na.rm=T)
    if (is.na(valid) & !na.omit) valid=1
    if (valid != 0)
      pointData <- rbind (pointData,cbind(vData[i,],pixel))    # Only keep the value if it non-zero (and hence, non-null)
    utils::setTxtProgressBar (pb,i)
  }
  close (pb)

  # Assign the output array some meaningful names based on the layers requested
  if (is.null(colNames[1])) colNames <- names (rData)
  if (nrow(pointData) > 0) names (pointData)[(ncol(vData)+1):ncol(pointData)] <- colNames
  return (pointData)
}

#' A utility function to generate various classification models from a single input dataset
#'
#' @param data: the input dataframe
#' @param modelTypes: a vector of model types to generate. One or more of c('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm','svm')
#' @param x (optional): vector names of 'predictor' variables to use. Defaults to all columns less the y varible
#' @param y (optional): the name of the column of the 'response' variable. Defaults to first column.
#' @param grouping (optional): a transformation vector for input classes. Defaults to identity.
#' @param echo (optional): should the function report it's progress?
#' @param rf.args (optional): arguments to pass to random forest type models
#' @param nn.args (optional): arguments to pass to nearest neighbour type models
#' @param gbm.args (optional): arguments to pass to gbm
#' @param svm.args (optional): arguments to pass to svm
#' @return A named list of models
#'
generateModels <- function (data, modelTypes, x=NULL, y=NULL, grouping=NULL, echo=T, rf.args=NULL, nn.args=NULL, gbm.args=NULL,svm.args=NULL) {
  # Preprocess which variables to use for x and y, and create a formula object
  if (is.null(y)) y <- names(data)[1]
  if (!is.factor(data[,y])) stop ("y needs to be a factor")
  defaultCols <- names(data)[-match(y,names(data))]
  if (is.null(x)) cols <- defaultCols else cols <- x[x %in% defaultCols]
  x <- cols[0 == apply (data[cols],2,FUN=function (x) {sum(is.na(x))})]         # Eliminate columns with NA values
  fx <- formula(paste(y,"~",paste(x,collapse='+')))

  # Generate argument lists for each function type; i.e. merge default and provided arguments
  #   Note: for some reason the different rf algorithms get different answers for the 'same' formula of mtry so it is specified manually
  #         importance for the randomForest package is boolean: FALSE if this is 'none' and TRUE otherwise
  replaceArgs <- function (passed, default) {
    default[names(default) %in% names(passed)] <- NULL
    c(passed,default)
  }
  rf.args <-  replaceArgs (rf.args,  list(mtry=floor(sqrt(length(x))), importance='permute', na.action='na.omit', proximity=F))
  if (!rf.args$importance %in% c("permute", "random", "permute.ensemble","random.ensemble", "none")) rf.importance <- 'none'
  nn.args  <- replaceArgs (nn.args,  list(k=3, kmax=7, kernel='rectangular', scale=T))
  gbm.args <- replaceArgs (gbm.args, list(distribution='multinomial', n.trees=1000, keep.data=TRUE))
  svm.args <- replaceArgs (svm.args, list(scale=F, probability=T))

  # Transform the dataset, trim, and scale if necessary
  if (is.null(grouping)) grouping <- 1:max(as.numeric(levels(data[,y])))        # Note! Care needs to be taken here... does this transformation function reference factor entry number or the actual value?
  data[,y] <- sort.levels(as.factor( grouping[as.numeric(as.character(data[,y]))] ))
  if (nn.args$scale) data <- cbind (data[,-match(x,names(data)),drop=F],scale(data[,x]))
  else data <- data[,c(y,x)]

  # Build the models
  rF <- rFSRC <- fnn.FNN <- fnn.class <- kknn <- gbm <- svm <- NULL
  if ('rF' %in% modelTypes) { if (echo) cat("rF... ")
    rF <- do.call(randomForest::randomForest, c(list(formula=quote(fx), data=quote(data), importance=(rf.args$importance != 'none')), rf.args[names(rf.args) != 'importance'])) }
  if ('rFSRC' %in% modelTypes) { if (echo) cat("rFSRC... ")
    rFSRC <- do.call(randomForestSRC::rfsrc, c(list(formula=quote(fx), data=quote(data)), rf.args)) }
  if ('fnn.FNN' %in% modelTypes) { if (echo) cat("fnn.FNN... ");
    fnn.FNN <- do.call(FNN::knn.cv, list(train=quote(data[,x]), cl=quote(data[,y]), prob=T, k=nn.args$k)) }
  if ('fnn.class' %in% modelTypes) { if (echo) cat("fnn.class... ")
    fnn.class <- do.call(class::knn.cv, list(train=quote(data[,x]), cl=quote(data[,y]), prob=T, k=nn.args$k)) }
  if ('kknn' %in% modelTypes) { if (echo) cat("kknn... ")
    kknn <- do.call(kknn::train.kknn, list(formula=quote(fx), data=quote(data), kmax=nn.args$kmax, kernel=nn.args$kernel)) }
  if ('gbm' %in% modelTypes) { if (echo) cat("gbm... ")
    gbm <- do.call(gbm::gbm, c(list(formula=quote(fx), data=quote(data)), gbm.args)) }
  if ('svm' %in% modelTypes) { if (echo) cat("svm... ")
    svm <- do.call(e1071::svm, c(list(formula=quote(fx), data=quote(data)), svm.args)) }
  if (echo) cat("\n")

  # Since fnn.FNN and class.fnn functions do not return a class object, label the classes for later identification; randomForest labels as two classes; append data as attributes to these classes when not already included
  if ('rF' %in% modelTypes)        { class (rF) <- rev(class(rF)); attr(rF,'rf.args') <- rf.args; attr(rF,'data') <- data }
  if ('rFSRC' %in% modelTypes)     { attr(rFSRC,'rf.args') <- rf.args }
  if ('fnn.FNN' %in% modelTypes)   { fnn.FNN   <- structure (list(knn=fnn.FNN,   formula=fx, train=data[,x], classes=data[,y]), class='fnn.FNN', nn.args=nn.args) }
  if ('fnn.class' %in% modelTypes) { fnn.class <- structure (list(knn=fnn.class, formula=fx, train=data[,x], classes=data[,y]), class='fnn.class', nn.args=nn.args) }
  if ('kknn' %in% modelTypes)      { class (kknn) <- rev(class(kknn)); attr(kknn,'nn.args') <- nn.args }
  if ('gbm' %in% modelTypes)       { attr(gbm,'gbm.args') <- gbm.args }
  if ('svm' %in% modelTypes)       { attr(svm,'svm.args') <- svm.args; attr(svm,'data') <- data }

  # Add attributes to the list of models and return it
  retObj <- list ('rF'=rF, 'rFSRC'= rFSRC, 'fnn.FNN'=fnn.FNN, 'fnn.class'=fnn.class, 'kknn'=kknn, 'gbm'=gbm, 'svm'=svm)
  retObj <- retObj[sapply(retObj, function(x) !is.null(x))]
  attributes(retObj) <- c (attributes(retObj), list(
    modelTypes=modelTypes,
    formula=fx,
    grouping=grouping))
  return ( retObj )
}

#' Generates basic error statistics for a classification model
#'
#' @details Create some notes here on how each accuracy rate is computed and it's significance???
#'
#' @param x the prediction classes
#' @param y the validation classes
#' @param digits (optional) the number of digits to output for the error
#' @return the error data as a five element list: confusion matrix, user accuracy, producer accuracy, overall accuracy, kappa
#'
classAcc <- function (pred, valid, digits=3, classNames=NULL) {

  pred <- trim.levels(pred);          valid <- trim.levels(valid)
  pred <- merge.levels(pred,valid);   valid <- merge.levels(valid,pred)
  if (is.null(classNames)) { classNames <- as.numeric(levels(valid)) } else { classNames <- classNames [as.numeric(levels(valid))] }
  conf <- table(pred,valid)                                                     # Confusion matrix
  userTot <- apply (conf,1,sum)
  prodTot <- apply (conf,2,sum)
  Tot <- sum(conf)

  userAcc <- diag(conf)/userTot                               # User accuracy rates--
  prodAcc <- diag(conf)/prodTot                               # Producer accuracy rates--
  overallAcc <- sum(diag(conf))/Tot                           # Overall accuracy rate
  kappa <- (Tot*sum(diag(conf)) - sum(userTot*prodTot)) / (Tot^2 - sum(userTot*prodTot))

  dimnames(conf) <- list(classNames,classNames)
  names (userAcc) <- classNames
  names (prodAcc) <- classNames
  return (list(confMatrix=conf, userAcc=userAcc, prodAcc=prodAcc, overallAcc=overallAcc, kappa=kappa))
}

#' Generate an estimate for variable importance for nearest neighbour models
#'
#' In an attempt to establish some indication of variable importance for nearest neighbour models, this algorithm computes the overall accuracies of the specified
#'   model successively leaving out one predictor variable. The VIMP score is then calculated by linearly scaling the overall accuracies such that the version with
#'   the largest drop in overall accuracy scores 1.0, and the model with the smallest drop in overall accuracy scores 0.0
#'
#' The function returns a named vector of the VIMP scores for each variable
#'
#' @param model is the nearest neighbour model to test
#'
nnVIMP <- function (model) {
  # Extract info encoded in model objects
  if ('fnn.FNN' %in% class(model) || 'fnn.class' %in% class(model)) {
    fx <- model$formula
    data <- cbind(model$classes,model$train)
    names(data)[1] <- as.character(fx[[2]])
    cA <- classAcc(model$knn, data[,1])
  } else if ('kknn' %in% class(model)) {
    fx <- formula(model$terms)
    data <- model$data
    cA <- classAcc(model$fitted.values[[ model$best.parameters[[2]] ]], data[,as.character(fx[[2]])])     # ??? This will break when more than one model kernel is specified!
  } else stop ('Need to specify a model of one of the following classes: fnn.FNN, fnn.class, kknn')

  # Build a new model, each one less one term, and compute its accuracy
  VIMP <- t(data.frame(complete=cA$userAcc))
  overall <- cA$overallAcc
  for (i in attr(terms(fx),'term.labels')) {
    tmp.fx <- update.formula(fx,as.formula(paste0("~ . -",i)))
    x <- attr(terms(tmp.fx),'term.labels')
    y <- as.character(tmp.fx[[2]])
    tmp.model <- generateModels(data, class(model)[[1]], x=x, y=y, nn.args=attr(model,'nn.args'), echo=F)[[1]]    # Since this is a list of length one, convert it to a simple model reference

    if ('fnn.FNN' %in% class(model) || 'fnn.class' %in% class(model))
      cA <- classAcc(tmp.model$knn, data[,y])
    else if ('kknn' %in% class(model))
      cA <- classAcc(tmp.model$fitted.values[[ tmp.model$best.parameters[[2]] ]], data[,y])     # ??? This will break when more than one model kernel is specified!
    VIMP <- rbind(VIMP,cA$userAcc)
    overall <- c(overall,cA$overallAcc)
  }

  # Convert output accuracies to a VIMP metric
  VIMP <- cbind(decreaseAcc=overall,VIMP)
  VIMP <- -sweep(VIMP,2,VIMP[1,])
  VIMP <- VIMP[2:nrow(VIMP),]
  row.names(VIMP) <- attr(terms(fx),'term.labels')
  (VIMP)
}

#' Generate accuracy statistics for a list of models
#'
#' Given a colleciton of models, this function computes several accuracy and VIMP metrics. See details for a description of each structure returned.
#'
#' The function returns a list of different accuracy and VIMP datastructures
#' \describe{
#'   \item{\sQuote{confMatrix}}{a named list of confusion matrices for each model in the list. Each confusion matrix is a data.frame}
#'   \item{\sQuote{userAcc}}{a data.frame in which each column represents the user acceracy for a given model. Rows are the accuracies for
#'      that input class. The final row is the overall user accuracy for that model technique. User accuracy, also called commission error, is the
#'      the percent of pixels on the map that are what they are predicted to be; that is, the number of correctly identified sites over the number
#'      that the model predicts to be in that class.}
#'   \item{\sQuote{prodAcc}}{a data.frame in which each column represents the producer acceracy for a given model. Rows are the accuracies for
#'      that input class. The final row is the overall producer accuracy for that model technique (the same as for userAcc). Producer accuracy, also called omission error,
#'      is the the percent of pixels on the map that are labelled correctly; that is, the number of correctly identified sites over the number
#'      that the actually are that class.}
#'   \item{\sQuote{kappa}}{a vector of kappa values for each model type. The kappa-statistic is a measure of how much better this model predicts output
#'      classes than would be done by chance alone. It is computed as the ratio of the oberved accuracy less that expected by chance, over 1-(chance
#'      probabilities). K = (observed accuracy – chance agreement) / (1 – chance agreement). ??? Add the formula as a tex formula... }
#'   \item{\sQuote{VIMP}}{a named list of variable importance (VIMP) matrices. Each matrix is a data.frame in which columns are different classes
#'      and rows are input variables used in the model. Entries in the table are the variable importance of an input variable on particular class.}
#'   \item{\sQuote{VIMPoverall}}{a data.frame showing the overall variable importance (VIMP) for a given model. Columns are different models and entries
#'      in the table are the standardized VIMP for an imput variable on that model. Values have been standardized by column so they are (somewhat) comparable
#'      as the algorithm by which VIMP is computed varies by model--hence the largest value will be 1 in every column. This makes the columns approximately
#'      comparable, however, values should be taken with a grain of salt, and perhaps rank order is the most robust comparison.}
#' }
#'
#' @param models a list of model objects on which to find error statistics
#'
#' @return a list of errors/accuracies: list(confMatrix, userAcc, prodAcc, kappa, VIMP, VIMPoverall)
#'
modelAccs <- function (models, classNames=NULL) {
  confMatrix <- userAcc <- prodAcc <- kappa <- VIMP <- VIMPoverall <- NULL
#  envData <- attr(models,'data')
  y <- as.character(attr(models,'formula')[[2]])
  for (i in models) {
# ??? Clean up this mess: repeated lines, small changes...
    if ('randomForest' %in% class(i)) {
      tmp <- classAcc(i$predicted, attr(i,'data')[,y], classNames=classNames)
      confMatrix <- c(confMatrix, list(rF=tmp$confMatrix))
      userAcc <- cbind(userAcc, rF=c(tmp$userAcc,overall=tmp$overallAcc))
      prodAcc <- cbind(prodAcc, rF=c(tmp$prodAcc,overall=tmp$overallAcc))
      kappa <- c(kappa,tmp$kappa)
      tmp <- i$importance
      if (!is.null(classNames)) colnames(tmp) <- classNames[as.numeric(colnames(tmp))]
      VIMP <- c(VIMP, list(rF=tmp[,1:(ncol(tmp)-2)]))
      VIMPoverall <- cbind(VIMPoverall, rF=tmp[,ncol(tmp)-1]/max(tmp[,ncol(tmp)-1]))
    } else if ('rfsrc' %in% class(i)) {
      tmp <- classAcc(i$class.oob, i$yvar, classNames=classNames)
      confMatrix <- c(confMatrix, list(rFSRC=tmp$confMatrix))
      userAcc <- cbind(userAcc, rFSRC=c(tmp$userAcc,overall=tmp$overallAcc))
      prodAcc <- cbind(prodAcc, rFSRC=c(tmp$prodAcc,overall=tmp$overallAcc))
      kappa <- c(kappa,tmp$kappa)
      tmp <- i$importance
      if (!is.null(classNames)) colnames(tmp) <- classNames[as.numeric(colnames(tmp))]
      VIMP <- c(VIMP, list(rFSRC=tmp[,2:ncol(tmp)]))
      VIMPoverall <- cbind(VIMPoverall,rFSRC=tmp[,1]/max(tmp[,1]))
    } else if ('fnn.FNN' %in% class(i)) {
      tmp <- classAcc(i$knn, i$classes, classNames=classNames)                                                # ??? Classnames is optional!
      confMatrix <- c(confMatrix, list(FNN=tmp$confMatrix))
      userAcc <- cbind(userAcc, knn.FNN=c(tmp$userAcc,overall=tmp$overallAcc))
      prodAcc <- cbind(prodAcc, knn.FNN=c(tmp$prodAcc,overall=tmp$overallAcc))
      kappa <- c(kappa,tmp$kappa)
      tmp <- nnVIMP(i)
      if (!is.null(classNames)) colnames(tmp) <- classNames[as.numeric(colnames(tmp))]
      VIMP <- c(VIMP, list(fnn.FNN=tmp[,2:ncol(tmp)]))
      VIMPoverall <- cbind(VIMPoverall,fnn.FNN=tmp[,1])
    } else if ('fnn.class' %in% class(i)) {
      tmp <- classAcc(i$knn, i$classes, classNames=classNames)                                                # ??? Classnames is optional!
      confMatrix <- c(confMatrix, list(FNN=tmp$confMatrix))
      userAcc <- cbind(userAcc, knn.class=c(tmp$userAcc,overall=tmp$overallAcc))
      prodAcc <- cbind(prodAcc, knn.class=c(tmp$prodAcc,overall=tmp$overallAcc))
      kappa <- c(kappa,tmp$kappa)
      tmp <- nnVIMP(i)
      if (!is.null(classNames)) colnames(tmp) <- classNames[as.numeric(colnames(tmp))]
      VIMP <- c(VIMP, list(fnn.class=tmp[,2:ncol(tmp)]))
      VIMPoverall <- cbind(VIMPoverall,fnn.class=tmp[,1])
    } else if ('kknn' %in% class(i)) {
      tmp <- classAcc(i$fitted.values[[ i$best.parameters[[2]] ]], i$data[,y], classNames=classNames)          # ??? Classnames is optional!
      confMatrix <- c(confMatrix, list(FNN=tmp$confMatrix))
      userAcc <- cbind(userAcc, kknn=c(tmp$userAcc,overall=tmp$overallAcc))
      prodAcc <- cbind(prodAcc, kknn=c(tmp$prodAcc,overall=tmp$overallAcc))
      kappa <- c(kappa,tmp$kappa)
      tmp <- nnVIMP(i)
      if (!is.null(classNames)) colnames(tmp) <- classNames[as.numeric(colnames(tmp))]
      VIMP <- c(VIMP, list(kknn=tmp[,2:ncol(tmp)]))
      VIMPoverall <- cbind(VIMPoverall,kknn=tmp[,1])
    } else if ('gbm' %in% class(i)) {
      tmp <- classAcc( factor( i$classes[apply(predict (i,attr(i,'data'),gbm::gbm.perf(i,plot.it=F),type='response'),1,which.max)] ) , attr(i,'data')[,y], classNames=classNames)
      confMatrix <- c(confMatrix, list(gbm=tmp$confMatrix))
      userAcc <- cbind(userAcc, gbm=c(tmp$userAcc,overall=tmp$overallAcc))
      prodAcc <- cbind(prodAcc, gbm=c(tmp$prodAcc,overall=tmp$overallAcc))
      kappa <- c(kappa,tmp$kappa)
      tmp <- summary(i,order=F,plotit=F)
      VIMPoverall <- cbind(VIMPoverall,gbm=tmp[,2]/max(tmp[,2]))
    } else {
      warning(paste("Could not find model class: ",class(i)))
    }
  }
  return (list(confMatrix=confMatrix, userAcc=userAcc, prodAcc=prodAcc, kappa=kappa, VIMP=VIMP, VIMPoverall=VIMPoverall))
}

#' Compute the VIF of the terms in a formula
#'
#' Given an input dataset, this function generates VIF output for each term on the left hand side of the specified
#'   formula. It gives considerable output than the typcial VIF function--see Details.
#'
#' The function returns a list with more terms than the typical VIF function:
#' \describe{
#'   \item{\sQuote{vif}}{the resulting VIF scores for each term in the model}
#'   \item{\sQuote{rSquared}}{the resulting R-square with that term \emph{removed} from the model}
#'   \item{\sQuote{est}}{a matrix of parameter estimates from each linear model. Rows are for each parameter
#'      that was estimated, and columns are the parameter estimates. Not that the parameter that is being
#'      estimated is represented by a 0 in the estimates since it was not included in the model.}
#'   \item{\sQuote{stdErr}}{a matrix of the standard error of each parameter estimate. Format is the same as for est.}
#'   \item{\sQuote{t.value}}{the t.value for each parameter estimate. Format is the same as for est.}
#'   \item{\sQuote{p.value}}{the p.value for each parameter estimate. Format is the same as for est.}
#' }
#'
#' @param formula a formula
#' @param data the input data to use in the regressions
#'
#' @return a list with the following elements: list (vif, rSquared, est, stdErr, t.value, p.value). See Details
#'
NPELvif <- function (formula, data) {
  cols <- attr(terms(fx),'term.labels')
  est=    matrix(NA,length(cols),length(cols)+1); dimnames(est)     <- list(cols,c('(Intercept)',cols))
  stdErr= matrix(NA,length(cols),length(cols)+1); dimnames(stdErr)  <- list(cols,c('(Intercept)',cols))
  t.value=matrix(NA,length(cols),length(cols)+1); dimnames(t.value) <- list(cols,c('(Intercept)',cols))
  p.value=matrix(NA,length(cols),length(cols)+1); dimnames(p.value) <- list(cols,c('(Intercept)',cols))
  rSq <- vector ('numeric',length(cols)); names(rSq) <- cols

  for (i in 1:length(cols)) {

    lmTmp <- lm (formula=update.formula(fx,paste0(cols[i],' ~ . -',cols[i])),data=df)
    sTmp <- summary(lmTmp)            # Following rbind looses rownames when sTmp is a matrix
    coeffs <- data.frame(sTmp$coefficients)
    index <- sum(!sTmp$aliased[1:i])
    coeffs <- rbind(coeffs[1:index,], rep(0,4), if (index < nrow(coeffs)) coeffs[(index+1):nrow(coeffs),]) # Insert the row that represents this factor as 0's...
    rownames(coeffs)[index+1] <- cols[i]                                #  .... and give it a name
    est    [i,!sTmp$aliased] <- coeffs[,1]
    stdErr [i,!sTmp$aliased] <- coeffs[,2]
    t.value[i,!sTmp$aliased] <- coeffs[,3]
    p.value[i,!sTmp$aliased] <- coeffs[,4]
    rSq[i] <- sTmp$r.squared
  }
  return (list(vif=1/(1-rSq), rSquared=rSq, coeff=est, stdErr=stdErr, t.value=t.value, p.value=p.value))
}
