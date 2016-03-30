# Functions used to generate and evaluate models
# Created 9.Oct.2015 from pre-existing code file started 6.Apr.2015

##### generateModels #####
#' Generate Models
#'
#' This function builds a collection of models from a single input dataset. It can handle either classification or regression data; that is,
#' either categorical or continuous data.
#'
#' In the most basic sense, this function is a loop wrapping the code to generate a model. However, it also standardizes the inputs for all
#' the model packages and generates meaningful default arguments for all the supported packages. It is possible to pass the function either
#' a formula object, or a list of x and y names from which to generate the models---it will compute whichever is not specified.
#'
#' The various arguments are the most complex part of this function. Reasonably meaningful default values are generated within the function,
#' but the user always has the option to override them. In most cases it is likely there will be at least a few arguments that will need to
#' be provided. The argument lists are divided up by model \emph{type}, not package:
#' \itemize{
#'   \item Random Forest---currently: \pkg{\link[randomForest]{randomForest}}, and \pkg{\link[randomForestSRC:rfsrc]{randomForestSRC}}.
#'   \itemize{
#'     \item \code{mtry} the two different implementation of random forests, while they specify that they compute the number of variables to use at
#'       each node split the same way, actually arrive at different answers internally---that is, given the defaults, they do not generate the
#'       same output. By specifying it here, using the same formula they specify as the default, it is possible ensure that they are doing the
#'       same thing; defaults to \code{floor(sqrt(length(x)))}.
#'     \item \code{importance} one of the benefits of random forests is that it is relatively easy to compute a variable importance metric (VIMP).
#'       While only \pkg{randomForestSRC} currently allows multiple options for methods, these options can be specified here (including
#'       \sQuote{\code{none}} and the arguments for \pkg{randomForest} will be generated automatically; defaults to \sQuote{\code{permute}}
#'     \item \code{na.action} what to do when na values are encountered; defaults to \sQuote{\code{na.omit}}.
#'     \item \code{proximity} should proximity information be computed; see packages for more help.
#'   }
#'   \item Nearest Neighbour---currently: \pkg{\link[FNN:knn.cv]{FNN}}, \pkg{\link[class:knn.cv]{class}}, and \pkg{\link[kknn:train.kknn]{kknn}}.
#'   \itemize{
#'     \item \code{k} the number of neighbours considered (for FNN and class); defaults to 3.
#'     \item \code{kmax} the kknn package optimized the number of neighbours considered---this specified the maximum number of neighbours for that
#'       optimization; defaults to 7, the default provided in kknn.
#'     \item \code{kernel} the kknn package allows the selection of different kernel functions as to how to weight the distance metric---this
#'      specifies which to use; defaults to \sQuote{\code{rectangular}}. It is possible to use more than one and it will optimize over
#'      them all.
#'     \item \code{scale} should we scale the data before running the model fit; defaults to TRUE.
#'   }
#'   \item GBM---currently: \pkg{\link[gbm]{gbm}}
#'   \itemize{
#'     \item \code{distribution} which distribution to assume for the data. Classification data defaults to \sQuote{\code{in the package}} and this
#'       package follows that.
#'     \item \code{n.trees} the maximum number of trees to grow. Note that this is \emph{not} the optimal number of trees! This is an overfit model; use
#'       \code{\link[gbm]{gbm.perf}} to find the optimal model; defaults to 1000.
#'     \item \code{keep.data} should the data be embedded in the model. Since other methods in this package need the data, default is TRUE; this also
#'       prevents the data from potentially being stored twice.
#'   }
#   \item SVM---currently: \pkg{\link[=svm]{e1071}}
#   \itemize{
#     \item \code{\bold{scale}} ; defaults to FALSE.
#     \item \code{\bold{probability}} ; defaults to TRUE.
#   }
#' }
#'
#' @param data the input data frame, see \code{\link{siteData}} for more information and an example dataset.
#' @param modelTypes a character vector of model types to generate; one or more of \code{\link{suppModels}}.
#' @param fx (optional) a formula object specifying the variable relationships; will be generated from x and y if unspecified.
#' @param x (optional) vector names of 'predictor' variables to use; defaults to all columns less the y variable; defaults to all columns
#'   other than y if fx is also not provided.
#' @param y (optional) the name of the column of the 'response' variable; defaults to first column if fx is also not provided. It can be
#'   either categorical or continuous data, and it will attempt to coerce vectors of unknown types (e.g. boolean) into one of these two
#'   groups, albeit in a rather rudimentary fashion. If it cannot succeed it will complain.
#' @param grouping (optional) a transformation vector for input classes; if not provided, no grouping will be used. See
#'   \code{\link{ecoGroup}} for more information about this technique.
#' @param echo (optional) should the function report it's progress? Defaults to TRUE, but useful for automation.
#' @param rf.args (optional) a list of arguments to pass to random forest type models; defaults will be generated for unspecified values.
#' @param nn.args (optional) a list of arguments to pass to nearest neighbour type models; defaults will be generated for unspecified
#'   values.
#' @param gbm.args (optional) a list of arguments to pass to gbm; defaults will be generated for unspecified values.
# @param svm.args (optional) a list of arguments to pass to svm; defaults will be generated for unspecified values.
#' @return A named list of models with attributes specifying the data, the function used, and the class.
#'
#' @seealso
#' See the package help \pkg{\link{NPEL.Classification}} for an overview of the analysis process.
#'
#' For reading-in model data: \code{\link{readTile}}, \code{\link[maptools]{readShapePoints}}, and \code{\link{extractPoints}}; or the
#' \pkg{\link[raster]{raster}} package help for reading-in raster files directly.
#'
#' For examples on computing derived raster variables, e.g. NDVI, slope, etc. see the example code in \code{\link{egTile}}
#'
#' For examples on what to do with the generated models see: \code{\link{modelAccs}}, \code{\link{writeTile}}, and \code{\link{plotTile}}
#'
#' Also see any of the supported packages, currently: \pkg{\link[randomForest]{randomForest}},
#' \pkg{\link[randomForestSRC:rfsrc]{randomForestSRC}}, \pkg{\link[FNN:knn.cv]{FNN}}, \pkg{\link[class:knn.cv]{class}},
#' \pkg{\link[kknn:train.kknn]{kknn}}, and \pkg{\link[gbm]{gbm}}.
#'
#' @examples
#' data ('siteData')
#' modelRun <- generateModels (data = siteData,
#'                             modelTypes = suppModels,
#'                             x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
#'                             y = 'ecoType',
#'                             grouping = ecoGroup[['domSpecies','transform']],
#'                             gbm.args = list (interaction.depth=7, shrinkage=0.005, cv.folds=0) )
#' @export
generateModels <- function (data, modelTypes, fx=NULL, x=NULL, y=NULL, grouping=NULL, echo=T, rf.args=NULL, nn.args=NULL, gbm.args=NULL) {
  # Preprocess and check fx, x, y
  fx2vars(fx, x, y, names(data))
  if (isCat(data[,y])) {
    data[,y] <- trimLevels(as.factor(data[,y]))
  } else if (!isCont(data[,y])) stop("generateModels: specified y satisfied neither isCat nor isCont; check data class.")
  x <- x[0 == apply (data[,x],2,FUN=function (x) {sum(is.na(x))})]         # Eliminate columns with NA values

  # Generate argument lists for each function type, i.e. merge default and provided arguments
  #   Note: for some reason the different rf algorithms get different answers for the 'same' formula of mtry so it is specified manually
  replaceArgs <- function (passed, default) {
    default[names(default) %in% names(passed)] <- NULL
    c(passed,default)
  }
  rf.args <-  replaceArgs (rf.args,  list(mtry=floor(sqrt(length(x))), importance='permute', na.action='na.omit', proximity=F))
  nn.args  <- replaceArgs (nn.args,  list(k=3, kmax=7, kernel='rectangular', scale=T))
  gbm.args <- replaceArgs (gbm.args, list(n.trees=1000, keep.data=TRUE))
#  svm.args <- replaceArgs (svm.args, list(scale=F, probability=T))

  # Group the dataset if necessary, and trim it to contain only the used variables
  if (!isCat(data[,y]) && !is.null(grouping)) {
    warning("generateModels: cannot group a non-factor variable; ignoring grouping.")
    grouping <- NULL
  }
  if (!is.null(grouping)) {
    if (min(factorValues(data[,y])) < 1) stop("generateModels: cannot group factors with indices less than 1.")
    data[,y] <- sortLevels(as.factor( grouping[factorValues(data[,y])] ))
  }
  data <- data[,c(y,x)]

  # Build the models
  args <- list('randomForest'=rf.args,'rfsrc'=rf.args,'fnn.FNN'=nn.args,'fnn.class'=nn.args,'kknn'=nn.args,'gbm'=gbm.args) #,'svm'=svm.args) # A lookup table for matching arguments with classes
  retObj <- list()
  for (i in modelTypes) {
    if ( !(i %in% contModels) && !isCat(data[,y]) ) {
      warning(paste0("generateModels: ",i," requires categorical data; build a classifier based site ID and use impute to calculate the result. Removing model."))
      modelTypes <- modelTypes[i != modelTypes]
    } else {
      if (echo) print (paste0("Generating: ",i))
      retObj <- c( retObj,list(buildModel(i,data,fx,args[[i]])) )
    }
  }
  names (retObj) <- modelTypes
  return ( retObj )
}

##### npelVIMP #####
#' Generate an estimate for variable importance for nearest neighbour models
#'
#' There is no well-established way of determining variable importance for nearest neighbour classifiers. In an effort to generate \emph{some} useful
#' metric for comparison with other models, we developed this leave-one-out type approach; it is analogous to a variable inflation metric.
#' The algorithm proceeds as follows:
#' \enumerate{
#'   \item Given a model, find the formula it was generated with.
#'   \item Compute the overall accuracy as well as the accuracy-by-class (producer accuracy) for that model.
#'   \item Re-generate the model using all but one of the input variables\ldots
#'   \item \ldots and save the overall and class level accuracies.
#'   \item Repeat for each input variable so we have an accuracy metric for a model in which all-but-one variable is included.
#'   \item Standardize each variable (column) by the change from the accuracy of the complete model:
#'           \deqn{Acc_{complete}-Acc_{without.variable.x}}{Acc_complete-Acc_without.variable.x}
#' }
#'
#' However, once the nearest-neighbour estimates were achieved we realized that we still couldn't compare because of the different
#' algorithms used; hence we expanded this function to accept any of the models this package deals with. But see limitations for a (short)
#' discussion on why this method is not the greatest.
#'
#' @section Limitations:
#'
#' While this algorithm is simple, easy to compute, and applicable to \emph{any} model, it has some (serious) limitations:
#'
#' \itemize{
#'   \item The most notable is that it is not clear that a variable's impact on a model's accuracy is well correlated with the drop in
#'   overall accuracy! Sure, this can be one definition of a VIMP metric, but this is much coarser than those use by, for example, random
#'   forest. Hence, while this function gives some idea of which variables are contributing to accuracy, it is not clear that the scale is
#'   consistent between variables. Hence, a VIMP of 0.1 for a given variable on a given class may not be the same as a VIMP of 0.1 for a
#'   different variable.
#'   \item Many of the classifiers used in this package are sensitive to colinearity in the data; and there is typically much colinearity in
#'   remote sensed data! We can easily generate 20 or more variables (layers) from the imagery and elevation data alone. It is clear that
#'   these variables cannot \emph{all} be orthogonal. Hence, removing a variable when there is significant colinearity may have no effect on
#'   the overall accuracy, or worse, it may have only a \sQuote{stochastic} effect, which is to say, it depends on small perturbations in
#'   the input data.
#'   \item It is not uncommon to see models \emph{improving} when variables are removed. To a certain extent this is to be expect, however,
#'   in our experience it occurs with more frequency than might be expected. Our hypothesis is that the limitations discussed above
#'   contribute to this effect.
#' }
#'
#' Hence, our recommendation is to consider the VIMP data produced by this function with caution: perhaps only consider the rank order, or
#' look only at gross effects. We have considered standardizing the output in other ways, e.g. by the largest value in each row (each
#' variable that has been removed) so the maximum in each row is unity, but it isn't immediately clear what the results mean. So, while
#' there is considerable literature on using leave-one-out type approaches for cross-validation, until the technique of using it as a VIMP
#' metric can be further studied, the results of this function fall under \emph{caveat emptor}. Enjoy!
#'
#' @section Warning:
#' Given that each model type, and even each model implementation uses different algorithms for computing VIMP, the output from this
#' function does not fall on the same scale as that given by other methods. This is not a limitation of our function, it is a limitation of
#' VIMP in general. In fact, that our model can compute VIMP on all the models used is one of its strengths. However, do not try and compare
#' (or plot) the VIMP output from different models/packages against each other; they are only valid as relative values.
#'
#' @param model the classifier to test
#' @param calc (optional) should the function recalculate the VIMP; that is, if the model is one of the types that computes VIMP during
#'   generation, should we recalculate it, or use the pre-existing VIMP; currently: \pkg{\link[randomForest]{randomForest}}, \pkg{\link[randomForestSRC:rfsrc]{randomForestSRC}},
#'   and \pkg{\link[gbm]{gbm}}.
#' @param echo (optional) should the function inform the user about it's progress
#' @return returns a data frame with rows as the variables, and columns being the classes. The first column is the overall VIMP for that variable.
#' @seealso For VIMP information about packages used that have the metric build-in: \code{\link[randomForest]{importance}},
#'   \code{\link[randomForestSRC]{rfsrc}}, and \code{\link[gbm]{summary.gbm}}.
#'
#' @examples
#' data ('siteData')
#' modelRun <- generateModels (data = siteData,
#'                             modelTypes = suppModels,
#'                             x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
#'                             y = 'ecoType',
#'                             grouping = ecoGroup[['domSpecies','transform']])
#' npelVIMP (modelRun$rfsrc,calc=F)
#' npelVIMP (modelRun$rfsrc,calc=T)
#' @export
npelVIMP <- function (model, calc=F, echo=T) {
  # If we don't need to calculate, check if this is a model that contains VIMP and return it, otherwise continue...
  if (!calc) {
    tmp <- getVIMP(model)
    if (!is.null(tmp)) return(tmp)
  }

  # Initialization
  if (echo) print (paste0("Computing VIMP for model: ",class(model)[[1]]))
  fx <- getFormula(model)
  df <- getData(model)
  y <- as.character(fx[[2]])
  x <- attr(terms(fx),'term.labels')
  if (length(x) < 3) stop("npelVIMP: not able to compute post hoc VIMP on models with only two variables.")

  # Get an overall error
  VIMP <- NULL
  cA <- classAcc(getFitted(model), df[,y])
  if (!is.null(cA$prodAcc)) VIMP <- as.data.frame(t(data.frame(complete=cA$prodAcc)))
  overall <- cA$overallAcc

  # Build a new model, each one minus one term; compute and compile its accuracy
  for (i in x) {
    if (echo) print (paste0('Removing: ',i))
    tmp.x <- x[i!=x]
    tmp.model <- generateModels(df, class(model)[[1]], x=tmp.x, y=y, nn.args=attr(model,'nn.args'), echo=F)[[1]] # Convert from list of length 1 to object
    cA <- classAcc(getFitted(tmp.model), df[,y])
    if (!is.null(cA$prodAcc)) VIMP <- rbind(VIMP,cA$prodAcc)
    overall <- c(overall,cA$overallAcc)
  }
  # Convert output accuracies to a VIMP metric
  VIMP <- cbind(decreaseAcc=overall,VIMP)
  VIMP <- -sweep(VIMP,2,as.numeric(VIMP[1,]))
  VIMP <- VIMP[-1,,drop=F]
  row.names(VIMP) <- x
  return(VIMP)
}

##### npelVIF #####
#' Compute the variable inflation factor of the terms in a formula
#'
#' Given an input dataset, this function generates the VIF for each predictor variable. It gives considerably more output than the typical
#' VIF function---see details.
#'
#' The function returns a named list of several details from the computation of the VIF scores :
#' \itemize{
#'   \item \code{vif} the resulting VIF scores for each term in the model.
#'   \item \code{rSquared} the resulting R-square with that term \emph{removed} from the model.
#'   \item \code{est} a matrix of parameter estimates from each linear model. Rows are for each parameter that was estimated, and columns
#'   are the parameter estimates. Note that the parameter that is being estimated is represented by NA in the estimates since it was not
#'   included in the model.
#'   \item \code{stdErr} a matrix of the standard error for each parameter estimate in each model; the format is the same as for \code{est}.
#'   \item \code{t.value} the t.value for each parameter estimate in each model; the format is the same as for \code{est}.
#'   \item \code{p.value} the p.value for each parameter estimate in each model; the format is the same as for \code{est}.
#' }
#'
#' @param fx a formula for which to test the VIF
#' @param data a data frame with columns corresponding to the formula
#' @return a list with the following elements: list (vif, rSquared, est, stdErr, t.value, p.value). See Details
#'
#' @examples
#' data (siteData)
#' npelVIF(formula('Ecotype ~ brtns+grnns+wetns+dem+slp+asp+hsd'),siteData)
#' @export
npelVIF <- function (fx, data) {
  cols <- attr(terms(fx),'term.labels')
  est=    matrix(NA,length(cols),length(cols)+1); dimnames(est)     <- list(cols,c('(Intercept)',cols))
  stdErr= matrix(NA,length(cols),length(cols)+1); dimnames(stdErr)  <- list(cols,c('(Intercept)',cols))
  t.value=matrix(NA,length(cols),length(cols)+1); dimnames(t.value) <- list(cols,c('(Intercept)',cols))
  p.value=matrix(NA,length(cols),length(cols)+1); dimnames(p.value) <- list(cols,c('(Intercept)',cols))
  rSq <- vector ('numeric',length(cols)); names(rSq) <- cols

  for (i in 1:length(cols)) {
    lmTmp <- lm (formula=update.formula(fx,paste0(cols[i],' ~ . -',cols[i])),data=data)
    sTmp <- summary(lmTmp)
    coeffs <- data.frame(sTmp$coefficients) # The rbind loses rownames when sTmp is a matrix
    index <- sum(!sTmp$aliased[1:i])
    coeffs <- rbind(coeffs[1:index,], rep(NA,4), if (index < nrow(coeffs)) coeffs[(index+1):nrow(coeffs),]) # Insert the row that represents this factor as 0's...
    rownames(coeffs)[index+1] <- cols[i]                                #  .... and give it a name
    est    [i,!sTmp$aliased] <- coeffs[,1]
    stdErr [i,!sTmp$aliased] <- coeffs[,2]
    t.value[i,!sTmp$aliased] <- coeffs[,3]
    p.value[i,!sTmp$aliased] <- coeffs[,4]
    rSq[i] <- sTmp$r.squared
  }
  return (list(vif=1/(1-rSq), rSquared=rSq, coeff=est, stdErr=stdErr, t.value=t.value, p.value=p.value))
}

##### classAcc #####
#' Generates basic error statistics for a model
#'
#' A reasonable question is: what is the accuracy of the model we have created; this function aims to provide information towards answering
#' that question. There are a number of different metrics used depending primarily on whether the model encodes categorical or continuous
#' data---see below for details.
#'
#' The metrics returned for \bold{categorical} models are:
#' \itemize{
#'   \item \code{confMatrix} a named list of confusion matrices for each model in the list. Each confusion matrix is a data.frame with
#'     predicted values as rows and actual values as columns.
#'   \item \code{userAcc} a data frame in which each column represents the user accuracy for a given model, and rows are the accuracies for
#'     that input class. The final row is the overall user accuracy for that model. User accuracy, the inverse of so-called commission
#'     error, is the the portion of pixels that are what they were predicted to be; that is, the number of correctly identified sites
#'     divided by the number sites that the model predicted to be in that class.
#'   \item \code{prodAcc} a data frame in which each column represents the producer accuracy for a given model, and rows are the accuracies
#'     for that input class. The final row is the overall producer accuracy for that model. Producer accuracy, the inverse of so-called
#'     omission error, is the percent of pixels that are labelled correctly; that is, the number of correctly identified sites divided by
#'     the number that are actually of that class.
#'   \item \code{classLevelAcc} a vector of class-level accuracies---another statistic for measuring accuracy as distinct from producer or
#'     user error. It is based on omission and commission errors (see producer and user accuracies), such that \eqn{N_ommission} is the
#'     number of incorrectly classified points in a \emph{column} of the confusion matrix, and \eqn{N_commission} is the number of
#'     incorrectly classified points in a \emph{row} of the confusion matrix. Given that, class-level accuracy can be computed as:
#'       \deqn{Acc_{class level} = \frac{N_correct}{N_correct + N_ommission + N_commission}}{Acc_class.level = N_correct / (N_correct +
#'             N_ommission + N_commission)}
#'   \item \code{kappa} a vector of kappa values for each model type. The \eqn{\Kappa}-statistic is a measure of how much better this model
#'     predicts output classes than would be done by chance alone. It is computed as the ratio of the observed accuracy less that expected
#'     by chance, standardized by unity less the probability by chance alone.
#'       \deqn{\Kappa = \frac{Accuracy.obs - Agree.chance}{1 - Agreement.chance}}{K = (Acc_obs - Acc_chance) / (1 - Acc_chance)}
#' }
#' The metrics returned for \bold{continuous} models are:
#' \itemize{
#'   \item \code{overallAcc} is the overall r-squared, computed by \eqn{1 - \frac{SS.residual}{SS.total}}{1 - SS.residual/SS.total}
#'   \item \code{mse} is the raw mean squared error, computed by \eqn{mean(SS.residual)/N}
#' }
#'
#' @param pred the predicted classes.
#' @param valid the validation classes.
#' @param digits (optional) the number of digits to output for the error; defaults to 3.
#' @param classNames (optional) a character vector of class names (strings). It is necessary because in grouped models, there are no
#'   meaningful classnames stored internal to the model. classNames will be subsetted to include only the levels that are actually present
#'   in the model. See \code{\link{ecoGroup}} for more information on how to garner and store useful grouping labels.
#' @return Note: this function returns different data depending on the whether the model is categorical or continuous:
#' \itemize{
#'   \item \code{categorical} a five element named list: \code{confMatrix} = confusion matrix, \code{userAcc} = user accuracies,
#'     \code{prodAcc} = producer accuracies, \code{classLevelAcc} = class-level accuracies, \code{overallAcc} = overall accuracy,
#'     \code{kappa} =kappa
#'   \item \code{continuous} a two element names list: \code{overallAcc} = overall accuracy, \code{mse} = mean squared error
#' }
#'
#' @seealso
#'   \code{\link{generateModels}} for creating models; \code{\link{isCat}}, \code{\link{isCont}} for how categorical/continuous type is evaluated.
#' @examples
#' data ('siteData')
#' modelRun <- generateModels (data = siteData,
#'                             modelTypes = suppModels,
#'                             x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
#'                             y = 'ecoType',
#'                             grouping = ecoGroup[['domSpecies','transform']])
#' model <- modelRun$randomForest
#' mAcc <- classAcc (getFitted(model),getData(model)[['ecoType']],
#'                   classNames=ecoGroup[['domSpecies','labels']])
#' str (mAcc,1)
#'
#' modelRun <- generateModels (data = siteData,
#'                             modelTypes = contModels,
#'                             x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
#'                             y = 'easting')
#' model <- modelRun$randomForest
#' mAcc <- classAcc (getFitted(model),getData(model)[['easting']])
#' str (mAcc,1)
#' @export
classAcc <- function (pred, valid, digits=3, classNames=NULL) {
  if (isCat(pred)) { classAcc.Cat (pred, valid, digits, classNames) }
  else if (isCont(pred)) { classAcc.Cont (pred, valid, digits, classNames) }
  else stop("classAcc: predictor satisfied neither categorical or continuous criteria; check data.")
}
classAcc.Cat <- function (pred, valid, digits=3, classNames=NULL) {
  pred <- trimLevels(pred);          valid <- trimLevels(valid)
  pred <- mergeLevels(pred,valid);   valid <- mergeLevels(valid,pred)
  if (!is.null(classNames) && length (classNames) != max(as.numeric(levels(valid)))) { warning("classAcc: classnames does not contain the same number of values as there are classes; using default values"); classNames <- NULL }
  if (is.null(classNames)) { classNames <- as.numeric(levels(valid)) } else { classNames <- classNames [as.numeric(levels(valid))] }

  conf <- table(pred,valid)                                   # Confusion matrix
  userTot <- apply (conf,1,sum)
  prodTot <- apply (conf,2,sum)
  Tot <- sum(conf)

  userAcc <- diag(conf)/userTot                               # User accuracies
  prodAcc <- diag(conf)/prodTot                               # Producer accuracies
  overallAcc <- sum(diag(conf))/Tot                           # Overall accuracy
  classLevelAcc <- diag(conf)/(userTot+prodTot-diag(conf))         # Class-level accuracies
  kappa <- (Tot*sum(diag(conf)) - sum(userTot*prodTot)) / (Tot^2 - sum(userTot*prodTot))

  dimnames(conf) <- list(classNames,classNames)
  names (userAcc)  <- classNames
  names (prodAcc)  <- classNames
  names (classLevelAcc) <- classNames
  return (list(confMatrix=conf, userAcc=userAcc, prodAcc=prodAcc, classLevelAcc=classLevelAcc, overallAcc=overallAcc, kappa=kappa))
}
classAcc.Cont <- function (pred, valid, digits, classNames) {
  SS.res <- sum( (pred-valid)^2 )
  SS.tot <- sum((valid-mean(valid))^2)
  rsq <- 1-SS.res/SS.tot
  mse <- mean(SS.res)/length(pred)
  # summary (lm('y~x',data.frame(x=valid,y=pred)))
  # prsq <- 1-mse/var(valid)                                    # Pseudo R-square as reported by randomForest
  return (list(overallAcc=rsq, mse=mse))
}

##### validate #####
#' Check the accuracy of a model using independent validation data
#'
#' This function is a wrapper for classAcc that, given an independent validation dataset, uses the model to come up with new predicted
#' values. It then calls \code{\link{classAcc}} with these two validation datasets (the one generated by the model and the true values) to
#' report class accuracies.
#'
#' @param model the model to test.
#' @param valid the validation dataset; must contain all the parameters used in the model.
#' @param ... other variables to pass to \code{\link{classAcc}}.
#'
#' @return Returns the result from \code{\link{classAcc}}
#' @seealso \code{\link{classAcc}}
#'
#' @examples
#' data ('siteData')
#' # With categorical data
#' gen <- sample(1:nrow(siteData),floor(nrow(siteData)*0.5))
#' modelRun <- generateModels(data = siteData[sort(gen),],
#'                            modelTypes = suppModels,
#'                            x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
#'                            y = 'ecoType',
#'                            grouping = ecoGroup[['identity','transform']],
#'                            echo = FALSE)
#' valid <- siteData[-gen,]
#' valid$ecoType <- as.factor(ecoGroup[['identity','transform']][valid$ecoType])
#' validate(modelRun[[2]],valid)
#' validModels(modelRun,valid)
#'
#' # With continuous data
#' gen <- sample(1:nrow(siteData),floor(nrow(siteData)*0.5))
#' modelRun <- generateModels(data = siteData[sort(gen),],
#'                            modelTypes = contModels,
#'                            x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
#'                            y = 'easting',
#'                            echo = FALSE)
#' valid <- siteData[-gen,]
#' validate(modelRun[[2]],valid)
#' modelsValid(modelRun,valid)
#' @export
validate <- function(model, valid,...){
  x <- y <- NULL
  fx2vars (getFormula(model),x,y,names=names(valid))
  res <- buildPredict(model)(model, valid[,x])                # rFSRC is cranky about types and levels in y variables so just don't supply it as it isn't necessary
  if (isCat(model)) res <- suppressWarnings(prob2class(res))  # prob2class throws an warning if there is only one column; this should only occur if model is FNN
  classAcc(res,valid[,y],...)
}

##### modelAccs #####
#' Generate accuracy statistics and variable importance for a list of models; a wrapper for \code{classAcc}.
#'
#' Given a list of models, this function runs \code{\link{classAcc}} and \code{\link{npelVIMP}} on each. See details for a
#' description of how the data is encapsulated for return.
#'
#' The function returns a named list of the various accuracy and variable importance metrics:
#' \itemize{
#'   \item \code{Accuracies} contains the complete names list returned by \code{\link{classAcc}}; see the help on that method for more
#'     information on that data structure.
#'   \item \code{VIMP} a named list containing the variable importance matrices (VIMP) for each included model. Each entry in the list is a
#'     data frame in which columns are different classes and rows are different input variables; specific entries represent the variable
#'     importance of a given input variable on a particular class.
#'   \item \code{VIMPoverall} a data.frame showing the overall variable importance (VIMP) for a given model.
#'
#'   Columns are different models and entries in the table are the standardized VIMP for an input variable on that model. Values have been
#'   standardized by column so they are (somewhat) comparable as the algorithm by which VIMP is computed varies by model--hence the largest
#'   value will be 1 in every column. This makes the columns approximately comparable, however, values should be taken with a grain of salt,
#'   and perhaps rank order is the most robust comparison.
#' }
#' @note
#'   See the details section of \code{\link{npelVIMP}} for a discussion of the limitations of our VIMP metric.
#'
#' @param models is either a list of model objects on which to find error statistics, or a single model to evaluate.
#' @param classNames (optional) class names to attach to the tables; only used if the data is categorical.
#' @param calc (optional) passed on to \code{\link{npelVIMP}}, defaults to false.
#' @param echo (optional) should the function print out it's progress, defaults to false.
#'
#' @return This function returns different values depending on whether the model is categorical or continuous. For categorical data it
#'   returns a named list of accuracy and VIMP statistics:
#'   \itemize{
#'     \item \code{confMatrix} = confusion matrix
#'     \item \code{userAcc} = user accuracy
#'     \item \code{prodAcc} = producer accuracy
#'     \item \code{overallAcc} = overall accuracy
#'     \item \code{kappa} = kappa
#'     \item \code{VIMP} = VIMP matrices
#'     \item \code{VIMPoverall} = overall VIMP
#'   }
#' For continuous data:
#'   \itemize{
#'     \item \code{overallAcc} = overall r-squared
#'     \item \code{mse} = mean squared error
#'   }
#' @seealso \code{\link{classAcc}} for more on continuous and categorical accuracies, and \code{\link{npelVIMP}} for more on how that metric
#'   is computed and it's limitations. Also see \code{\link{modelsValid}} for a function to compute accuracies using an independent dataset.
#'
#' @examples
#' # Categorical Data
#' data ('siteData')
#' modelRun <- generateModels (data = siteData,
#'                             modelTypes = suppModels,
#'                             x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
#'                             y = 'ecoType',
#'                             grouping = ecoGroup[['domSpecies','transform']],
#'                             echo = FALSE)
#' mE <- modelAccs (modelRun, ecoGroup[['domSpecies','labels']])
#' str(mE,1)
#'
#' # Continuous Data
#' modelRun <- generateModels (data = siteData,
#'                             modelTypes = contModels,
#'                             x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
#'                             y = 'easting',
#'                             echo = FALSE)
#' mE <- modelAccs (modelRun)
#' str(mE,1)
#' @export
modelAccs <- function (models, classNames=NULL, calc=F, echo=T) {
  if (!'list' %in% class(models)[[1]]) models <- list (models)                # In case it is only a single model, wrap it in a list
  if (isCat(models[[1]])) { wrapAccs.Cat(models=models, valid=NULL, func=classAcc, VIMP=TRUE, calc=calc, classNames=classNames, echo=echo) }
  else if (isCont(models[[1]])) { wrapAccs.Cont(models=models, valid=NULL, func=classAcc, VIMP=TRUE, calc=calc, echo=echo) }
  else stop("modelAccs: model satisfied neither categorical or continuous criteria.")
}

##### modelsValid #####
#' Generate validation statistics for a list of models; a wrapper for \code{validate}.
#'
#' Given a list of models, this function runs \code{\link{validate}} on each. See details for a description of how the data is encapsulated
#' for return.
#'
#' @param models is either a list of model objects on which to find error statistics, or a single model to evaluate.
#' @param valid a validation dataset with which to test the accuracy of the models provided.
#' @param classNames (optional) class names to attach to the tables; only used if the data is categorical.
#' @param calc (optional) passed on to \code{\link{npelVIMP}}, defaults to false
#' @param echo (optional) should the function print out it's progress, defaults to false
#'
#' @return As with \code{\link{modelAccs}}, this function returns different values depending on whether the model is categorical or
#'   continuous. For categorical data it returns a named list of accuracy statistics:
#'   \itemize{
#'     \item \code{confMatrix} = confusion matrix
#'     \item \code{userAcc} = user accuracy
#'     \item \code{prodAcc} = producer accuracy
#'     \item \code{overallAcc} = overall accuracy
#'     \item \code{kappa} = kappa
#'   }
#' For continuous data:
#'   \itemize{
#'     \item \code{overallAcc} = overall r-squared
#'     \item \code{mse} = mean squared error
#'   }
#' @seealso \code{\link{classAcc}} for more on the computation of continuous and categorical accuracies, and \code{\link{modelAccs}} for the
#'   corresponding function that will compute accuracy based on the dataset used for model building.
#'
#' @examples
#' # With categorical data
#' data ('siteData')
#' gen <- sample(1:nrow(siteData),floor(nrow(siteData)*0.5))
#' modelRun <- generateModels(data = siteData[sort(gen),],
#'                            modelTypes = suppModels,
#'                            x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
#'                            y = 'ecoType',
#'                            grouping = ecoGroup[['identity','transform']],
#'                            echo = FALSE)
#' valid <- siteData[-gen,]
#' valid$ecoType <- as.factor(ecoGroup[['identity','transform']][valid$ecoType])
#' mV <- modelsValid(modelRun,valid)
#' str(mV,2)
#'
#' # With continuous data
#' gen <- sample(1:nrow(siteData),floor(nrow(siteData)*0.5))
#' modelRun <- generateModels(data = siteData[sort(gen),],
#'                            modelTypes = contModels,
#'                            x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
#'                            y = 'easting',
#'                            echo = FALSE)
#' valid <- siteData[-gen,]
#' mV <- modelsValid(modelRun,valid)
#' str(mV,2)
#' @export
modelsValid <- function(models, valid, classNames=NULL, calc=F, echo=T){
  if (!'list' %in% class(models)[[1]]) models <- list (models)                # In case it is only a single model, wrap it in a list
  if (isCat(models[[1]])) { wrapAccs.Cat(models=models, valid=valid, validate, VIMP=FALSE, calc=FALSE, classNames=classNames, echo=echo) }
  else if (isCont(models[[1]])) { wrapAccs.Cont(models=models, valid=valid, func=validate, VIMP=FALSE, calc=FALSE, echo=echo) }
  else stop("validModels: model satisfied neither categorical or continuous criteria.")
}


##### wrapAccs.Cat #####
#' Thin wrappers to package up categorical/continuous accuracies/validation data for a group of models.
#'
#' These functions are intended for internal use---as such no defaults are provided and there is little to no internal parameter
#' checking. Using this function it is possible, however, to do things like recover VIMP data using a validating dataset, which is not
#' possible using the default package functions.
#'
#' @param models the model group to process.
#' @param valid validation data if relevant, i.e. if func = 'validate'.
#' @param func the function to use to evaluate accuracy: classAcc or validate.
#' @param VIMP should we add the VIMP data.
#' @param calc should we compute the VIMP metric from scratch, only referenced if VIMP = TRUE.
#' @param classNames the classnames.
#' @param echo should the function print out processing messages as it runs.
#'
#' @return a the class that \code{\link{modelAccs}} or \code{\link{modelValid}} needs.
#' @export
wrapAccs.Cat  <- function(models, valid, func, VIMP, calc, classNames, echo) {
  y <- NULL
  fx2vars(getFormula(models[[1]]),y=y)
  if (!is.null(classNames) && length (classNames) != max(as.numeric(levels(getData(models[[1]])[,y])))) { warning("modelAccs: classnames does not contain the same number of values as there are classes; using default values"); classNames <- NULL }

  userAcc <- prodAcc <- VIMPoverall <- confMatrix <- kappa <- outVIMP <- colNames <- NULL
  for (i in models) {
    if (echo) print (paste0('Computing accuracy: ',class(i)[[1]]))
    colNames <- c(colNames, class(i)[[1]])

    if (substitute(func) == 'classAcc') { tmp <- classAcc(getFitted(i), getData(i)[,y], classNames=classNames) }
    if (substitute(func) == 'validate') { tmp <- validate(i, valid) }

    confMatrix <- c(confMatrix, list(tmp$confMatrix))
    user <- c(tmp$userAcc,tmp$overallAcc);    user <- data.frame(class=as.numeric(names(user)),user)
    prod <- c(tmp$prodAcc,tmp$overallAcc);    prod <- cbind (class=as.numeric(names(prod)),prod)
    if (is.null(userAcc)) {
      userAcc <- user
      prodAcc <- prod
    } else {
      userAcc <- merge(userAcc, user, by='class', all=T)  # Cannot just cbind as they might not have the same number of classes...
      prodAcc <- merge(prodAcc, prod, by='class', all=T)
      userAcc['Row.names'] <- prodAcc['Row.names'] <- NULL
      names(userAcc) <- names(prodAcc) <- c('class',colNames)
    }
    kappa <- c(kappa,tmp$kappa)

    if (VIMP) {
      tmp <- npelVIMP (i,calc=calc,echo=echo)
      cols <- 2:(ncol(tmp)-( if ('gbm' %in% class(i) && !calc) 1 else 0 ))

      classN <- classNames
      if (!is.null(classN) && length (classN) != max(as.numeric(levels(getData(i)[,y])))) { warning("modelAccs: classnames does not contain the same number of values for this model; using default values"); classN <- NULL }
      if (is.null(classN)) { classN <- as.numeric(levels(getData(i)[,y])) } else { classN <- classN [as.numeric(levels(getData(i)[,y]))] }
      colnames(tmp)[cols] <- classN

      outVIMP <- c(outVIMP, list(tmp[,2:ncol(tmp)]))
      VIMPoverall <- as.data.frame(cbind(VIMPoverall, tmp[,1]))
    }
  }
  l <- length(userAcc$class); rownames(userAcc) <- c(userAcc$class[1:(l-1)],'overall')
  l <- length(prodAcc$class); rownames(prodAcc) <- c(prodAcc$class[1:(l-1)],'overall')
  userAcc <- userAcc[,-1]
  prodAcc <- prodAcc[,-1]
  names(userAcc) <- names(prodAcc) <- names(confMatrix) <- names(kappa) <- colNames

  if (VIMP) {
    names(outVIMP) <- names(VIMPoverall) <- colNames
    return (list(confMatrix=confMatrix, userAcc=userAcc, prodAcc=prodAcc, kappa=kappa, VIMP=outVIMP, VIMPoverall=VIMPoverall))
  } else {
    return (list(confMatrix=confMatrix, userAcc=userAcc, prodAcc=prodAcc, kappa=kappa))
  }
}
#' @rdname wrapAccs.Cat
#' @export
wrapAccs.Cont <- function(models, valid, func, VIMP, calc, echo) {
  x <- y <- NULL
  fx2vars(getFormula(models[[1]]),x=x,y=y)
  overallAcc <- VIMPoverall <- colNames <- NULL
  for (i in models) {
    if (echo) print (paste0('Computing accuracy: ',class(i)[[1]]))
    colNames <- c(colNames, class(i)[[1]])

    if (substitute(func) == 'classAcc') { tmp <- classAcc(getFitted(i), getData(i)[,y]) }
    if (substitute(func) == 'validate') { tmp <- validate(i, valid) }
    overallAcc <- rbind(overallAcc, data.frame(overallAcc=tmp$overallAcc, mse=tmp$mse))

    if (VIMP) {
      tmp <- npelVIMP (i, calc=calc, echo=echo)
      VIMPoverall <- as.data.frame(cbind(VIMPoverall, tmp[,1]))
    }
  }
  overallAcc <- as.data.frame(overallAcc)
  rownames(overallAcc) <- colNames
  if (VIMP) {
    colnames(VIMPoverall) <- colNames
    rownames(VIMPoverall) <- x
    return (list(overallAcc=overallAcc, VIMPoverall=VIMPoverall))
  } else {
    return (list(overallAcc=overallAcc))
  }
}

