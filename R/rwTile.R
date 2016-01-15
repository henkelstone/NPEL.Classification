# I/O routines for raster tiles
# Created 9.Oct.2015 from prexisting code file started 6.Apr.2015

#' Read a collection of layers
#'
#' The function reads a collection of geotiffs and creates a single raster.stack object
#'
#' @param rasterPath path to the raster file
#' @param rasterName name of the file
#' @param layers a character list of layers to read. The filename used will be 'rasterName_layer.tif'
#' @param labels (optional) a list of names for the columns of the final raster. Will attempt to use the layer names if it appears there is only one layer per file.
#' @param NAval the value to be interpreted as NA; passed to raster::NAvalue
#' @return a raster.stack of the objects in the folder
#'
readTile <- function (rasterPath, rasterName, layers, labels=NULL, NAval=NULL) {
  retList <- list ()
  rasterName <- sub('.tif','',rasterName)
  for (l in layers) retList <- c(retList,  base=raster::brick(paste0(rasterPath,rasterName,l,'.tif')))
  retStack <- raster::stack(retList)

  if (!is.null(labels)) names(retStack) <- labels
  else if (raster::nlayers (retStack) == length(layers)) names(retStack) <- layers # If there is only one layer per file, then use the layers as names.

  if (!is.null(NAval)) raster::NAvalue(retStack) <- NAval
  return (retStack)
}

#' Merge a list of raster objects into a single brick
#'
#' A utility function to merge a list of raster* objects and output them as a brick. Note that all
#' the objects will need to obey the limitations of brick or this function will fail.
#'
#' @param dataList a list of raster* objects
#' @param path (optional) path to the output file. If unspecified, getwd() will be used.
#' @param fileName (optional) output filename to associate with the new brick. If unspecified, the filename of the first element will be used.
#' @return a brick with the specified data
#'
mergeTile <- function (dataList, path='', fileName='') {
  if (path == '') path <- getwd()
  if (fileName == '') fileName <- tail(strsplit (filename(rasterData[[1]]), '/')[[1]],1)
  tmp <- raster::stack (c(rasterData[1:length(rasterData)]))
  return ( raster::brick(tmp, values=T, filename=paste0(path,fileName)) )
}

#' Computes an output landscape
#'
#' This function computes and outputs a generated landscape, and optionally the probabilities
#'   of the winning class and/or every possible class.
#'
#' Note: outputing all the probabilities can generate quite a large datafile, that is, in the order of a GB or more.
#'
#' @param outFilename the output filename
#' @param inImage a raster* of the input data
#' @param model the model to use for predicting output values. The model class
#'   must have a predict.model function.
#' @param layers one of: "class" the chosen class; "prob" the probability of
#'   this class; "threshold" same as class where probability is greater than
#'   or equal to threshold (below), 0 otherwise; "all" the probabilies for all
#'   classes (one layer per possibility). Note: this may be a large dataset!
#' @param threshold if the "threshold" layer is selected, this needs to be set to the threshold probability (0:1).
#' @param n.all if the 'all' group is selected in layers, specify how many layers this is going to be.
#' @param labels.all if the 'all' group is selected in layers, specify labels for these layers.
#' @param ... other variables to pass to the model predict function.
#' @return the new raster* object with the output landscape
#'
writeTile <- function (outFilename, inImage, model, layers=c("class"), threshold=0, n.all=0, labels.all=NULL, ...) {
  # ??? need to add package prefix to each function call

  # Figure out what type of predict function we need to call. I'd like this to be more elegant, but the nearest neighbour models do not follow the standard predict model so I have to work around it.
  probs = T                                             # Are the outputs of 'predict' probabilities of class occurance (preferred) or simply the classes themselves (knn only to date)
  if ("randomForest" %in% class(model)) func <- function (model,data,...) return( predict(model,(function(tmp){ tmp[is.na(tmp)] <- 0; return (tmp)} )(data),type='prob',...) )    # Pass through to predict.randomForest
  else if ("rfsrc" %in% class(model))   func <- function (model,data,...) return( predict(model,data,importance='none',na.action='na.impute',...)$predicted )  # Pass through and output the probability table from the new data
  else if ("fnn" %in% class(model)) {
    if (length(layers) > 1 || !('class' %in% layers)) warning ("Warning: Cannot generate anything other than class output for fnn models. Setting output to 'class' only.")
    layers = 'class'
    probs<-F
    func <- function (model,data,...) { data[is.na(data)] <- 0; return ( knn (model$train,data[names(model$train)],model$classes,...) ) } # If we have created a fnn class then this is where it will get run
  }
  else if ("kknn" %in% class(model))    func <- function (model,data,...) return( kknn(formula=model$terms,train=model$data,test=(function(tmp){ tmp[is.na(tmp)] <- 0; return (tmp)} )(data),k=model$best.parameters[[2]],kernel=model$best.parameters[[1]],na.action=na.pass(),...)$prob)      # Predict for package:kknn
  else if ("gbm" %in% class(model))     func <- function (model,data,...) return( predict (model,data,n.trees=(function(){ capture.output(tmp <- gbm.perf(model,plot.it=F)); return(tmp); })(),type='response',...)[,,1] ) # The convoluted anonymous inline function is used to supress the print output from gbm.perf...
  else if ("svm" %in% class(model))     func <- function (model,data,...) return( attr(predict(model,(function(tmp){ tmp[is.na(tmp)] <- 0; return (tmp)} )(data),probability=T,na.action=na.pass,...),'probabilities') )
  else stop (paste("Error: unable to predict output from model class --",class(model)))

  # Compute the number of output layers and generate names for them.
  nlayers <- 0
  labels <- NULL
  index <- data.frame(class=0, prob=0)              # Location of the class and prob column in the output (better than saving another copy)
  for (l in layers) {
    if (l == 'class')      { nlayers <- nlayers + 1; labels <- 'Class'; index$class <- 1 }
    if (l == 'prob' )      { nlayers <- nlayers + 1; labels <- c(labels,'Prob'); index$prob <- index$class+1 }
    if (l == 'threshold' ) { nlayers <- nlayers + 1; labels <- c(labels,'Threshold'); if (!(threshold > 0 && threshold < 1)) stop ("Error: threshold probability must be between 0 and 1.") }
    if (l == 'all'  ) if (n.all == 0) stop ("Error: if 'all' probabilities are to be output n.all must report the number of output classes.")
    else {
      nlayers <- nlayers + n.all
      if (is.null(labels.all)) stop ("Error: if 'all' probabilities are to be output n.all must be set.")
      else labels <- c(labels,labels.all)
    }
  }

  # Create raster shell for output
  if (nlayers == 1) outImage <- raster(inImage)         # Create an empty output rasterLayer...
  else outImage <- brick(inImage,nl=nlayers)            # ... or if multiple layers have been requested, create an empty output rasterBrick
  names (outImage) <- labels                            # Set the layer names
  outImage <- writeStart(outImage, filename=outFilename, format='GTiff', overwrite=TRUE)
  bS <- blockSize(inImage)                              # Compute the output block sized as the default for the image

  # Internal functions to simplify code below and reduce duplication
  computeClass <- function () {
    index <- as.numeric(apply (prob, 1, which.max))     # Get the position of the (first) maximum element--the winning class--for each cell. The cast to numeric solves the problem that which.max returns a list if there are NAs. This seems to be faster than appending the max column to the dataset and indexing that location... Don't know why?
    return (as.numeric(colnames(prob))[index])     # Extract the class name from the column headers
  }
  computeMaxProb <- function () { return (apply(prob, 1, max)) } # Extract the maximum probability for each cell

  # Inner loop: step through each datablock, run that data through our classifier (model), generate the required layers, and output the results to a datafile.
  pb <- txtProgressBar (0,bS$n,style=3)
  for (i in 1:bS$n) {
    prob <- func(model, as.data.frame(getValues(inImage,bS$row[i],bS$nrows[i])), ...) # Use the model to predict classes for these cells

    if (!probs) { outData <- as.numeric(levels(prob)[prob]) # If the output is only classes, then we don't have anything interesting to do here
    } else {
      outData <- NULL
      if (match('class',layers,nomatch=0)) outData <- computeClass()
      if (match('prob',layers,nomatch=0)) outData <- cbind(outData,computeMaxProb())
      if (match('threshold',layers,nomatch=0)) {          # If requested, extract the threshold class
        if (index$class==0) class <- computeClass() else class <- outData[,index$class]
        if (index$prob==0)  maxProb <- computeMaxProb() else maxProb <- outData[,index$prob]
        class[maxProb < threshold] <- 0                   # Set entries below the threshold to 0
        outData <- cbind(outData, class)
      }
      if (match('all',layers,nomatch=0)) outData <- cbind(outData,prob) # Add all the probabilities if requested
    }
    writeValues (outImage,outData,bS$row[i])            # Output the data to the file
    setTxtProgressBar (pb,i)
  }
  close (pb)
  writeStop(outImage)                                   # Stop writing and close the file
  return (outImage)
}

#' Write multiple output tiles
#'
#' A utility function to output multiple datafiles in a single folder.
#'
#' The output filename is <path><base><model type><ext>
#'
#' @param path the path of the folder
#' @param base the base part of the filename
#' @param ext the latter part of the filename
#' @param models a list of model names
#' @param layers one or more of 'class':the output classes, 'prob':the probability of that class, 'all': the probability for every possible class
#' @return the time it took to execute
#'
writeTiles <- function (path, base, extension, modelRun, models, layers=c('class')) {
  #??? need to deal with threshold properly!!
  #??? eliminate require calls

  for (fType in models) {
    if (fType == 'rF')    { model <- rF <- modelRun[['rF']];       n.all = length(rF$classes);                labels.all = paste0('Prob.',rF$classes);                require (randomForest) }
    if (fType == 'rFSRC') { model <- rFSRC <- modelRun[['rFSRC']]; n.all = length(levels(rFSRC$class));       labels.all = paste0('Prob.',levels(rFSRC$class));       require (randomForestSRC) }
    if (fType == 'fnn')   { model <- fnn <- modelRun[['fnn']];     n.all = 0;                                 labels.all = NULL;                                      require (FNN) }
    if (fType == 'kknn')  { model <- kknn <- modelRun[['kknn']];   n.all = length(levels(kknn$data$Ecotype)); labels.all = paste0('Prob.',levels(kknn$data$Ecotype)); require (kknn) }
    if (fType == 'gbm')   { model <- gbm <- modelRun[['gbm']];     n.all = length(gbm$classes);               labels.all = paste0('Prob.',gbm$classes);               require (gbm) }
    if (fType == 'svm')   { model <- svm <- modelRun[['svm']];     n.all = length(svm$levels);                labels.all = paste0('Prob.',svm$levels);                require (e1071) }
    print (fName <- paste0(path, base, fType, extension))
    return ( system.time( writeTile(outFilename=fName, inImage=inImage, model=model, layers, n.all=n.all, labels.all=labels.all) ) )#,threshold=0.5) )
  }
}
