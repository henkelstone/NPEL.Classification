# Helper functions for classificaiton for raster image data
# Created by Jonathan Henkelman 6.Apr.2015
#
# These functions were abstracted from the code written to analyze the raster data once it 
# became apparent that it was going to have to run on multiple tiles, with multiple layers
# etc. It was no longer straightforward to maintain the code, so these functions were generated
# to facilitate future work.

sort.levels <- function (x) {
  # I cannot find a utility function in package:base to do this, so this function reassigns a factor the same data, but with the levels in sorted order. This
  # facilitates output in several functions, e.g. table
  #   x: the input factor variable
  # returns the same data with the levels sorted
  return ( factor(x,levels=sort(as.numeric(levels(x)))) )
}
trim.levels <- function (x) {
  # I cannot find a utility function in package:base to do this either; this function trims the levels attribute to represent only the levels that are actually present.
  #   x: the input factor variable
  # returns the same data with the levels clipped to reflect the factors that are present
  return ( factor(x,levels=levels(factor(x))) )
}
merge.levels <- function (x,y) {
  # A utility function to merge the levels of two factors; the factors of y not present in x will be added to the x levels.
  #   x,y: the factors to merge
  # returns the factor x with the merged levels of the two factor variables

  x <- factor(x,union(levels(x),levels(y)))
  return ( sort.levels(x) )
}
minExtent <- function (...) {
  # A utility function to find the minimum extent of two Raster* object
  #   ...: the raster* object to compare
  # returns the minimum extent
  
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

readTile <- function (rasterPath, rasterName, layers, labels=NULL, NAval=NULL) {
  # Read in the raster data for a tile. Inputs are:
  #   rasterPath: path to the raster file
  #   rasterName: name of the file
  #   layers: a character list of layers to read. The filename used will be 'rasterName_layer.tif'
  #   labels (optional): a list of names for the columns of the final raster. Will attempt to use the layer names if it appears there is only one layer per file.
  # Returns a stack of raster*
  
  retList <- list ()
  rasterName <- sub('.tif','',rasterName)       # Strip off the .tif if present
  for (l in layers) retList <- c(retList,  base=brick(paste0(rasterPath,rasterName,'_',l,'.tif')))
  retStack <- stack(retList)
  
  if (!is.null(labels)) names(retStack) <- labels
  else if (nlayers (retStack) == length(layers)) names(retStack) <- layers # If there is only one layer per file, then use the layers as names.
  
  if (!is.null(NAval)) NAvalue(retStack) <- NAval
  return (retStack)
}
mergeTile <- function (dataList, path='', fileName='') {
  # A utility function to merge a list of raster* and output them as a brick. This has not been extensively tested and it will fail if the criteria for
  # brick() are not met.
  #   dataList: a list of raster* objects
  #   path: path to the output file. If it is not specified, the current working directory will be used
  #   fileName: a filename to use. If it is not specified, the filename of the first element in the list will be used.
  # Returns: a brick with the specified data
  
  if (path == '') path <- getwd()
  if (fileName == '') fileName <- tail(strsplit (filename(rasterData[[1]]), '/')[[1]],1)
  tmp <- stack (c(rasterData[1:length(rasterData)]))
  return ( brick(tmp, values=T, filename=paste0(path,fileName)) )
}
writeTile <- function (outFilename, inImage, model, layers=c("class"), threshold=0, n.all=0, labels.all=NULL, ...) {
  # This function computes the output classes using the model specified. As output it expects a probabiliy for each possible class. It then scans the 
  # probabilities for the maximum value. If this is all that is desired, 
  #   outFilename: the output filename
  #   inImage: the input data in the form of a raster*
  #   model: the model to use for predicting output values. Model package must have a predict.'model' function.   
  #   layers: one of 
  #     "class": the chosen class; 
  #     "prob": the probability of this class; 
  #     "threshold": same as class where probability is greater than or equal to threshold (below), 0 otherwise;
  #     "all": the probabilies for all classes (one layer per possibility). Note: this may be a large dataset!!
  #   threshold: if the 'threshold' layer is selected, this needs to be set to the threshold probability (0..1)
  #   n.all: if the 'all' group is selected in layers, specify how many layers this is going to be.
  #   labels.all: if the 'all' group is selected in layers, specify labels for these layers
  #   ...: other variables to pass to the model predict function. Varies widely depending on the type of the model.
  # Returns the new raster* object
  
  # Figure out what type of predict function we need to call. I'd like this to be more elegant, but the nearest neighbour models do not follow the standard predict model so I have to work around it.
  probs = T                                             # Are the outputs of 'predict' probabilities of class occurance (preferred) or simply the classes themselves (knn only to date)
  if ("randomForest" %in% class(model)) func <- function (model,data,...) return ( predict(model,(function(tmp){ tmp[is.na(tmp)] <- 0; return (tmp)} )(data),type='prob',...) )    # Pass through to predict.randomForest
  else if ("rfsrc" %in% class(model))   func <- function (model,data,...) return  (predict(model,data,importance='none',na.action='na.impute',...)$predicted )  # Pass through and output the probability table from the new data
  else if ("fnn" %in% class(model)) {
    if (length(layers) > 1 || !('class' %in% layers)) warning ("Warning: Cannot generate anything other than class output for fnn models. Setting output to 'class' only.")
    layers = 'class'
    probs<-F
    func <- function (model,data,...) { data[is.na(data)] <- 0; return ( knn (model$train,data[names(model$train)],model$classes,...) ) } # If we have created a fnn class then this is where it will get run
  } 
  else if ("kknn" %in% class(model))    func <- function (model,data,...) return( kknn(formula=model$terms,train=model$data,test=(function(tmp){ tmp[is.na(tmp)] <- 0; return (tmp)} )(data),k=model$best.parameters[[2]],kernel=model$best.parameters[[1]],na.action=na.pass(),...)$prob)      # Predict for package:kknn
  else if ("gbm" %in% class(model))     func <- function (model,data,...) return( predict (model,data,n.trees=(function(){ capture.output(tmp <- gbm.perf(gbm,plot.it=F)); return(tmp); })(),type='response',...)[,,1] ) # The convoluted anonymous inline function is used to supress the print output from gbm.perf...
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
plotTile.base <- function (data, layers=NULL, discrete=F, title="", maxpixels=500000, reduction=1, ...) {
  # The thin wrapper provided by rasterVis does not do a great job with multiple layer rasters. This is another attempt that provides more general
  # functionality useful to this project. The function plots each layer as a facet; if you want to see less layers use the raster
  # function subset to reduce the number of layers fed in.
  #   data: a raster* object
  #   layers: the list of layers to include
  #   title: a title for the plot
  #   maxpixels: the maximum number of pixels to output 
  #   reduction: is a reducing scale factor applied to both x and y axis. Resampling will generate whichever is less: ncell/(reduction^2) or maxpixels
  #   ...: other parameters to pass to ggplot
  # Returns a ggplot object which can have other geoms, scales, etc. added to it, particularly with PlotTile

  if (!require(ggplot2)) stop ("Error: package ggplot2 is required to plot")
  if (!is.null(layers)) data <- subset(data,layers)

  # Theres a glitch in sampleRegular: we can't just simply ask for sR(data, number of cells, xy=T) as it doesn't return the same values for xy as if we
  # do it step by step: x <- sR(data,#cells,xy=F,raster=T); xyFromCell(x,1:#cells). I don't know why this is, but it causes artifacts in the final plot
  # so it is necessary to do the work around shown here... It just means we have to extract the data twice so it's a bit slower.
  colNames <- names (data)
  data <- sampleRegular(data, size=min(ncell(data)/(reduction*reduction),maxpixels),asRaster=T) # Resample if necessary, but return a raster so we can extract the coords afterwards.
  data <- data.frame(xyFromCell(data,1:ncell(data)), getValues (data))                          # Extract the data required for the plot
  names(data) <- c('x','y',colNames)
  dat <- reshape(data=data,direction='long',idvar=1:2,varying=3:dim(data)[2],v.names='value',timevar='type',times=names(data)[3:dim(data)[2]]) # Massage into long form; over a GB for a full tile!

  # Create a base plot object with some useful aesthetics and that holds the data; then if this is not the only plot the user wants, they should be able to output more without needing to resample the data
  gp <- ggplot(aes(x = x, y = y), data = dat, ...) +  
    theme(axis.text.y=element_text(angle=90,hjust=0.5)) + coord_equal() +
    labs(title=title,x="Latitude (UTM)",y="Longitude (UTM)",fill="Value")
}
plotTile <- function (gp, layers, discrete, colours, labels=NULL,...){
  # Take the base ggplot created by plotTile.base and give it colours and a fill scale, i.e. plot it with the specific output in mind. These two functions
  # have been split apart so it is only necessary to resample the data once, but several plots can then be created. 
  #   gp: the base ggplot to which to add a colour scale and aesthetic
  #   layers: a character vector of the levels which to plot
  #   discrete: force the plot to attempt a discrete fill axis
  #   colours: specify the colours use; a list if discrete is TRUE, or 2/3 colours for the gradient if discrete is FALSE
  #   labels: if desired, specify the names of the labels for each class
  # Returns the ggplot object with additional aes and scale
  
  # Note: ggplot saves the data in the object which both allows this, but also makes for a large object... like over a GB for a full tile -- I recommend against
  # saving the R database on exit without being sure you have time for this file to save/load.
  
  if (!require(ggplot2)) stop ("Error: package ggplot2 is required to plot")
#  if (!require(plyr)) stop ("Error: package plyr is required to plot")
  if (discrete) { if (is.null(colours)) stop ("Error: if discrete is specified, so must be colours") }
  else          { if (is.null(colours) || !sum(length(colours) == c(2,3))) stop("Error: either 2 or 3 colours need to be specified for continuous gradient scales") }
  
  gp <- gp %+% subset(gp$data,gp$data$type %in% layers)   # Subset the data. Necessary to do it this way so we don't have to specify the subset to every item, i.e. scale, geom, facet, etc.
  if (length(list(...))) gp <- gp + ...                   # Add any extra parameters the user may have supplied
  if (discrete) {                                         # If it's discrete then the user should have supplied the colours
    if (is.null(labels)) return( gp + aes(fill=factor(value)) + geom_raster() + scale_fill_manual(values=colours) )
    else                 return( gp + aes(fill=factor(value)) + geom_raster() + scale_fill_manual(values=colours, labels=labels) )
  } else {
    if (length (colours) == 2) return( gp + aes(fill=value) + geom_raster() + scale_fill_gradient (low=colours[1], high=colours[2]) + facet_wrap(~ type) )
    if (length (colours) == 3) return( gp + aes(fill=value) + geom_raster() + scale_fill_gradient2(low=colours[1], mid=colours[2], high=colours[3], midpoint=(max(gp$data$value,na.rm=T)-min(gp$data$value,na.rm=T))/2) + facet_wrap(~ type) )
  }
}

writeTiles <- function (path, base, extension, models, layers=c('class')) {
  # Utility function for output multiple plots of multiple datafiles in a single folder. The output filename will be the model type sandwiched between base and extension.
  #   path: the path of the folder
  #   base: the base part of the filename
  #   extension: the latter part of the filename
  #   models: a list of model names
  #   layers: one or more of 'class':the output classes, 'prob':the probability of that class, 'all': the probability for every possible class
  for (fType in models) {
    if (fType == 'rF')    { model <- rF;    n.all = length(rF$classes);                labels.all = paste0('Prob.',rF$classes);                require (randomForest) }
    if (fType == 'rFSRC') { model <- rFSRC; n.all = length(levels(rFSRC$class));       labels.all = paste0('Prob.',levels(rFSRC$class));       require (randomForestSRC) }
    if (fType == 'fnn')   { model <- fnn;   n.all = 0;                                 labels.all = NULL;                                      require (FNN) }
    if (fType == 'kknn')  { model <- kknn;  n.all = length(levels(kknn$data$Ecotype)); labels.all = paste0('Prob.',levels(kknn$data$Ecotype)); require (kknn) }
    if (fType == 'gbm')   { model <- gbm;   n.all = length(gbm$classes);               labels.all = paste0('Prob.',gbm$classes);               require (gbm) }
    if (fType == 'svm')   { model <- svm;   n.all = length(svm$levels);                labels.all = paste0('Prob.',svm$levels);                require (e1071) }
    print (fName <- paste0(path, base, fType, extension))
    system.time( writeTile(outFilename=fName, inImage=inImage, model=model, layers, n.all=n.all, labels.all=labels.all) )#,threshold=0.5) )
  }
}
plotTiles <- function (path, base, extension, models, type='pdf',...) {
  # Utility function for output multiple plots of multiple datafiles in a single folder
  #   path: the path of the folder
  #   base: the base part of the filename
  #   extension: the latter part of the filename
  #   models: a list of model names
  #   type: type out output to use
  #   ...: other parameters to pass to the device function
  
  for (fType in models) {
    fName <- paste0(path,base,fType,extension)
    Tile <- brick(paste0 (fName,'.tif'))  #    Tile <- crop (Tile,extent(439733.60, 440733.60, 6332774.82, 6333774.82))
    if (nlayers(Tile) > 1) { names(Tile) <- c('Class', 'Prob', paste0('Prob.',1:(nlayers(Tile)-2))) } else { names(Tile) <- c('Class') }

    if (type=='png') png(paste0(fName,'.png'),...)
    else pdf(paste0(fName,'.pdf'),width=10.0,height=7.5,onefile=T)
    
    gp <- plotTile.base(Tile, layers=names(Tile), maxpixels=1000000)
    plotTile(gp, layers='Class', discrete=T, colours=ecoGroup[['domSpecies','colours']]) #,aes(alpha='Prob'))
    if (fType != 'fnn') plotTile(gp, layers=c('Prob',paste('Prob',1:7,sep='.')), discrete=F, colours=c('grey17','red','yellow'))
#    Sys.sleep(15)
    dev.off(dev.cur())
  }
}

extractPoints <- function (rData,...) UseMethod("extractPoints")
extractPoints.RasterStackBrick <- function (rData, vData, colNames=NA) {
  # Extracts the data corresponding to each point from the raster data.
  #   rData: the raster data brick
  #   vData: the vector data
  #   colNames: a collection of column names. If unspecified the function will pull them from the layers in the brick.
  # Returns: a data frame with the data extracted from the raster layers specified
  
  pointData <- data.frame()
  len <- dim(vData)[1]
  spPoints <- SpatialPoints(vData[,c('lat','long')], proj4string=rData@crs)
  pb <- txtProgressBar (0,len,style=3)
  for (i in 1:len) {
    pixel <- extract (rData, spPoints[i])
    result <- sum(pixel!=0, na.rm=T)
    if (result != 0) pointData <- rbind (pointData,cbind(vData[i,],pixel))    # Only keep the value if it non-zero (and hence, non-null)
    
    setTxtProgressBar (pb,i)
  }
  close (pb)
  
  # Assign the output array some meaningful names based on the layers requested
  if (is.na(colNames[1])) colNames <- names (rData)
  names (pointData)[(ncol(vData)+1):ncol(pointData)] <- colNames
  return (pointData)
}
extractPoints.list <- function (rData, vData,layers='all', colNames=NA) {
  
  # Extracts the data corresponding to each point from the raster data.
  #   rData: the raster data set(s) as read in my readTile; this function only handles a single layer at a time
  #   vData: the vector data
  #   layers: a character list of layers to read; can be one of 'base', 'ndvi', 'msavi', 'savi' or 'all'
  #   colNames: a collection of column names. If unspecified the function will pull them from the layers in the brick.
  # Returns: a data frame with the data extracted from the raster layers specified

  pointData <- data.frame()
  len <- dim(vData)[1]
  if (layers=='all') layers <- c('base','ndvi','msavi','savi')
  spPoints <- SpatialPoints(vData[,c('lat','long')], proj4string=rData[[layers[1]]]@crs)
  pb <- txtProgressBar (0,len,style=3)
  for (i in 1:len) {
    pixel <- NULL
    for (l in layers) pixel <- cbind (pixel,extract (rData[[l]], spPoints[i]))  # This does not appear to be slower than doing one layer first and it allows the progressBar to work; it is much faster than converting the whole dataset to a rasterStack!!
    result <- sum(pixel!=0,na.rm=T)
    if (result != 0) pointData <- rbind (pointData,cbind(vData[i,],pixel))    # Only keep the value if it non-zero (and hence, non-null)
    
    setTxtProgressBar (pb,i)
  }
  close (pb)
  
  # Assign the output array some meaningful names based on the layers requested
  if (is.na(colNames)) {
    colNames <- NULL
    for (l in layers) {
      if (l == 'base') colNames <- c(colNames,paste0('Band',1:8))
      else colNames <- c(colNames,l)
    }
  }
  names (pointData)[(ncol(vData)+1):ncol(pointData)] <- colNames
  return (pointData)
}

classErr <- function (x, y, digits=3) {
  # A helper function to print out basic summary statistics from a classification analysis. Currently prints the confusion matrix, the err rate
  # for each input class, and the overall error rate.
  #   x: training classes.
  #   y: output classes.
  #   digits: number of digits to output for the error (optional: defaults to 3)
  # Returns the error data as a 3 element list: confusion matrix, error rates by input, overall error rate
  
  x <- trim.levels(x);      y <- trim.levels(y)
  x <- merge.levels(x,y);   y <- merge.levels(y,x)
  conf <- table(x,y)                                  # Confusion matrix
  err <- (summary(x) - diag(conf))/summary(x)         # Error rates
  err.overall <- sum(err*summary(x),na.rm=T)/sum(summary(x),na.rm=T)  # Overall error rate; just errors averaged weighted by frequency    
  return (list(confMatrix = conf, errRates = err, errOverall = err.overall))
}
