# I/O routines for raster tiles
# Created 9.Oct.2015 from pre-existing code file started 6.Apr.2015

##### readTile #####
#' Read a collection of tif layers from a folder
#'
#' The function reads a collection of geotiffs from a folder and creates a single raster.stack object. It is used to streamline the input of
#' many layers at once since Rgdal does not, at the time of this writing (15.Feb.2016) allow the direct processing of raster gdb files. Each
#' layer should be in it's own geoTiff, although multiple bands within a geoTiff are allowed. Each file should be named with the layer name
#' (see parameters). Optional labels can be supplied, but if no, need to be supplied for all layers.
#'
#' @param rasterPath path to the raster file
#' @param layers a character list of layer names to read. The filename used will be '<layer[i]>.tif'
#' @param labels (optional) a list of names for the columns of the final raster; will use layer names if this is not specified.
#' @param NAval (optional) the value to be interpreted as NA; passed to raster::NAvalue
#' @return a raster.stack of the objects in the folder
#'
#' @seealso See the sample data \code{\link{egTile}} for an example, and the associated help for examples on how to generate derived data.
#' @examples
#' \dontrun{
#' # Read egTile from the provided geoTiff's; gisPath must point to package installion folder.
#' path <- system.file("extdata", "egTile", package = "testdat")
#' rData <- readTile(gisPath, layers=c('base','grnns','wetns','brtns','dem','slp','asp','hsd'))
#'
#' # Set the aspect layer to NA wherever slope is 0
#' rData$asp <- calc (rData$asp,fun=function(x){ x[x == -1] <- NA; x })
#' }
#' @export
readTile <- function (rasterPath, layers, labels=NULL, NAval=NULL) {
  retList <- list ()
  for (l in layers) retList <- c(retList,  raster::brick(paste0(rasterPath,l,'.tif')))
  retStack <- raster::stack(retList)

  if (!is.null(labels) && length(names(retStack))==length(labels)) names(retStack) <- labels
  if (!is.null(NAval)) raster::NAvalue(retStack) <- NAval
  return (retStack)
}

##### extractPoints #####
#' Extracts the raster data corresponding to each point in the vector dataset.
#'
#' This function looks up each point in a raster (point) dataset and returns the corresponding data from a raster.* object using
#' sp::SpatialPoints. It is used to build combined remote-sensed and field data that can be input into a classification model.
#'
#' @param rData the raster data as a raster.* object; can be read in using \code{\link{readTile}}
#' @param vData the point data as a data frame or a SpatialPointsDataFrame, i.e. read in with maptools::readShapePoints
#' @param locs the columns which contain the locations\ldots c(easting, northing)
#' @param na.omit (optional) remove NA points from the dataset? defaults to TRUE
#' @return a data frame with the combined data: the original vector data with the extracted raster data columns appended
#' @seealso \code{\link{readTile}} for reading in a collection of geoTiff's, \code{\link{egTile}} for more information on the raster format
#'   and some examples on generating derived raster layers, and \code{\link{siteData}} for an example of the output of this function. See
#'   also \code{\link[maptools]{readShapePoints}} for more on reading-in vector data from a shapefile.
#'
#' @examples
#' library (maptools)
#' vData <- readShapePoints (system.file("extdata/Plots", "Plots.shp", package = "NPEL.Classification"))
#' siteData <- extractPoints(egTile,vData,c('EASTING','NORTHING'))
#' detach ('package:maptools',unload=T)
#' @export
extractPoints <- function (rData, vData, locs, na.omit=T) {
  spPoints <- sp::SpatialPoints(vData[,c(locs[1],locs[2])], proj4string=rData@crs)
  pixel <- raster::extract(rData, spPoints)
  if (is.vector(pixel)) pixel <- as.matrix(pixel)     # If rData is a layer it returns a vector; convert it to a matrix so apply will work
  valid <- apply (pixel,1,function(x){sum(x!=0, na.rm=T)})
  valid[!valid & !na.omit] <- 1
  valid <- valid!=0
  cbind(as.data.frame(vData)[valid,],pixel[valid,])
}

##### writeTile #####
#' Compute and output a landscape
#'
#' This function renders a landscape, which means taking a model and a set of raster layers (stack or brick), then dropping all the pixels
#' through the model to get outputs (see details), and writing the result to a file. Since many raster.* objects are too big to (reasonably)
#' hold in memory, the landscape is broken into blocks and processed in series similar to \code{\link[raster]{calc}}.
#'
#' Given the variety of inputs and outputs that could be used, this function (attempts) to streamline the process for the user by providing
#' a consistent interface, dealing with at least some of the differences internally. Various conditions that influence what type and class
#' of data are returned are: the model being used, whether the data is categorical or continuous, and of course, user preference.
#'
#' Model packages/types:
#' \itemize{
#'   \item Random Forest models are probabilistic---that is, they output the class that has the largest number of occurences in each
#'   individual tree. Hence they can output a wide variety of different statistics.
#'   \item Nearest Neighbour algorithms, by their very nature do not generate class probabilities; they output information about the nearest
#'   existing data-point (in phase space). Hence, the option to output probabilities is disabled for the two primitive nearest-neighbour
#'   packages (fnn, class).
#   However, the kknn package implements a kernel function that transforms nearest-neighbour distance to probability; hence, probability can
#   be output for models generated using this package.
#'   \item The GBM package reports probabilities, so if desired, this can be used as a measure of certainty.
#' }
#'
#' Input data type: for continuous data it is meaningful to talk about the probability of a class occuring, especially in contrast to the
#' probability of another class occuring. Of course with continuous data, this doesn't make sense; a model generated on continuous data
#' (that is a regression model) can only report it's best estimate of the continuous variable at each point.
#'
#' User preference: given those particulars, the user can choose what output they might want:
#' \itemize{
#'   \item For continuous data there isn't much choice... use the 'class' option.
#'   \item For categorical data it depends on whether the package/algorithm allows for probabilities. \emph{Assuming} that these are
#'     generated the following output options are offered:
#'     \itemize{
#'       \item \sQuote{\code{class}} will output the \sQuote{winning} class, that is, the class with the highest probability of occurrence.
#'       \item \sQuote{\code{prob}} the probability with which the \sQuote{winning} class won.
#'       \item \sQuote{\code{threshold}} if we don't trust the classes with \sQuote{winning} probabilities below a certain threshold, then
#'         this can be selected and classes that don't meet this requirement will be set to 0 in the output map.
#'       \item \sQuote{\code{all}} output all the probability for all the classes as a multi-layer map. Note that this can be an
#'         exceptionally large datafile--- as in the order of many gigabytes. Plan accordingly.
#'     }
#' }
#'
#' @section Warning: Runtimes may be long! On a 2015 iMac (Intel I5 quad-core 3.3 GHz, 8GB RAM, and SSD) it takes 12-15 hours to do a full
#' rendering of approximately 15k x 15k pixels (=225 Mpix). Also, output file sizes can be large(ish) --- ~2+ GB.
#'
#' @param model the model to use for predicting output values, often created using \code{\link{generateModels}}.
#' @param inRdata a raster stack (or brick) of the input data; can be loaded using \code{\link{readTile}}
#' @param outFilename the output filename
#' @param layers (optional) one or more of:
#'   \itemize{
#'     \item \code{\sQuote{class}} the chosen class---also use for continuous data;
#'     \item \code{\sQuote{prob}} the probability of this class;
#'     \item \code{\sQuote{threshold}} same as class where probability is greater than or equal to threshold (below), otherwise 0;
#'     \item \code{\sQuote{all}} the probabilities for all classes---one layer per class\ldots see Details.
#'    }
#' @param threshold (optional) this needs to be provided if \code{\sQuote{threshold}} output is selected: \eqn{0 <
#'   Th < 1}.
#' @param labels.all (optional) if 'all' is selected, specify labels for these layers; the default label is "prob.<class name>"
#' @param ... variable(s) to pass to the model prediction function.
#' @return a new raster.brick object pointing to \file{outFilename} containing the output landscape.
#'
#' @seealso \code{\link{writeTiles}} for an automated way to render all models in a model block
#' @examples
#' models <- generateModels (siteData, suppModels, x=c('brtns','grnns','wetns','dem','slp','asp','hsd'), y='ecoType')
#' fName <- paste0(dirname(tempfile()),'/Tmp_rfsrc.tif')
#' writeTile (models$rfsrc, egTile, fName, layers='class')
#' writeTiles (list(models$rfsrc), egTile, base='/Tmp_', path=dirname(fName), layers=c('class','prob','threshold','all'), threshold=0.5)
#' unlink (fName)
#'
#' \dontrun{
#' # Example of real code with the option of cropping to a specific area
#' data ('Output/siteData.dat')
#'
#' # Choose a model with specified groupings to load
#' type <- c('Full','Reduced')[2]                          # Select either full or reduced grouping
#' load (paste0('Output/',type,' Models/identity.dat'))
#' load (paste0('Output/',type,' Models/domSpecies.dat'))
#' load (paste0('Output/',type,' Models/domGroup.dat'))
#' load (paste0('Output/',type,' Models/MaxGranularity.dat'))
#' rm (type)
#'
#' inRdata <- rData
#' # Choose a block and crop if desired
#' IB_1 <- maptools::readShapePoly('Input/Intensive Blocks/IB_1')
#' IB_2 <- maptools::readShapePoly('Input/Intensive Blocks/IB_2')
#' IB_3 <- maptools::readShapePoly('Input/Intensive Blocks/IB_3')
#' IB_4 <- maptools::readShapePoly('Input/Intensive Blocks/IB_4')
#' IB_5 <- maptools::readShapePoly('Input/Intensive Blocks/IB_5')
#' IB_6 <- maptools::readShapePoly('Input/Intensive Blocks/IB_6')
#' inRdata <- crop (inRdata,extent(IB_3))
#'
#' # Output a tile--be sure the filename matches the parameters! There is no other check!!
#' writeTile(modelRun$gbm,
#'           inRdata,
#'           paste0(gisPath,'Renderings/identity_full_gbm_IB3.tif'),
#'           layers=c('class','prob'))
#' writeTiles(modelRun,
#'            egTile,
#'            'Renderings/identity_reduced_', gisPath, '_IB3.tif',
#'            layers=c('class'))
#' }
#' @export
writeTile <- function (model, inRdata, outFilename, layers=c("class"), threshold=0, labels.all=NULL, ...) {
  # Parameter checks and setup
  layers <- layers[layers %in% c('class','prob','threshold','all')]
  if (isCont(model)) {
    if (!'class' %in% layers) layers <- c('class',layers)
    if (length(layers) > 1) {
      warning("writeTile: continuous data can only output the class layer; ignoring all others.")
      layers <- 'class'
    }
  }

  if ('threshold' %in% layers && !'class' %in% layers) { warning("writeTile: class must be generated to use threshold; adding class to output."); layers <- c('class',layers) }
  if ('threshold' %in% layers && !'prob'  %in% layers) { warning("writeTile: prob must be generated to use threshold; adding prob to output.");   layers <- c('prob',layers)  }
  if ('threshold' %in% layers && (threshold<=0 || threshold>=1)) stop ("writeTile: threshold must be specified between but not equal to 0 and 1.")

  if ( pmatch ('fnn',class(model),nomatch=0) ) {
    if (length(layers) > 1 || !'class' %in% layers) warning ("writeTile: Cannot generate anything other than class output for fnn.* models. Setting output to 'class' only.")
    layers = 'class'
  }

  if (file.access(dirname(outFilename),2)) stop(paste0("writeTile: Not able to write to the specified folder: ",dirname(outFilename)))

  n.all <- 0
  if ('all' %in% layers) {
    n.all <- length(getClasses(model))
    if (is.null(labels.all)) labels.all <- paste0('prob.',getClasses(model))
    layers <- layers['all' != layers]
  } else labels.all <- NULL                             # Don't let bad user input confuse the titles
  if (length(labels.all) != n.all) stop("writeTile: number of labels provided does not match the number of output classes.")

  # Create the shell of a raster.brick for holding output
  outImage <- raster::brick(inRdata, values=FALSE, nl=length(layers)+n.all)
  names (outImage) <- c(layers,labels.all)
  outImage <- raster::writeStart(outImage, filename=outFilename, format='GTiff', overwrite=T)
  bS <- raster::blockSize(inRdata)

  # Inner loop: step through each data block, run that data through our classifier (model), generate the required layers, and output the results to a datafile.
  predictor <- buildPredict(model)
  pb <- txtProgressBar (0,bS$n,style=3)
  for (i in 1:bS$n) {
    prob <- predictor(model, as.data.frame(raster::getValues(inRdata,bS$row[i],bS$nrows[i])), ...)
    if (isCont(model)) {
      outData <- list(prob)
    } else {
      if( pmatch ('fnn',class(model),nomatch=0) ) { outData <- factorValues(prob) } # ??? can get rid of this as prob2class will return the same while throwing an error...
      else {
        #      outClass <- as.numeric(colnames(prob)[ apply(prob, 1, which.max) ])
        outData <- list ( (if ('class' %in% layers) outClass <- factorValues(prob2class(prob))),
                          (if ('prob' %in% layers) maxProb <- apply(prob, 1, max)),
                          (if ('threshold' %in% layers) { threshClass <- outClass; threshClass[maxProb < threshold] <- 0; threshClass }),
                          (if (n.all > 0) prob) )
      }
    }
    outImage <- raster::writeValues (outImage, matrix(unlist(outData),ncol=raster::nlayers(outImage),byrow=F), bS$row[i])
    setTxtProgressBar (pb,i)
  }
  close (pb)
  outImage <- raster::writeStop(outImage)
  return (outImage)
}

##### writeTiles #####
#' Write multiple output tiles
#'
#' A utility function to output all the possible model renderings model list. It builds the output filename as follows: <path><base><model class><ext>
#'
#' Sometimes it is desirable to render all the models in a block (a list of models), for example for comparison---this function performs
#' that task. It is a wrapper that loops through all models and calls \code{\link{writeTile}} with a generated filename to which to
#' render.

#' @section Warning:
#' Runtimes may be long! On an 2015 iMac (Intel I5 quad-core 3.3 GHz, 8GB RAM, and SSD) it takes 12-15 hours to do a full rendering of a
#' single tile approximately 15k x 15k pixels (=225 Mpix). Also, output file sizes can be large(ish)---~2+ GB... and this is only for a
#' single tile. It is recommended that writeTiles be reserved for outputing comparison tiles of a smaller size, or subsampled data.
#'
#' @param models a list of model names, the standard output from \code{\link{generateModels}}
#' @param inRdata is the image from which to generate the rendering, which can be read in using \code{\link{readTile}}
#' @param base the base portion of the filename
#' @param path (optional) the path of the folder; defaults to './' the current directory
#' @param extension (optional) the latter part of the filename; defaults to '.tif'
#' @param echo (optional) should the function report its progress; defaults to TRUE
#' @param ... other parameter(s) to pass on to \code{\link{writeTile}}, e.g. threshold, labels, et.c
#' @return Invisibly returns the time it took each model output to execute
#'
#' @seealso \code{\link{writeTile}} for the engine that does the rendering
#' @examples
#' # See the examples at writeTile.
#' @export
writeTiles <- function (models, inRdata, base, path='./', extension='.tif', echo=T, ...) {
  for (m in models) {
    fName <- paste0(path, base, class(m)[[1]], extension)
    if (echo) print (paste0('Writing: ',fName,' ...'))
    invisible(system.time( writeTile(m, inRdata, fName,...) ))
  }
}

##### impute #####
#' Impute data from a NN model
#'
#' This function takes an input raster, that is, an image on a categorical variable, and generates an image (or images) of other data using
#' the input raster as a key or index variable. It is used to render maps of variables that are otherwise unsuitable using Nearest Neighour
#' algorithms such as continuous variables.
#'
#' It is perhaps easiest to explain the concept of imputation using an example: consider the case where the input raster represents the ID
#' of the nearest neighbour to that pixel; it is possible to impute environmental data by looking up each ID in the original dataset and
#' assigning that pixel the value (of the environmental variable) from that site. So if this pixel is nearest (in phase space) to site
#' No.153, then we can infer (impute) that it is also most likely to have similar environmental characteristics. This is significantly
#' better than generating a map of classes, then inferring values from the mean of the class as there is a huge amount of information lost
#' in mapping \code{N} sites to \code{k} classes.
#'
#' This function is essentially similar to the SQL/database command JOIN; that is, it joins two groups of data using a common column, such
#' that every time a value \code{y} occurs in the first table, some or all of the addition columns in the second table \code{x} are appended
#' to the result. It is a glorified form of lookup table in which the vector of lookup values is all the pixels in the image.
#'
#' Of course it is, in principle, possible to use this function to impute data that has been generated by some other type of model, however,
#' the other methods included in this package are all able to generate continuous variable output directly. Imputation has only the benefit
#' that it is possible to produce multiple output from a single rendering simply by imputing a different (suite of) variable(s).
#'
#' @section Warning:
#' In an effort to streamline usage, this function will attempt to coerce non-numeric data into something that can be written using the \link{raster} package.
#' To this end, if the data is found to be other than numeric, it is converted to numeric using the command
#' \code{as.numeric(factor(x))}, which, as has been observed before in this documentaiton returns the \emph{indicies} of the factors. It
#' \emph{should} be possible to recover the values of the indicies using this same typecast, however, there is a risk that there could be
#' some glitch or error, and a mismapping could result between factor indicies and actual values.
#'
#' \bold{It would be \emph{much} safer} to do your own typecast \bold{\emph{before}} passing the data to \code{impute}! 'Nuf said...
#'
#' @param inRdata a raster* object with the spatial data---typcially a map of indecies to the lookup table.
#' @param iData or imputation data; the lookup table that takes map values (e.g. \code{siteID}) and converts them to some other value, e.g. a site
#'   characteristic such as cover, tree density, species composition, etc.
#' @param outFilename a file to hold the output resulting raster object.
#' @param fx (optional) a one-sided formula specifying which column(s) in \code{iData} to output; will either be computed from x if omited, or
#'   assume to be all columns in \code{iData}.
#' @param x (optional) a list of columns from \code{iData} to output; will be computed from fx if omited.
#' @param y (optional) the pivot column that connects the model and the imputation data; if neither fx nor this is specified then it is
#'   assumed to be the first column in \code{iData}.
#'
#' @return a raster.brick of the imputed data with as many layers as specified.
#' @seealso
#'   \code{\link{factor}} and \code{\link{ecoGroup}} for more information on the factor index gotcha.
#'   \code{\link{generateModels}}, and \code{\link{writeTile}} for more information on building models for imputation purposes.
#' @export
#'
#' @examples
#' iData <- cbind(siteID=factor(1:nrow(siteData)),siteData)
#' models <- generateModels(iData, suppModels[!suppModels == contModels][1], x=c('brtns','grnns','wetns','dem','slp','asp','hsd'), y='siteID')
#'
#' fNN <- paste0(dirname(tempfile()),'/Tmp_nn.tif')
#' egData <- writeTile (models[[1]], egTile, fNN, layers='class')
#'
#' fImpute <- paste0(dirname(tempfile()),'/Tmp_nnImpute.tif')
#' egImpute <- impute (egData, iData, fImpute, formula('siteID ~ ecoType + bedrockD + parentMaterial'))
#' plot (egImpute)
#'
#' unlink (fNN)
#' unlink (fImpute)
impute <- function(inRdata, iData, outFilename, fx=NULL, x=NULL, y=NULL) {
  if (file.access(dirname(outFilename),2)) stop(paste0("impute: Not able to write to the specified folder: ",dirname(outFilename)))
  fx2vars(fx, x, y, names(iData))
  if (class(inRdata) %in% c('RasterStack','RasterBrick')) inRdata <- inRdata$y  # Just in case this was a multilayer raster
  iData <- sapply(iData[,c(y,x)], function(x){
    if ('numeric' %in% class(x)) return (x);
    return (as.numeric(factor(x)))
  })

  outImage <- raster::brick(inRdata, values=FALSE, nl=length(x))
  names(outImage) <- x
  outImage <- raster::writeStart(outImage, filename=outFilename, format='GTiff', overwrite=T)
  bS <- raster::blockSize(inRdata)
  pb <- txtProgressBar (0,bS$n,style=3)
  for (i in 1:bS$n) {
    outData <- as.vector(raster::getValues(inRdata,bS$row[i],bS$nrows[i]))
    outImage <- raster::writeValues(outImage, iData[outData,x], bS$row[i])      # The only real working line...
    setTxtProgressBar(pb,i)
  }
  close(pb)
  outImage <- raster::writeStop(outImage)
  return (outImage)
}

