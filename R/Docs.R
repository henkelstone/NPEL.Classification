# Miscellaneous documentation that didn't fit elsewhere
# Created 9.Oct.2015 from pre-existing code file started 6.Apr.2015

##### Constants #####
#' Package Constants
#'
#' Global constants that define the bounds of this packages functionality
#'
#' @section Constants:
#' \itemize{
#'   \item \code{suppModels}: supported models---the model types (packages) this package currently supports.
#'   \item \code{probModels}: probabilistic models---models types that will return probabilities when created on categorical data.
#'   \item \code{contModels}: continuous models---models that can be used (natively) with continuous variables. Of course other
#'     classification algorithms, for example nearest neighbour, can be run on an index variable then continuous data can then be
#'     \code{\link[=impute]{imputed}} from this.
#' }
#'
#' @usage suppModels, probModels, contModels
#' @format A list of models that are supported. These represent the internal codes used by this package.
#' @name Constants
#' @docType data
#' @aliases suppModels probModels contModels
#'
#' @examples
#' \dontrun{
#' suppModels <- c('randomForest','rfsrc','fnn.FNN','fnn.class','kknn','gbm')  # Which packages (model classes) are supported by NPEL.Classification
#' probModels <- c('randomForest','rfsrc','fnn.FNN','fnn.class','kknn','gbm')  # Which packages generate probabilities when passed categorical data
#' contModels <- c('randomForest','rfsrc','kknn','gbm')                               # Which packages are able to handle continuous data
#' }
NULL

##### NPEL.Classification #####
#' For aiding in the processing and classification of remote sensed data, and rendering of imputed maps
#'
#' @section Functions:
#' This package aims to aid and simplify the following tasks:
#' \itemize{
#'   \item reading and writing multilayer raster TIFs (relying heavily on the raster package)
#'   \item sampling locational data from these rasters, i.e. extracting raster data for field sites
#'   \item grouping/lumping classes for reduced or simplified analysis, e.g. to increase sample size in each class
#'   \item streamlining building classification models from several packages
#'   \item to aid in the analysis of bundles of models, particularly accuracy metrics but also a limited VIMP metric
#'   \item streamlining the rendering of output rasters derived from these classifiers and the input rasters
#'   \item to provide plotting of these data
#'   \item to allow extensibility of the above functionality
#'   \item to provide education and examples for this type of analysis
#' }
#'
#' These tasks fall into six main groups with the following functions associated with each task:
#' \enumerate{
#' \item Read in a raster
#'   \itemize{
#'     \item \code{\link{readTile}} -- read a collection of TIF files from a folder and compile them into a single raster.stack.
#'   }
#' \item Extract data from the raster
#'   \itemize{
#'     \item \code{\link{extractPoints}} -- given a collection of spatial points (package:maptools), extract the raster
#'       data under them. This is allows the construction of a model linking site characteristics--most notably
#'       ecoSite--to remote sense variables.
#'   }
#' \item Generate Models
#'   \itemize{
#'     \item \code{\link{generateModels}} -- given some data and a list of model types, create a list of models. This returns a
#'       list of models, which together can be treated as a whole using many of the analytical functions.
#'   }
#' \item Assess model accuracy and variable importance
#'   \itemize{
#'     \item \code{\link{npelVIMP}} -- generate variable importance data for a model. This was developed as a way of
#'       finding VIMP for nearest neighbour models but has been expanded to generate VIMP data for all models included in
#'       this package using the same leave-one-out technique.
#'     \item \code{\link{npelVIF}} -- compute the variable inflation factor for a model.
#'     \item \code{\link{classAcc}} -- compute the accuracies for a specified model: class accuracies for categorical data, and R-squared
#'       for regression models.
#'     \item \code{\link{modelAccs}} -- report the accuracies and VIMP data for a list of models.
#'     \item \code{\link{validate}} -- validate a specified model, that is, use a validation dataset to determine accuracy: class accuracies
#'       for categorical data, and R-squared for regression models.
#'     \item \code{\link{modelsValid}} -- validate a list of models.
#'     \item \code{\link{nnErrMap}} -- produce a map of nearest neigbour distances or errors.
#'   }
#' \item Render Output
#'   \itemize{
#'     \item \code{\link{writeTile}} -- generate a output raster(s) given a collection of input rasters and a single model.
#'     \item \code{\link{writeTiles}} -- generate a collection output raster given a input rasters and a list of models.
#'     \item \code{\link{impute}} -- create a map of a variable based via a lookup table and second map.
#'   }
#' \item Visualize the results
#'   \itemize{
#'     \item \code{\link{plotTile}} -- plot a single model; doesn't work yet???
#'     \item \code{\link{plotTiles}} -- plot a list of models; doesn't work yet???
#'   }
#' }
#'
#' @section Sample Data:
#' A small selection of data has been included in the package for didactic and testing purposes:
#' \itemize{
#'   \item \code{\link{egTile}} -- a sample tile comprising an .rda file linked to a (small) collection of tifs
#'   \item \code{\link{siteData}} -- a (small) subset of site data
#'   \item \code{\link{ecoGroup}} -- an example transformation 'function' including labels and suggested colours
#'   \item \code{\link{water}} -- an example of a water mask
#' }
#'
#' @section Utility functions and Constants:
#' There are several other common tasks that this package aims to streamline:
#' \itemize{
#' \item A few utilities function encapsulating common tasks:
#'   \itemize{
#'     \item \code{\link{sortLevels}} -- sort the levels of a factor so they are in order
#'     \item \code{\link{trimLevels}} -- trim the levels of a factor so only levels that appear in the variable are present
#'     \item \code{\link{mergeLevels}} -- merge the levels of two factor variables
#'     \item \code{\link{factorValues}} -- as outlined in the warnings section of the help file for factors
#'       (\code{?factor}) there is a common gotcha when dealing with factors: converting numerical factors using
#'       \code{as.numeric} returns the factor \emph{indices} not the values as expected.
#'     \item \code{\link{rad2deg}} -- convert radians to degrees; for slope, aspect, hillshade etc.
#'     \item \code{\link{deg2rad}} -- convert degrees to radians; for slope, aspect, hillshade etc.
#'     \item \code{\link{fx2vars}} -- convert a formula object to lists of names of x and y, and vice versa.
#'     \item \code{\link{prob2class}} -- convert a matrix of probabilities into a factor of classes; each column is taken to represent a
#'       different class and each row is a different datapoint.
#'   }
#' \item Object Oriented access to model internals: The various modelling packages that are used by \pkg{NPEL.Classification} all have
#' different ways of storing their internals, nor do they all store the \emph{same} data. In a few cases, the existing package did not even
#' wrap their models in classes; this has been done in this package so access to the models can be standardized through S3 overloading.
#' These OOP methods clean up the situation by providing a common interface for all the data, and in some cases generating the data when
#' necessary.
#'   \itemize{
#'     \item \code{\link{isCat}} -- was the model built with categorical data
#'     \item \code{\link{isCont}} -- was the model built with continuous data
#'     \item \code{\link{getData}} -- the data used to build this model.
#'     \item \code{\link{getClasses}} -- the list of classes present in this model.
#'     \item \code{\link{getProb}} -- the probability matrix from a model built with categorical data; if the parent package doesn't
#'       natively support probabilities, a matrix will be generated in which the selected class has probability=1 and the others are 0.
#'     \item \code{\link{getFormula}} -- the formula used to generate this model.
#'     \item \code{\link{getArgs}} -- the arguments specific to this model type used when building this model.
#'     \item \code{\link{getFitted}} -- the fitted data from the original dataset.
#'     \item \code{\link{getVIMP}} -- variable importance data; generated for some model types.
#'     \item \code{\link{buildModel}} -- a single interface for building a model from any of the supported packages; see the documentation
#'       for more information on how the desired model type is passed to the function.
#'     \item \code{\link{buildPredict}} -- build a function that can be used to predict new values for this model; again this function
#'       standardizes the interface across all supported model types.
#'   }
#' \item Global Package Constants: These constants define the scope of this package with respect to the models/packages it uses.
#'   \itemize{
#'     \item \code{\link{suppModels}} -- these are the model types that NPEL.Classification currently supports.
#'     \item \code{\link{probModels}} -- these are the model types that can give probability outputs when built on categorical values.
#'     \item \code{\link{contModels}} -- these are the model types that (natively) support continuous variables.
#'   }
#' }
#'
#' @section Future Expansion:
#' While this section is more theoretical, a word on adding other modelling packages to this package. Given the object oriented (OOP) nature
#' of the implementation, adding packages \emph{should} be a task comprising adding the relevant OOP code; that is, add a relevant function
#' for every overloaded function in the package. At the time of this writing, all of these functions can be found in the file
#' \code{OOP_util.R}.
#'
#' Of course, return values need to be consistent with existing expectations. It is also necessary to update the global constants
#' which show which packages are supported: see \code{\link[NPEL.Classification:Constants]{Constants}} for more information.
#'
#' And finally, thorough testing... Testing code could be added to the testing suite, but it could also remain outside the package if the
#' new functionality is only to be used locally. For that matter, extra OOP code for expanding the functionality need not be added to the
#' package, but could remain external as long as the correct overloading are used!
#'
#' Good luck and I hope this work does what you need it to do...
#'
#' @section Warning:
#' NPEL.Classification must be \emph{before} \pkg{raster} on the search path; in particular \code{getData} is a valid function in both
#' packages. If you are getting very unusual errors, consider detaching and reattaching this package so it is found first.
#'
#' @name NPEL.Classification
# @docType package
#' @seealso
#' The code in this package depends heavily on the \pkg{\link[raster]{raster}} package. A couple of functions utilize
#' \pkg{\link[maptools]{maptools}}.
#'
#' Currently supported modelling packages are: \pkg{\link[randomForest]{randomForest}}, \pkg{\link[randomForestSRC:rfsrc]{randomForestSRC}},
#' \pkg{\link[FNN:knn.cv]{FNN}}, \pkg{\link[class:knn.cv]{class}}, \pkg{\link[kknn:train.kknn]{kknn}}, and \pkg{\link[gbm]{gbm}}.
NULL

##### ecoGroup #####
#' An example of a transformation 'function'
#'
#' This dataset is a list, cast in matrix format, that can be used to lump ecotypes into coarser groups for analysis and
#' plotting. The current implementation includes vectors for: transformation, naming/labelling, and suggested plotting
#' colours
#'
#' This built-in transformation 'function' encodes several different grouping scenarios. Each is provided with labels
#' for the groups as well as suggested colours. Following is a description of the four build-in encoding for the FEC
#' classification system \cite{McLaughlan, M. S., Robert A. Wright, and R. D. Jiricka. Field Guide to the Ecosites of
#' Saskatchewan's Provincial Forests. Prince Albert, Sask: Ministry of Environment, 2010.}
#'
#' \tabular{lccc}{
#' \bold{Ecotype} \tab\bold{Dominant Species Class} \tab\bold{Dominant Group Class} \tab\bold{Maximum Granularity Class}\cr
#'   BS 01 -- Sand heather \tab 1 (barren) \tab 1 (barren) \tab 1 (sparse vegetation)\cr
#'   BS 02 -- Lichen felsenmeer \tab 1 \tab 1 \tab 1\cr
#'   BS 03 -- Jack pine, blueberry, lichen \tab 2 (pine) \tab 2 (conifer) \tab 2\cr
#'   BS 04 -- Jack pine, black spruce, feathermoss \tab 2 \tab 2 \tab 3\cr
#'   BS 05 -- Jack pine, birch, feathermoss \tab 2 \tab 2 \tab 4\cr
#'   BS 06 -- Jack pine, aspen, alder \tab 2 \tab 2 \tab 5\cr
#'   BS 07 -- Black spruce, blueberry, lichen \tab 3 (bs) \tab 2 \tab 6\cr
#'   BS 08 -- Black spruce, birch, lichen \tab 3 \tab 2 \tab 7\cr
#'   BS 09 -- Black spruce, pine, feathermoss \tab 3 \tab 2 \tab 8\cr
#'   BS 10 -- Black spruce, birch, feathermoss \tab 3 \tab 2 \tab 9\cr
#'   BS 11 -- White spruce, fir, feathermoss \tab 4 (ws) \tab 2 \tab 10\cr
#'   BS 12 -- White spruce, crowberry, feathermoss \tab 4 \tab 2 \tab 11\cr
#'   BS 13 -- Birch, black spruce, aspen \tab 5 (birch) \tab 3 (decid) \tab 12\cr
#'   BS 14 -- Birch, lingonberry, lab tea \tab 5 \tab 3 \tab 13\cr
#'   BS 15 -- Aspen, birch, alder \tab 6 (aspen) \tab 3 \tab 14\cr
#'   BS 16 -- Black spruce, poplar, alder swamp \tab 7 (swamp) \tab 4 (wetland) \tab 15 (group swamp in with treed bog)\cr
#'   BS 17 -- Black spruce bog \tab 8 (bog) \tab 4 \tab 15\cr
#'   BS 18 -- Lab tea shrubby bog \tab 8 \tab 4 \tab 16\cr
#'   BS 19 -- Graminoid bog \tab 8 \tab 4 \tab 17 (sparse bog)\cr
#'   BS 20 -- Open bog \tab 8 \tab 4 \tab 17\cr
#'   BS 21 -- Tamarack fen \tab 9 (fen) \tab 4 \tab 18 (upright fen)\cr
#'   BS 22 -- Leatherleaf fen \tab 9 \tab 4 \tab 18\cr
#'   BS 23 -- Willow shrubby fen \tab 9 \tab 4 \tab 19\cr
#'   BS 24 -- Graminoid fen \tab 9 \tab 4 \tab 20 (sparse fen)\cr
#'   BS 25 -- Open fen \tab 9 \tab 4 \tab 20\cr
#'   BS 26 -- Rush sandy shore \tab 10 (shore) \tab 4 \tab 21 (shoreline)\cr
#'   BS 27 -- Sedge rocky shore \tab 10 \tab 4 \tab 21\cr
#' }
#'
#' @docType data
#' @aliases ecoGroup
#' @usage ecoGroup[[which.grouping,type.of.data]]
#' @param which.grouping one of \code{c('identity','domSpecies','domGroup','maxGranularity')}, see Format.
#' @param type.of.data one of \code{c('transform','labels','colours')}, see Format.
#'
#' @format A list cast into matrix format with four rows (grouping scenarios) and three columns (possible data types for
#'   that scenario). The four different grouping scenarios are:
#' \itemize{
#'   \item \bold{Identity} (\sQuote{\code{identity}}) maps the class onto itself, that is, there is no lumping, but
#'     labels and colours are provided.
#'   \item \bold{Dominant Species} (\sQuote{\code{domSpecies}}) groups classes by the dominant (tree) species.
#'   \item \bold{Dominant Group} (\sQuote{\code{domGroup}}) groups classes by the dominant growth-form.
#'   \item \bold{Maximum Granularity} (\sQuote{\code{maxGranularity}}) groups the classes in what is deemed reasonable
#'     for our data, that is, we attempt to maximize the number of groupings while maintaining a reasonable number of
#'     samples in each group for statistical significance and reliability.}
#'
#' The different types of data that can be extracted are:
#' \itemize{
#'   \item\bold{Transformation Vector} (\sQuote{\code{transform}}) a transformation vector converting input factors
#'     (ecoSites) to output factors (groupings). Functionally it is a lookup table that maps input classes to output
#'     classes. See \sQuote{examples} for how to use it.
#'   \item\bold{Labels} (\sQuote{\code{labels}}) meaningful names for the output groups.
#'   \item\bold{Plotting Colours} (\sQuote{\code{colours}}) suggested plotting colours for the output groups. These
#'     are easy to pass to, for example, plotting functions.}
#'
#' @section Warning:
#' The following expressions will \emph{not} work as transformation functions. See \code{\link{factor}}, especially the \code{Warning}
#' section for more information on this gotcha.
#'
#' \code{siteData$Ecotype} returns a factor list, not a numeric list representing the factors.
#'
#' \code{ecoGroup[['identity','transform']][siteData$Ecotype]} will regroup based on the factor level \emph{indices}, NOT the factors.
#'
#' \code{as.numeric(siteData$Ecotype]} returns the factor \emph{indices}, not a numeric representation of the factors.
#'
#' \code{(1:27)[siteData$Ecotype]} returns the factor \emph{index} rather than the ecoGroup.
#'
#' @examples
#' data (siteData)
#' ecoGroup[['domSpecies','transform']][ factorValues(siteData$ecoType) ]
#'
#' ## Code for generating this object--so you can make your own!
#' # Create a shell to fill with data
#' ecoGroup <- list(); length(ecoGroup) <- 12;
#' dim(ecoGroup) <- c(4,3)               # Three rows (classifiers), three columns (type of data)
#' rownames(ecoGroup)=c('identity','domSpecies','maxGranularity','domGroup')
#' colnames(ecoGroup)=c('transform','labels','colours')
#'
#' # Fill with data in increasing order of lumping
#' ecoGroup['identity',] <- list(
#'   c(1:27),
#'   paste0('BS',1:27),
#'   c('#FFFF00','#E6E600','#FF9900','#E47A07','#BD6A06','#965B05','#4C7300','#5E8D00','#70A800',
#'     '#82C300','#008C4B','#00A04B','#ABFF8F','#42FF07','#7EFF54','#41DBCF','#00A884','#3FC5A5',
#'     '#7EE2C6','#BEFFE8','#97E2FF','#8DD4F0','#83C6E1','#79B8D2', '#6FABC3','#AC44E7','#C077E3')
#' )
#' ecoGroup['maxGranularity',] <- list(
#'   c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,15,16,17,17,18,18,19,20,20,21,21),
#'   c('Sparse',paste0('BS',3:15),'Treed bog/swamp','BS18','Sparse bog','Upright fen','BS23','Sparse fen','Shoreline'),
#'   c('#E6E600','#FF9900','#E47A07','#BD6A06','#965B05','#4C7300','#5E8D00','#70A800','#82C300',
#'     '#008C4B','#00A04B','#ABFF8F','#42FF07','#7EFF54','#00A884','#3FC5A5','#BEFFE8','#97E2FF',
#'     '#83C6E1','#6FABC3','#C077E3')
#' )
#' ecoGroup['domSpecies',] <- list(
#'   c(1,1,2,2,2,2,3,3,3,3,4,4,5,5,6,7,8,8,8,8,9,9,9,9,9,10,10),
#'   c('1'="Barren",'2'="Pine",'3'="Black Sp",'4'="White Sp",'5'="Birch",'6'="Aspen",'7'="Swamp",
#'     '8'="Bog",'9'="Fen",'10'="Shore"),
#'   c('E6E600','E47A07','70A800','00A04B','42FF07','41DBCF','3FC5A5','83C6E1','C077E3','004DA8','000000')
#' )
#' ecoGroup['domGroup',] <- list(
#'   c(1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4),
#'   c("Barren","Conifer","Decid","Wetland"),
#'   c('E6E600','00A04B','42FF07','83C6E1')
#' )
"ecoGroup"

##### siteData #####
#' An example of site data; this is the portion of the data collected in-house.
#'
#' This data frame contains the site data collected in-house, with the corresponding remote sensed data, both Landsat and DEM and their
#' respective derived categories. See the details section for more information on each included variable.
#'
#' In order to build classification models the NPEL collected field data. This vector (point) data was overlaid on the raster data, and the
#' value at each point (site) was extracted. This yielded a dataset of sites with both field characteristics and remote sensed data. This dataset
#' is the basis for the model building in this package. What is included here is a subset both of variables and sites that can be used for
#' learning and testing this package. The sites are only those collected in-house at the NPEL; because our project is done in collaboration with
#' several other agencies we combined their data with ours to produce a larger dataset that spanned more geographic area and more ecosite types.
#' Many of the variable s collected are not relevant to building models (e.g. plot number, date, etc.) and have been pruned from this example. Also
#' several of the remote sensed layers did not prove to be sufficiently linearly independent enough to warrant inclusion in the model; several
#' redundant variables are included here for the purposes of demonstrating dimensional analysis but some have been trimmed in order to keep the
#' example manageable.
#'
#' \itemize{
#'   \item \code{block} the block it was sampled under, a component of the sampling regime: c('LA','B1','B2',...,'B6') -- large area, blocks 1..6
#'   \item \code{landClass} the pre-sampling classification of this site, i.e. the stratum it was sampled under
#'   \item \code{GPSzone} UTM zone of this site; 13 for all our sites
#'   \item \code{easting} UTM easting of this site
#'   \item \code{northing} UTM northing of this site
#'   \item \code{year} the year the field site was visited
#'   \item \code{slope} field recorded slope of the site, useful to compare with DEM data
#'   \item \code{aspect} field recorded aspect of the site, useful to compare with DEM data
#'   \item \code{ecoType} the ecotype the site was classed as
#'   \item \code{base.1:7} Landsat base layers
#'   \item \code{grnns, wetness, brtns} the so-called \sQuote{Tasseled-Cap} variables; they reflect greenness, wetness, and brightness respectively
#'   \item \code{dem} the basic elevations from the digital elevation model
#'   \item \code{slp} slope, as derived from the DEM
#'   \item \code{asp} aspect, as derived from the DEM; note that different algorithms use different values for NA, a reliable filter is that aspect is NA when slope == 0
#'   \item \code{hsd} hillshade, a measure of how much light the surface receives -- derived from the DEM
#' }
#'
#' @docType data
#' @name siteData
#' @usage siteData$'variables'
#' @param variables is the variable to extract from the dataset
#' @format A data frame with columns as variables and rows as sites.
#' @source
#' \itemize{
#'   \item \bold{Landsat data}: available from the U.S. Geological Survey
#'   \item \bold{DEM data}: available from the U.S. Geological Survey National Elevation Dataset
#' }
#' @seealso See \code{\link{extractPoints}} for a function to extract raster data for these sites, and \code{\link{generateModels}} for model building.
#' @examples
#' library (NPEL.Classification)
#' data(siteData)
#' names(siteData)
#'
#' siteData$ecoType
#' siteData[,'ecoType']
#' siteData[,c('easting','northing')]
#'
#' modelRun <- generateModels (data = siteData,
#'                             modelTypes = suppModels,
#'                             fx = formula('ecoType ~ brtns+grnns+wetns+dem+slp+asp+hsd'),
#'                             grouping = ecoGroup[['domSpecies','transform']])
#'
#' \dontrun{
#' vData <- maptools::readShapePoints ('Input/Plots/Plots')         # Read in from a ESRI shapefile
#' vData$FEC <- as.factor(substr(as.character(vData$FEC),3,6))      # Extract the FEC number
#' names (vData) <- c('objID','plotNum','block','type','landClass','ecoRegion','GPSzone','etc..') }
"siteData"

##### egTile #####
#' An example tile for debugging and didactic purposes
#'
#' Given that it can be a challenge to build a raster dataset there is a mini dataset included with the package. Due to size limitations it
#' does not contain very many layers and is limited to only 100 x 100 pixels. However, it is sufficient to illustrate the data format
#' expected by the package, and it allows for new users to \sQuote{play} with the package. It also facilitates testing of code because it is
#' so small and code executes relatively quickly.
#'
#' Given the limitations imposed by the need to have a portable library (read: small enough to easily distribute on over the internet), this
#' raster is both small and has only a few layers. The layers included are those that fell out of our dimensional analysis as being the most
#' significant of the Landsat and DEM data. The raw Landsat data is included so that users could generate other derived layers if they
#' wished; code is provided in the example section. Following is a list of included raster variables:
#'
#' \itemize{
#'   \item \code{base_1:7} the base Landsat layers
#'   \item \code{grnns, wetness, brtns} the so-called \sQuote{Tasseled-Cap} variables; they reflect greenness, wetness, and brightness respectively
#'   \item \code{dem} the basic elevations from the digital elevation model
#'   \item \code{slp} slope, as derived from the DEM
#'   \item \code{asp} aspect, as derived from the DEM; note that different algorithms use different values for NA, a reliable filter is that aspect is NA when slope == 0
#'   \item \code{hsd} hillshade, a measure of how much light the surface receives -- derived from the DEM
#' }
#'
#' @note Note that to date, the aspect computed in the terrain package does not match that generated by ArcGIS. At the time of writing
#'   (15.Feb.2015) this difference has still not been resolved. The same issue applies to hillshade (hsd). As ever, use any variable with
#'   care, and the understanding that it may be flawed.
#'
#' @docType data
#' @aliases egTile
#' @usage egTile$variable
#' @param variable is the layer to extract from the raster
#' @format A 100 x 100 grid containing a few useful Landsat and DEM derived layers
#' @seealso \code{\link{raster}} package
#' @source
#' \itemize{
#'   \item \bold{Landsat data}: available from the U.S. Geological Survey
#'   \item \bold{DEM data}: available from the U.S. Geological Survey National Elevation Dataset
#' }
#' @examples
#' library (NPEL.Classification)
#' library (raster)
#' data(egTile)
#' summary (egTile)
#' gisPath = './'
#'
#' # Create meaningful aliases for the Landsat layer names
#' blue <- egTile$'base.2'
#' green <- egTile$'base.3'
#' red <- egTile$'base.4'
#' NIR <- egTile$'base.5'
#' SWIR1 <- egTile$'base.6'
#' SWIR2 <- egTile$'base.7'
#'
#' # Generate Landsat derived data
#' ndvi <- (NIR - red)/(NIR + red)
#' ndwi <- (green - NIR)/(green + NIR)
#' evi <- 2.5 * (NIR-red)/(1+NIR + 6*red + 7.5*blue)
#' mavi <- 0.5 + NIR - 0.5*sqrt( (1+2*NIR)^2 - 8*(NIR-red) )
#' nred <- NIR/red
#' ngrn <- NIR/green
#' grnns <- -0.1603*blue - 0.2819*green - 0.4934*red + 0.7940*NIR - 0.0002*SWIR1 - 0.1446*SWIR2
#' brtns <-  0.2043*blue + 0.4158*green + 0.5524*red + 0.5741*NIR + 0.3124*SWIR1 + 0.2303*SWIR2
#' wetns <-  0.0315*blue + 0.2021*green + 0.3102*red + 0.1594*NIR - 0.6806*SWIR1 - 0.6109*SWIR2
#'
#' LandSAT <- stack (ndvi, ndwi, evi, mavi, nred, ngrn, grnns, brtns, wetns)
#' names (LandSAT) <- c('ndvi','ndwi','evi','mavi','nred','ngrn','grnns','brtns','wetns')
#' rm (blue, green, red, NIR, SWIR1, SWIR2, ndvi, ndwi, evi, mavi, nred, ngrn, grnns, brtns, wetns)
#' \dontrun{writeRaster (LandSAT,filename=paste0(gisPath,'LandSAT'), overwrite=TRUE)}
#'
#' # Generate DEM derived data using built in functions
#' slope <-  terrain(egTile$dem, opt='slope', unit='degrees')
#' aspect <- terrain(egTile$dem, opt='aspect', unit='degrees')
#' aspect[slope < 1e-10] <- NA
#' aspect[abs(aspect-360) < 1e-10] <- 0
#' TPI <-    terrain(egTile$dem, opt='TPI')
#' TRI <-    terrain(egTile$dem, opt='TRI')
#' rough <-  terrain(egTile$dem, opt='roughness')
#' flow <-   terrain(egTile$dem, opt='flowdir')
#' hsd <-    hillShade(deg2rad(slope), deg2rad(aspect), angle=53.3, direction=199.8, normalize=TRUE)
#'
#' # Generate DEM derived data manually to match ArcGIS output
#'  RI <- focal(egTile$dem, w=matrix(1, nrow=3, ncol=3),
#'              fun=function(x){sqrt( sum((x-x[5])^2,na.rm=TRUE)/8 )}, pad=TRUE, padValue=NA)
#' # SRR <- focal(egTile$dem, w=matrix(1, nrow=3, ncol=3),    # Cute but slow so do it the long way
#' #              fun=function(x){ m <- min(x); (mean(x)-m)/(max(x)-m) }, pad=TRUE, padValue=NA)
#' eMin <- focal(egTile$dem, w=matrix(1, nrow=3, ncol=3), fun=min, pad=TRUE, padValue=NA)
#' eMax <- focal(egTile$dem, w=matrix(1, nrow=3, ncol=3), fun=max, pad=TRUE, padValue=NA)
#' eAvg <- focal(egTile$dem, w=matrix(1/9, nrow=3, ncol=3), pad=TRUE, padValue=NA)
#' SRR <- (eAvg-eMin) / (eMax-eMin)
#' SRR[abs(eMax-eMin) < 0.0001] <- NA
#' rm (eMin, eMax, eAvg)
#'
#' DEM <- stack (slope, aspect, TPI, TRI, rough, SRR, RI, flow, hsd)
#' names (DEM) <- c('slp','asp','TPI','TRI','rough','srr','ri','flow','hsd')
#' rm (slope, aspect, TPI, TRI, rough, SRR, RI, flow, hsd)
#' \dontrun{writeRaster (DEM,filename=paste0(gisPath,'DEM'), overwrite=TRUE)}
#' detach('package:raster',unload=TRUE)
"egTile"

##### Water #####
#' An example of a water mask
#'
#' Since it is unlikely to have field sites situated on water, most classification models are not well equipped to handle water areas.
#' Typically this is handled by simply ignoring them when building the model. However, depending on the post-rendering analysis, it can be
#' useful to mask them out. This \code{Raster.Layer} is a sample water mask for the sample dataset provided \code{\link{egTile}}. See the
#' examples section for how to use this mask.
#'
#' @docType data
#' @name water
#' @usage water
#' @format A Raster.Layer object with an example water mask: values of 1 indicate water, values of 128 indicate non-water.
#' @seealso See \code{\link{writeTile}} and \code{\link{writeTiles}} for examples of rendering.
#' @examples
#' data(egTile)
#' data(water)
#' plot (egTile)
#' plot (raster::mask(egTile,water,maskvalue=1,updatevalue=NA))
"water"
