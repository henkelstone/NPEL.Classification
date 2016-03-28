# Functions used to plot tiles and output
# Created 9.Oct.2015 from pre-existing code file started 6.Apr.2015

##### plotTile.base #####
#' Creates a base ggplot object of the raster object
#'
#' @param data a raster* object to plot
#' @param layers (optional) the layers to include as facets
#' @param title (optional) a title for the plot
#' @param maxpixels (optional) the maximum number of pixels to output
#' @param reduction (optional) is a reducing scale factor applied to both x and y axis; will use whichever is less: ncell/(reduction^2) or maxpixels
#' @param ... other parameters for ggplot
#' @return a ggplot object which can have geoms, scales, etc. added to it (see PlotTile).
#' @export
plotTile.base <- function(data, layers=NULL, title="", maxpixels=500000, reduction=1, ...) {
  if (!requireNamespace('ggplot2')) stop("ggplot2 package required for plotting")
  if (!is.null(layers)) data <- subset(data,layers)

  # Theres a glitch in sampleRegular: we can't just simply ask for sR(data, number of cells, xy=T) as it doesn't return the same values for xy as if we
  # do it step by step: x <- sR(data,#cells,xy=F,raster=T); xyFromCell(x,1:#cells). I don't know why this is, but it causes artifacts in the final plot
  # so it is necessary to do the work around shown here... It just means we have to extract the data twice so it's a bit slower.
  colNames <- names(data)
  data <- raster::sampleRegular(data, size=min(raster::ncell(data)/(reduction*reduction),maxpixels),asRaster=T)   # Return a raster so we can extract the coords afterwards.
  data <- data.frame(raster::xyFromCell(data,1:raster::ncell(data)), raster::getValues(data))
  names(data) <- c('x','y',colNames)
  dat <- reshape(data=data,direction='long',idvar=1:2,varying=3:dim(data)[2],v.names='value',timevar='type',times=names(data)[3:dim(data)[2]]) # Massage into long form; over a GB for a full tile!

  # Create a base plot object with some useful aesthetics and that holds the data; then if this is not the only plot the user wants, they should be able to output more without needing to resample the data
  return( ggplot2::ggplot(ggplot2::aes(x = 'x', y = 'y'), data = 'dat', ...) +
            ggplot2::theme(axis.text.y=ggplot2::element_text(angle=90,hjust=0.5)) + ggplot2::coord_equal() +
            ggplot2::labs(title=title,x='Latitude (UTM)',y='Longitude (UTM)',fill='Value') )
}

##### plotTile #####
#' Given a ggplot base object create a finished plot
#' @param gp the base ggplot to which to add a colour scale and aesthetic (see plotTile.base)
#' @param layers a character vector of the levels which to plot
#' @param discrete force the plot to attempt a discrete fill axis
#' @param colours specify the colours use; a list if discrete is TRUE, or, 2 or 3 colours for the gradient if discrete is FALSE
#' @param labels if desired, specify the names of the labels for each class
#' @param ... other parameters to pass
#' @return the ggplot object with additional aes and scale
plotTile <- function(gp, layers, discrete, colours, labels=NULL,...){

  # Note: ggplot saves the data in the object which both allows this, but also makes for a large object... like over a GB for a full tile -- I recommend against
  # saving the R database on exit without being sure you have time for this file to save/load.

  if (!requireNamespace('ggplot2')) stop("ggplot2 package required for plotting")
  if (discrete) { if (is.null(colours)) stop ("plotTile: if discrete is specified, so must be colours") }
  else          { if (is.null(colours) || !sum(length(colours) == c(2,3))) stop("plotTile: either 2 or 3 colours need to be specified for continuous gradient scales") }

  gp <- ggplot2::`%+%`(gp, subset(gp$data,gp$data$type %in% layers))   # Subset the data. Necessary to do it this way so we don't have to specify the subset to every item, i.e. scale, geom, facet, etc.
  if (length(list(...))) gp <- gp + ...                   # Add any extra parameters the user may have supplied
  if (discrete) {                                         # If it's discrete then the user should have supplied the colours
    if (is.null(labels)) return( gp + ggplot2::aes(fill='factor(value)') + ggplot2::geom_raster() + ggplot2::scale_fill_manual(values=colours) )
    else                 return( gp + ggplot2::aes(fill='factor(value)') + ggplot2::geom_raster() + ggplot2::scale_fill_manual(values=colours, labels=labels) )
  } else {
    if (length (colours) == 2) return( gp + ggplot2::aes(fill='value') + ggplot2::geom_raster() + ggplot2::scale_fill_gradient (low=colours[1], high=colours[2]) + ggplot2::facet_wrap('~type') )
    if (length (colours) == 3) return( gp + ggplot2::aes(fill='value') + ggplot2::geom_raster() + ggplot2::scale_fill_gradient2(low=colours[1], mid=colours[2], high=colours[3], midpoint=(max(gp$data$value,na.rm=T)-min(gp$data$value,na.rm=T))/2) + ggplot2::facet_wrap('~type') )
  }
}

##### plotTiles #####
#' Outputs multiple plots of multiple datafiles into a single folder
#' @param path the path of the folder
#' @param base the base part of the filename
#' @param extension the filename 'extension'; may contain more than the strict extension
#' @param models a list of model names
#' @param type (optional) type out output to use
#' @param ... other parameters to pass to the device function
plotTiles <- function(path, base, extension, models, type='pdf',...) {
  for (fType in models) {
    fName <- paste0(path,base,fType,extension)
    Tile <- raster::brick(paste0(fName,'.tif'))
    if (raster::nlayers(Tile) > 1) { names(Tile) <- c('Class', 'Prob', paste0('Prob.',1:(raster::nlayers(Tile)-2))) } else { names(Tile) <- c('Class') }

    if (type=='png') png(paste0(fName,'.png'),...)
    else pdf(paste0(fName,'.pdf'),width=10.0,height=7.5,onefile=T)

    gp <- plotTile.base(Tile, layers=names(Tile), maxpixels=1000000)
    plotTile(gp, layers='Class', discrete=T, colours=NPEL.Classification::ecoGroup[['domSpecies','colours']]) #,aes(alpha='Prob'))
    if (fType != 'fnn') plotTile(gp, layers=c('Prob',paste('Prob',1:7,sep='.')), discrete=F, colours=c('grey17','red','yellow'))
    #    Sys.sleep(15)
    dev.off(dev.cur())
  }
}
