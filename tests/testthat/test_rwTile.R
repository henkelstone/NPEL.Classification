# Automated testing of 'rwTile.R' in the NPEL package
# Created by Jonathan Henkelman 4.Feb.2016

library(NPEL.Classification)
context("rwTile Functions")
verbose = FALSE
cat('\n')

##### Setup testing environment #####
data('egTile')
data('siteData')
fName <- paste0(system.file("extdata", "Plots", package = "NPEL.Classification"),'/')
vData <- maptools::readShapePoints (paste0(fName,'Plots'))

##### readTile and extractPoints #####
test_that("readTile", {
  fName <- paste0(system.file("extdata", "egTile", package = "NPEL.Classification"),'/')
  expect_true(!is.null( readTile(fName, c('base','grnns','wetns','brtns','dem','slp','asp','hsd')) ))
})

test_that("extractPoints", {
  expect_true(!is.null( extractPoints(egTile, vData, c('EASTING','NORTHING')) ))                  # Should work
  expect_true(!is.null( extractPoints(egTile[[1]], vData, c('EASTING','NORTHING')) ))             # Should work on a layer
  expect_true(!is.null( extractPoints(raster::brick(egTile), vData, c('EASTING','NORTHING')) ))   # Should work on a brick
  expect_true(!is.null( extractPoints(egTile, as.data.frame(vData), c('EASTING','NORTHING')) ))   # Should work with points in a data frame
})

##### Test rendering categorical models #####
models <- generateModels (
  data = NPEL.Classification::siteData,
  modelTypes = suppModels,
  x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
  y = 'ecoType',
  grouping = ecoGroup[['identity','transform']],
  nn.args = list (scale=F),
  gbm.args = list (interaction.depth=7, shrinkage=0.005, cv.folds=0) )

test_that("writeTile(s)", {
  fName <- tempfile()
  expect_error  (writeTiles (list(models$rfsrc), egTile, base='/Tmp_', path='/Oops',         layers=c('all'), labels.all=c('a','b'), echo=verbose), regexp="writeTile: Not able to write to the specified folder.")
  expect_error  (writeTiles (list(models$rfsrc), egTile, base='/Tmp_', path=dirname(fName),  layers=c('class','prob','threshold'), echo=verbose),   regexp="writeTile: threshold must be specified between but not equal to 0 and 1.")
# expect_warning(writeTiles (list(models$rfsrc), egTile, base='/Tmp_', path=dirname(fName),  layers=c('prob', 'threshold'), threshold=0.5, echo=verbose),regexp="writeTile: class must be generated to use threshold; adding class to output.")
# expect_warning(writeTiles (list(models$rfsrc), egTile, base='/Tmp_', path=dirname(fName),  layers=c('class','threshold'), threshold=0.5, echo=verbose),regexp="writeTile: prob must be generated to use threshold; adding prob to output.")
# expect_warning(writeTiles (list(models$fnn.FNN), egTile, base='/Tmp_', path=dirname(fName),layers=c('class','prob'), threshold=0.5, echo=verbose),     regexp="writeTile: Cannot generate anything other than class output for fnn.* models. Setting output to 'class' only.")
  expect_error  (writeTiles (list(models$rfsrc), egTile, base='/Tmp_', path=dirname(fName),  layers=c('class','prob','threshold'),   echo=verbose), regexp="writeTile: threshold must be specified between but not equal to 0 and 1.")
  expect_error  (writeTiles (list(models$rfsrc), egTile, base='/Tmp_', path=dirname(fName),  layers=c('all'), labels.all=c('a','b'), echo=verbose), regexp="writeTile: number of labels provided does not match the number of output classes.")
  expect_true (is.null(writeTiles (list(models$rfsrc), egTile, base='/Tmp_', path=dirname(fName), layers=c('class','prob','threshold','all'), threshold=0.5, echo=T))) # Should work
  unlink (paste0(dirname(fName),'/Tmp_rfsrc.tif'))
  unlink (paste0(dirname(fName),'/Tmp_fnn.FNN.tif'))
})


test_that("impute", {
  fNN <- paste0(dirname(tempfile()),'/Tmp_nn.tif')
  fImpute <- paste0(dirname(tempfile()),'/Tmp_nnImpute.tif')
  iData <- cbind(siteID=factor(1:nrow(siteData)),siteData)
  models <- generateModels(iData, suppModels[!suppModels %in% contModels][2], x=c('brtns','grnns','wetns','dem','slp','asp','hsd'), y='siteID')
  egData <- writeTile (models[[1]], egTile, fNN, layers='class')

  expect_error(impute (egData, iData, './Oops/Oops.dat', formula('siteID ~ ecoType + bedrockD + parentMaterial')), regexp="impute: Not able to write to the specified folder")
  egImpute <- impute (egData, iData, fImpute, formula('siteID ~ ecoType + bedrockD + parentMaterial'))
  unlink (fNN)
  unlink (fImpute)
})

##### Test rendering continuous models #####
  models <- generateModels (
    data = NPEL.Classification::siteData,
    modelTypes = contModels,
    x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
    y = 'easting',
    nn.args = list (scale=F),
    gbm.args = list (interaction.depth=7, shrinkage=0.005, cv.folds=0) )

test_that("writeTile(s)", {
  fName <- tempfile()
  expect_warning(writeTiles (list(models$rfsrc), egTile, base='/Tmp_', path=dirname(fName), layers=c('class','prob','threshold'), echo=verbose), regexp="writeTile: continuous data can only output the class layer; ignoring all others.")
  expect_true (is.null(writeTiles (list(models$rfsrc), egTile, base='/Tmp_', path=dirname(fName), layers=c('class'), echo=verbose))) # Should work
  unlink (paste0(dirname(fName),'/Tmp_rfsrc.tif'))
})
